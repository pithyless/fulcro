(ns fulcro.ui.spec-forms-spec
  (:require
    #?(:clj [taoensso.timbre :as timbre])
            [fulcro.client.primitives :as prim :refer [defui]]
            [fulcro-spec.core :refer [behavior specification assertions component when-mocking provided]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [fulcro.client.core :as fc]
            [fulcro.client.logging :as log]
            [fulcro.client.mutations :as m]
            [fulcro.client.util :as uu]
            [fulcro.client.data-fetch :as df]
            [fulcro.ui.spec-forms :as f]))

(defui Locale
  static prim/IQuery
  (query [this] [:db/id ::country (f/get-form-query)])
  static prim/Ident
  (ident [this props] [:locale/by-id (:db/id props)]))

(s/def ::country keyword?)

(defui Phone
  static prim/IQuery
  (query [this] [:db/id ::locale (prim/get-query Locale)
                 (f/get-form-query)
                 ::phone-number])
  static prim/Ident
  (ident [this props] [:phone/by-id (:db/id props)]))

(s/def ::phone-number (s/and string? #(re-matches #"[-0-9()]+" %)))

(defui Person
  static prim/IQuery
  (query [this] [:db/id ::person-name
                 (f/get-form-query)
                 {::phone-numbers (prim/get-query Phone)}])
  static prim/Ident
  (ident [this props] [:person/by-id (:db/id props)]))

(s/def ::person-name (s/and string? #(not (empty? (str/trim %)))))

(specification "build-form" :focused
  (let [person      {:db/id 3 ::person-name "J.B." ::person-age 49 ::phone-numbers []}
        form        (f/build-form person {::f/id     "person-form-3"
                                          ::f/fields #{::person-name ::person-age}})
        form-config (::f/form-config form)]
    (behavior "Lays out the given initial pristine state and config."
      (assertions
        "Places a valid form-config under the ::f/form-config key"
        (s/valid? ::f/form-config form-config) => true
        (s/explain-data ::f/form-config form-config) => nil
        "Returns something that looks like the regular entity"
        (dissoc form ::f/form-config) => person
        "Includes empty subforms if none are present"
        (::f/subforms form-config) => #{}
        "Places (in pristine-state) just the original fields from the entity state that belong in the form"
        (::f/pristine-state form-config) => (select-keys person #{::person-name ::person-age})))))

(let [locale                                  (f/build-form {:db/id 22 ::country :US} {::f/id "us-locale" ::f/fields #{::country}})
      phone-numbers                           [{:db/id 2 ::phone-number "555-1212" ::locale locale} {:db/id 3 ::phone-number "555-1212"}]
      phone-number-forms                      (mapv #(f/build-form % {::f/id       (str "phone-form-" (:db/id %))
                                                                      ::f/subforms #{::locale}
                                                                      ::f/fields   #{::phone-number}}) phone-numbers)
      person                                  {:db/id 1 ::person-name "Bo" ::phone-numbers phone-number-forms}
      person-form                             (f/build-form person {::f/id       "form-1"
                                                                    ::f/subforms #{::phone-numbers}
                                                                    ::f/fields   #{::person-name}})
      validated-person                        (-> person-form
                                                (assoc-in [::f/form-config ::f/complete?] #{::person-name})
                                                (assoc-in [::phone-numbers 0 ::f/form-config ::f/complete?] #{::phone-number})
                                                (assoc-in [::phone-numbers 0 ::locale ::f/form-config ::f/complete?] #{::country})
                                                (assoc-in [::phone-numbers 1 ::f/form-config ::f/complete?] #{::phone-number}))
      state-map                               (prim/tree->db [{:the-person (prim/get-query Person)}] {:the-person person-form} true)
      validated-tree                          (fn [class form-to-validate]
                                                (as-> state-map sm
                                                  (f/validate* sm form-to-validate)
                                                  (prim/db->tree [{:k (prim/get-query class)}] sm sm)
                                                  (get sm :k)))
      person-with-incomplete-name             (assoc-in validated-person [::f/form-config ::f/complete?] #{})
      person-with-incomplete-nested-form      (assoc-in validated-person [::phone-numbers 0 ::locale ::f/form-config ::f/complete?] #{})
      person-with-invalid-name                (assoc validated-person ::person-name "")
      person-with-invalid-nested-phone-locale (assoc-in validated-person [::phone-numbers 0 ::locale ::country] "England")]

  (specification "dirty?" :focused
    (behavior "is a UI (tree) operation for checking if the form has been modified from pristine"
      (assertions
        "is false if there are no changes"
        (f/dirty? person-form) => false
        "is true if the data has changed in the top-level form"
        (f/dirty? (assoc person-form ::person-name "New name")) => true
        "is true if any subform item has changed"
        (f/dirty? (assoc-in person-form [::phone-numbers 0 ::phone-number] "555-1111")) => true
        (f/dirty? (assoc-in person-form [::phone-numbers 0 ::locale ::country] :MX)) => true
        (f/dirty? (assoc-in person-form [::phone-numbers 1 ::phone-number] "555-1111")) => true)))

  (specification "validity" :focused
    (behavior "is a UI (tree) operation for checking if the form (or fields) are valid. It:"
      (assertions
        "returns :unchecked if the fields have not been interacted with"
        (f/validity person-form) => :unchecked
        "returns :valid if all fields are complete and valid"
        (f/validity validated-person) => :valid
        "returns :unchecked if any field is not marked as complete"
        (f/validity person-with-incomplete-name) => :unchecked
        "returns :unchecked if any NESTED fields are not marked as complete"
        (f/validity person-with-incomplete-nested-form) => :unchecked
        "returns :invalid if any top-level property is invalid"
        (f/validity person-with-invalid-name) => :invalid
        "returns :invalid if any nexted property is invalid"
        (f/validity person-with-invalid-nested-phone-locale) => :invalid)))
  (specification "valid?" :focused
    (assertions
      "Returns true if validity is :valid"
      (f/valid? validated-person) => true
      "Returns false if validity is :unchecked"
      (f/valid? person-with-incomplete-nested-form) => false
      "Returns false if validity is :invalid"
      (f/valid? person-with-invalid-name) => false))
  (specification "checked?" :focused
    (assertions
      "Returns true if validity is :valid or :invalid"
      (f/checked? validated-person) => true
      (f/checked? person-with-invalid-name) => true
      (f/checked? person-with-invalid-nested-phone-locale) => true
      "Returns false if validity is :unchecked"
      (f/checked? person-with-incomplete-nested-form) => false))
  (specification "invalid?" :focused
    (assertions
      "Returns true if validity is :invalid"
      (f/invalid? person-with-invalid-name) => true
      (f/invalid? person-with-invalid-nested-phone-locale) => true
      "Returns false if validity is :unchecked"
      (f/invalid? person-with-incomplete-nested-form) => false
      "Returns false if validity is :valid"
      (f/invalid? validated-person) => false)))
