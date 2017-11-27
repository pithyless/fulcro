(ns fulcro.client.forms-state-cards
  (:require [clojure.spec.alpha :as s]
            [devcards.core :as dc :refer-macros [defcard-doc]]
            [fulcro.client.cards :refer [defcard-fulcro]]
            [fulcro.client.logging :as log]
            [fulcro.ui.elements :as ele]
            [fulcro.client.mutations :as m :refer [defmutation]]
            [fulcro.client.core :as fc]
            [fulcro.ui.form-state :as f]
            [fulcro.ui.bootstrap3 :as bs]
            [fulcro-css.css :as css]
            [fulcro.util :as util]
            [fulcro.client.primitives :as prim :refer [defui]]
            [fulcro.client.core :refer [defsc]]
            [fulcro.client.dom :as dom]
            [fulcro.ui.form-state :as fs]
            [garden.core :as g]))

(defcard-doc
  "# Fulcro Forms State Management

  Forms in Fulcro 2 are a complete rewrite from the forms support in Fulcro 1 (which can still be used).
  ")

(declare Root PhoneForm)

(defn render-field [component field renderer]
  (let [form         (prim/props component)
        entity-ident (prim/get-ident component form)
        id           (str (first entity-ident) "-" (second entity-ident))
        is-dirty?    (f/dirty? form field)
        clean?       (not is-dirty?)
        validity     (f/validity form field)
        is-invalid?  (= :invalid validity)
        value        (get form field "")]
    (renderer {:dirty?   is-dirty?
               :ident    entity-ident
               :id       id
               :clean?   clean?
               :validity validity
               :invalid? is-invalid?
               :value    value})))

(defn input-with-label
  "A non-library helper function, written by you to help lay out your form."
  ([component field field-label validation-string input-element]
   (render-field component field
     (fn [{:keys [invalid? id dirty?]}]
       (bs/labeled-input {:error           (when invalid? validation-string)
                          :id              id
                          :warning         (when dirty? "(unsaved)")
                          :input-generator input-element} field-label))))
  ([component field field-label validation-string]
   (render-field component field
     (fn [{:keys [invalid? id dirty? value invalid ident]}]
       (bs/labeled-input {:value    value
                          :id       id
                          :error    (when invalid? validation-string)
                          :warning  (when dirty? "(unsaved)")
                          :onBlur   #(prim/transact! component `[(f/validate! {:entity-ident ~ident
                                                                               :field        ~field})])
                          :onChange #(m/set-string! component field :event %)} field-label)))))

(s/def ::phone-number #(re-matches #"\(?[0-9]{3}[-.)]? *[0-9]{3}-?[0-9]{4}" %))

(defmutation submit-phone [{:keys [id]}]
  (action [{:keys [state]}]
    (swap! state f/commit-form* [:phone/by-id id])))

(defsc PhoneForm [this {:keys [:db/id ::phone-type ::phone-number :ui/dropdown] :as props} _ _]
  {:query     [:db/id ::phone-type ::phone-number {:ui/dropdown (prim/get-query bs/Dropdown)} (f/get-form-query)]
   :ident     [:phone/by-id :db/id]
   :css       [[:.modified {:color :red}]]
   :protocols [static fc/InitialAppState
               (initial-state [c {:keys [id type number]}]
                 (f/build-form {:db/id         id
                                :ui/dropdown   (bs/dropdown :phone-type "Type" [(bs/dropdown-item :home "Home") (bs/dropdown-item :work "Work")])
                                ::phone-type   type
                                ::phone-number number}
                   {::f/id "phone-form" ::f/fields #{::phone-number ::phone-type}}))]}
  (let [{:keys [hidden]} (css/get-classnames Root)]
    (dom/div #js {:className "form"}
      (input-with-label this ::phone-number "Phone:" "10-digit phone number is required.")
      (input-with-label this ::phone-type "Type:" ""
        (fn [attrs]
          (bs/ui-dropdown dropdown
            :stateful? true
            :onSelect (fn [v] (m/set-value! this ::phone-type v)))))
      (bs/button {:onClick #(prim/transact! this `[(submit-phone {:id ~id})])} "Commit!"))))

(def ui-phone-form (prim/factory PhoneForm {:keyfn :db/id}))

(defn style-element
  "Returns a React Style element with the (recursive) CSS of the given component. Useful for directly embedding in your UI VDOM."
  [component]
  (dom/style (clj->js {:dangerouslySetInnerHTML {:__html (g/css (css/get-css component))}})))

(defsc Root [this {:keys [:ui/react-key :root/phone]} _ _]
  {:query         [:ui/react-key {:root/phone (prim/get-query PhoneForm)}]
   :css           [[:.hidden {:display "none"}]]
   :css-include   [PhoneForm]
   :initial-state {:root/phone {:id 1 :type :home :number "555-1212"}}}
  (ele/ui-iframe {:frameBorder 0 :width 500 :height 200}
    (dom/div #js {:key react-key}
      (dom/link #js {:rel "stylesheet" :href "bootstrap-3.3.7/css/bootstrap.min.css"})
      (style-element Root)
      (ui-phone-form phone))))

(defcard-fulcro form-state-card-1
  Root
  {}
  {:inspect-data true})
