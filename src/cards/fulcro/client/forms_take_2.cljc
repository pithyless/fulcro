(ns fulcro.client.forms-take-2
  (:require [clojure.spec.alpha :as s]
            [devcards.core :as dc :refer [defcard-doc]]
            [fulcro.client.logging :as log]
            [fulcro.client.mutations :as m :refer [defmutation]]
            [fulcro.client.core :as fc]
            [fulcro.util :as util]
            [fulcro.client.primitives :as prim :refer [defui]]
    #?(:clj
            [clojure.future :refer :all])
            [fulcro.client.core :refer [defsc]]
            [fulcro.client.dom :as dom]))

(defcard-doc
  "# Fulcro 2 Forms

  Forms in Fulcro 2 are a complete rewrite from the forms support in Fulcro 1 (which can still be used).
  ")


(s/def ::id any?)
(s/def ::fields (s/every keyword? :kind set?))
(s/def ::subforms (s/every keyword? :kind set?))
(s/def ::pristine-state (s/map-of keyword? any?))
(s/def ::complete? (s/every keyword? :kind set?))
(s/def ::form-config (s/keys :req [::id ::fields ::pristine-state] :opt [::subforms]))
(s/def ::field-tester (s/fspec
                        :args (s/cat :ui-entity (s/keys :req [::form-config]) :field (s/? keyword?))
                        :ret boolean?))

(defsc FormConfig [this {:keys [::id ::fields ::subforms ::pristine-state]} _ _]
  {:query [::id ::fields ::subforms ::pristine-state]
   :ident [::config-table ::id]}
  (dom/div nil
    (dom/h4 nil "Form Config")
    (dom/ul nil
      (dom/li nil (str "id" id))
      (dom/li nil (str "fields" fields))
      (dom/li nil (str "subforms" subforms))
      (dom/li nil (str "pristine-state" pristine-state)))))

(def ui-form-config (prim/factory FormConfig {:keyfn ::id}))

(defn build-form
  "Build an entity's initial state based on prisine-state and a set of keywords that designate the
  data that make up the form fields."
  [{:keys [::id ::fields ::subforms ::pristine-state] :or {::subforms #{}} :as form-config}]
  (merge pristine-state {::form-config form-config}))

(s/fdef build-form
  :args (s/cat :config ::form-config)
  :ret (s/keys :req [::form-config]))

(defn dirty?
  "Returns true if the given ui-entity-props that are configured as a form differ from the pristine version.
  Recursively follows subforms if given no field. Returns true if anything doesn't match up.

  If given a field, it only checks that field."
  ([ui-entity-props field]
   (let [{:keys [::pristine-state]} (::form-config ui-entity-props)
         current  (get ui-entity-props field)
         original (get pristine-state field)]
     (not= current original)))
  ([entity]
   (let [{:keys [::pristine-state ::subforms ::form-fields]} (::form-config entity)
         dirty-field?     (fn [k] (dirty? entity k))
         subform-entities (map (fn [k] (get entity k)) subforms)]
     (or
       (some dirty-field? form-fields)
       (some dirty? subform-entities)))))

(s/def dirty? ::field-tester)

(defn valid?
  "Returns :valid if all of the declared fields on the given ui-entity-props conform to their specs.  Returns
  :invalid if all of the fields have been interacted with, and any are not valid. Returns :unchecked if
  any field is not yet been interacted with. Fields are marked as having been interacted with either by
  or by programmatic action on your part:

  The  validate* mutation helper can be used in a mutation to mark fields ready for validation.

  If given a field then it checks just that field."
  ([ui-entity-props field]
   (let [{{complete? ::complete?} ::form-config} ui-entity-props]
     (cond
       (not (complete? field)) :unchecked
       (not (s/valid? field (get ui-entity-props field))) :invalid
       :else :valid)))
  ([ui-entity-props]
   (let [{{form-fields ::form-fields} ::form-config} ui-entity-props]
     (reduce (fn [result k]
               (let [v (valid? ui-entity-props k)]
                 (cond
                   (= :invalid v) (reduced :invalid)        ; invalid overrides unchecked
                   (= :unchecked v) :unchecked              ; unchecked rides along unless invalid
                   (and (= :valid result) (= :valid v)) :valid
                   :otherwise result))) :valid form-fields))))

(s/def valid? ::field-tester)

(defn- get-in-following-idents
  "Get the item at the given path. If an path segment is an ident, follow that to continue the get."
  ([state-map path]
   (let [k         (first path)
         remainder (rest path)
         item      (get state-map k)
         is-ident? (util/ident? item)
         real-item (if is-ident? (get-in state-map item) item)]
     (if (seq remainder)
       (get-in-following-idents state-map (rest path) real-item)
       real-item)))
  ([state-map path starting-item]
   (let [k         (first path)
         remainder (rest path)
         item      (get starting-item k)
         is-ident? (util/ident? item)
         real-item (if is-ident? (get-in state-map item) item)]
     (if (seq remainder)
       (get-in-following-idents state-map (rest path) real-item)
       real-item))))

(defn update-forms
  "Recursively update a form and its subforms.

  state-map : The application state map
  xform : A function (fn [entity form-config] [entity' form-config']) that is passed the normalized entity and form-config,
    and must return an updated version of them. Should not affect fields that are not involved in that specific level of the form.
  starting-entity-ident : An ident in the state map of an entity that has been initialized as a form."
  [state-map xform starting-entity-ident]
  (let [entity         (get-in state-map starting-entity-ident)
        config-ident   (get entity ::form-config)
        config         (get-in state-map config-ident)
        {:keys [::subforms]} config
        [updated-entity updated-config] (xform entity config)
        subform-idents (map #(get-in state-map (conj starting-entity-ident %)) subforms)]
    (as-> state-map sm
      (assoc-in sm starting-entity-ident updated-entity)
      (assoc-in sm config-ident updated-config)
      (reduce (fn [s id]
                (assert (util/ident? id) "Subform component is normalized")
                (update-forms s xform id)) sm subform-idents))))

(s/fdef update-forms
  :args (s/cat :state map? :xform ::form-operation :ident util/ident?)
  :ret map?)

(defn validate*
  "Mark the fields complete so that validation checks will return values. Follows the subforms recursively through state,
  unless a specific field is given. NOTE: Your forms and form configs MUST be normalized (all have Idents and join queries)."
  ([state-map entity-ident field]
   (let [form-config-path (conj entity-ident ::form-config)
         form-config-path (if (util/ident? (get-in state-map form-config-path))
                            (get-in state-map form-config-path)
                            (do
                              (log/error (str "FORM NOT NORMALIZED: " entity-ident))
                              form-config-path))
         legal-fields     (get-in state-map (conj form-config-path ::form-fields) #{})
         complete-path    (conj form-config-path ::complete?)]
     (assert (legal-fields field) "Validating a declared field")
     (update-in state-map complete-path (fnil conj #{}) field)))
  ([state-map entity-ident]
   (update-forms state-map
     (fn validate*-step [e form-config]
       [e (assoc form-config ::complete? (::form-fields form-config))]) entity-ident)))

(s/fdef validate*
  :args (s/cat :state map? :entity-ident util/ident? :field (s/? keyword?))
  :ret map?)

(s/fspec ::form-operation
  :args (s/cat :entity map? :config ::form-config)
  :ret (s/cat :entity map? :config ::form-config))

(defn reset-form*
  "Copy the pristine state over top of the originating entity at the given form. When used inside of a
  mutation, be sure you target this correctly at the form, not the app state. Recursively follows subforms in
  app state. Returns the new app state map."
  [state-map entity-ident]
  (update-forms state-map (fn reset-form-step [e {:keys [::pristine-state] :as config}]
                            [(merge e pristine-state) config]) entity-ident))

(defn commit-form*
  "Overwrite the pristine state for the form tracking of the entity. This does
  no sanity checks, so you might want to ensure the thing is valid first! Recursively commits sub forms,
  and returns the updated state-map."
  [state-map entity-ident]
  (update-forms state-map (fn commit-form-step [e {:keys [::form-fields] :as config}]
                            (let [new-pristine-state (select-keys e form-fields)]
                              [e (assoc config ::pristine-state new-pristine-state)])) entity-ident))

(defmutation reset-form!
  "Reset the form from its pristine state. Requires the form's ident. See reset-form* for a function
   composable within mutations."
  [{:keys [form-ident]}]
  (action [{:keys [state]}]
    (swap! state reset-form* form-ident)))

(defmutation validate!
  "Mutation: Trigger validation for an entire (recursively) form, or just a field (optional)"
  [{:keys [entity-ident field]}]
  (action [{:keys [state]}]
    (if field
      (swap! state validate* entity-ident field)
      (swap! state validate* entity-ident))))






(defn field-with-label
  "A non-library helper function, written by you to help lay out your form."
  [label field-name rendered-field error-message]
  (dom/div #js {:className (str "form-group" (when error-message " has-error"))}
    (dom/label #js {:className "col-sm-2" :htmlFor field-name} label)
    (dom/div #js {:className "col-sm-10"} rendered-field)
    (dom/span #js {:className (str "col-sm-offset-2 col-sm-10 " field-name
                                (when (blank? error-message) " hidden"))} error-message)))

(s/def :db/id (s/or :temp prim/tempid? :real pos-int?))
(s/def :phone/type #{:home :work})
(s/def :phone/number #"[-0-9.()]+")

(defui ^:once PhoneForm
  static fc/InitialAppState
  (initial-state [this {:keys [id type number]}]
    (build-form
      {::id             (str "phone-form-" id)
       ::fields         #{:phone/type :phone/number}
       ; a set of the fields where interaction has completed (disables validation if not present)
       ::complete?      #{}
       ::subforms       #{:key-to-subform}
       ::pristine-state {:db/id id :phone/type type :phone/number number}}))
  static prim/IQuery
  (query [this] [:db/id :phone/type :phone/number (form-query PhoneForm)])
  static prim/Ident
  (ident [this props] [:phone/by-id (:db/id props)])
  Object
  (render [this]
    (let [form (prim/props this)]
      (dom/div #js {:className "form-horizontal"}
        ))))
