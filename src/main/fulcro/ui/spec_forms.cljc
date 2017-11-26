(ns fulcro.ui.spec-forms
  (:require [clojure.spec.alpha :as s]
    #?(:clj
            [clojure.future :refer :all])
            [devcards.core :as dc :refer [defcard-doc]]
            [fulcro.client.logging :as log]
            [fulcro.client.mutations :as m :refer [defmutation]]
            [fulcro.client.core :as fc]
            [fulcro.util :as util]
            [fulcro.client.primitives :as prim :refer [defui]]
            [fulcro.client.core :refer [defsc]]
            [fulcro.client.dom :as dom]))

(s/def ::id any?)
(s/def ::fields set? #_(s/every keyword? :kind set?))
(s/def ::subforms set? #_(s/every keyword? :kind set?))
(s/def ::pristine-state map? #_(s/map-of keyword? any?))
(s/def ::complete? set? #_(s/every keyword? :kind set?))
(s/def ::form-config (s/keys :req [::id ::fields ::pristine-state] :opt [::complete? ::subforms]))
(s/def ::field-tester (s/fspec
                        :args (s/cat :ui-entity (s/keys :req [::form-config]) :field (s/? keyword?))
                        :ret boolean?))
(s/def ::form-operation (s/fspec
                          :args (s/cat :entity map? :config ::form-config)
                          :ret (s/cat :entity map? :config ::form-config)))

(defsc FormConfig [this {:keys [::id ::fields ::subforms ::pristine-state]} _ _]
  {:query [::id ::fields ::subforms ::pristine-state]
   :ident [::FORM-CONFIGS ::id]}
  (dom/div nil
    (dom/h4 nil "Form Config")
    (dom/ul nil
      (dom/li nil (str "id" id))
      (dom/li nil (str "fields" fields))
      (dom/li nil (str "subforms" subforms))
      (dom/li nil (str "pristine-state" pristine-state)))))

(def ui-form-config (prim/factory FormConfig {:keyfn ::id}))

(defn get-form-query
  "Returns the query join to the form config."
  []
  {::form-config (prim/get-query FormConfig)})

(defn build-form
  "Build an entity's initial state based on prisine-state and a set of keywords that designate the
  data that make up the form fields."
  [entity {:keys [::id ::fields ::subforms] :as form-config}]
  (let [pristine-state (select-keys entity fields)]
    (merge entity {::form-config (assoc form-config ::pristine-state pristine-state
                                                    ::subforms (or subforms #{}))})))

(s/fdef build-form
  :args (s/cat :entity map? :config ::form-config)
  :ret (s/keys :req [::form-config]))


(defn immediate-subforms
  "Get the instances of the immediate subforms that are joined into entity by
   subform-join-keys (works with to-one and to-many). Entity is a denormalized (UI) entity.

   Returns a sequence of those entities (all denormalized)."
  [entity subform-join-keys]
  (mapcat #(let [v (get entity %)]
             (if (sequential? v) v [v])) subform-join-keys))

(defn dirty?
  "Returns true if the given ui-entity-props that are configured as a form differ from the pristine version.
  Recursively follows subforms if given no field. Returns true if anything doesn't match up.

  If given a field, it only checks that field."
  ([ui-entity-props field]
   (let [{{pristine-state ::pristine-state} ::form-config} ui-entity-props
         current  (get ui-entity-props field)
         original (get pristine-state field)]
     (not= current original)))
  ([entity]
   (let [{:keys [::pristine-state ::subforms ::fields]} (::form-config entity)
         dirty-field?     (fn [k] (dirty? entity k))
         subform-entities (immediate-subforms entity subforms)]
     (boolean
       (or
         (some dirty-field? fields)
         (some dirty? subform-entities))))))

(s/def dirty? ::field-tester)

(defn unspeced-or-valid?
  "Returns true if the value is valid, or there is no spec for it."
  [spec-key value]
  (or (not (s/get-spec spec-key))
    (s/valid? spec-key value)))

(defn merge-validity
  "Returns a new validity based on the combination of two.

  * :valid :valid = :valid
  * :valid :invalid = :invalid
  * :valid :unchecked = :unchecked
  * :invalid :valid = :invalid
  * :invalid :invalid = :invalid
  * :invalid :unchecked = :unchecked
  * :unchecked :valid = :unchecked
  * :unchecked :invalid = :unchecked
  * :unchecked :unchecked = :unchecked
  "
  [a b]
  (cond
    (or (= :unchecked a) (= :unchecked b)) :unchecked
    (and (= :valid a) (= :valid b)) :valid
    :otherwise :invalid))

(defn validity
  "Returns :valid if all of the declared fields on the given ui-entity-props conform to their specs.  Returns
  :invalid if all of the fields have been interacted with, and any are not valid. Returns :unchecked if
  any field is not yet been interacted with. Fields are marked as having been interacted with either by
  or by programmatic action on your part:

  The  validate* mutation helper can be used in a mutation to mark fields ready for validation.

  If given a field then it checks just that field."
  ([ui-entity-props field]
   (let [{{complete? ::complete?} ::form-config} ui-entity-props
         complete? (or complete? #{})]
     (cond
       (not (complete? field)) :unchecked
       (not (unspeced-or-valid? field (get ui-entity-props field))) :invalid
       :else :valid)))
  ([ui-entity-props]
   (let [{{:keys [::fields ::subforms ::complete?]} ::form-config} ui-entity-props
         complete?          (or complete? #{})
         immediate-subforms (immediate-subforms ui-entity-props subforms)
         field-validity     (fn [current-validity k] (merge-validity current-validity (validity ui-entity-props k)))
         subform-validities (map validity immediate-subforms)
         subform-validity   (reduce merge-validity :valid subform-validities)
         this-validity      (reduce field-validity :valid fields)]
     (merge-validity this-validity subform-validity))))

(s/def validity ::field-tester)

(defn valid?
  "Returns true if the denormalized (UI) form is :valid (recursively). Returns false if unchecked or invalid. Use checked?
  or `validity` for better detail."
  [ui-form]
  (= :valid (validity ui-form)))

(defn checked?
  "Returns true if the denormalized (UI) form is :valid (recursively). Returns false if unchecked or invalid. Use checked?
  or `validity` for better detail."
  [ui-form]
  (not= :unchecked (validity ui-form)))

(defn invalid?
  "Returns true if the denormalized (UI) form is :valid (recursively). Returns false if unchecked or invalid. Use checked?
  or `validity` for better detail."
  [ui-form]
  (= :invalid (validity ui-form)))

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
         legal-fields     (get-in state-map (conj form-config-path ::fields) #{})
         complete-path    (conj form-config-path ::complete?)]
     (assert (legal-fields field) "Validating a declared field")
     (update-in state-map complete-path (fnil conj #{}) field)))
  ([state-map entity-ident]
   (update-forms state-map
     (fn validate*-step [e form-config]
       [e (assoc form-config ::complete? (::fields form-config))]) entity-ident)))

(s/fdef validate*
  :args (s/cat :state map? :entity-ident util/ident? :field (s/? keyword?))
  :ret map?)

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
  (update-forms state-map (fn commit-form-step [e {:keys [::fields] :as config}]
                            (let [new-pristine-state (select-keys e fields)]
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
