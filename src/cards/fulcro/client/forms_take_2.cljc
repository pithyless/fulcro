(ns fulcro.client.forms-take-2
  (:require [clojure.spec.alpha :as s]
            [devcards.core :as dc :refer [defcard-doc]]
            [fulcro.client.logging :as log]
            [fulcro.client.mutations :as m :refer [defmutation]]
            [fulcro.client.core :as fc]
            [fulcro.ui.spec-forms :as f]
            [fulcro.util :as util]
            [fulcro.client.primitives :as prim :refer [defui]]
            [fulcro.client.core :refer [defsc]]
            [fulcro.client.dom :as dom]))

(defcard-doc
  "# Fulcro 2 Forms

  Forms in Fulcro 2 are a complete rewrite from the forms support in Fulcro 1 (which can still be used).
  ")



#_(defn field-with-label
    "A non-library helper function, written by you to help lay out your form."
    [label field-name rendered-field error-message]
    (dom/div #js {:className (str "form-group" (when error-message " has-error"))}
      (dom/label #js {:className "col-sm-2" :htmlFor field-name} label)
      (dom/div #js {:className "col-sm-10"} rendered-field)
      (dom/span #js {:className (str "col-sm-offset-2 col-sm-10 " field-name
                                  (when (blank? error-message) " hidden"))} error-message)))

(s/def :db/id (s/or :temp prim/tempid? :real pos-int?))
(s/def :phone/type #{:home :work})
(s/def :fulcro.ui.spec-forms-spec/phone-number #"[-0-9.()]+")

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
