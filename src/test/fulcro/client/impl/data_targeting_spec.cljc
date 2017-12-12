(ns fulcro.client.impl.data-targeting-spec
  (:require
    [fulcro.client.data-fetch :as df]
    [fulcro.client.impl.data-targeting :as targeting]
    [fulcro.client.util :as util]
    #?@(:cljs
        [[goog.log :as glog]])
    [fulcro.client.primitives :as prim :refer [defui defsc]]
    [clojure.test :refer [is are]]
    [fulcro-spec.core :refer
     [specification behavior assertions provided component when-mocking]]
    [fulcro.client.mutations :as m :refer [defmutation]]
    [fulcro.client.logging :as log]
    [fulcro.client.impl.protocols :as omp]
    [fulcro.client :as fc]
    [fulcro.client.impl.data-fetch :as f]
    [fulcro.history :as hist]
    [fulcro.client.impl.protocols :as p]))

(specification "Special targeting"
  (assertions
    "Is detectable"
    (targeting/special-target? (df/append-to [])) => true
    (targeting/special-target? (df/prepend-to [])) => true
    (targeting/special-target? (df/replace-at [])) => true
    (targeting/special-target? (df/multiple-targets [:a] [:b])) => true)
  (component "process-target"
    (let [starting-state      {:root/thing [:y 2]
                               :table      {1 {:id 1 :thing [:x 1]}}}
          starting-state-many {:root/thing [[:y 2]]
                               :table      {1 {:id 1 :things [[:x 1] [:x 2]]}}}]
      (component "non-special targets"
        (assertions
          "moves an ident at some top-level key to an arbitrary path (non-special)"
          (targeting/process-target starting-state :root/thing [:table 1 :thing]) => {:table {1 {:id 1 :thing [:y 2]}}}
          "creates an ident for some location at a target location (non-special)"
          (targeting/process-target starting-state [:table 1] [:root/thing]) => {:root/thing [:table 1]
                                                                                 :table      {1 {:id 1 :thing [:x 1]}}}
          "replaces a to-many with a to-many (non-special)"
          (targeting/process-target starting-state-many :root/thing [:table 1 :things]) => {:table {1 {:id 1 :things [[:y 2]]}}}))
      (component "special targets"
        (assertions
          "can prepend into a to-many"
          (targeting/process-target starting-state-many :root/thing (df/prepend-to [:table 1 :things])) => {:table {1 {:id 1 :things [[:y 2] [:x 1] [:x 2]]}}}
          "can append into a to-many"
          (targeting/process-target starting-state-many :root/thing (df/append-to [:table 1 :things])) => {:table {1 {:id 1 :things [[:x 1] [:x 2] [:y 2]]}}}
          ; Unsupported:
          ;"can replace an element in a to-many"
          ;(df/process-target starting-state-many :root/thing (df/replace-at [:table 1 :things 0])) => {:table {1 {:id 1 :things [[:y 2] [:x 2]]}}}
          "can affect multiple targets"
          (targeting/process-target starting-state-many :root/thing (df/multiple-targets
                                                                      (df/prepend-to [:table 1 :stuff])
                                                                      [:root/spot]
                                                                      (df/append-to [:table 1 :things]))) => {:root/spot [[:y 2]]
                                                                                                              :table     {1 {:id     1
                                                                                                                             :stuff  [[:y 2]]
                                                                                                                             :things [[:x 1] [:x 2] [:y 2]]}}})))))
