; Copyright (c) Nicolas Buduroi. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 which can be found in the file
; epl-v10.html at the root of this distribution. By using this software
; in any fashion, you are agreeing to be bound by the terms of this
; license.
; You must not remove this notice, or any other, from this software.

(ns lobos.connectivity
  (:refer-clojure :exclude [defonce])
  (:require [clojure.java.jdbc :as sqlint]))

(in-ns 'clojure.java.jdbc)
(let [old *db*] (def ^{:private false :dynamic true} *db* old))
(let [old get-connection] (def get-connection (fn [& args] (apply old args))))
(def find-connection* find-connection)
(def connection* connection)
