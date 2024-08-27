(ns prstv-doc.make
  (:gen-class)
  (:require [scicloj.clay.v2.api :as clay]))

(defn make-html [_]
  (clay/make!
   {:format [:quarto :html]
    :base-source-path "src/prstv_doc"
    ;; :show nil
    :source-path "prstv_doc.clj"}))
    ;; :clean-up-target-dir true}))

(comment
  (make-html nil)
  (clay/browse!))
