(ns il-to-ld.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [il-to-ld.il-parser :as il]
            [il-to-ld.ld-parser :as ld])
  (:gen-class))

;; Main function to compile IL to LD
(defn compile-il-to-ld [il-code]
  (il/il-to-ld il-code))

;; Function to compile LD to IL
(defn compile-ld-to-il [ld-xml]
  (ld/ld-to-il ld-xml))

(defn -main
  "Compile IL code to LD code or LD code to IL code"
  [& args]
  (if (= (count args) 3)
    (let [mode (first args)
          input-file (second args)
          output-file (nth args 2)
          input-code (slurp input-file)]
      (case mode
        "il2ld" (let [ld-code (compile-il-to-ld input-code)]
                  (spit output-file ld-code)
                  (println "IL to LD compilation successful. Output written to" output-file))
        "ld2il" (let [il-code (compile-ld-to-il input-code)]
                  (spit output-file il-code)
                  (println "LD to IL compilation successful. Output written to" output-file))
        (println "Unknown mode. Use 'il2ld' or 'ld2il'")))
    (println "Usage: lein run [il2ld|ld2il] input-file output-file")))