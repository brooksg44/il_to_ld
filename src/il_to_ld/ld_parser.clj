(ns il-to-ld.ld-parser
  (:require [clojure.xml :as xml]
            [clojure.zip :as zip]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]))

;; Define the LD grammar in EBNF - simplified for essential elements based on IEC 61131-3
(def ld-grammar "
  <ladder> = network+
  network = element+
  element = contact | coil | branch
  contact = name, negated?
  coil = name, (negated | set | reset)?
  branch = element+
  name = string
  negated = boolean
  set = boolean
  reset = boolean
")

;; Define specs for LD elements
(s/def ::name string?)
(s/def ::negated boolean?)
(s/def ::set boolean?)
(s/def ::reset boolean?)
(s/def ::contact (s/keys :req-un [::name] :opt-un [::negated]))
(s/def ::coil (s/keys :req-un [::name] :opt-un [::negated ::set ::reset]))
(s/def ::contacts (s/coll-of ::contact))
(s/def ::branch (s/keys :req-un [::contacts]))
(s/def ::element (s/or :contact ::contact
                       :coil ::coil
                       :branch ::branch))
(s/def ::network (s/coll-of ::element))
(s/def ::ladder (s/coll-of ::network))

;; Parse LD XML into a data structure
(defn parse-ld-xml [xml-str]
  (let [input-stream (io/input-stream (.getBytes xml-str))
        parsed-xml (xml/parse input-stream)]
    (zip/xml-zip parsed-xml)))

;; Extract attributes from an XML element
(defn get-attrs [element]
  (:attrs element))

;; Convert a contact to an IL instruction
(defn contact-to-il [contact prev-instr]
  (let [attrs (get-attrs contact)
        name (:name attrs)
        negated (= (:negated attrs) "true")]
    (cond
      (nil? prev-instr) [(if negated
                           (str "LDN " name)
                           (str "LD " name))]

      (= (:tag prev-instr) :contact) [(if negated
                                        (str "ANDN " name)
                                        (str "AND " name))]

      :else [(if negated
               (str "ORN " name)
               (str "OR " name))])))

;; Convert a coil to an IL instruction
(defn coil-to-il [coil]
  (let [attrs (get-attrs coil)
        name (:name attrs)
        negated (= (:negated attrs) "true")
        set (= (:set attrs) "true")
        reset (= (:reset attrs) "true")]
    (cond
      set [(str "S " name)]
      reset [(str "R " name)]
      negated [(str "STN " name)]
      :else [(str "ST " name)])))

;; Process a branch in the LD network (parallel paths)
(defn process-branch [branch prev-instr]
  (let [elements (:content branch)]
    ;; Generate OR instructions for each contact in the branch
    (mapcat (fn [element]
              (when (= (:tag element) :contact)
                (let [attrs (get-attrs element)
                      name (:name attrs)
                      negated (= (:negated attrs) "true")]
                  [(if negated
                     (str "ORN " name)
                     (str "OR " name))])))
            elements)))

;; Convert an LD element to IL instructions
(defn element-to-il [element prev-instr]
  (case (:tag element)
    :contact (contact-to-il element prev-instr)
    :coil (coil-to-il element)
    :branch (process-branch element prev-instr)
    []))

;; Process a network in the LD program
(defn network-to-il [network]
  (let [elements (:content network)]
    (loop [remaining elements
           instructions []
           prev-element nil]
      (if (empty? remaining)
        instructions
        (let [current (first remaining)
              new-instructions (element-to-il current prev-element)]
          (recur (rest remaining)
                 (concat instructions new-instructions)
                 current))))))

;; Convert the entire LD program to IL
(defn ld-to-il-list [ld-xml]
  (let [root (parse-ld-xml ld-xml)
        networks (filter #(= (:tag %) :network) (zip/children root))]
    (mapcat network-to-il networks)))

;; Main function to compile LD to IL
(defn ld-to-il [ld-xml]
  (let [il-instructions (ld-to-il-list ld-xml)]
    (str/join "\n" il-instructions)))

;; Validate LD code using specs
(defn validate-ld [ld-xml]
  (let [parsed (parse-ld-xml ld-xml)]
    (s/valid? ::ladder parsed)))

;; Generate explanation for LD validation errors
(defn explain-ld-validation [ld-xml]
  (let [parsed (parse-ld-xml ld-xml)]
    (s/explain-str ::ladder parsed)))