(ns il-to-ld.il-parser
  (:require [instaparse.core :as insta]
            [instaparse.transform :as transform]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

;; Define records for LD elements
(defrecord Contact [name negated])
(defrecord Coil [name negated set reset])
(defrecord Branch [contacts])

;; Define the EBNF grammar for IL based on IEC 61131-3
(def il-grammar "
  <program> = instruction-list
  <instruction-list> = (instruction <whitespace?>)+
  instruction = label? operation operand? comment?
  label = identifier <':'>
  operation = <whitespace?> operator modifier?
  operator = 'LD' | 'ST' | 'S' | 'R' | 'AND' | 'OR' | 'XOR' | 'NOT' | 'ADD' | 'SUB' | 'MUL' | 'DIV' | 'GT' | 'GE' | 'EQ' | 'NE' | 'LE' | 'LT' | 'JMP' | 'CAL' | 'RET'
  modifier = <whitespace?> ('N' | 'C' | 'P')
  operand = <whitespace> (variable | constant)
  variable = identifier
  constant = numeric-literal | character-string
  numeric-literal = integer | real
  integer = #'[0-9]+'
  real = #'[0-9]+\\.[0-9]+'
  character-string = <'\"'> #'[^\"]*' <'\"'>
  identifier = #'[A-Za-z_][A-Za-z0-9_]*'
  comment = <whitespace> <'(*'> #'[^*]*\\*+([^)*][^*]*\\*+)*' <')'>
  whitespace = #'[ \\t\\n\\r]+'
")

;; Define the IL parser using Instaparse
(def il-parser (insta/parser il-grammar))

;; Define specs for IL elements
(s/def ::label string?)
(s/def ::operator #{"LD" "ST" "S" "R" "AND" "OR" "XOR" "NOT" "ADD" "SUB" "MUL" "DIV" "GT" "GE" "EQ" "NE" "LE" "LT" "JMP" "CAL" "RET"})
(s/def ::modifier #{"N" "C" "P"})
(s/def ::operand string?)
(s/def ::comment string?)
(s/def ::instruction (s/keys :opt-un [::label ::operator ::modifier ::operand ::comment]))
(s/def ::instruction-list (s/coll-of ::instruction))

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

;; Function to transform parsed IL to an intermediate representation
(defn transform-il [parsed]
  (transform/transform
   {:instruction (fn [& args]
                   (let [parts (apply hash-map (flatten (filter vector? args)))]
                     {:label (get parts :label)
                      :operator (get parts :operator)
                      :modifier (get parts :modifier)
                      :operand (get parts :operand)
                      :comment (get parts :comment)}))
    :label (fn [id] [:label id])
    :operation (fn [& args]
                 (let [modifier (second args)]
                   (if modifier
                     [[:operator (first args)] [:modifier modifier]]
                     [[:operator (first args)]])))
    :operator identity
    :modifier identity
    :operand (fn [arg] [:operand arg])
    :variable identity
    :constant identity
    :numeric-literal identity
    :integer identity
    :real identity
    :character-string identity
    :identifier identity
    :comment (fn [text] [:comment text])}
   parsed))

;; Convert IL instructions to LD networks
(defn il-to-ld-networks [instructions]
  (loop [instr instructions
         networks []
         current-network {:elements []}
         branches []
         accumulator nil]
    (if (empty? instr)
      (if (seq (:elements current-network))
        (conj networks current-network)
        networks)
      (let [i (first instr)
            op (:operator i)
            mod (:modifier i)
            operand (:operand i)]
        (case op
          "LD" (let [new-network {:elements [(->Contact operand (= mod "N"))]}]
                 (recur (rest instr)
                        (if (seq (:elements current-network))
                          (conj networks current-network)
                          networks)
                        new-network
                        []
                        operand))

          "ST" (let [updated-network (update current-network :elements conj (->Coil operand false false false))]
                 (recur (rest instr)
                        networks
                        updated-network
                        branches
                        operand))

          "S" (let [updated-network (update current-network :elements conj (->Coil operand false true false))]
                (recur (rest instr)
                       networks
                       updated-network
                       branches
                       operand))

          "R" (let [updated-network (update current-network :elements conj (->Coil operand false false true))]
                (recur (rest instr)
                       networks
                       updated-network
                       branches
                       operand))

          "AND" (let [updated-network (update current-network :elements conj (->Contact operand (= mod "N")))]
                  (recur (rest instr)
                         networks
                         updated-network
                         branches
                         (str accumulator " AND " operand)))

          "OR" (let [branch-contact (->Contact operand (= mod "N"))
                     updated-branches (conj branches branch-contact)
                     updated-network (if (empty? branches)
                                       ;; First OR instruction creates a branch element
                                       (update current-network :elements conj (->Branch [branch-contact]))
                                       ;; Subsequent OR instructions add to the last branch
                                       (update-in current-network [:elements (dec (count (:elements current-network)))]
                                                  (fn [last-elem]
                                                    (if (instance? Branch last-elem)
                                                      (update last-elem :contacts conj branch-contact)
                                                      last-elem))))]
                 (recur (rest instr)
                        networks
                        updated-network
                        updated-branches
                        (str accumulator " OR " operand)))

          ;; Handle other operators similarly
          (recur (rest instr)
                 networks
                 current-network
                 branches
                 accumulator))))))

;; Generate LD XML representation from networks
(defn ld-networks-to-xml [networks]
  (str/join "\n"
            (map (fn [network]
                   (str "<network>\n"
                        (str/join "\n"
                                  (map (fn [element]
                                         (cond
                                           (instance? Contact element)
                                           (format "  <contact name=\"%s\"%s />"
                                                   (:name element)
                                                   (if (:negated element) " negated=\"true\"" ""))

                                           (instance? Coil element)
                                           (format "  <coil name=\"%s\"%s%s%s />"
                                                   (:name element)
                                                   (if (:negated element) " negated=\"true\"" "")
                                                   (if (:set element) " set=\"true\"" "")
                                                   (if (:reset element) " reset=\"true\"" ""))

                                           (instance? Branch element)
                                           (str "  <branch>\n"
                                                (str/join "\n"
                                                          (map (fn [contact]
                                                                 (format "    <contact name=\"%s\"%s />"
                                                                         (:name contact)
                                                                         (if (:negated contact) " negated=\"true\"" "")))
                                                               (:contacts element)))
                                                "\n  </branch>")

                                           :else
                                           (str "  <!-- Unsupported element: " element " -->")))
                                       (:elements network)))
                        "\n</network>"))
                 networks)))

;; Main IL to LD conversion function
(defn il-to-ld [il-code]
  (let [parsed (il-parser il-code)
        transformed (transform-il parsed)
        ld-networks (il-to-ld-networks transformed)
        ld-xml (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                    "<ladder>\n"
                    (ld-networks-to-xml ld-networks)
                    "\n</ladder>")]
    ld-xml))

;; Validate IL code using specs
(defn validate-il [il-instructions]
  (s/valid? ::instruction-list il-instructions))

;; Generate explanation for validation errors
(defn explain-il-validation [il-instructions]
  (s/explain-str ::instruction-list il-instructions))