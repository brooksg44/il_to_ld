(ns il-to-ld.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [il-to-ld.core :as core]
            [il-to-ld.il-parser :refer [il-parser transform-il]]
            [il-to-ld.ld-parser :as ld-parser]
            [instaparse.core :as insta]
            [clojure.string :as str]))

(deftest parse-il-test
  (testing "Basic IL parsing"
    (let [il-code "LD X0\nAND X1\nST Y0"]
      (is (not (insta/failure? (il-parser il-code)))))))

(deftest transform-il-test
  (testing "Transform IL to intermediate representation"
    (let [il-code "LD X0\nAND X1\nST Y0"
          parsed (il-parser il-code)
          transformed (transform-il parsed)]
      (is (= (count transformed) 3))
      (is (= (:operator (first transformed)) "LD"))
      (is (= (:operand (first transformed)) "X0")))))

(deftest compile-il-to-ld-test
  (testing "Compile IL to LD"
    (let [il-code "LD X0\nAND X1\nST Y0"
          ld-xml (core/compile-il-to-ld il-code)]
      (is (string? ld-xml))
      (is (.contains ld-xml "<contact name=\"X0\"")))))

(deftest compile-ld-to-il-test
  (testing "Compile LD to IL"
    (let [ld-xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ladder>
<network>
  <contact name=\"X0\" />
  <contact name=\"X1\" />
  <coil name=\"Y0\" />
</network>
</ladder>"
          il-code (core/compile-ld-to-il ld-xml)]
      (is (string? il-code))
      (is (.contains il-code "LD X0"))
      (is (.contains il-code "AND X1"))
      (is (.contains il-code "ST Y0")))))

(deftest bidirectional-compilation-test
  (testing "Bidirectional compilation IL -> LD -> IL"
    (let [original-il "LD X0\nAND X1\nST Y0"
          ld-xml (core/compile-il-to-ld original-il)
          result-il (core/compile-ld-to-il ld-xml)]
      (is (= (str/trim original-il)
             (str/trim result-il))))))

(deftest branch-conversion-test
  (testing "Branch handling in IL to LD conversion"
    (let [il-code "LD X0\nAND X1\nOR X2\nST Y0"
          ld-xml (core/compile-il-to-ld il-code)]
      (is (.contains ld-xml "<branch>"))
      (is (.contains ld-xml "<contact name=\"X2\"")))))

(deftest negated-contact-test
  (testing "Negated contacts in both directions"
    (let [il-code "LD X0\nANDN X1\nST Y0"
          ld-xml (core/compile-il-to-ld il-code)]
      (is (.contains ld-xml "negated=\"true\""))

      (let [negated-ld "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ladder>
<network>
  <contact name=\"X0\" />
  <contact name=\"X1\" negated=\"true\" />
  <coil name=\"Y0\" />
</network>
</ladder>"
            result-il (core/compile-ld-to-il negated-ld)]
        (is (.contains result-il "ANDN X1"))))))

(deftest set-reset-coil-test
  (testing "Set and Reset coils"
    (let [il-code "LD X0\nS M0\nLD X1\nR M0"
          ld-xml (core/compile-il-to-ld il-code)]
      (is (.contains ld-xml "set=\"true\""))
      (is (.contains ld-xml "reset=\"true\""))

      (let [sr-ld "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<ladder>
<network>
  <contact name=\"X0\" />
  <coil name=\"M0\" set=\"true\" />
</network>
<network>
  <contact name=\"X1\" />
  <coil name=\"M0\" reset=\"true\" />
</network>
</ladder>"
            result-il (core/compile-ld-to-il sr-ld)]
        (is (.contains result-il "S M0"))
        (is (.contains result-il "R M0"))))))