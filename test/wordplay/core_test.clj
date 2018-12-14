(ns wordplay.core-test
  (:require [clojure.test :refer :all]
            [wordplay.core :refer :all]))

(deftest all-words-test
  (testing "all-words fail."
    (is (false? (empty? (all-words "words.txt"))))
  ))

(deftest uses-letters-test
  (testing "uses-letters fail."
    (is (true? (uses-letters "shelf" "shl")))
    (is (false? (uses-letters "shelf" "shlk")))
    (is (false? (uses-letters "shelf" "shll")))
    (is (true? (uses-letters "shellfish" "shll")))
    (is (false? (uses-letters "shellfish" "shlll")))
  ))

(deftest words-that-use-test
  (testing "words-that-use fail."
    (is (words-that-use "xylem") (list "xylem" "xylems"))
  ))

(deftest word-anagrams-test
  (testing "word-anagrams fail."
    (is (word-anagrams "reset") (list "steer" "reset" "ester" "trees"))
  ))

(deftest uses-only-letters-test
  (testing "uses only letters fail."
    (is (true? (uses-only-letters "waffle" "waffle")))
    (is (true? (uses-only-letters "waffle" "wafle")))
    (is (false? (uses-only-letters "waffles" "wafle")))
  ))

(deftest words-that-use-only-test
  (testing "words-that-use-only fail."
    (is (= (list "a" "aa" "at" "att" "ta" "tat")
           (words-that-use-only "ta")))
  ))

(deftest anagram?-test
  (testing "anagram? fail."
    (is (true? (anagram? "shelf" "flesh")))
    (is (false? (anagram? "shelf" "sflesh")))
    (is (false? (anagram? "shelf" "kflesh")))
    (is (false? (anagram? "shelf" "lesh")))
  ))

(deftest no-spaces-test
  (testing "no-spaces fail."
    (is (= (no-spaces "a b c ") "abc"))
  ))

(deftest all-phrases-test
  (testing "all-phrases fail."
    (is (= (all-phrases "tat") (list "att" "tat")))
    (is (= (all-phrases "taa") (list "a at" "a ta" "at a" "ta a")))
  ))

(deftest phrase-anagrams-test
  (testing "phrase-anagrams fail."
    (is (= (phrase-anagrams "i am") (list "a mi" "am i" "i am" "i ma" "ma i" "mi a" "aim" "ami")))
  ))
