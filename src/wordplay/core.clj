(ns wordplay.core
    (:require [clojure.string :as str]))

(defn all-words
  "Reads the entire word list and returns a collection."
  ([]
    (all-words "words.txt"))
  ([path]
    (str/split-lines (slurp path)))
  )

(defn uses-letters
  "Checks if a word w uses each letter in string letters.  Letters that appear n times in letters should appear at least n times in word in order to return true."
  [word letters]
  (if (empty? letters)
    true
    (let [replaced (str/replace-first word (first letters) "")]
      (if (= word replaced)
        false
        (recur replaced (rest letters)))))
  )

(defn words-that-use
  "Returns all words in wordlist that use letters."
  [letters]
  (filter #(uses-letters % letters) (all-words))
  )

(defn anagram?
  "Checks if a word uses all letters exactly as many times as they appear in letters."
  [word letters]
  (and (uses-letters word letters)
       (= (count word) (count letters))))

(defn word-anagrams
  "Returns all words in wordlist that use only these letters."
  [letters]
  (filter #(anagram? % letters) (all-words))
  )

(defn uses-only-letters
  "Checks if word uses only letters that appear in letters (any number of times)."
  [word letters]
  (if (empty? word)
    true
    (if (str/index-of letters (first word))
        (recur (rest word) letters)
        false)))

(defn words-that-use-only
  "Returns list of all words that use only letters that appear in letters (any number of times)."
  [letters]
  (filter #(uses-only-letters % letters) (all-words))
  )

(defn all-phrases
  "Returns all phrases with as many letters as letters and which consist of only words that use letters in letters."
  [letters]
  (let [words (words-that-use-only letters)]
    nil)
  )

(defn phrase-anagrams
  "Returns combinations of words that are anagrams of a given phrase."
  [phrase]
  (let [no-spaces (str/replace phrase " " "")]
    (filter #(anagram? % no-spaces) (all-phrases no-spaces)))
  )
