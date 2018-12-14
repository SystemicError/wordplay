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

(defn no-spaces
  "Removes spaces from a string."
  [word]
  (str/replace word " " "")
  )

(defn all-phrases
  "Returns all phrases with as many letters as letters and which consist of only words that use letters in letters."
  [letters]
  (loop [words (filter #(<= (count %) (count letters)) (words-that-use-only letters))
         partial-phrases (filter #(< (count %) (count letters)) words)
         complete-phrases (filter #(= (count %) (count letters)) words)]
    (if (empty? partial-phrases)
      complete-phrases
      ; append all words to all partial phrases, moving
      ; any that are as long as letters to complete-phrases
      ; and deleting any that are larger
      (let [appended (concat (for [p partial-phrases w words] (str p " " w)))
            completed (filter #(= (count letters) (count (no-spaces %))) appended)
            dummy (println "\nwords:  " words "\npp:  " partial-phrases
                           "\ncp:  " complete-phrases)]
        (recur words
               (filter #(< (count (no-spaces %)) (count letters)) appended)
               (concat completed complete-phrases)))))
  )

(defn phrase-anagrams
  "Returns combinations of words that are anagrams of a given phrase."
  [phrase]
  (filter #(anagram? (no-spaces %) (no-spaces phrase)) (all-phrases (no-spaces phrase)))
  )
