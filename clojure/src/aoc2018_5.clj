(ns aoc2018_5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))
;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; logic
;; ascii code의 값 차이(절대값)로 비교
;; reduce로 한번 순회하면서 반응을 일으킨다.

(defn get-input-puzzle
  "문제의 인풋 데이터를 읽어 반환하는 함수
   input: 인풋 데이터 파일 이름
   output: 문자열 전체"
  [filename]
  (let [input-file (io/resource filename)]
    (slurp input-file)))

;; 
(defn reactive-with?
  "두 Unit를 비교해 반응 가능한지를 반환하는 함수"
  [unit-a unit-b]
  (let [opposite-polarity 32]
    (-> unit-a
        int
        (- (int unit-b))
        abs
        (= opposite-polarity))))

(reactive-with? \a \A)

;; polymer -> unit (element)
(defn react-polymer-with-an-element
  "acc 시퀀스와 현재 unit을 받아 반응 가능하다면 마지막 요소를 제거하고 아니라면 현재 요소를 추가한 acc 시퀀스를 반환한다.
     input: current-polymer:char reactor:seq
     output: reactor:seq"
  [polymer unit]
  (if (reactive-with?
       (if (> (count polymer) 0) (first polymer) 0)
       unit)
    (rest polymer)
    (conj polymer unit)))

(react-polymer-with-an-element '(\a \b \c) \C)

(defn chain-react
  "인풋 데이터 전체를 입력받아 반응을 일으키고 남은 unit들의 이름을 문자열로 반환
   input: 전체 입력 데이터:str
   output: residual-elements:string"
  [input]
  (->> input
       (reduce #(react-polymer-with-an-element %1 %2) '())
       reverse
       (apply str)))

(defn get-length-after-reaction
  "전체 데이터를 입력받아 반응을 일으키고 남은 unit의 개수를 count하여 반환한다.
   input: 전체 입력 데이터:str
   output: residual-count:integer"
  [input]
  (->> input
       chain-react
       count))


(get-length-after-reaction "dabAcCaCBAcCcaDA")
(get-length-after-reaction "abBA")
(def input-puzzle (get-input-puzzle "day5.txt"))
input-puzzle
(get-length-after-reaction input-puzzle)


;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.
;; map으로 자료구조 변환해서 리팩토링해보기(ascii 대신)
;; polymer mapping table을 만드는게 이름짓기나 변경에 유연한 설계가 될듯..ㅇ!

(defn convert-upper-opposite-type
  "small->big 타입으로 바꾸는 함수 (a->A only)
     input: element:char
     output: converted-element:char"
  [element]
  (char (- (int element) 32)))



(convert-upper-opposite-type \a)

(defn remove-specific-type
  "반응 물질과 제거할 타입을 입력받아 특정 타입을 모두 없애는 함수
      input: input-elements:str
      output: removed-elements:str"
  [input-elements removal-element]
  (->> input-elements
       (remove
        (fn [element]
          (or
           (reactive-with? element removal-element)
           (reactive-with? element (convert-upper-opposite-type removal-element)))))))

(remove-specific-type "Aabc"  \a)

(defn react-with-remove-atoz
  "a~z까지 각각 가능한 타입을 제거한 후 가장 짧은 결과를 반환
          input: input-elements:str
          output: shortest-element-count:integer"
  [input-elements]
  (let [elements-range (range 97 123)]
    (->>
     (map (fn [removal-element] (char removal-element)) elements-range)
     (map (fn [removal-element] (remove-specific-type input-elements removal-element)))
     (map get-length-after-reaction)
     sort
     first)))

(react-with-remove-atoz input-puzzle)
