
(ns aoc2018-1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-input-puzzle
  "문제의 인풋 데이터를 읽은 다음 파싱된 값을 제공하는 함수
   input: 인풋 데이터 파일 이름
   output: newline 문자 기준으로 split된 리스트"
  [filename]
  (let [input-file (io/resource filename)]
    (map #(Integer/parseInt %) (str/split (slurp input-file) #"\n"))))

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력

;; reduce 풀이
(defn sum-numbers [input]
  (reduce + input))

;; apply를 이용한 풀이
(defn sum-numbers [input]
  (apply + input))

(comment
  (sum-numbers [1 2 3])
  )

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...

;; 더하면서 set에 저장
(defn find-first-duplicate [input]
  (loop [input-cycle (cycle input) result #{} reduce-sum 0]
    (let [current-reduce-sum (+ reduce-sum (first input-cycle))]
      (if (contains? result current-reduce-sum)
        current-reduce-sum
        (recur (rest input-cycle) (conj result current-reduce-sum) current-reduce-sum)))))

;; 리팩터링 -> reduce를 사용해볼 것, 무한 시퀀스를 만드는 메서드, 시퀀스를 순회하며 중복을 확인하는 메서드로 분리
;; contains? boolean을 반환할 때 ?를 convention으로 사용 
;; 자료구조(hashset, hashmap, vector -> index)를 함수로 사용할 수 있다.


;; 리팩터링
;; 누적합 lazy seq 생성
(defn applied-frequency [input]
  (reductions + (cycle input))
  )

;; 중복 발생하는 첫 값을 반환하는 함수
(defn find-first-dup [acc-sum-seq] 
  (reduce (fn [acc-set val]
            (if (contains? acc-set val)
              (reduced val)
              (conj acc-set val)))
          #{}
          acc-sum-seq))

(defn find-first-duplicate-sum [input]
  (find-first-dup (applied-frequency input)))


(comment
  (def puzzle (get-input-puzzle "day1.txt")) 
  (sum-numbers puzzle)
  (find-first-duplicate puzzle)
  (applied-frequency [3 3 4 -2 -4])
  (find-first-duplicate-sum [3 3 4 -2 -4])
  )