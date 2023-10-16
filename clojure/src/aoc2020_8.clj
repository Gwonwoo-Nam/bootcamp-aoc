(ns aoc2020_8
  (:require [clojure.string :as str]))

;; command의 무한 시퀀스를 만든다. 
;; 음수도 단방향의 시퀀스 흐름으로 정의할 수 있다. 
;; ->> take를 이용한 풀이
;; jmp +1은 rest의 first
;; jmp +2는 rest
;; jmp -3는 전체 사이즈(9) - 3 = +6과 동일
;; 순서 정보를 기록하기 위해 augment id가 필요

;; 단일 instruction line parse
;; trim은 별도로 빼기
;; email pattern 정의하는 경우 -> trim은 포함안되어야함
;; 정규식 detail vs naive
(defn parse-instruction [line]
  (let [[_match opcode operand] (re-matches #"\s*(acc|jmp|nop)\s+([\+\-]\d+)\s*" line)]
    {:opcode opcode
     :operand (parse-long operand)}))

;; 전체 instruction lines parse
;; id? 적절한 이름
(defn parse-instructions [raw-text]
  (->> (str/split-lines raw-text)
       (map parse-instruction)
       (map-indexed (fn [id instruction] (assoc instruction :id id)))))

;; 무한한 instruction queue로 생성
;; instructions program
(defn instructions->instructions-queue [instructions]
  (cycle instructions))

;; 음수일 때 보수를 취함 -4, 9 -> 4
(defn calc-complement-argument [argument instructions-size]
  (if (neg? argument)
    (+ instructions-size argument)
    argument))

;; instructions-queue에서 instructions의 개수를 구함
;; id가 2번째로 0이 되는 것을 찾는 로직 
(defn count-unique-instructions [instructions-queue]
  (let [temporal-period (->> instructions-queue
                             (map-indexed (fn [id instruction]
                                            {:counter id
                                             :instruction instruction}))
                             (filter #(= (:id (:instruction %)) 0))
                             (map :counter))]
    (- (second temporal-period) (first temporal-period))))

(defn my-add
  ([] 0)
  ([x] x)
  ([x y] (+ x y))
  )
(comment
  (my-add 10 20)
  )

;; 단일(instructions seq에서 첫번째) instruction을 처리
;; acc, instructions -> acc와 변경된 instructions을 반환
(defn process-an-instruction [context]
  (let [acc (:acc context)
        instructions-queue (:instructions-queue context)
        instruction (first instructions-queue)
        id (:id instruction)
        operation (:operation instruction)
        argument (:argument instruction)]
    (case operation
      "nop" {:processed-id (conj (:processed-id context) id)
             :acc acc
             :instructions-queue (rest instructions-queue)}
      "acc" {:processed-id (conj (:processed-id context) id)
             :acc (+ acc argument)
             :instructions-queue (rest instructions-queue)}
      "jmp" {:processed-id (conj (:processed-id context) id)
             :acc acc
             :instructions-queue (drop (calc-complement-argument
                                        argument
                                        (count-unique-instructions instructions-queue))
                                       instructions-queue)})))


;; 일반화한 고정점 함수
;; f(x) = x -> 만들어보기

(defn fixed-point
  ([f x]
   (fixed-point identity f x))
  ([key-fn f x]
    (reduce (fn [x x']
              (if (= (key-fn x) (key-fn x'))
                (reduced x)
                x'))
            (iterate f x))))

(defn solve-part1 [input]
  (let [instructions-queue (->> input
                                (parse-instructions)
                                (instructions->instructions-queue))
        initial-context {:processed-id #{}
                         :acc 0
                         :instructions-queue instructions-queue}]
    (->> initial-context
         (fixed-point-by :processed-id process-an-instruction)
         (:acc))))

(comment
  (parse-instruction " acc +1 ")
  (parse-instructions " jmp +2
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6 ")
  (def input "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")
  (def instructions-queue (instructions->instructions-queue (parse-instructions input)))

  instructions-queue
  (count-unique-instructions instructions-queue)
  (def initial-context {:processed-id #{}
                        :acc 0
                        :instructions-queue instructions-queue})
  (process-an-instruction initial-context) 
  
  (calc-complement-argument -3 9) 

  (calc-complement-argument 2 (count-unique-instructions instructions-queue)) 
  
  (fixed-point process-an-instruction initial-context)


  (solve-part1 input)
  )

(defn remove-a-space
  [s] (str/replace-first s #"\s" ""))
(iterate remove-a-space "hello world! my name is ...")
;;test
(comment   (remove-a-space "hello world! my name is ...")
           (fixed-point-by str/upper-case remove-a-space "hello world! my name is ..."))

;; part 2

;; jmp or nop을 하나씩 toggle했을 때
;; pos 값을 추가하고, fix-point에 도달했을 때 pos > size이 되는 acc 값