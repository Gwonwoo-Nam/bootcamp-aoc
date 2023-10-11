(ns aoc2018_4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [aoc2018_4_data/test :as test]))
;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up

;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up

;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.
;; 

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 다음날 0시 5분~11분, 다다음날 0시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.

;; 유의사항
;; 입력 파싱 시 정규 표현식을 적용하기
;; 데이터 구조화하기
;; 처음부터 네이밍 고려하기

;; 입력 처리
;; input - raw work-data
;; output - {time-stamp: 1518-11-05 00:03, keyword: Guard, id: 10}
;; begin shift의 상태를 어떻게 처리해야하지..? -> reduce를 이용해 최근 값으로 채우기
;; falls asleep 키워드로부터 wakes up이 나올 때까지 시간을 모두 기록한다.
;; reduce
;; 정렬

;; 메인 로직
;; 수면 시간의 빈도를 계산해야하므로 각 Guard가 잠든 상태를 모두 기록해야한다.
;; {:id 1 :time-stamp 46}, {:id 1 :time-stamp 47}
;; 최장수면시간 : Guard id별로 수면 시간을 group-by한 후 count
;; 빈도 : Guard Id와 time-stamp별로 group-by한 후 max인 time-stamp를 찾는다.


(defn tokenize-single-work-data
  "단일 근무 데이터를 읽어들여 토큰들로 분리하는 함수
   input - single-data:str
   output - data-tokens:(str)"
  [single-data]
  (-> single-data
      (str/replace #"falls asleep" "sleep")
      (str/replace #"wakes up" "awake")
      (str/replace #"\[|\]|\#" "")
      (str/split #" ")))

(defn parse-single-work-data
  "단일 근무 데이터를 파싱하는 함수
     input - single-data:str
     output - parsed-single-data:(map)
     ex - {:date 1518-11-01, :time 00:00, :keyword " Guard ", :id 10}"
  [single-data]
  (let [single-data (tokenize-single-work-data single-data)
        parsed-time (map parse-long
                         (->
                          (second single-data)
                          (str/split #":")))]
    {:date (first single-data)
     :time (+ (* 60 (first parsed-time)) (last parsed-time))
     :minute (last parsed-time)
     :keyword (get single-data 2)
     :id (->
          (if (nil? (get single-data 3))
            nil
            (parse-long (get single-data 3))))}))

(defn fill-guard-id-information
  "sleep, wake-up keyword 데이터에서 생략된 guard id를 이전 데이터의 id로 채워서 반환하는 함수
   input - single-work-data:(map)
   output - single-work-data:(map)"
  [work-data]
  (reduce (fn [acc item]
            (if (:id item)
              (conj acc item)
              (let [item' (assoc item :id (:id (last acc)))]
                (conj acc item'))))
          [] work-data))

(defn parse-work-data
  "전체 근무 데이터를 읽어들여 id를 채우고 시간 순 정렬하여 반환하는 함수
       input - raw-work-data:str
       output - parsed-single-data:((map))
       ex - [{:date 1518-11-01, :time 00:00, :keyword Guard, :id 10}]"
  [raw-work-data]
  (->> raw-work-data
       (str/split-lines)
       (map str/trim)
       (map tokenize-work-data)
       (sort-by (juxt :date :time))
       (fill-guard-id-information)))

;; work-data -> asleep-guard-data
(defn convert-to-asleep-guard-data
  "파싱된 전체 근무 데이터를 가드의 수면시간 데이터로 변환하는 함수
         input - parsed-work-data:((map))
         output - asleep-guard-data:((map))
         ex - [{:date 1518-11-01, :time 00:00, :keyword Guard, :id 10}]"
  [parsed-work-data]
  (->> parsed-work-data
       (remove #(= (:keyword %) "Guard"))
       (reduce (fn [acc item]
                 (if (= (:keyword item) "sleep")
                   (conj acc
                         {:id (:id item) :sleep (:minute item)})
                   (conj acc
                         (assoc (last acc) :awake (:minute item))))) [])
       (filter #(> (count %) 2))))

(defn convert-to-guard-asleep-frequency
  "가드의 수면시간 데이터를 통계처리를 위해 수면 빈도 데이터로 변환하는 함수
           input - asleep-guard-data:((map))
           output - asleep-frequency-data:((map))
           ex - [{:id 2207, :asleep-at 35}]"
  [asleep-guard-data]
  (->> asleep-guard-data
       (mapcat (fn [single-asleep-data]
                 (for [asleep-at (range (:sleep single-asleep-data) (:awake single-asleep-data))]
                   {:id (:id single-asleep-data) :asleep-at asleep-at})))))

(defn analyze-guard-asleep-pattern
  [raw-work-data]
  "전체 근무 데이터를 입력받아 분석된 수면 빈도 데이터를 반환하는 함수
             input - raw-work-data:str
             output - asleep-frequency-data:((map))
             ex - [{:id 2207, :asleep-at 35}]"
  (->> raw-work-data
       (parse-work-data)
       (convert-to-asleep-guard-data)
       (convert-to-guard-asleep-frequency)))

(comment
  (tokenize-single-work-data "[1518-11-01 00:00] Guard #10 begins shift")
  (parse-single-work-data "[1518-11-01 00:00] Guard #10 begins shift")

  (parse-work-data test)
  (def test-parsed-data (parse-work-data test))
  (convert-to-asleep-guard-data test-parsed-data)
  (def test-guard-asleep-data (convert-to-asleep-guard-data test-parsed-data))
  test-guard-asleep-data

  (convert-to-guard-asleep-frequency test-guard-asleep-data)

  (analyze-guard-asleep-pattern test))

;; iterate -> lazy-seq
;; re-seq, re-matches, re-find 차이

(defn find-most-frequently-asleep-guard-data
  "가장 많이 졸은 가드의 데이터를 반환하는 함수
   input - asleep-frequency-data:((map))
   output - asleep-frequency-data:((map))
   ex - [{:id 2207, :asleep-at 35}]"
  [asleep-frequency-data]
  (->> asleep-frequency-data
       (group-by :id)
       (vals)
       (sort-by count)
       (last)))

(defn find-most-frequently-asleep-guard-id
  "가장 많이 졸은 가드의 id를 반환하는 함수
     input - asleep-frequency-data:((map))
     output - id:int"
  [asleep-frequency-data]
  (->> asleep-frequency-data
       (find-most-frequently-asleep-guard-data)
       (first)
       (:id)))

(defn find-most-frequently-asleep-at-by-most-asleep-guard
  "가장 많이 졸은 가드의 가장 자주 잠을 잔 시각을 반환하는 함수
       input - asleep-frequency-data:((map))
       output - id:int"
  [asleep-frequency-data]
  (->> asleep-frequency-data
       (find-most-frequently-asleep-guard-data)
       (map :asleep-at)
       (frequencies)
       (sort-by val)
       (keys)
       (last)))

; 특정 시각에 가장 많이 졸은 시각을 구하는 함수
  ;; Guard별 frequency의 max의 max
(defn find-most-frequently-asleep-at-multiplied-by-most-asleep-guard-id
  "전체 데이터를 입력받아 가장 많이 졸은 가드의 id와 가장 자주 잠을 잔 시각(분)의 곱을 반환하는 함수
         input - raw-input-data:str
         output - answer:int"
  [raw-input-data]
  (let [analyzed-guard-asleep-pattern (analyze-guard-asleep-pattern raw-input-data)]
     (* (find-most-frequently-asleep-guard-id analyzed-guard-asleep-pattern)
        (find-most-frequently-asleep-at-by-most-asleep-guard analyzed-guard-asleep-pattern))))

(comment
  (find-most-frequently-asleep-guard-data (analyze-guard-asleep-pattern test))
  (find-most-frequently-asleep-guard-id (analyze-guard-asleep-pattern test))
  (find-most-frequently-asleep-at-by-most-asleep-guard (analyze-guard-asleep-pattern test))
  (find-most-frequently-asleep-at-multiplied-by-most-asleep-guard-id test)
  )


;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.

;; 특정 시각에 가장 많이 졸은 가드 ID를 구하는 함수
  ;; Guard별 frequency의 max의 max


(defn frequency-by-guard [asleep-guard-by-id-data]
  (->> asleep-guard-by-id-data
       (frequencies)
       (sort-by val)))



(defn find-most-frequently-asleep-at 
  "특정 시각에 가장 많이 졸은 가드 ID와 가장 자주 잠을 잔 시각을 반환하는 함수
               input - analyzed-guard-asleep-pattern:((map))
               output - id and time:map"
  [analyzed-guard-asleep-pattern]
  (->> analyzed-guard-asleep-pattern
               (group-by :id)
               vals
               (map frequency-by-guard)
               (map last)
               (sort-by val)
               last
               key))

(defn find-most-frequently-asleep-at-multiplied-by-id
  "특정 시각에 가장 많이 졸은 가드 ID와 가장 자주 잠을 잔 시각의 곱을 반환하는 함수
             input - raw-input-data:str
             output - answer:int"
  [raw-input-data]
  (let [analyzed-guard-asleep-pattern (analyze-guard-asleep-pattern raw-input-data)]
    (let [result
          (find-most-frequently-asleep-at analyzed-guard-asleep-pattern)]
      (* (:id result) (:asleep-at result)))))

(comment

  (frequency-by-guard ({:id 2477, :asleep-at 21}
                       {:id 2477, :asleep-at 22}
                       {:id 2477, :asleep-at 23}))

  (find-most-frequently-asleep-at-multiplied-by-id test))


