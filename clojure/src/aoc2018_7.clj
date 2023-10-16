(ns aoc2018_7
  (:require [clojure.string :as str]))

;; parse stage
;; input을 requirement / step으로 나누어 저장

(defn parse-instruction [line]
  (let [trimmed-line (str/trim line)
        pattern #"Step ([A-Z]) must be finished before step ([A-Z]) can begin."
        [_match requirement step] (re-matches pattern trimmed-line)]
    {:requirement requirement :step step}))

(defn parse-instructions [raw-text]
  (->> (str/split-lines raw-text)
       (map parse-instruction)))

(comment
  (parse-instruction " Step C must be finished before step A can begin.")

  (def parsed-instructions (parse-instructions "Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.")))

;; process stage
;; requirement가 충족된 것을 filter
;; 충족된 것 중 오름차순 정렬해 하나를 실행
;; 실행한 step의 목록을 저장
;; 이때 같은 것은 list로 묶어준다.
;; {:requirement ["C"] :step "A"}
;; {:requirement ["C"] :step "F"}
;; {:requirement ["A"] :step "B"}
;; {:requirement ["A"] :step "D"}
;; {:requirement ["B" "D" "F"] :step "E"}
;; 오름차순 정렬 -> 같은 step의 requirement끼리 묶기

;; 전체 그룹 찾기
(defn get-possible-step-pool [instructions]
  (->> instructions
       (mapcat (fn [instruction] [(:requirement instruction) (:step instruction)]))
       distinct))

;; 일치하는게 있는지 확인
(defn in?
  ([coll elm]
   (in? identity coll elm))
  ([key-fn coll elm]
   (some #(= elm (key-fn %)) coll)))

;; 명시되지 않은 스텝을 명시
(defn find-steps-without-requirement [instructions]
  (->> (get-possible-step-pool instructions)
       (remove (fn [instruction] (in? instructions instruction)))
       (map (fn [group] {:step group}))))

(defn collect-step [instructions]
  (:step (first instructions)))

(defn collect-requirements [instructions]
  (->> instructions
       (map :requirement)))

(defn initial-context [instructions]
  (let [preprocessed-instructions
        (->> instructions
             (concat (find-steps-without-requirement instructions))
             (group-by :step)
             (vals)
             (map (fn [grouped-instructions] {:requirement (collect-requirements grouped-instructions)
                                              :step (collect-step grouped-instructions)}))
             (sort-by :step))]
  {:in-progress []
   :done []
   :current-time 0
   :instructions preprocessed-instructions
   :workers 5}))

(comment 

  (in? :step parsed-instructions "C")

  (find-steps-without-requirement parsed-instructions)

  (collect-step [{:requirement "B", :step "E"} {:requirement "D", :step "E"} {:requirement "F", :step "E"}])

  (collect-requirements [{:requirement "B", :step "E"} {:requirement "D", :step "E"} {:requirement "F", :step "E"}])

  parsed-instructions

  (def context (initial-context parsed-instructions))

  context
  )

;; aggregate stage
;; 다음에 할 것 구하기
;; remove : have-done에 포함되어있는 step 제외
;; filter : requirement가 모두 have-done에 포함된 케이스만

(defn find-available-steps [context]
  (->> (:instructions context)
       (filter (fn [instruction] (or (empty? (clojure.set/difference (set (:requirement instruction)) (set (:done context))))
                                     (= #{nil} (clojure.set/difference (set (:requirement instruction)) (set (:done context)))))))
       (remove (fn [instruction] (in? (:done context) (:step instruction))))
       (remove (fn [instruction] (in? (remove nil? (map :step (:in-progress context))) (:step instruction))))))

(defn process-an-instruction [context]
  (let [next-step (:step (first (find-available-steps context)))]
    (if (nil? next-step)
      context
      {:done (conj (:done context) next-step)
       :instructions (:instructions context)})))

(defn fixed-point
  ([f x]
   (fixed-point identity f x))
  ([key-fn f x]
   (reduce (fn [x x']
             (if (= (key-fn x) (key-fn x'))
               (reduced x)
               x'))
           (iterate f x))))

(comment

  (find-available-steps context)

  (process-an-instruction context)

  (fixed-point :done process-an-instruction context))

;; part 2
;; done, in-progress 두 단계로 저장

(defn get-work-duration [step]
  (let [step-list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
    (get
     (->> step-list
          (map-indexed vector)
          (map (fn [[idx step]] {(str step) (+ 61 idx)}))
          (into {})) step)))

;; 가능 worker수 만큼 available steps에서 가져와 할당한다.
(defn dispatch-work [context]
  (let [idle-worker-count (- (:workers context) (count (:in-progress context)))]
    (->> (find-available-steps context)
         (take idle-worker-count)
         (map (fn [work] {:step (:step work)
                          :remaining-seconds (get-work-duration (:step work))})))))

(defn update-completed-work [context]
  (let [in-progress (:in-progress context)]
    (->> in-progress
         (map (fn [work] {:step (:step work)
                          :remaining-seconds (dec (:remaining-seconds work))}))
         (filter #(zero? (:remaining-seconds %)))
         (map :step))))

;; 일시작 -> in-progress
;; in-progress의 사이즈가 worker 사이즈가 될 때까지
;; in-progress [{step : "A", remaining-seconds : 3}]
;; 1초 남았을 때 progress 시 in-progress -> done으로 이동 및 progress에서 삭제
(defn progress-work [context]
  (let [in-progress (->> (:in-progress context)
                         (map (fn [work] {:step (:step work)
                                          :remaining-seconds (dec (:remaining-seconds work))}))
                         (remove #(zero? (:remaining-seconds %))))
        context' {:in-progress in-progress
                  :done (if (empty? (update-completed-work context))
                          (:done context)
                          (concat (:done context) (update-completed-work context)))
                  :instructions (:instructions context)
                  :current-time (:current-time context)
                  :workers (:workers context)}]
    (concat (dispatch-work context') in-progress)))

(defn process-an-instruction-by-time [context] 
  {:in-progress (progress-work context)
   :done (if (empty? (update-completed-work context))
           (:done context)
           (concat (:done context) (update-completed-work context)))
   :instructions (:instructions context)
   :current-time (inc (:current-time context))
   :workers (:workers context)})


(comment
  (get-work-duration "A")
  (update-completed-work test)
  (progress-work test)

  test
  (find-available-steps test) 
  (progress-work test)
  (progress-work (process-an-instruction-by-time test))
  (dispatch-work (process-an-instruction-by-time test))
  (process-an-instruction-by-time test)

  (fixed-point :in-progress process-an-instruction-by-time context))
