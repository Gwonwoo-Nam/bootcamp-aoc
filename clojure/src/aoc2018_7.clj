(ns aoc2018_7
  (:require [clojure.string :as str]))

;;input의 패턴 정의
(def condition-pattern #"Step ([A-Z]) must be finished before step ([A-Z]) can begin.")

(defn parse-condition
  "하나의 조건문을 파싱한다.
   ex)
   Step A must be finished before step B can begin.
   => {:requirement A :step B}"
  [line]
  (let [[_match requirement step] (re-matches condition-pattern line)]
    {:requirement requirement :step step}))

(defn parse-conditions [raw-text]
  "여러 조건절이 담긴 puzzle 입력을 받아 조건의 모음으로 파싱한다.
   ex)
   Step A must be finished before step B can begin.
   Step C must be finished before step D can begin.
   => ({:requirement A :step B} {:requirement C :step D})"
  (->> (str/split-lines raw-text)
       (map str/trim)
       (map parse-condition)))

(defn get-possible-step-pool [conditions]
  "condition절을 분석하여 모든 conditions를 포함하는 step의 시퀀스를 생성한다.
   ex)
   ({:requirement A :step B} {:requirement C :step D})
   => (A B C D)"
  (->> conditions
       (mapcat (fn [instruction] [(:requirement instruction) (:step instruction)]))
       distinct))

(defn in?
  "element이 collection의 원소 중 일치하는게 있는지 여부를 반환한다.
   ex)
   (in? [1 2 3] 2) => true
   (in? :key ({:key A} {:key B}) A) => true"
  ([coll elm]
   (in? identity coll elm))
  ([key-fn coll elm]
   (some #(= elm (key-fn %)) coll)))


(defn find-steps-without-requirement [conditions]
  "조건이 없어 생략된 step의 목록을 반환한다.
   ex)
   => ({:step B} {:step C})"
  (->> (get-possible-step-pool conditions)
       (remove (fn [condition] (in? :step conditions condition)))
       (map (fn [step] {:step step}))))


(defn conditions->work-instructions
  "conditions를 분석해 작업 지침을 만든다.
   작업 지침은 
   1. Step(단계)의 순서대로 정렬된 규칙의 모음을 의미한다.
   2. pre-requisites는 특정 step을 실행하기 위한 모든 steps의 모음읻자.
   ex)
   ({:requirement (C), :step A}
   {:requirement (A), :step B}
   {:requirement (), :step C}
   {:requirement (A), :step D}
   {:requirement (B D F), :step E}
   {:requirement (C), :step F})"
  [conditions]
  (->> conditions
       (concat (find-steps-without-requirement conditions))
       (group-by :step)
       (vals)
       (map (fn [grouped-conditions] {:pre-requisites (->> grouped-conditions
                                                           (map :requirement)
                                                           (remove nil?))
                                      :step (:step (first grouped-conditions))}))
       (sort-by :step)))

(defn setup-initial-work-status
  "초기 work-status를 생성한다.
   work-status는 work-instructions과 현재 작업에 대한 메타정보를 포함한다.
   :current-time - 작업의 현재 진행 시각
   :in-progress-jobs - 진행 중인 작업의 목록
   :done - 완료 작업 목록
   :work-instructions - 작업지침
   :idle-workers - 유휴 작업 가능 인원 수

   ex)
   {:current-time 0,
    :in-progress-jobs [],
    :done [],
    :work-instructions
    ({:requirement (C), :step A}
    {:requirement (A), :step B}
    {:requirement (), :step C}
    {:requirement (A), :step D}
    {:requirement (B D F), :step E}
    {:requirement (C), :step F}),
    :idle-workers 2}" 
  ([work-instructions]
   (setup-initial-work-status work-instructions 5))
  ([work-instructions number-of-workers]
   {:current-time 0
    :in-progress-jobs []
    :done []
    :work-instructions work-instructions
    :idle-workers number-of-workers}))

(defn subset?
  "a가 b의 부분집합인지 아닌지를 판별한다.
   ex)
   (subset? #{1 2} #{1 2 3}) => true"
  [a b]
  (empty? (clojure.set/difference a b)))

(defn find-available-steps
  "work-status를 읽어들여 다음의 가능한 step 정보를 가져온다.
   다음 규칙을 따른다.
   1. requirement가 모두 done에 포함된 케이스만 포함한다.
   2. 작업 완료된 step은 제외한다.
   3. 작업 진행 중인 step도 제외된다.
   ex)
   ({:pre-requisites (), :step C})"
  [work-status]
  (->> (:work-instructions work-status) 
       (filter (fn [instruction] (subset? (set (:pre-requisites instruction)) 
                                          (set (:done work-status)))))
       (remove (fn [instruction] (in? (:done work-status) (:step instruction))))
       (remove (fn [instruction] (in? (map :step (:in-progress-jobs work-status))
                                      (:step instruction))))))

(defn fixed-point
  "고정점을 찾는 함수"
  ([f x]
   (fixed-point identity f x))
  ([key-fn f x]
   (reduce (fn [x x']
             (if (= (key-fn x) (key-fn x'))
               (reduced x)
               x'))
           (iterate f x))))

;; 아래는 part 1에서만 사용되는 함수입니다.
(defn complete-incoming-work
  "work-status를 읽어들여 다음 작업을 즉시 실행 완료한다.
   ex)
   {:done [C A],
    :work-instructions
    .. 생략"
  [work-status]
  (let [{done :done} work-status
        next-step (-> work-status
                      find-available-steps
                      first
                      :step)]
    (if (nil? next-step)
      work-status
      (assoc work-status :done (conj done next-step)))))

(defn solve-part1 [raw-text]
  (let [initial-work-status (->> raw-text
                                 parse-conditions
                                 conditions->work-instructions
                                 setup-initial-work-status)]
    (fixed-point :done complete-incoming-work initial-work-status)))

;; part 2

(def steps [:A :B :C :D :E :F :G :H :I :J
            :K :L :M :N :O :P :Q :R :S :T 
            :U :V :W :X :Y :Z])

(def step->work-duration
  "작업 이름과 작업 소요 시간(seconds)을 hash map에 저장
   ex)
   (step->work-duration :A) => 0"
  (zipmap steps
          (range 0 (count steps))))

(defn get-work-duration
  "작업 이름(keyword)과 기본 근무 시간(seconds)을 입력받아 작업 소요 시간(seconds)을 구하는 함수
   ex)
  (get-work-duration 1 :A) => 1"
  ([step] (get-work-duration 61 step))
  ([default-work-duration step]
    (+ default-work-duration (step->work-duration step))))

;; 0. dispatch-works-to-idle-workers : 다음 작업을 할당하고, 유휴 worker의 수를 업데이트한다.
;; 1. progress-works : 다음 마감되는 업무시간까지 시간 n초 증가시키고, 진행한 작업의 잔여 시간을 n초씩 감소시킨다.
;; 2. complete-works : 잔여 시간이 0초인 작업을 완료 처리한다. (마감처리)

(defn instruction->work
  "작업 지침을 입력받아 작업 이름과 잔여 시간의 정보를 갖는 실행 중인 일로 변환한다.
   ex)
   {:pre-requisites (C), :step A} => {:step C, :remaining-seconds 63}"
  [instruction]
  {:step (:step instruction)
   :remaining-seconds (get-work-duration (keyword (:step instruction)))})

(defn dispatch-works-to-idle-workers
  [work-status] 
  "근무 상태를 입력받아 다음 작업을 할당하고, 유휴 worker의 수를 업데이트한 근무 상태를 반환한다.
   ex)
   input ->
   {:in-progress (),
    ...
    :current-time 0,
    :idle-workers 2}
   output ->
   {:in-progress-jobs ({:step C, :remaining-seconds 63}),
    ...
    :current-time 0,
    :idle-workers 1}"
  (let [{in-progress-jobs :in-progress-jobs 
         idle-workers :idle-workers} work-status        
        dispatched-works (->> (find-available-steps work-status)
                              (take idle-workers)
                              (map instruction->work))]
    (-> work-status
        (assoc :in-progress-jobs (concat in-progress-jobs dispatched-works))
        (assoc :idle-workers (- idle-workers (count dispatched-works))))))

(defn calc-consequtive-work-time
  "남은 작업 중 다음 마감 가능한 작업까지 연속해서 얼마 일해야할 지 계산한다.
   ex)
   ({:step B, :remaining-seconds 5}, {:step C, :remaining-seconds 62})
   => 5(초)" 
  [works]
  (if (empty? works)
    0
    (->> works
         (map :remaining-seconds)
         (apply min))))

(defn progress-a-work
  "일과 근무시간을 입력받아 잔여 시각을 근무시간만큼 감소시켜 반환한다.
   ex)
   {:step C, :remaining-seconds 63} -> {:step C, :remaining-seconds 62}"
  ([work]
   (progress-a-work work 1))
  ([work work-time]
   (let [progressed-time (- (:remaining-seconds work) work-time)]
     (assoc work :remaining-seconds progressed-time))))

(defn progress-works
  "근무 상태를 입력받아 현재 시각을 1초 증가시키고, 진행한 작업의 잔여 시간을 1초씩 감소시킨 근무 상태를 반환한다.
   ex)
   input ->
   {:in-progress-jobs ({:step C, :remaining-seconds 63}),
    ...
    :current-time 0,
    :idle-workers 1}
   output ->
   {:in-progress-jobs ({:step C, :remaining-seconds 62}),
    ...
    :current-time 1,
    :idle-workers 1}"
  [work-status]
  (let [{in-progress-jobs :in-progress-jobs
         current-time :current-time} work-status
        work-time (calc-consequtive-work-time in-progress-jobs)
        progressed-jobs (map #(progress-a-work % work-time) in-progress-jobs)]
    (-> work-status
        (assoc :in-progress-jobs progressed-jobs)
        (assoc :current-time (+ current-time work-time)))))

(defn works->work-names
  [works]
  "일의 시퀀스를 입력받아 이름의 시퀀스로 반환한다.
   ex) ({:step C, :remaining-seconds 63} {:step B, :remaining-seconds 2})
   => (C B)"
  (map :step works))

(defn complete-works
  "근무 상태를 입력받아 진행 중인 job 중 remaining-time이 0가 된 job을 done에 추가하고,
   progress-job에서 삭제하며 인원 수만큼 유휴 작업 인원을 증가시켜 반환한다.
   ex)
   input ->
   {:in-progress-jobs ({:step C, :remaining-seconds 0}),
    :done []
    ...
    :current-time 62,
    :idle-workers 1}
   output ->
   {:in-progress-jobs (),
    :done [C]
    ...
    :current-time 62,
    :idle-workers 2}"
  [work-status]
  (let [{in-progress-jobs :in-progress-jobs
         done :done
         idle-workers :idle-workers} work-status
        completed-works (->> in-progress-jobs
                             (filter #(zero? (:remaining-seconds %))))
        completed-work-names (concat done (works->work-names completed-works))
        remaining-works (->> in-progress-jobs
                             (remove #(zero? (:remaining-seconds %))))]
    (-> work-status
        (assoc :in-progress-jobs remaining-works)
        (assoc :done completed-work-names)
        (assoc :idle-workers (+ idle-workers (count completed-works))))))

(defn execute-work-cycle
  "근무 상태를 입력받아 업무 사이클을 1회 수행 한다. 여기서 업무 사이클이란, 최소 한 작업을 완료상태로 변경시킬 수 있는 업무량을 의미한다.
   ex)
   input ->
   {:in-progress-jobs ({:step C, :remaining-seconds 3}),
    :done []
    ...
    :current-time 62,
    :idle-workers 1}
   output ->
   {:in-progress-jobs (),
    :done [C]
    ...
    :current-time 65,
    :idle-workers 2}"
  [work-status]
  (->> work-status
       dispatch-works-to-idle-workers
       progress-works
       complete-works))

(defn solve-part2 [raw-text]
  (let [initial-work-status (->> raw-text
                                  parse-conditions
                                  conditions->work-instructions
                                  setup-initial-work-status)]
     (fixed-point :current-time execute-work-cycle initial-work-status)))


(comment
  (def input "Step C must be finished before step A can begin.
                             Step C must be finished before step F can begin.
                             Step A must be finished before step B can begin.
                             Step A must be finished before step D can begin.
                             Step B must be finished before step E can begin.
                             Step D must be finished before step E can begin.
                             Step F must be finished before step E can begin.")
  (solve-part1 input)
  (solve-part2 input)
)
