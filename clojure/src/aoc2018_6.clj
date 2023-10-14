(ns aoc2018_6
  (:require [clojure.string :as str]))

;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.


;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.


;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf


;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)

;; .은 포함하지 않음
;; 시간에 따른 흐름을 배제하고 manhatten distance가 최소인 포인트만 찾으면 어디에 포함될 지  쉽게 알 수 있음
;; 지도위의 모든 지점에서 다른 모든 좌표와 맨해튼 거리를 계산해서 어디에 속하는지 판단 가능

;; 접근법
;; 0,0부터 max x, max y의 cartesian product를 생성함
;; 각 point에서 맨해튼 거리로 가까운 점을 판단
;; map을 다채움
;; group-by로 영역을 분류하고, filter로 infinite-coordinate에 속하는 영역을 제외하고 최대값을 count한다.
;; 외곽에 있는 것을 제외? -> 확실하지 않음

;; id를 부여

(defn parse-point [line]
  (let [[_match x y] (re-matches #"\s*(\d+)[\s,]+(\d+)\s*" line)]
    {:x (parse-long x) :y (parse-long y)}))


(defn parse-base-points [lines]
  (->> (str/split-lines lines)
       (map parse-point)
       (map-indexed (fn [id point] {:id id :point point}))))

;; 모든 coordinate를 포함하는 최소-최대 Plane 생성

;; min-key, max-key로 리팩토링 (last O(n))
;; 성급한 최적화 X -> 병목이 발생할 때 고려해볼수도
;; comp, map으로 리팩토링
(defn generate-plane-by-bases [bases]
  (let [min-x (apply min-key identity (map #((comp :x :point) %) bases))
        max-x (apply max-key identity (map #((comp :x :point) %) bases))
        min-y (apply min-key identity (map #((comp :y :point) %) bases))
        max-y (apply max-key identity (map #((comp :y :point) %) bases))]
    (for [x (range min-x (inc max-x)) y (range min-y (inc max-y))]
      {:x x :y y})))

;; 두 점 사이 맨해튼 거리 계산
;; manhatten을 굳이 명시할 필요도 없어보임
;; 3차원으로 바꿨을 때 유연
;; coordinate system - distance
;; keyword ; data structure, protocol interface
;; SICP

(defn calculate-distance
  [point-a point-b]
  (+
   (abs (- (:x point-a) (:x point-b)))
   (abs (- (:y point-a) (:y point-b)))))

(calculate-distance {:x 1 :y 2} {:x 3 :y 0})

;; 가장 가까운 coordinate의 id를 선택
;; 동률일 때 . 넣는 로직?
(defn decide-base-id-to-belong
  [bases point-on-plane]
  (->> bases
       (map (fn [base]
              {:distance (calculate-distance
                          (:point base) point-on-plane)
               :id (:id base)}))
       (sort-by :distance) 
       first
       :id))

;; tie인지 아닌지 확인하는 함수
(defn is-tie?
  [bases point-on-plane]
  (let [distance-from-point
        (->> bases
             (map (fn [base] (calculate-distance (:point base) point-on-plane)))
             (sort))]
    (= (first distance-from-point)
       (second distance-from-point))))

;; 가장짧은 맨해튼 거리의 plane을 완성
(defn fill-base-plane
  [generated-plane bases]
  (->> generated-plane
       (map (fn [point]
              {:base-id (decide-base-id-to-belong bases point)
               :point point
               :is-tie (is-tie? bases point)}))))

;; edge에 있는 coordinate-id 집합을 찾음
;; base, point를 구분
;; find-overflow-bases
;; 적절한 수준의 추상화
;; sequence를 계속 유지하다가 자료구조로 가공하는것은 마지막에 하는게 좋다.
(defn find-overflow-bases
  [belonging-base-plane]
  (let [min-x (apply min-key identity (map #((comp :x :point) %) belonging-base-plane))
        max-x (apply max-key identity (map #((comp :x :point) %) belonging-base-plane))
        min-y (apply min-key identity (map #((comp :y :point) %) belonging-base-plane))
        max-y (apply max-key identity (map #((comp :y :point) %) belonging-base-plane))]
    (->> belonging-base-plane
         (filter (fn [point-on-plane] (or (= min-x (:x (:point point-on-plane)))
                                          (= max-x (:x (:point point-on-plane)))
                                          (= min-y (:y (:point point-on-plane)))
                                          (= max-y (:y (:point point-on-plane))))))
         (map :base-id)
         (set))))

;; set의 용도에 맞게 사용
(defn find-max-area-of-finite-coordinate
  [belonging-base-plane]
  (let [infinite-set-of-coordinate-id (find-overflow-bases belonging-base-plane)]
    (->> belonging-base-plane
         (remove #(contains? infinite-set-of-coordinate-id (:base-id %)))
         (remove :is-tie)
         (group-by :base-id) 
         (vals)
         (map count)
         (apply max-key))))

(comment

  (parse-point " 1, 1")

  (def test-str "1, 1
                                             1, 6
                                             8, 3
                                             3, 4
                                             5, 5
                                             8, 9")
  (parse-base-points test-str)

  (def list-of-bases (parse-base-points test-str))
  list-of-bases

  (generate-plane-by-bases list-of-bases)

  (decide-base-id-to-belong list-of-bases {:x 1 :y 4})
  (is-tie? list-of-bases {:x 1 :y 4})

  (def base-plane ( generate-plane-by-bases list-of-bases))
  base-plane
  (def manhatten-space (fill-base-plane base-plane list-of-bases))
  manhatten-space

  (find-overflow-bases manhatten-space)
  (find-max-area-of-finite-coordinate manhatten-space))



(comment)
  ;;(finite-coordinate? {:x 1 :y 1} list-of-coordinates)


;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 미만인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.

(defn calculate-safe-distance-from-point
  [list-of-coordinates point]
  (->> list-of-coordinates
       (map (fn [coordinate] (calculate-distance
                              (:point coordinate) point)))
       (reduce +)))

(calculate-safe-distance-from-point list-of-bases {:x 1 :y 2})

manhatten-space

(defn calculate-safe-area-size
  [list-of-coordinates manhatten-space]
  (->> manhatten-space
       (map #(calculate-safe-distance-from-point list-of-coordinates (:point %)))
       (filter #(< % 10000))
       (count)))

(calculate-safe-area-size list-of-bases manhatten-space)
