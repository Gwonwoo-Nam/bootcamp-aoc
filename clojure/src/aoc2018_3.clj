(ns aoc2018_3
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn get-input-puzzle
  "문제의 인풋 데이터를 읽은 다음 파싱된 값을 제공하는 함수
   input: 인풋 데이터 파일 이름
   output: newline 문자 기준으로 split된 리스트"
  [filename]
  (let [input-file (io/resource filename)]
    (map #(parse-long %) (str/split (slurp input-file) #"\n"))))
;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)
;; 라인별로 map을 만든다



;; 입력을 라인별로 split된 벡터로 좌우 white space trimming하여 반환한다.
(defn split-input-lines [input]
  (->> input
       (str/split-lines)
       (map str/trim)))

;; 불필요한 문자를 제거하고 (id, x, y ,width, height)로 반환한다.
(defn split-by-format [input]
  (->> input
       (map #(str/split % #"@|:| |#|,|x"))
       (map (fn [l] (remove #(= % "") l)))
       (map (fn [l] (map #(Integer/parseInt %) l)))))


(defn input-stream [input]
  (->> input
       (split-input-lines)
       (split-by-format)))

;; 입력 테스트
(comment
  (def puzzle "#1 @ 1,3: 4x4
               #2 @ 3,1: 4x4
            #3 @ 5,5: 2x2")

  (split-input-lines puzzle)
  (split-by-format (split-input-lines puzzle))
  (input-stream puzzle)

  (input-stream "#1 @ 1,3: 4x4
                 #2 @ 3,1: 4x4
              #3 @ 5,5: 2x2"))

;; x,y 좌표의 범위를 생성한다.
(defn cartesian-product [x-loc y-loc width height]
  (for [x (range x-loc (+ x-loc width)) y (range y-loc (+ y-loc height))]
    [x y]))


;; map에 좌표 : ID로 값을 저장한다.
;; 인자의 개수에 대해서.. 일반적으로 map으로 받아서 map으로 반환
;; x-loc y-loc width height를 하나의 벡터로 묶기?
;; 같은 위상끼리 구조화
;; :id ~ :patch ~
;; :id ~ :patch {:xloc , :yloc ~ }

(defn mark-on-map [id x-loc y-loc width height]
  (->> (cartesian-product x-loc y-loc width height)
       (map (fn [[x y]] {:id id :coord [x y]}))))

;; 생성된 map을 합친다. 이 때 중복되는 값이 있으면 count한다.
;; 문제 참고해서 네이밍
(defn merge-patches [parsed]
  (->>
   (mapcat (fn [[id x-loc y-loc width height]] (mark-on-map id x-loc y-loc width height)) parsed)))
;; apply concat
;; => mapcat으로 한줄로 리팩토링 가능
;; 가변 인자일 때 %1, %2 등은 생략 가능

;; map의 좌표만 가져와서 빈도 분석
(defn analyze [data]
  (->> data
       (map :coord)
       frequencies
       vals
       (filter #(> % 1))
       count))

;; 최종함수
(defn output-stream [data]
  (->> data
       input-stream
       merge-patches
       analyze))

;; 자료구조 map을 활용 input의 이름 붙이기
(analyze [{:id 3, :coord [6 6]} {:id 3, :coord [6 6]}])
(filter (complement number?) (vals {:id 3, :coord [6 6]}))
(comment

  (cartesian-product 1 3 4 4)

  (mark-on-map 1 1 3 4 4)

  (first (mark-on-map 1 1 3 4 4))

  (merge-patches parsed)

  (analyze (merge-patches parsed))

  (output-stream puzzle))


;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

;; map에 좌표, [id list]를 저장하여 id의 개수가 2개 이상인 것만 filter, id의 목록으로 추출한다.
;; 전체 id의 목록에서 겹치는 id를 소거하여 안겹치는 id를 찾는다.

(defn find-all-id [data]
  (->> data
       (map :id)
       (set)))


(defn navigate-element [elem]
  (map :id elem))

(navigate-element [{:id 3 :coord [4]}, {:id 4 :coord [4]}])

;; flatten 대신 mapcat을 사용
;; map 안에서 map을 호출하지 않도록 -> 개별 함수로
(defn find-overlapped-id [data]
  (->> data
       (group-by :coord)
       (vals)
       (filter #(> (count %) 1))
       (mapcat #(navigate-element %)) 
       (into #{})))


;; set을 아래에서 하는 방법
;; map -> patch
;; 함수이름, 인터페이스로 동작을 추측할 수 있게
;; let binding
(defn get-difference [data]
  (first
   (clojure.set/difference
    (find-all-id (merge-patches data))
    (find-overlapped-id (merge-patches data)))))

(defn find-unique-patch [input]
  (->> input
       (input-stream)
       (get-difference)))

(comment
  (def data (input-stream "#1 @ 1,3: 4x4
                   #2 @ 3,1: 4x4
                #3 @ 5,5: 2x2"))
  (merge-patches data)
  (find-overlapped-id (merge-patches data))
  (find-all-id (merge-patches data))

  (find-unique-patch "#1 @ 1,3: 4x4
                                        #2 @ 3,1: 4x4
                                     #3 @ 5,5: 2x2"))
