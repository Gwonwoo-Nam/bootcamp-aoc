(ns aoc2018-2
  (:require [clojure.string :as str]))

;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; 예)
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12


(require '[clojure.string :as str])

;; 문자열 파싱
;; (defn parse-string [input]
;;   (let [input (str/replace input " " "")]
;;     (for [row (str/split input #"\n")] row)))

;; 문자열 파싱
(defn parse-string [input]
  (->> (str/split input #"\n") 
       (map #(str/trim %))))

(comment
  (parse-string "abcdef
                         bababc
                         ababab"))


;; count가 2인 글자를 set에 추가

;; 한 줄의 문자열에 들어있는 글자의 count 수가 2 혹은 3인 문자들 반환
(defn count-chars [row count]
  (set (keys (filter #(= (val %) count) (frequencies row)))))


;; 한줄 단위로 

;; 2 혹은 3개의 문자열들 저장하는 set
;; 함수의 단위를 작게 나누어서 리팩토링 해보기
;; pos, inc, seq, split-lines 등 core 함수
;; 쓰레딩 매크로
(defn get-repeated-charset [input c]
  (->> input
       (map #(count-chars % c))
       (filter seq)
       count))

(defn count-repeated-chars [input]
  (* 
   (count (get-repeated-charset input 2)) 
   (count (get-repeated-charset input 3))))


(comment

  (get-repeated-charset ["aabbcc"
                         "bbcadf"
                         "cadght"] 2)
  (count-repeated-chars (parse-string "abcdef
                                                bababc
                                                abbcde
                         abcccd
                         aabcdd
                         abcdee
                         ababab"))
)

;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.

;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz



;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.

;; cartesian product으로 모든 경우의 수
;; combination



(defn count-diff [str1 str2]
  (->>
   (map #(= %1 %2) str1 str2)
   (filter false?)
   (count))
)

(defn nearly-same? [[str1 str2]]
  (= (count-diff str1 str2) 1))

(defn combinations [parsed-string]
  (for [x parsed-string y parsed-string] [x y]))

(defn find-nearly-same [str] 
  (->>
   (combinations (parse-string str))
   (filter nearly-same?)
   (first))
)

(defn conj-same [[str1 str2]]
  (->>
   (map (fn [c1 c2] (when (= c1 c2) c1)) str1 str2)
   (filter (complement nil?))
   (reduce str "")))

(defn answer [str] 
   (conj-same (find-nearly-same str)))


(comment
  (def parsed (parse-string "abcdef
                        bababc
                                                abbcde
                                                abcccd
                                                aabcdd
                                                abcdee
                                                ababab"))
  (combinations parsed)
  (count-diff "abc" "bbc")
  (nearly-same? ["abc" "bbc"])
  (find-nearly-same "abcdef
                                            bababc
                                                                    abbcde
                                                                    abcccd
                                                                    aabcdd
                                                                    abcdee
                                                                    ababab")
  (conj-same ["abcdef" "abcdee"])

  (answer "abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz")
  )

;; #################################
;; ###        Refactoring        ###
;; #################################
