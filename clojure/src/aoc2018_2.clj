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
(defn parse-string [input]
  (->> (str/split-lines input) 
       (map str/trim)))

;; 한 줄의 문자열에 들어있는 글자의 count 수가 2 혹은 3인 문자들 반환
;; set은 중복제거 용도보다는 distinct를 대신 사용
;; count 대신 다른 이름
;; entry 
;; keep으로 변경해보기
(defn count-chars [row cnt]
  (->> row
       (frequencies) 
       (filter #(= (val %) cnt)) 
       (keys)))

;; 2 혹은 3개의 문자열들 저장하는 set
;; 함수의 단위를 작게 나누어서 리팩토링 해보기
;; pos, inc, seq, split-lines 등 core 함수
;; 쓰레딩 매크로

(defn count-repeated-charset [input c]
  (->> input
       (keep #(count-chars % c)) 
       count))


;; 2개의 문자열, 3개의 문자열의 개수를 곱하여 반환
(defn count-repeated-chars [input]
  (let [parsed-input (parse-string input)]
    (*
     (count-repeated-charset parsed-input 2)
     (count-repeated-charset parsed-input 3))))

(comment

  (def strex "abcdef
                                                               bababc
                                                               abbcde
                                        abcccd
                                        aabcdd
                                        abcdee
                                        ababab")
  
  (parse-string strex)
  
  (get-repeated-charset ["abcdef" "bababc" "abbcde"] 2)
  (count-repeated-chars str)
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


;; 두 문자열 간 글자수가 일치하지 않는 개수를 계산하는 함수
(defn count-diff [str1 str2]
  (->> 
   (remove #(= % str1) str2)
   (count)))

;; remove로 변경안됨..
(defn count-diff [str1 str2]
  (->>
   (map #(= %1 %2) str1 str2) 
   (remove true?)
   (count)))

(count-diff "abc" "aab")

;; 일치하지 않는 개수가 1개인지 여부를 반환하는 함수
(defn nearly-same? [[str1 str2]]
  (= (count-diff str1 str2) 1))

;; 문자열의 가능한 조합을 반환하는 함수
(defn cartesian-product [parsed-string]
  (for [x parsed-string y parsed-string] [x y]))

;; 모든 조합에서 nearly-same인 문자열을 찾는 함수
;; [String] -> String
(defn find-nearly-same [str] 
  (->>
   (cartesian-product (parse-string str))
   (filter nearly-same?)
   (first)))

;; nearly-same인 경우 일치하는 문자열만 filter하여 반환하는 함수
;; str, count 등 네이밍에 사용하지 않기
;; parsing 위치에 대해서 고민 -> 재사용성
;; AoC 문제 참고 -> 도메인 용어 사용
(defn conj-same [[str1 str2]]
  (->>
   (map (fn [c1 c2] (when (= c1 c2) c1)) str1 str2)
   (remove nil?)
   (apply clojure.core/str)))

(conj-same ["abc" "abb"])

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
  (cartesian-product parsed)
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
