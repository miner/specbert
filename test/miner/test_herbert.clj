(ns miner.test-herbert
  (:require [clojure.test :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.test.check :as tc]
            [miner.specbert :refer :all]
            ;; [miner.tagged :as tag]
            clojure.string))

(deftest basics
  (is (s/valid? integer? 10))
  (is (s/valid? string? "foo"))
  (is (s/valid? symbol? 'foo))
  (is (s/valid? keyword? :foo))
  (is (s/valid? float? 1.23))
  (is (not (s/valid? symbol? :foo))))

(deftest numbers
  (is (s/valid? even? 10))
  (is (not (s/valid? (s/& integer? even?) 'foo)))
  (is (s/valid? (s/and integer? pos? (complement neg?) (complement odd?) (complement zero?)) 10))
  (is (not (s/valid? (s/and integer? (s/or :p pos? :n neg? :o odd? :z zero?)) 'foo)))
  (is (s/valid? (s/* (s/or :n neg? :z zero?)) [-1 0.0 0 -100.0 0])))

(deftest nested 
  (is (s/valid? (s/cat :i integer? :s symbol? :str string? :kw keyword?) '(10 foo "foo" :foo)))
  (is (not (s/valid? (s/cat :i integer? :s symbol? :str string? :kw keyword?) '(10 :a foo "foo" :foo))))
  (is (s/valid? (s/* (s/cat :kw keyword? :i integer?)) '(:a 10 :b 20 :c 30)))
  (is (s/valid? (s/+ (s/cat :kw keyword? :i integer? :s symbol?)) '(:a 10 foo :b 20 bar))))


(s/def :x/a20 (s/* (s/and integer? odd? #(<= 0 % 20))))

(deftest complicated
  (are [val result]
      (= (s/valid? (s/cat :m (s/keys :req-un [:x/a20]) :s symbol?
                          :m4 (s/& integer? #(zero? (mod % 4))))
                   val)
         result)
       '[{:a20 [1]} foo 4] true
       '[{:a20 [1 3 5]} foo 24] true
       '[{:a20 (1 2 3)} foo 4] false
       '[{:a20 []} foo 84] true
       '[{:a20 [:b 1 2 3]} foo 4] false
       '[{:b [1 2 3]} foo 4] false
       '[{:a20 [11 21 33]} foo 8] false))


(s/def :x/a integer?)
(s/def :x/b symbol?)
(s/def :x/c string?)
(s/def :x/d nil?)

(deftest kw-maps
  (are [val result] (= (s/valid? (s/keys :req-un [:x/a :x/b] :opt-un [:x/c]) val) result)
       {:a 1 :b 'foo :c "foo"}   true
       {:a 1 :b 'foo}   true
       {:a 1 :b 'foo :d 'bar}   true  
       {:a 1 :b 'foo :c 'bar}   false
       {:a :kw :b 'foo}   false
       {:b 'foo :c "foo"} false)
  (are [val result] (= (s/valid? (s/keys :req-un [:x/a :x/b] :opt-un [:x/d]) val) result)
       ;; :d is essentiall disallowed, technically can only have nil value
       {:a 1 :b 'foo :d "foo"}   false
       {:a 1 :b 'foo}   true
       {:a 1 :b 'foo :e 'bar}   true  
       {:a 1 :b 'foo :d 'bar}   false
       {:a 1 :b 'foo :d nil}  true))


;; looks like string keys are not supported
;; (deftest other-keys

;; n/a (deftest quoted-kws

;; n/a (deftest single-optional

;; n/a (deftest sets


;; Note (or xxx+ yyy+) works but (or xxx* yyy*) can fail since zero xxx matches and yyy doesn't
;; get the chance after that.  Remember, no backtracking.  The + lets it work as expected.

(s/def ::syms+ (s/+ symbol?))
(s/def ::syms*  (s/* symbol?))
(s/def ::strs+ (s/+ string?))
(s/def ::strs* (s/* string?))


(deftest or-plus
  (are [val] (s/valid? (s/cat :i integer? :kw keyword?
                              :ss (s/alt :y ::syms* :t ::strs*)) val)
       '[10 :a foo bar baz]
       '[10 :a "foo" "bar" "baz"])
  (are [val] (s/valid? (s/cat :i integer? :kw keyword?
                              :ss (s/alt :sy ::syms+ :st ::strs+)) val)
       '[10 :a foo bar baz]
       '[10 :a "foo" "bar" "baz"]))

(defn plus2 [n] (+ 2 n))
(defn nodd [n] (if (odd? n) (dec (- n)) (inc n)))


(defn iter [f]
  (fn [coll]
    (every? true? (map #(= (f %) %2) coll (rest coll)))))

(defn series?
  ([coll] (series? + 1 coll))
  ([n coll] (series? + n coll))
  ([f n coll]
   ((iter #(f % n)) coll)))

(defn indexed [f]
  (fn [coll]
    (every? true? (map-indexed (fn [i x] (= x (f i))) coll))))

(deftest stepping2
  (is (s/valid? (s/& (s/+ even?) (partial series? 4)) [2 6 10 14]))
  (is (s/valid? (s/& (s/+ integer?) (partial series? * 3)) [2 6 18 54]))
  (is (not (s/valid? (s/& (s/+ even?) (partial series? 4)) [2  10  14])))
  (is (s/valid? (s/& (s/+ integer?) (iter plus2)) [11 13 15 17 19]))
  (is (s/valid? (s/&  (s/+ integer?) (indexed nodd)) [1 -2 3 -4 5 -6])))


(deftest binding-with-when 
  (is (s/valid? (s/& (s/cat :n integer? :m integer?) (fn [m] (= (* (:n m) 2) (:m m))))
                [2 4]))
  (is (not (s/valid? (s/& (s/cat :n integer? :m integer?) (fn [m] (= (* (:n m) 3) (:m m))))
                [2 4])))
  (is (s/valid? (s/& (s/cat :n integer? :ks (s/coll-of keyword?))
                     (fn [m] (= (:n m) (count (:ks m)))))
                [3 [:a :b :c]])))

;; n/a (deftest binding-with-implied-when 

;; n/a (deftest step-count

(deftest and-or
  (is (s/valid? (s/cat :ik (s/alt :i integer? :k keyword?) :e (s/and integer? even?)) [:a 4]))
  (is (s/valid? (s/cat :ks (s/alt :k keyword? :s symbol?) :o (s/and integer? odd?)) [:a 7])))

(deftest not-constraints
  (is (s/valid? (s/cat :nk (s/* (complement symbol?))) [:a]))
  (is (s/valid? (s/cat :iks (s/alt :i integer? :k keyword? :s symbol?) :ni (complement integer?))
                [:a :a]))

  (is (not (s/valid? (s/cat :iks (s/alt :i integer? :k keyword? :s symbol?)
                       :ne (s/and integer? (complement even?)))
                [:a 6.1]))))



;; not the best way to handle this case, but imagine a fancier function
;; n/a (deftest with-constraints

(s/def ::e20 (s/and integer? even? #(<= 0 % 20)))

(deftest pred-args
  (is (s/valid? (s/cat :es (s/+ ::e20) :kw keyword?) [4 10 18 :a]))
  (is (not (s/valid? (s/cat :es (s/+ ::e20) :kw keyword?) [4 30 18 :a])))
  (is (s/valid? (s/& (s/cat :lo integer? :hi integer? :es (s/+ ::e20) :kw keyword?)
                     #(every? (fn [x] (<= (:lo %) x (:hi %))) (:es %)))
                [4 20 14 10 18 :a])))

(deftest strings
  (is (s/valid? string? "foobar"))
  (is (s/valid? (s/and string? #(re-matches #"f.*r" %)) "foobar"))
  (is (not (s/valid? (s/and string? #(re-matches #"f.*r" %)) "xfoobar"))))


(s/def ::a integer?)
(s/def ::bb integer?)
(s/def ::b (s/keys :req-un [::bb]))

(deftest nested-map-when
  (is (s/valid? (s/keys :req-un [::a ::b])
                {:a 10 :b {:bb 10}}))
  (is (s/valid? (s/and (s/keys :req-un [::a ::b])
                     #(= (:a %) (:bb (:b %))))
                {:a 10 :b {:bb 10}}))
  (is (not (s/valid? (s/and (s/keys :req-un [::a ::b])
                          #(= (:a %) (:bb (:b %))))
                     {:a 11 :b {:bb 10}}))))


;; n/a (deftest solo-constraints-for-equality

;; n/a (deftest solo-count

(deftest bind-args 
  (is (s/valid? (s/& (s/cat :a integer? :b integer? :c integer?) #(<= (:a %) (:c %) (:b %)))
                [3 7 5])))


;; n/a (deftest underbar-bind-args 

;; n/a (deftest as-bind-args 

;; n/a (deftest quoted-syms

#_
(deftest regex-forms
  (is (s/valid? '(kw ":foo/.*") :foo/bar))
  (is (not (s/valid? '(kw ":foo/.*") :foo)))
  (is (s/valid? '(kw ":miner[.]test-herbert/foo") ::foo))
  (is (s/valid? '(str "foo/.*") "foo/bar"))
  (is (s/valid? '(str "fo+.ar") "fooooobar"))
  (is (s/valid? '(sym "user/.*") 'user/foobar))
  (is (not (s/valid? '(sym "user/.*") :user/foobar))))

(defn palindrome? [s]
  (and (string? s)
       (= s (clojure.string/reverse s))))

#_
(deftest grammar
  (is (s/valid? '(grammar {:a over3 :b long}
                          over3 (pred miner.test-herbert/over3?)
                          long int)
                 {:a 42 :b 42}))
  (is (s/valid? '(grammar [pal+]
                          palindrome (pred miner.test-herbert/palindrome?)
                          pal {:len (:= len int) :palindrome (and palindrome (cnt len))})
                 [{:palindrome "civic" :len 5}
                  {:palindrome "kayak" :len 5} 
                  {:palindrome "level" :len 5}
                  {:palindrome "ere" :len 3}
                  {:palindrome "racecar" :len 7}])))

(s/def ::A (s/or :a (partial = :a)
                  :b (s/cat :b (partial = :b) :as (s/+ ::A))))

(deftest recursive
  (is (s/valid? ::A :a))
  (is (s/valid? ::A [:b :a]))
  (is (s/valid? ::A [:b [:b :a :a] :a])))

(s/def ::Ax (s/or :a (partial = :a) :v (s/cat :av (partial = :av) :a1 ::Ax)))
(s/def ::Bx (s/or :b (partial = :b) :v (s/cat :bv (partial = :bv) :b1 ::Bx)))

(deftest recursive-grammar
  (let [r? #(s/valid? (s/cat :a ::Ax :b ::Bx) %)]
    (is (r?  [:a :b]))
    (is (r? [[:av :a] :b]))
    (is (r? [[:av :a] [:bv :b]]))
    (is (r? [[:av [:av :a]] [:bv [:bv :b]]]))))


;; others very similar
#_
(deftest recursive-doubly
  (let [r? (conform '[(:= a (or :a [:av a])) (:= b (or :b [:bv b]))] )]
    (is (r? [:a :b]))
    (is (r? [[:av :a] :b]))
    (is (r? [[:av :a] [:bv :b]]))
    (is (r? [[:av [:av :a]] [:bv [:bv :b]]]))))

#_
(deftest recursive-plus
  (let [r? (conform '[(:= a (or :a [:av a+])) (:= b (or :b [:bv b]))] )]
    (is (r? [:a :b]))
    (is (r? [[:av :a :a] :b]))
    (is (r? [[:av :a [:av :a [:av :a]]] [:bv :b]]))
    (is (r? [[:av [:av :a :a :a] :a] [:bv [:bv :b]]]))))


;; n/a
#_
(deftest quantified-keys-vals
  (is (s/valid? '{kw* int*} {:a 42}))
  (is (s/valid? '{kw* int*} {}))
  (is (s/valid? '{kw+ int+} {:a 42}))
  (is (not (s/valid? '{kw+ int+} {})))
  (is (not (s/valid? '{kw* int*} {'a 42})))
  (is (not (s/valid? '{kw* int*} {:a 'b52})))
  (is (s/valid? '{(* (or sym kw)) (* (or sym int))} {:a 'b52}))
  (is (s/valid? '(map (+ (or sym kw)) (+ (or sym int))) {'b 'b52}))
  (is (s/valid? '(map (* (or sym kw)) (* (or sym int))) {'b 52}))
  (is (s/valid? '(map sym* any*) {'b 52}))
  (is (s/valid? '(map any* any*) {'b 52}))
  (is (s/valid? 'map {'b 52}))
  (is (not (s/valid? '(map (* (or sym kw)) (* (or sym int))) {:a :b52}))))
  
(defrecord Foo [a])

#_
(deftest on-records
  (is (s/valid? '{:a int} (->Foo 42)))
  (is (s/valid? '(& (:= rec {:a int}) (when (instance? miner.test_herbert.Foo rec)))
                 (->Foo 42)))
  (is (not (s/valid? '(& (:= rec {:a int}) (when (instance? miner.test_herbert.Foo rec)))
                 {:a 42})))
  (is (not (s/valid? '(& (:= rec {:a int}) (when (instance? miner.test_herbert.Foo rec)))
                 {->Foo "bar"}))))

#_
(deftest on-records-by-class
  (is (s/valid? '{:a int} (->Foo 42)))
  (is (s/valid? '(and {:a int} (class miner.test_herbert.Foo))
                 (->Foo 42)))
  (is (not (s/valid? '(and {:a int} (class miner.test_herbert.Foo))
                 {:a 42})))
  (is (not (s/valid? '(and {:a int} (class miner.test_herbert.Foo))
                 {->Foo "bar"}))))

#_
(deftest records-by-tag
  (is (s/valid? '(tag miner.test-herbert/Foo) (->Foo 42)))
  (is (s/valid? '(tag miner.test-herbert/Foo {:a 42}) (->Foo 42)))
  (is (s/valid? '(tag "miner[.]test-.*/Foo") (->Foo 42)))
  (is (not (s/valid? '(tag "miner/test-.*Foo") (->Foo 42))))
  (is (s/valid? '(tag foo.bar/Baz) (tag/read-string "#foo.bar/Baz {:a 42}")))
  (is (not (s/valid? '(tag miner.test-herbert/Bad) (->Foo 42))))
  (is (not (s/valid? '(tag foo.wrong/Bar) (tag/read-string "#foo.bar/Baz {:a 42}"))))
  (is (s/valid? '(tag miner.test-herbert/Foo {:a int}) (->Foo 42)))
  (is (s/valid? '(tag "miner[.]test-herbert/F.*" {:a int}) (->Foo 42)))
  (is (not (s/valid? '(tag "miner[.]test-herbert/F.*" {:a sym}) (->Foo 42))))
  (is (s/valid? '(tag foo.bar/Baz {:a int}) (tag/read-string "#foo.bar/Baz {:a 42}")))
  (is (not (s/valid? '(tag miner.test-herbert/Foo {:b any}) (->Foo 42))))
  (is (not (s/valid? '(tag foo.wrong/Bar {:a int}) (tag/read-string "#foo.bar/Baz {:a 42}")))))

#_
(deftest dates-and-uuid
  (is (s/valid? '(tag inst) (java.util.Date.)))
  (is (s/valid? '(tag inst) (java.sql.Timestamp. 0)))
  (is (s/valid? '(tag inst) (java.util.Calendar/getInstance)))
  (is (s/valid? '(tag inst "1970-01-01T00:00:00.000-00:00") (java.util.Date. 0)))
  (is (s/valid? '(tag inst "1970.*") (java.util.Date. 0)))
  (is (s/valid? '(tag inst "1970.*") #inst "1970"))
  (is (not (s/valid? '(tag inst "1970.*") "1970")))
  (is (not (s/valid? '(tag inst "1980.*") #inst "1990"))))

#_
(deftest nested-inst-and-uuid
  (is (s/valid? '[[:create-resource
                   [:resource/id (tag uuid "552fc7b3-8905-46fd-b50b-dd613c940504")]
                   {:occurrence/place 281474976711832,
                    :occurrence/start-time (tag inst)}]
                  [:create-resource
                   [:resource/id (tag uuid)]
                   {:occurrence/place 281474976711832,
                    :occurrence/start-time (tag inst "1970-01-01T00:00:00.002-00:00")}]
                  [:create-resource
                   [:resource/id (tag uuid)]
                   {:occurrence/place 281474976711832,
                    :occurrence/start-time (tag inst "1970-01-01T00:00:00.003-00:00")}]]

                 [[:create-resource
                   [:resource/id #uuid "552fc7b3-8905-46fd-b50b-dd613c940504"]
                   {:occurrence/place 281474976711832,
                    :occurrence/start-time #inst "1970-01-01T00:00:00.001-00:00"}]
                  [:create-resource
                   [:resource/id #uuid "552fc7b3-5ffa-4eb8-b3ac-8e3b15bcead0"]
                   {:occurrence/place 281474976711832,
                    :occurrence/start-time #inst "1970-01-01T00:00:00.002-00:00"}]
                  [:create-resource
                   [:resource/id #uuid "552fc7b3-af79-489c-8eaf-09e3845106d0"]
                   {:occurrence/place 281474976711832,
                    :occurrence/start-time #inst "1970-01-01T00:00:00.003-00:00"}]])))

#_
(deftest readme-examples
  (is (= ((conform '[(:= A int) (:= B int) (:= C int+ A B)]) [3 7 4 5 6])
         '{C [4 5 6], B 7, A 3}))
  (is (= ((conform '[(:= MAX int) (:= XS int+ MAX)]) [7 3 5 6 4])
         '{XS [3 5 6 4], MAX 7}))
  (is (s/valid? '{:a int :b [sym+] :c str} '{:a 42 :b [foo bar baz] :c "foo"}))
  (is (s/valid? '{:a int :b sym :c? [str*]} '{:a 1 :b foo}))
  (is (s/valid? '{:a int :b sym :c? [str*]} '{:a 1 :b foo :c ["foo" "bar" "baz"]}))
  (is (not (s/valid? '{:a int :b sym :c? [str*]} '{:a foo :b bar})))
  (is (s/valid? '{:a (:= A int) :b sym :c? [A+]} '{:a 1 :b foo :c [1 1 1]}))
  (is (s/valid? '{:x? sym ':k? int} {:k? 10}))
  (is (not (s/valid? '{:x? sym ':k? int} {:k 10}))))

#_
(deftest grammar-with-regex
  (is (s/valid? '(grammar [person+] 
                          phone (str #"\d{3}+-\d{3}+-\d{4}+") 
                          person {:name str :phone phone}) 
                 [{:name "Steve" :phone "408-555-1212"}
                  {:name "Jenny" :phone "415-867-5309"}]))
  ;; only difference is a regex above and a string with escape notation below
  (is (s/valid? '(grammar [person+] 
                          phone (str "\\d{3}+-\\d{3}+-\\d{4}+") 
                          person {:name str :phone phone}) 
                 [{:name "Steve" :phone "408-555-1212"}
                  {:name "Jenny" :phone "415-867-5309"}])))

#_
(deftest nested-grammar
  (is (not (s/valid? '(grammar [short+] short (grammar sh sh (int 255))) '[2 3000 2 4])))
  (is (s/valid? '(grammar [short+] short (grammar sh sh (int 255))) '[2 3 2 4])))

#_
(deftest merging-grammar
  (let [s1 '(grammar [iii+] iii (int 3))
        s2 '(grammar [sss+] sss (sym "..."))
        s3 '(grammar [kkk+] kkk (kw ":..."))]
    (is (= (schema-merge '[iii sss kkk] s1 s2 s3)
           '(grammar [iii sss kkk]
                    iii (int 3)
                    sss (sym "...")
                    kkk (kw ":..."))))
    (is (= (schema-merge '(grammar [jjj sss kkk] jjj {:a iii}) s1 s2 s3)
           '(grammar [jjj sss kkk]
                    iii (int 3)
                    sss (sym "...")
                    kkk (kw ":...")
                    jjj {:a iii})))))

#_
(deftest char-lits
  (is (s/valid? \k \k))
  (is (s/valid? [\f \o \o] (seq "foo")))
  (is (s/valid? \f (first "foo")))
  (is (s/valid? \o (last "foo")))
  (is (s/valid? '[char+] (vec (seq "bar")))))

#_
(deftest char-regex
  (is (s/valid? '(char "x") \x))
  (is (s/valid? '(char "[a-z]") \x))
  (is (not (s/valid? '(char "[a-z]") \X)))
  (is (not (s/valid? '(char "[a-z]") 42)))
  (is (not (s/valid? '(char "[a-z]x") \x))))

#_
(deftest top-kw
  ;; top-level :k? is still optional; use ':k? if the qmark is part of the literal
  (is (s/valid? [:k?] [:k]))
  (is (s/valid? [:k?] []))
  (is (s/valid? [':k] [:k]))
  (is (s/valid? '[(? :k)] [:k]))
  (is (s/valid? '[(? :k)] []))
  ;; tricky needs both quotes below to work as literal
  (is (s/valid? '[':k?] [:k?]))
  (is (not (s/valid? '[':k?] [:k])))
  (is (s/valid? '[(? ':k?)] [:k?]))
  (is (not (s/valid? '[(? ':k?)] [:k])))
  (is (s/valid? '[(? :k)] [:k]))
  (is (not (s/valid? '[(? :k)] [:k?])))
  (is (not (s/valid? [:k?] [42])))
  (is (not (s/valid? [:k?] [:a]))))

#_
(deftest singleton-collections
  (is (s/valid? '[int] [1]))
  (is (s/valid? '(seq int) [1]))
  (is (s/valid? '(vec int) [1]))
  (is (s/valid? '(list int) '(1)))
  (is (not (s/valid? '[int] [])))
  (is (not (s/valid? '(seq int) [])))
  (is (not (s/valid? '(list int) ())))
  (is (not (s/valid? '(vec int) [])))
  (is (s/valid? '[int] [1 2]))
  (is (s/valid? '(seq int) [1 2]))
  (is (s/valid? '(list int) '(1 2)))
  (is (s/valid? '(vec int) [1 2]))
  (is (not (s/valid? '[int?] [1 2])))
  (is (not (s/valid? '(seq int?) [1 2])))
  (is (not (s/valid? '(list int?) '(1 2))))
  (is (not (s/valid? '(vec int?) [1 2]))))

#_
(deftest pair-collections
  (is (s/valid? '[int int] [1 2]))
  (is (s/valid? '(seq int int) [1 2]))
  (is (s/valid? '(vec int int) [1 2]))
  (is (s/valid? '(list int int) '(1 2)))
  (is (not (s/valid? '[int int] [])))
  (is (not (s/valid? '(seq int int) [])))
  (is (not (s/valid? '(list int int) ())))
  (is (not (s/valid? '(vec int int) [])))
  (is (not (s/valid? '[int int] [1 2 3])))
  (is (not (s/valid? '(seq int int) [1 2 3])))
  (is (not (s/valid? '(list int int) '(1 2 3))))
  (is (not (s/valid? '(vec int int) [1 2 3]))))

#_
(deftest empty-collections
  (is (not (s/valid? '[] [1])))
  (is (not (s/valid? '() '(1))))
  (is (s/valid? '[] []))
  (is (s/valid? '[] ()))
  (is (s/valid? '(seq) [1]))
  (is (s/valid? '(seq) '(1)))
  (is (s/valid? '(list) '(1)))
  (is (s/valid? '(vec) [1]))
  (is (not (s/valid? {} {:a 1})))
  (is (s/valid? '(map) {:a 1}))
  (is (s/valid? 'map {:a 1}))
  (is (s/valid? 'map {}))
  (is (s/valid? '(map) {}))
  (is (s/valid? 'list '(1)))
  (is (s/valid? 'seq '(1)))
  (is (s/valid? 'seq [1]))
  (is (s/valid? 'vec [1]))
  (is (s/valid? 'list ()))
  (is (s/valid? 'seq ()))
  (is (s/valid? 'seq []))
  (is (s/valid? 'vec []))
  (is (not (s/valid? '() [1])))
  (is (not (s/valid? '() '(1))))
  (is (not (s/valid? '[] [1])))
  (is (not (s/valid? '() '(1))))
  (is (s/valid? '[] []))
  (is (s/valid? '(seq) []))
  (is (s/valid? '(list) ()))
  (is (s/valid? '(vec) [])))


#_
(deftest disallowed-keys
  (are [schema val] (not (s/valid? schema val))
       ;; note: not
       '{:a int :b str} {:a 10}
       '{:a int :b str} {:b "foo"}
       '{(* (or :a :b)) (* any)} {:a 10 :b 20 :c 30}
       '{(or :a :b) int} {:a 10 :b 20 :c 30}))

#_
(deftest single-maps
  (is (s/valid? '[(* {:a int})] [{:a 1}]))
  (is (s/valid? '[(* (map :a int))] [{:a 1}]))
  (is (s/valid? '[(* {:a int})] [{:a 1 :b 2}]))
  (is (s/valid? '[(* (map :a int))] [{:a 1 :b 2}]))
  (is (not (s/valid? '[(* {:a int})] {:a 1})))
  (is (not (s/valid? '[(* (map :a int))] {:a 1})))
  (is (not (s/valid? '[(* {:a int})] {:a 1 :b 2})))
  (is (not (s/valid? '[(* (map :a int))] {:a 1 :b 2}))))

#_
(deftest many-quantified-maps
  (is (s/valid? '{kw int} {:a 1 :b 2}))
  (is (not (s/valid? '{kw int} {})))
  (is (s/valid? '{kw int} {:a 1 :b 2}))
  (is (s/valid? '{kw int} {:a 1}))
  (is (s/valid? '{kw? int?} {}))
  (is (s/valid? '{kw? int?} {:a 1}))
  (is (not (s/valid? '{kw? int?} {:a 1 :b 2})))
  (is (not (s/valid? '{kw+ int+} {})))
  (is (s/valid? '{kw* int*} {:a 1 :b 2}))
  (is (s/valid? '{kw* int*} {}))
  (is (s/valid? '{(* kw) (* int)} {:a 1 :b 2}))
  (is (s/valid? '(map kw int) {:a 1 :b 2}))
  (is (s/valid? '(map kw? int?) {}))
  (is (s/valid? '(map kw? int?) {:a 1}))
  (is (not (s/valid? '(map kw? int?) {:a 1 :b 2})))
  (is (not (s/valid? '(map kw+ int+) {})))
  (is (s/valid? '(map kw+ int+) {:a 1 :b 2}))
  (is (s/valid? '(map kw* int*) {:a 1 :b 2}))
  (is (s/valid? '(map (* kw) (* int)) {:a 1 :b 2}))
  (is (not (s/valid? '{kw int} {:a 1 :b 2 :c :xxx})))
  (is (not (s/valid? '{kw* int*} {:a 1 'xxx 2})))
  (is (not (s/valid? '{(* kw) (* int)} {:a 1 :b "foo"})))
  (is (not (s/valid? '(map kw int) [:a 1 :b 2])))
  (is (not (s/valid? '(map kw* int*) [{:a 1 :b 2}])))
  (is (not (s/valid? '(map (* kw) (* int)) {:a :b})))
  (is (not (s/valid? '{kw+ int+} {})))
  (is (not (s/valid? '(map kw+ int+) {})))
  (is (s/valid? '{int* kw*} {1 :a 2 :b}))
  (is (s/valid? '{(int* 10) kw*} {1 :a 2 :b}))
  (is (s/valid? '{(int 10) kw} {1 :a 2 :b}))
  (is (not (s/valid? '{(int* 10) kw*} {11 :a 2 :b})))
  (is (s/valid? '(map int* kw*) {1 :a 2 :b}))
  (is (s/valid? '(map (int* 10) kw*) {1 :a 2 :b}))
  (is (not (s/valid? '(map (int 10) kw) {11 :a 2 :b})))
  (is (not (s/valid? '(map (int* 10) kw*) {11 :a 2 :b}))))

#_
(deftest in-operator
  (is (s/valid? '(in #{:a :b :c}) :a))
  (is (s/valid? '(or :a :b :c) :a))    ;; better
  (is (s/valid? '[(:= mmm map) (* (in mmm))] [{:a 1 :b 2 :c 3} :a :b :c :a]))
  (is (s/valid? '[(:= mmm map) (in* mmm)] [{:a 1 :b 2 :c 3} :a :b :c :a :c]))
  ;; old way with "when"
  (is (s/valid? '[(:= mmm map) (* (:= k kw) (when (contains? mmm k)))]
                 [{:a 1 :b 2 :c 3} :a :b :c]))
  (is (s/valid? '[(:= vs vec) (* (in vs))] [[:a :b :c] :a :b :c :a]))
  (is (not (s/valid? '[(:= vs vec) (* (in vs))] [[:a :b :c] :a :b :c :a :d])))
  (is (s/valid? '[(:= vs list) (* (in vs))] '[(:a :b :c) :a :b :c :a]))
  (is (not (s/valid? '[(:= vs list) (* (in vs))] '[(:a :b :c) :a :d :c :a]))))
  
#_
(deftest collection-keys
  (is (s/valid? '{:a int '[:foo "bar"] str} '{:a 10 [:foo "bar"] "baz"}))
  (is (s/valid? '{:a int (? '[:foo "bar"]) str} '{:a 10 [:foo "bar"] "baz"}))
  (is (s/valid? '{:a int (? '[:foo "bar"]) str} '{:a 10}))
  (is (not (s/valid? '{:a int (? '[:foo "bar"]) str} '{:a "foo"})))
  (is (not (s/valid? '{:a int (? '[:foo "bar"]) str} '{:a 10 [:foo "bar"] 13})))
  (is (s/valid? '{'[:foo "bar"] str} '{:a 10 [:foo "bar"] "baz"}))
  (is (s/valid? '{(? '[:foo "bar"]) str} '{:a 10 [:foo "bar"] "baz"}))
  (is (s/valid? '{(? '[:foo "bar"]) str} '{:a 10}))
  (is (s/valid? '{(? '[:foo "bar"]) str} '{:a "foo"}))
  (is (not (s/valid? '{(? '[:foo "bar"]) str} '{[:foo "bar"] 13})))
  (is (not (s/valid? '{(? '[:foo "bar"]) str} '{:a 10 [:foo "bar"] 13}))))

;; need test for cache collisions in s/valid?  Here are some strings that hash the same
;; (apply = (map hash '("AaAaAa" "AaAaBB" "AaBBAa" "BBAaAa" "BBAaBB" "BBBBAa" "BBBBBB")))
;; => true
;; (= (hash "FB") (hash "Ea"))
;; => true

#_
(deftest conform-vars
  (let [colliding-strings '("AaAaAa" "AaAaBB" "AaBBAa" "BBAaAa" "BBAaBB" "BBBBAa"
                            "BBBBBB")
        matching-vars (map schema-conformance-var colliding-strings)]
    (is (apply = (map hash colliding-strings)))
    (is (= (hash "FB") (hash "Ea"))) ;; another example
    (is (apply distinct? matching-vars))
    (is (every? true? (map (fn [f x] (f x)) matching-vars colliding-strings)))))

