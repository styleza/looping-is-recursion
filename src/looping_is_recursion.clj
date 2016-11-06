(ns looping-is-recursion)

(defn power [base exp]
	(if (zero? exp) 1 
	(let [helper (fn [b x] 
		(if (zero? x)
			b
			(recur (* b base) (dec x))
		))] 
		(helper base (dec exp))
	))
)

(defn last-element [a-seq]
	(if (empty? (rest a-seq))
		(first a-seq)
		(recur (rest a-seq))
	)
)

(defn seq= [seq1 seq2]
	(cond
		(and (empty? seq1) (empty? seq2))
			true
		(or (empty? seq1) (empty? seq2))
			false
		(= (first seq1) (first seq2))
			(recur (rest seq1) (rest seq2))
		:else
			false
	)
)

(defn find-first-index [pred a-seq]
	(loop [n 0] 
		(cond
			(>= n (count a-seq))
				nil
			(empty? a-seq)
				nil
			(pred (get a-seq n))
				n
			:else
				(recur (inc n))
		)
	)
)

(defn avg [a-seq]
	(loop [i 0 s 0]
		(cond
			(>= i (count a-seq))
				(/ s (count a-seq))
			:else
				(recur (inc i) (+ s (get a-seq i)))
		)
	)
)

(defn parity [a-seq]
	(loop [i 0 odds #{} ]
		(cond
			(>= i (count a-seq))
				odds
			:else
				(recur (inc i)
					(if (contains? odds (get a-seq i))
						(disj odds (get a-seq i))
						(conj odds (get a-seq i))
					)
				)
				
		)
	)
)

(defn fast-fibo [n]
	(loop [i 0 f0 0 f1 1]
		(if (= i n)
			f0
			(recur (inc i) f1 (+ f0 f1))
		)
	)
)

(defn cut-at-repetition [a-seq]
	(loop [i 0 duk #{} ]
		(cond
			(>= i (count a-seq))
				(subvec a-seq 0 i)
			(contains? duk (get a-seq i))
				(subvec a-seq 0 i)
			:else
				(recur (inc i) (conj duk (get a-seq i)))
		)
	)
)

