(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _]  card
        rank-map {\T 10, \J 11, \Q 12, \K 13, \A 14}]
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (rank-map fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(defn pair? [hand]
  (let [hand-freq-map (->> (map rank hand)
                           (frequencies)
                           (vals)
                           (frequencies))]
    (and (= (hand-freq-map 2) 1)
         (= (hand-freq-map 1) 3))))

(defn three-of-a-kind? [hand]
  (let [hand-freq-map (->> (map rank hand)
                           (frequencies)
                           (vals)
                           (frequencies))]
    (and (= (hand-freq-map 3) 1)
         (= (hand-freq-map 1) 2))))

(defn four-of-a-kind? [hand]
  (let [hand-freq-map (->> (map rank hand)
                           (frequencies)
                           (vals)
                           (frequencies))]
    (and (= (hand-freq-map 4) 1)
         (= (hand-freq-map 1) 1))))

(defn flush? [hand]
  (let [hand-freq-map (->> (map suit hand)
                           (frequencies)
                           (vals)
                           (frequencies))]
    (= (hand-freq-map 5) 1)))

(defn full-house? [hand]
  (let [hand-freq-map (->> (map rank hand)
                           (frequencies)
                           (vals)
                           (frequencies))]
    (and (= (hand-freq-map 3) 1)
         (= (hand-freq-map 2) 1))))

(defn two-pairs? [hand]
  (let [hand-freq-map (->> (map rank hand)
                           (frequencies)
                           (vals)
                           (frequencies))]
    (or (= (hand-freq-map 2) 2)
        (= (hand-freq-map 4) 1))))

(defn straight? [hand]
  (let [sorted-by-rank (sort (map rank hand))
        replace-ace    (if (and (= (last sorted-by-rank) 14)
                                (= (first sorted-by-rank) 2))
                         (sort (replace {14 1} sorted-by-rank))
                         sorted-by-rank)
        low-card       (first replace-ace)
        high-card      (last replace-ace)]
    (= replace-ace (range low-card (inc high-card)))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers       #{[high-card? 0]      [pair? 1]
                         [two-pairs? 2]      [three-of-a-kind? 3]
                         [straight? 4]       [flush? 5]
                         [full-house? 6]     [four-of-a-kind? 7]
                         [straight-flush? 8]}
        checker-result (map (fn [h]
                              (let [[f s] h]
                                [(f hand) s]))
                            checkers)
        valid-hands    (filter (fn [h] (let [[f s] h] f)) checker-result)]
    (apply max (map second valid-hands))))
