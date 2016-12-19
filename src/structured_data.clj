(ns structured-data)

(defn do-a-thing [x]
      (let [myx (+ x x)]
           (Math/pow myx myx)))

(defn spiff [v]
      (+ (get v 0) (get v 2)))

(defn cutify [v]
      (conj v "<3"))

(defn spiff-destructuring [v]
      (let [[x y z] v]
           (+ x z)))

(defn point [x y]
      [x y])

(defn rectangle [bottom-left top-right]
      [bottom-left top-right])

(defn width [rectangle]
      (let [[[x1 y1] [x2 y2]] rectangle]
           (- x2 x1)))

(defn height [rectangle]
      (let [[[x1 y1] [x2 y2]] rectangle]
           (- y2 y1)))

;(defn square? [rectangle]
;      (let [[[x1 y1] [x2 y2]] rectangle]
;             (== (- x2 x1) (- y2 y1))))

(defn square? [rectangle]
      (== (width rectangle) (height rectangle)))

(defn area [rectangle]
      (* (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
      (let [[[x1 y1] [x2 y2]] rectangle
            [p1 p2] point]
           (and
             (<= x1 p1 x2)
             (<= y1 p2 y2))))

(defn contains-rectangle? [outer inner]
      (let [[i1 i2] inner]
           (and
             (contains-point? outer i1)
             (contains-point? outer i2)
             )))

(defn title-length [book]
      (count (get book :title)))

(defn author-count [book]
      (count (get book :authors)))

(defn multiple-authors? [book]
      (> (author-count book) 1))

(defn add-author [book new-author]
      (let [authors (get book :authors)
            newAuthors (conj authors new-author)]
           (assoc book :authors newAuthors)))

(defn alive? [author]
      (not (contains? author :death-year)))

(defn element-lengths [collection]
      (map count collection))

(defn second-elements [collection]
      (let [second (fn [col] (get col 1))]
           (map second collection)))

(defn titles [books]
      (map :title books))

(defn monotonic? [a-seq]
      (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
      (apply str (repeat n "*")))

(defn toggle [a-set elem]
      (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
      (not= (count a-seq) (count (set a-seq))))

(defn old-book->new-book [book]
      (let [authors (get book :authors)
            newAuthors (set authors)]
           (assoc book :authors newAuthors)))

(defn has-author? [book author]
      (let [authors (set (get book :authors))]
           (contains? authors author)))

(defn authors [books]
      (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
      (set (map :name (authors books))))

(defn author->string [author]
      (let [name (:name author)
            birth (if (:birth-year author) (str " (" (:birth-year author) " - ") "")
            death (if (:death-year author) (str (:death-year author) ")") ")")]
           (str name birth (if (clojure.string/blank? birth) "" death))))

(defn authors->string [authors]
      (apply str (interpose ", " (map author->string authors))))

(defn book->string [book]
      (str (:title book) ", written by " (authors->string (:authors book))))

(defn books->string [books]
      (let [amount (count books)
            number (if (> amount 1) (str amount " books. ") "1 book. ")]
           (if (= amount 0)
             "No books."
             (str number (apply str (map book->string books)) "."))))

(defn books-by-author [author books]
      (filter (fn [x] (has-author? x author)) books))

(defn author-by-name [name authors]
      :-)

(defn living-authors [authors]
      :-)

(defn has-a-living-author? [book]
      :-)

(defn books-by-living-authors [books]
      :-)

