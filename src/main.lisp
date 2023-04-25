(defpackage mahjong
  (:import-from :alexandria :length= :shuffle :when-let)
  (:import-from :trivia :match)
  (:use :cl))
(in-package :mahjong)

(defparameter *suites* '(:dots :bamboo :characters))
(defparameter *winds* '(:north :south :east :west))
(defparameter *dragons* '(:red :green :white))

(defstruct tile
  "An individual tile.

Suites faces are :dots, :bamboo, and :characters.
Honorary faces are :winds and :dragons

Suites values are a number from 1 to 9.
:winds values are :north, :south, :east, and :west
:dragons values are :red, :green, and :white"
  face
  value)

(defun honorary-members (face)
  "Return all members of an honorary face."
  (if (eq :winds face) *winds* *dragons*))

(defun honoraryp (face)
  "Whether the face is an honorary."
  (or (eq :winds face) (eq :dragons face)))

(defun suitesp (face)
  "Whether the face is a suite."
  (or (eq :dots face) (eq :bamboo face) (eq :characters face)))

(defun make-dots-tile (value)
  (make-tile :face :dots :value value))

(defun make-bamboo-tile (value)
  (make-tile :face :bamboo :value value))

(defun make-characters-tile (value)
  (make-tile :face :characters :value value))

(defun make-winds-tile (direction)
  (make-tile :face :winds :value direction))

(defun make-dragons-tile (color)
  (make-tile :face :dragons :value color))

(defun inc-tile (ti)
  "Return suites tile with a value one greater."
  (if (honoraryp (tile-face ti))
      (error "Attempted to increment an honorary tile.")
      (match ti
        ((tile :face f :value v)
         (make-tile :face f :value (1+ v))))))

(defun tile-comparator-value (tile)
  (with-accessors ((f tile-face) (v tile-value))
      tile
    (if (honoraryp f)
        (position v (honorary-members f))
        v)))

(defun sort-tiles (ls)
  (sort ls #'< :key #'tile-comparator-value))

(defun collapse-alist (alist)
  "Remove shadowed elements from ALIST."
  (remove-duplicates alist :key #'car :from-end t))

(defun group-by-alist (ls key-fn)
  "Group LS by KEY-FN into an alist. Key is determined by KEY-FN and value is a list of matches."
  (loop with tbl = nil
        for el in ls
        for k = (funcall key-fn el)
        for v = (cdr (assoc k tbl))
        do (setf tbl (acons k (cons el v) tbl))
        finally (return (collapse-alist tbl))))

(defun arrange-tiles (tiles)
  "Arrange TILES into an alist where key is the face and value is a sorted list of tiles."
  (loop with tbl = (group-by-alist tiles #'tile-face)
        for (k . v) in tbl
        collect (cons k (sort-tiles v))))

(defclass hand ()
  ((held
    :initarg :held
    :accessor held
    :documentation "Tiles the player has not shown. These are grouped by face and sorted.")
   (revealed
    :initform nil
    :accessor revealed
    :documentation "Tiles the player has shown, grouped into sets.")
   )
  (:documentation "Tiles held by an individual."
))

(defun make-hand (tiles)
  "Make a hand from a list of TILES. All tiles are held, not revealed."
  (let ((arranged (arrange-tiles tiles)))
    (make-instance 'hand :held arranged)))

(defun update-alist (alist key value)
  "Update ALIST to set KEY to VALUE without leaving a duplicate."
  (acons key value (remove key alist :key #'car)))

(defun lookup-alist (alist key)
  (cdr (assoc key alist)))

(defun update-hand-held (hand face fn)
  (let* ((held-tiles (held hand))
         (of-suite (lookup-alist held-tiles face)))
    (setf (held hand)
          (update-alist held-tiles face (funcall fn of-suite)))))

(defun discard (tile hand)
  "Discard TILE from HAND.

Returns the new hand."
  (update-hand-held hand
                    (tile-face tile)
                    (lambda (of-suite)
                      (delete tile of-suite :count 1 :test #'equalp))))

(defun insert-into-sorted (item ls &key (test #'<) (key nil))
  "Insert ITEM into an already sorted LS."
  (let ((citem (if key (funcall key item) item)))
    (loop for c on ls
          for el = (car c)
          while (funcall test (if key (funcall key el) el) citem)
          collect el into before
          finally (return (append before (list item) c)))))

(defun draw (tile hand)
  "Discard TILE from HAND.

Returns the new hand."
  (update-hand-held hand
                    (tile-face tile)
                    (lambda (of-suite)
                      (insert-into-sorted tile of-suite :key #'tile-comparator-value))))

(defmethod print-object ((obj hand) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((held held)
                     (revealed revealed))
        obj
      (format stream "Held: ~a~%Revealed: ~a" held revealed))))

(defun chunk-list (size ls)
  "Create sub-lists of SIZE from LS. The last list may be smaller."
  (loop
    for front = ls then next
    for next = (nthcdr size front)
    collect (ldiff front next)
    while next))

(defun drop-first-group (ls)
  "Return LS with the first group of equalp elements removed."
  (loop with f = (car ls)
        for c on ls
        while (equalp (car c) f)
        finally (return c)))

(defun group-list (ls)
  "Group LS with equalp elements in sublists."
  (loop
    for front = ls then next
    for next = (drop-first-group front)
    collect (ldiff front next)
    while next))

(defun find-max-honorary-sets (tile-list)
  "Find the maximum number of sets for a a TILE-LIST of single honorary face."
  (loop with sets and unused
        for g in (group-list tile-list)
        do (loop for cnk in (chunk-list 3 g)
                 if (alexandria:length= 3 cnk)
                   do (push cnk sets)
                 else
                   do (setf unused (nconc cnk unused)))
        finally (return (values sets unused))))

(defun get-peng (tile remaining)
  "Try to construct a 'peng' with TILE using the REMAINING list of tiles.

REMAINING must be sorted with TILE having been removed from the front."
  (when (> (length remaining) 1)
    (let ((start (subseq remaining 0 2))
          (following (nthcdr 2 remaining)))
      (when (every (lambda (x) (equalp x tile)) start)
        (list :set (cons tile start) :remaining following)))))

(defun get-chi (tile remaining)
  "Try to construct a 'chi' with TILE using the REMAINING list of tiles.

REMAINING must be sorted with TILE having been removed from the front."
  (block out
    (loop with next = (inc-tile tile) and chi = (list tile) and unused
          for xs on remaining
          for x = (first xs)
          if (equalp next x)
            do (progn (setf next (inc-tile x))
                      (push x chi)
                      (when (alexandria:length= 3 chi)
                        (return-from out (list :set (reverse chi)
                                               :remaining (append (reverse unused) (rest xs))))))
          else
            do (push x unused))))

(defun choose-best (possibilities cmp-fn)
  "Choose the best from a list of POSSIBILITIES. Uses CMP-FN which takes two parameters and returns whichever one is considered better."
  (loop with best = nil
        for p in possibilities
        if (or (not best) (funcall cmp-fn p best))
          do (setf best p)
        finally (return best)))

(defun pairp (ls)
  "Predicate which matches a LS of two equalp elements."
  (and (alexandria:length= 2 ls)
       (equalp (first ls) (second ls))))

(defun best-suite-sets (a b)
  "Return the better of the two list of sets for a single suite.

Each parameter is a p-list with the :sets that were able to be created and the :unused tiles not placed into a set."
  (let ((num-sets-a (length (getf a :sets)))
        (num-sets-b (length (getf b :sets)))
        (pair-unused-a (pairp (getf a :unused))))
    (cond ((> num-sets-a num-sets-b) a)
          ((< num-sets-a num-sets-b) b)
          ;; Prefer one with unused pair in case of tie.
          ;; This makes it easier to detect winning.
          (pair-unused-a a)
          (t b))))

(defun build-possibility (set-data)
  "Builds a set possibility from SET-DATA."
  (let ((child (find-max-suite-sets (getf set-data :remaining))))
    (list* :sets (cons (getf set-data :set) (getf child :sets))
           child)))

(defun find-max-suite-sets (tile-list)
  "Group suite TILE-LIST into the maximum number of sets."
  (when tile-list
    (let ((first-tile (first tile-list))
          (rest-tiles (rest tile-list))
          (possibilities nil))
      (alexandria:when-let (peng (get-peng first-tile rest-tiles))
        (push (build-possibility peng) possibilities))
      (alexandria:when-let (chi (get-chi first-tile rest-tiles))
        (push (build-possibility chi) possibilities))
      (let ((child (find-max-suite-sets rest-tiles)))
        (push (list :sets (getf child :sets)
                    :unused (cons first-tile (getf child :unused)))
              possibilities))
      (choose-best possibilities #'best-suite-sets))))

;; TODO: Include revealed too.
(defun find-max-sets (hand)
  "Group tiles into the maximum number of sets."
  (loop for (f . tile-list) in (held hand)
        for suite-results = (if (honoraryp f)
                                (find-max-honorary-sets tile-list)
                                (find-max-suite-sets tile-list))
        nconc (getf suite-results :sets) into sets
        nconc (getf suite-results :unused) into unused
        finally (return (values sets unused))))

(defclass game ()
  ((walls
    :initarg :walls
    :accessor walls
    :documentation "Unused tiles.")
   (hands
    :initarg :hands
    :accessor hands
    :documentation "Tiles in player hands.")
   (turn
    :initform 0
    :accessor turn
    :documentation "Which players turn it is. 0 indexed.")
   (turn-state
    :initform :to-discard
    :accessor turn-state
    :documentation "What the player needs to do."))
  (:documentation "Game state"))

(defun make-game (walls hands)
  (make-instance 'game :walls walls :hands hands))

(defmethod print-object ((obj game) stream)
  (print-unreadable-object (obj stream)
    (with-accessors ((hands hands)
                     (turn turn)
                     (turn-state turn-state))
        obj
      (format stream "Hands: ~a~%Turn: ~a~%Turn state: ~a" hands turn turn-state))))

(defun generate-tiles ()
  "Generate all tiles used in a mahjong game."
  (let* ((suites
           (loop for s in *suites*
                 nconc (loop for n from 1 to 10
                             collect (make-tile :face s :value n))))
         (winds (loop for d in *winds*
                      collect (make-winds-tile d)))
         (dragons (loop for c in *dragons*
                        collect (make-dragons-tile c)))
         (all (append suites winds dragons)))
    (loop repeat 4 append all)))

(defun start-game ()
  "Generate a mahjong game instance that represents the start of the game."
  (let* ((tiles (alexandria:shuffle (generate-tiles)))
         (p1 (subseq tiles 0 14))
         (p2 (subseq tiles 14 27))
         (p3 (subseq tiles 27 40))
         (p4 (subseq tiles 40 53))
         (walls (nthcdr 53 tiles)))
    (make-game walls (map 'vector #'make-hand (vector p1 p2 p3 p4)))))
