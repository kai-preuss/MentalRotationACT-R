;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    "Mental Spatial Transformation: Mental Rotation Task", Experiment, GUI & Spatial Model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    May 2020 by Kai Preuß
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	2023 Rework: New human comparison data, differentiated goal module activity, simulated fixation cross between trials, focus on research data by Hilton et al., 2022
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(written-for-act-r-version "7.14")

(format t "~&***** ***** ***** ***** ~%This is a model for a variation ~%of the Mental Rotation task, originally ~%by Shepard & Metzler (1971).~%You can run a random trial with (testr) ~%or 6 * 128 trials with (mr-experiment).~%
For further available commands, please see the program code.~%  -K.P. ~%***** ***** ***** ***** ~%")

; Human MR data: Block 1, 000 Degrees; Block 1, 050 Degrees; ... ; Block 6, 100 Degrees; Block 6, 150 Degrees
; Original data from Raddatz (2014)

; 2022 Edition: with Lindas cleaned data set - best parameters: lf 0.2, rt -1.0, ans 0.5, egs 1, s-delay 0.03
; by reference onset
; (defconstant *human-data*
	; '(2.3527 2.9658 3.4153 3.7833
	; 2.1674 2.5425 3.076 3.2642
	; 2.0174 2.4013 2.8001 3.1515
	; 2.0247 2.2672 2.7146 2.9467
	; 1.9022 2.2414 2.6055 2.8859
	; 1.8927 2.1831 2.4391 2.8566))
	
; (defconstant *human-data*
	; '(1.3527 1.9658 2.4153 2.7833
	; 1.1674 1.5425 2.076 2.2642
	; 1.0174 1.4013 1.8001 2.1515
	; 1.0247 1.2672 1.7146 1.9467
	; 0.9022 1.2414 1.6055 1.8859
	; 0.8927 1.1831 1.4391 1.8566))

; Chris' data set, with added 1s to change RT from after target onset to after reference onset
;(defparameter *human-data* 
;	'(2.4563 3.2508 3.9278 4.3881
;	2.2181 2.7204 3.332 3.6371
;	2.0923 2.5257 2.9787 3.5045
;	2.0778 2.425 2.8737 3.3061
;	1.961 2.2904 2.7521 3.159
;	1.9638 2.2751 2.5992 3.0652))

; Chris' newest data set, used for Hilton et al., 2022
(defparameter *human-data*
	'(2.337502 3.205542 3.817194 4.261577
	2.141281 2.571441 3.165364 3.516333
	1.996645 2.403729 3.023052 3.321777
	2.031293 2.274785 2.764578 3.213300
	1.859507 2.210421 2.690318 3.114642
	1.915229 2.291307 2.399042 2.960383))

; Chris' data set (old values)  - best parameters: lf 0.3, rt -1.0, ans 0.5, egs 2, s-delay 0.005
; by target onset
;(defconstant *human-data* 
;	'(1.4563 2.2508 2.9278 3.3881
;	1.2181 1.7204 2.332 2.6371
;	1.0923 1.5257 1.9787 2.5045
;	1.0778 1.425 1.8737 2.3061
;	0.961 1.2904 1.7521 2.159
;	0.9638 1.2751 1.5992 2.0652))
; even older, un-de-outlierd human data: '(1.2983 1.8098 2.1789 2.304 1.1462 1.4924 1.872 2.0028 0.9736 1.3631 1.6978 1.9472 1.0219 1.2904 1.594 1.8141 0.9134 1.2141 1.537 1.7836 0.9314 1.1634 1.3983 1.6901)
;(defconstant *subj103-data* '(1.2307 2.0683 2.1326 2.4817 1.0293 1.5103 2.1481 2.2844 0.8511 1.7097 1.9851 2.0159 0.8836 1.2843 1.5672 1.7181 0.8417 1.6066 1.5694 1.7702 0.8237 1.3433 1.360 1.8043))

; --------------------------------
;; Variables
; User Variables
;
; Where are the program files?
(defvar *use-custom-directory* nil) ; if user-defined directory should be used. if not, uses ACT-R/rotation/ for component-load
(setq *custom-directory* "~/rotation/") ; user-defined directory

(defparameter *sim-directory* (concatenate 'string *custom-directory* (namestring (pathname "Simulation Output May2023_250Hz/"))))
#(defparameter *source-data* (concatenate 'string *custom-directory* (namestring (pathname "MR-simulation-input.csv"))))
(defparameter *source-data* (concatenate 'string *custom-directory* (namestring (pathname "MR-simulation-input_additionalData.csv"))))

; Run for show or for data collection?
(setq *do-in-real-time* t) ; if model should solve puzzle in real time (good for showing off & debugging)
(defparameter *visible-experiment-window* t) ; sets the virtual window to visible (t) or virtual (nil)
(defparameter *visible-mental-window* t)
(defparameter *enable-trace* t) ; if the CLI trace should be enabled
(defparameter *use-seed-value* nil) ; if the model should always use the same randomization, leading to same outcomes

(defparameter *actr-enabled-p* t) ; if human or model should solve puzzle
(defparameter *include-workload* nil) ; if workload.lisp in the same directory should be loaded for use in workload computation

(defparameter *similarity-threshold* 20)
(defparameter *degrees-of-rotation* 45)

(defparameter *enable-sc* t)
(defparameter *enable-utility-learning* t)
(defparameter *utility-noise* 1)
(defparameter *reward* 15)
(defparameter *initial-utility* 12) ; 15 reward / 10 IU seems like a good combination
(defparameter *enable-production-compilation* nil)
(defparameter *pc-trace* nil)
(defparameter *enable-bl-learning* 0.5) ;0.5
(defparameter *activation-noise* 0.5) ; default: nil, recommended between 0.2 and 0.8 - but takes forever in long-term simulations if not NIL >:( or is it? i don't even know anymore
(defparameter *retrieval-threshold* -1.6) ;default: 0.0		;;; -0.5 gets almost a 50/50 strategy ratio (344 424), -1.0 gets (~160 ~710), -2.0 gets (26 742), 0.0 gets (~720 ~50)
(defparameter *latency-factor* 0.1) ; default: 1

(defparameter *spatial-delay* 0.003) ; default 1, höhöhö
(defparameter *spatial-complexity* 30) ; default 6 - but set high for now, as comparisons are counted as transformations. COUGH COUGH
(defparameter *spatial-complexity-factor* 0.25)	; NEW - sets influence of complexity on spatial delay, default: 0.02
(defparameter *declarative-finst-span* 10); default 3
(defparameter *visual-num-finsts* 10); default 4
(defparameter *visual-finst-span* 10); default 3
(defparameter *mental-display-delay* 0);
(defparameter *debug* t)

; Run randomly and/or only short durations?
(defparameter *runtime* 30) ; maximum allowed runtime for model in seconds (3600 = 60 minutes)

; window variables
(defparameter *experiment-window* nil) ; experiment window
(defparameter *mental-window* nil)
(defparameter *experiment-display* nil)
(defparameter *mental-display* nil)
;(defvar *camera-pos* '(0 0 -100)) ; was commented out? ; and now is again? it is truly a mystery

(defvar *window-width* 700)
(defvar *window-height* 525)
(defparameter *focus-distance* 100)
(defparameter *processing-cam-dir* '(0 0 0))
(defparameter *processing-cam-pos* '(0 0 0))
(defvar *left-stimulus-position* (list (/ *window-width* 4) (/ *window-height* 2))) ; center of left half of screen
(defvar *right-stimulus-position* (list (* 3 (/ *window-width* 4)) (/ *window-height* 2))) ; center of right half of screen
(defvar *ref-position* nil) ; if cube is left or right
(defvar *tar-position* nil) ; if folding pattern is left or right
(defvar *tar-rotation* nil)
(defvar *ref-stimulus* nil)
(defvar *tar-stimulus* nil)
(defparameter *initial-rotation* '(-15 15 0))
(defparameter *scale* 50)

(defvar *debug-match-counter* 0)
(defvar *debug-mismatch-counter* 0)

;;; internal variables
; (current-id (concatenate 'string (first stimulus) (format nil "-~d" (get-universal-time))))
(defparameter *puzzle-identifier* nil) ; Variable for holding puzzle ID
(defparameter *stage-markers* nil) ; Variable for collecting markers of cognitive stages
(defparameter *stage-markers-output* nil)
(defparameter *strategy-counter* '(0 0)) ; counts piecemeal / wholesale use until reset
(defparameter *response* 0) ; input key
(defparameter *correct-response* nil) ; correct key
(defparameter *current-stimulus* nil)
(defparameter *left-stimulus-drawn* nil) ; If left window side is already taken
(defparameter *prompted* nil) ; allows user input
(defparameter *done* nil) ; experiment is done
(defparameter *visual-feature-list* nil)

; --------------------------------
;; Loads model components either from ACT-R directory or from user-defined directory, depending on *use-custom-directory*
(defun component-load (filename)
  (if *use-custom-directory*
    (load (pathname (concatenate 'string *custom-directory* filename)))
    (load (pathname (concatenate 'string (namestring (pathname "../rotation/")) filename)))))

;; Components to load
(component-load "MR-Stimuli.lisp")
(if *include-workload*
	(component-load "workload_spatial.lisp"))

;;
; --------------------------------
; Draws a single cube at the desired position, with the desired rotation and the desired scale
; Test: (draw-cube '(50 0 100) '(0 0 0) 20)
(defun draw-cube (position rotation scale &optional (window *experiment-window*) (color black) (delete-old? nil))
	(if delete-old? (dolist (item delete-old?) (suppress-warnings (remove-items-from-exp-window window item))))
	(let* ((hscale (* scale 0.5)) (-hscale (* scale -0.5)) (ufl `(,-hscale ,hscale ,-hscale)) (ufr `(,hscale ,hscale ,-hscale))
			(ubl `(,-hscale ,hscale ,hscale)) (ubr `(,hscale ,hscale ,hscale)) (lfl `(,-hscale ,-hscale ,-hscale))
			(lfr `(,hscale ,-hscale ,-hscale)) (lbl  `(,-hscale ,-hscale ,hscale)) (lbr `(,hscale ,-hscale ,hscale))
			(corners (list ufl ufr ubl ubr lfl lfr lbl lbr)) (rotated-corners (mapcar #'(lambda (pts) (rotate-around-xyz pts rotation)) corners))
			(translated-corners (mapcar #'(lambda (pts) (translate-by-xyz pts position)) rotated-corners))
			(xy (mapcar #'convert-centered-coordinates-to-screen translated-corners))
			(cubelines nil))

		; Order of corners: ufl ufr ubl ubr lfl lfr lbl lbr
		; Upper Horizontals
		; FL to BL
		(push (add-line-to-exp-window window (first xy) (third xy) color) cubelines)
		; BL to BR
		(push (add-line-to-exp-window window (third xy) (fourth xy) color) cubelines)
		; BR to FR
		(push (add-line-to-exp-window window (fourth xy) (second xy) color) cubelines)
		; FR to FL
		(push (add-line-to-exp-window window (second xy) (first xy) color) cubelines)
		
		; Lower Horizontals
		; FL to BL
		(push (add-line-to-exp-window window (fifth xy) (seventh xy) color) cubelines)
		; BL to BR
		(push (add-line-to-exp-window window (seventh xy) (eighth xy) color) cubelines)
		; BR to FR
		(push (add-line-to-exp-window window (eighth xy) (sixth xy) color) cubelines)
		; FR to FL
		(push (add-line-to-exp-window window (sixth xy) (fifth xy) color) cubelines)
		
		; Verticals
		; UFL to LFL
		(push (add-line-to-exp-window window (first xy) (fifth xy) color) cubelines)
		; UBL to LBL
		(push (add-line-to-exp-window window (third xy) (seventh xy) color) cubelines)
		; UBR to LBR
		(push (add-line-to-exp-window window (fourth xy) (eighth xy) color) cubelines)
		; UFR to LFR
		(push (add-line-to-exp-window window (second xy) (sixth xy) color) cubelines)
		cubelines))
		
; Draws a stimulus consisting of multiple cubes at the desired position, with the desired rotation and the desired scale
; Example Stim: (setf mr-stim-1 '((0 3 3) (0 3 2) (0 3 1) (0 2 1) (0 1 1) (0 0 1) (1 0 1) (2 0 1) (3 0 1) (3 0 0)))
; Test: (draw-stimulus mr-stim-1 '(-30 0 80) '(5 10 50) 30)
(defun draw-stimulus (stim position rotation scale &optional (window *experiment-window*) (color black) (delete-old? nil))
	(if delete-old? (dolist (item delete-old?) (suppress-warnings (remove-items-from-exp-window window item))))
	(let* ((scaled-positions (mapcar #'(lambda (point) (mapcar #'(lambda (dim) (* dim scale)) point)) stim))
			(normalization (get-center scaled-positions)) (norm-positions (mapcar #'(lambda (x) (-list x normalization)) scaled-positions))
			(rot-positions (mapcar #'(lambda (pts) (rotate-around-xyz pts rotation)) norm-positions))
			(trans-positions (mapcar #'(lambda (pts) (translate-by-xyz pts position)) rot-positions))
			(stimlines nil))
		(dolist (cube trans-positions)
			(setf stimlines (append stimlines (draw-cube cube rotation scale window color))))
		stimlines))
		
; Chunk version (main part is done by create-stimulus):
(defun draw-stimulus-chunk (stimchunk rotation scale &optional (window *experiment-window*) (delete-old? nil))
	(if delete-old? (dolist (item delete-old?) (suppress-warnings (remove-items-from-exp-window window item))))
	(if (or (eq stimchunk 'clear) (eq stimchunk nil)) nil
		(let*
			((stimlines nil) (whole (car (last stimchunk))) (points (car (cdr (car (cdr (member 'points whole)))))) (color (cadr (member 'color whole))))
			;debug
			;(format t "Stimchunk: ~a~%" stimchunk)
			;(format t "Translated & Rotated Cube Positions: ~a~%" points)
			(dolist (cube points)
				(setf stimlines (append stimlines (draw-cube cube rotation scale window color))))
			stimlines)))

; Update version
(defun draw-stimulus-update (stimchunk rotation scale &optional (window *experiment-window*) (delete-old? nil))
	(if delete-old? (dolist (item delete-old?) (suppress-warnings (remove-items-from-exp-window window item))))
	(if (or (eq stimchunk 'clear) (eq stimchunk nil) (eq (chunk-slot-value-fct stimchunk 'points) nil)) nil
		(let*
			((stimlines nil) (points (chunk-slot-value-fct stimchunk 'points)) (color (chunk-slot-value-fct stimchunk 'color)))

			(dolist (cube points)
				(setf stimlines (append stimlines (draw-cube cube rotation scale window color))))
			stimlines)))

; (defun update-sim (stimulus)
	; (setf *mental-display* (update-points stimulus *mental-display* *mental-window*)))
	
(defun update-sim-hook (production)
	(setf *mental-display* (draw-stimulus-update (first (suppress-act-r-output (buffer-chunk spatial))) '(0 0 0) *scale* *mental-window* *mental-display*)))

;;
; --------------------------------
		
; ; Version with proper y-Axis
(defun convert-centered-coordinates-to-screen (coords)
	(list (+ (/ *window-width* 2) (first coords)) (- (/ *window-height* 2) (second coords))))
	
; ; Version with inverting y-Axis
; (defun convert-centered-coordinates-to-screen (coords)
	; (let ((center (list (/ *window-width* 2) (/ *window-height* 2))) (newcoords (list (first coords) (second coords))))
		; (+list center newcoords)))
		
; ; Version with proper y-Axis
(defun convert-screen-to-centered-coordinates (coords)
	(list (- (first coords) (/ *window-width* 2)) (+ (second coords) (/ *window-height* 2))))
	
; ; Version with inverting y-Axis
; (defun convert-screen-to-centered-coordinates (coords)
	; (let ((center (list (/ *window-width* 2) (/ *window-height* 2))) (newcoords (list (first coords) (second coords))))
		; (-list newcoords center)))
		
(defun convert-vector-to-angle (vector)
	(/ (* (atan (first vector) (second vector)) 180) pi))
	
(defun convert-angle-to-vector (angle)
	(list (cos (/ (* angle pi) 180)) (sin (/ (* angle pi) 180))))
	
(defun calculate-projection (vector3 &optional (simple nil) (camera-dir nil) (camera-pos nil) (focal 1024)) ; (focal 1024)
	(if (not (boundp '*camera-pos*)) (defparameter *camera-pos* '(0 0 0)))
	(if (not (boundp '*camera-dir*)) (defparameter *camera-dir* '(0 0 0)))
	(if (not (boundp '*window-width*)) (defparameter *window-width* 200))
	(if (not (boundp '*window-height*)) (defparameter *window-height* 200))
	(if simple (return-from calculate-projection (butlast vector3)))
	(if (not camera-dir) (setf camera-dir *camera-dir*))
	(if (not camera-pos) (setf camera-pos *camera-pos*))
	(let* ((x3d (- (first vector3) (first camera-pos)))
			(y3d (- (second vector3) (second camera-pos))) (z3d (- (third vector3) (third camera-pos)))
			(cam-angles (mapcar #'(lambda (x) (setf x (/ (* x pi) 180))) camera-dir))
			(cam-angle-x (first cam-angles)) (cam-angle-y (second cam-angles)) (cam-angle-z (third cam-angles))
			(camera-x3d (- (* (cos cam-angle-y) (+ (* (sin cam-angle-z) y3d) (* (cos cam-angle-z) x3d))) (* (sin cam-angle-y) z3d)))
			(camera-y3d (+ (* (sin cam-angle-x) (+ (* (cos cam-angle-y) z3d) (* (sin cam-angle-y) (+ (* (sin cam-angle-z) y3d) (* (cos cam-angle-z) x3d)))))
							(* (cos cam-angle-x) (- (* (cos cam-angle-z) y3d) (* (sin cam-angle-z) x3d)))))
			(camera-z3d (- (* (cos cam-angle-x) (+ (* (cos cam-angle-y) z3d) (* (sin cam-angle-y) (+ (* (sin cam-angle-z) y3d) (* (cos cam-angle-z) x3d)))))
							(* (sin cam-angle-x) (- (* (cos cam-angle-z) y3d) (* (sin cam-angle-z) x3d)))))
			(plane (list 0 0 focal)))
			(if (not (= camera-z3d 0))
				(let ((screen-x (+ (* (/ (third plane) camera-z3d) camera-x3d) (first plane)))
						(screen-y (+ (* (/ (third plane) camera-z3d) camera-y3d) (second plane))))
					(list (round screen-x) (round screen-y)))
				(let ((screen-x 0) (screen-y 0))
					(list screen-x screen-y)))))

; (defun foldarray-to-list (array)
  ; (let ((foldline nil) (foldlist nil))
    ; (dotimes (j 4)
      ; (dotimes (i 4)
        ; (setf foldline (append foldline (list (aref array j i)))))
      ; (setf foldlist (append foldlist (list foldline)))
      ; (setf foldline nil))
    ; foldlist))

;;
; --------------------------------
#|
; Groups together several point clouds and their respective limiting positions in the list.
; Variant A: group together multiple objects and return the group and their point cloud sizes
; (defun group-objects (&rest objects)
  ; (let ((result nil) (limiters nil))
    ; (dolist (object objects)
      ; (setf result (append result object))
      ; (setf limiters (append limiters (list (length object)))))
    ; (list result limiters)))
	
; Variant B: add an object to another object (includes groups), returns group and length of last object
(defun group-objects (&rest objects)
  (let ((result nil))
    (dolist (object objects)
      (setf result (append result object)))
    (list result (length (first (cdr objects))))))

; Degroups objects formerly grouped together into one object for folding purposes.
; Takes the meta object and delimiters (i.e., length of original point clouds) as input.
(defun degroup-objects (object delimiters)
	(let ((result nil) (old-delimiter 0))
		(dolist (delimiter delimiters)
			(setf result (nconc result (list (subseq object old-delimiter (+ old-delimiter delimiter)))))
			(setf old-delimiter (+ old-delimiter delimiter)))
		result))

; Checks if two points clouds are connected in at least 2 points
(defun is-connected (obj1 obj2)
	(let ((shared-points 0))
		(dolist (obj1-pts obj1)
			(dolist (obj2-pts obj2)
				(if (equal obj1-pts obj2-pts) (setf shared-points (1+ shared-points)))))
		(if (>= shared-points 2) t nil)))
		
; Returns the points that are equal in two lists of lists, e.g. returns coordinates where two point clouds are connected
(defun is-connected-where (obj1 obj2)
	(let ((shared-points nil))
		(dolist (obj1-pts obj1)
			(dolist (obj2-pts obj2)
				(if (equal obj1-pts obj2-pts) (setf shared-points (append shared-points (list obj1-pts))))))
		shared-points))
		
; Returns the objects shared by two structures
(defun shares-objects (obj1 delims1 obj2 delims2)
	(let ((obj1-list nil) (obj2-list nil) (counter 0))
		(dolist (delim delims1)
			(setf obj1-list (append obj1-list (list (subseq obj1 counter (+ counter delim)))))
			(setf counter (+ counter delim)))
		(setf counter 0)
		(dolist (delim delims2)
			(setf obj2-list (append obj2-list (list (subseq obj2 counter (+ counter delim)))))
			(setf counter (+ counter delim)))
		(flatten-group (is-connected-where obj1-list obj2-list))))

; Returns points belonging to the delimiters at the indexed positions
(defun return-from-group (indices delims points)
	(let ((delcounter 0) (new-points nil))
		(dotimes (n (length delims))
			(dolist (i indices)
				(if (= n i)
					(setf new-points (append new-points (subseq points delcounter (+ delcounter (nth n delims)))))))
		(setf delcounter (+ delcounter (nth n delims))))
    new-points))

; Reverses a spatial structure while keeping its objects intact
(defun spatial-reverse (lst delims)
	(let ((new-list nil) (current-nth 0))
		(dolist (delim delims)
		;(print (subseq lst current-nth (+ current-nth delim)))
			(setf new-list (append (subseq lst current-nth (+ current-nth delim)) new-list))
			(setf current-nth (+ current-nth delim)))
		new-list))
|#
;;
; --------------------------------

; Creates csv-style strings out of list elements
(defun list-to-csv (list)
	(let ((csv-string ""))
		(dotimes (n (length list))
			(if (not (eq n (- (length list) 1)))
				(setf csv-string (concatenate 'string csv-string (write-to-string (nth n list)) ","))
				(setf csv-string (concatenate 'string csv-string (write-to-string (nth n list))))))
		csv-string))

(defun csv-to-list (line)
	(mapcar (lambda (str) (if (not (ignore-errors (parse-integer (string-trim "\"" str)))) (string-trim "\"" str) (parse-integer (string-trim "\"" str)))) (split-str line ",")))

; Splits strings by the chosen separator (default: space)
(defun split-str (string &optional (separator " "))
  (split-1 string separator))

; Helper function for correct recursion of split-str
(defun split-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r)) (cons string r))))
	
(defun split-by (string &optional (delimiter #\;))
	(loop for i = 0 then (1+ j)
		as j = (position delimiter string :start i)
		collect (subseq string i j)
		while j))

(defun csv ()
	(read-from-csv "~/rotation/simulation_data_NP.csv"))
	
; Reads from csv files and creates a list of list, with each list representing one line (works with IDEs, but not with Standalone for...some reason?)
(defun read-from-csv (filepath)
	(with-open-file (stream filepath :direction :input)
		(let ((newlist nil) (entry nil))
			(loop for line = (read-line stream nil)
				while line do
					;(setf entry (mapcar (lambda (str) (if (not (ignore-errors (parse-integer (string-trim "\"" str)))) (string-trim "\"" str) (parse-integer (string-trim "\"" str)))) (split-str line ",")))
					(setf entry (mapcar (lambda (str) (string-trim "\"" str)) (split-str line ",")))
					(setf newlist (append newlist (list entry))))
			newlist)))

; THIRD VARIANT: Reads from csv files and creates a list of list, with each list representing one line
#||
(defun read-from-csv (filepath)
	(with-open-file (stream filepath :direction :input)
		(let ((newlist nil) (entry nil))
			(loop for line = (read-line stream nil)
				while line do
					;(print (string-trim "\"" line))
					(print line)
					(setf newlist (append newlist (list line))))
			newlist)))
|#

; Creates csv-files out of a list of lists (with each list resulting in a line). Input is considered to be long format ("list of rows").
(defun write-to-csv (results filepath)
	(with-open-file (str (pathname filepath)
		:direction :output
		:if-exists :overwrite
		:if-does-not-exist :create)
		(dolist (line results)
			(format str (concatenate 'string (list-to-csv line) "~%")))) T)

; As above, but input is in wide format ("list of columns").
(defun write-to-csv2 (results filepath)
	(with-open-file (str (pathname filepath)
		:direction :output
		:if-exists :overwrite
		:if-does-not-exist :create)
		(let* ((columns (length results)) (rows (length (first results))) (line nil))
			(dotimes (row rows)
				(dotimes (column columns)
					(setf line (append line (list (nth row (nth column results))))))
				(format str (concatenate 'string (list-to-csv line) "~%"))
				(setf line nil)))) T)

#|
; Like 'position', returns the positions of its search elements, but all positions.	
(defun all-positions (search lst)
	(let ((collector nil))
		(dotimes (n (length lst))
			(dolist (elem search)
				(if (eql (nth n lst) elem)
					(setf collector (append collector (list n))))))
		collector))
	
(defun all-nths (indices lst)
	(let ((collector nil))
		(dolist (i indices)
			(setf collector (append collector (list (nth i lst)))))
		collector))
|#

; Takes a group of objects and flattens their structure, so that all coordinates appear in one single list.	
(defun flatten-group (group)
	(let ((superlist nil))
		(dolist (object group)
			(setf superlist (append superlist object)))
		superlist))

; Returns the presumed to be matching piece for a piece from the reference stimulus from the target stimulus
(defun get-matched-piece (piece ref tar)
	(let* ((refwhole (second (nth 15 (first (last ref))))) (tarwhole (second (nth 15 (first (last tar)))))
			(piecepos (search piece refwhole :test #'equal)))
		;(format t "~&*DEBUG*~%piece: ~a,~%refwhole: ~a,~%tarwhole: ~a,~%piecepos: ~a~%" piece refwhole tarwhole piecepos)
		(if (eq piecepos nil) nil (subseq tarwhole piecepos (+ piecepos (length piece)))))) ; if piecepos is nil, new run wasn't properly initiated

(defun quick-compare-same (piece1 piece2)
	(let* ((center1 (get-center piece1)) (norm1 (mapcar #'(lambda (x) (-list x center1)) piece1))
			(center2 (get-center piece2)) (norm2 (mapcar #'(lambda (x) (-list x center2)) piece2)))
		(equal norm1 norm2)))
		
; quicker, safer variant, but doesn't use model-available knowledge, just world knowledge
; (defun quick-compare-same (match tar-rotation)
	; (let* ((rotation (nth 2 tar-rotation)) (same (and match (eq 0 rotation))))
		; same))

;;; broken (?)
; (defun quick-compare-mirrored (piece1 piece2)
	; (let* ((unrotation (mapcar #'(lambda (degree) (* -1 degree)) *initial-rotation*)) (center1 (get-center piece1)) (norm1 (mapcar #'(lambda (x) (-list x center1)) piece1))
			; (rot1 (mapcar #'(lambda (pts) (rotate-around-xyz pts unrotation)) norm1))
			; (center2 (get-center piece2)) (norm2 (mapcar #'(lambda (x) (-list x center2)) piece2))
			; (rot2 (mapcar #'(lambda (pts) (rotate-around-xyz pts unrotation)) norm2))		
			; (mirror2 (create-mirror-object rot2)))
		; (equal rot1 mirror2)))
		
(defun quick-compare-mirrored (match tar-rotation)
	(let* ((rotation (nth 2 tar-rotation)) (mirrored (and (not match) (eq 0 rotation))))
		mirrored))

(defun add-to-stage-markers (stagestring)
	(cond
		((string= stagestring "strategy-piecemeal") (incf (first *strategy-counter*)))
		((string= stagestring "strategy-wholesale") (incf (second *strategy-counter*)))
		(t nil))
	(push (list (mp-time) stagestring) *stage-markers*))

(defun finish-stage-markers (trialtime)
	(let* ((markers (reverse *stage-markers*)) (new-markers (mapcar #'(lambda (lst) (setf (first lst) (- (first lst) trialtime)) lst) markers)))
		new-markers))
		

;;
; --------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CREATE STIMULUS FUNCTIONS
(defun create-stimulus (stimulus 2Dposition rotation scale kind)
	(let* ((3Dposition (append 2Dposition (list *focus-distance*))) (scaled-positions (mapcar #'(lambda (point) (mapcar #'(lambda (dim) (* dim scale)) point)) (first stimulus)))
			(normalization (get-center scaled-positions)) (norm-positions (mapcar #'(lambda (x) (-list x normalization)) scaled-positions))
			(rot-positions (mapcar #'(lambda (pts) (rotate-around-xyz pts rotation)) norm-positions))
			(trans-positions (mapcar #'(lambda (pts) (translate-by-xyz pts 3Dposition)) rot-positions)) (trans-center (get-center trans-positions))
			;(screen-center (convert-centered-coordinates-to-screen (calculate-projection trans-center)))
			(screen-center (convert-centered-coordinates-to-screen trans-center))
			(temp-arm nil) (temp-center nil) (temp-screen-center nil) (delim-counter 0) (visual-chunks nil))

		; Single "arms"
		(dolist (delim (second stimulus))
			(setf temp-arm (subseq trans-positions delim-counter (+ delim-counter delim)))
			(setf arm-center (get-center temp-arm))
			;(setf screen-arm (convert-centered-coordinates-to-screen (calculate-projection arm-center)))
			(setf screen-arm (convert-centered-coordinates-to-screen arm-center))
			(setf delim-counter (+ delim-counter delim))
			
			;(setf visual-chunks (append visual-chunks `((isa (spatial-feature spatial-visual) screen-x ,(first screen-arm) screen-y ,(second screen-arm) kind ,(list kind nil) class ,(list nil kind) type piece color black points ,(list nil temp-arm) width ,(list nil (* scale 3)) height ,(list nil (* scale 3)))))))
				(setf visual-chunks (append visual-chunks `((isa (spatial-feature spatial-visual) screen-x ,(first screen-arm) screen-y ,(second screen-arm) kind ,(list kind nil) class ,(list nil kind) type piece color black points ,(list temp-arm temp-arm) width ,(list nil (* scale 3)) height ,(list nil (* scale 3)))))))		

		; Whole Object
		;(setf visual-chunks (append visual-chunks `((isa (spatial-feature spatial-visual) screen-x ,(first screen-center) screen-y ,(second screen-center) kind ,(list kind nil) class ,(list nil kind) type whole color black points ,(list nil trans-positions) width ,(list nil (* scale 5)) height ,(list nil (* scale 5))))))
		(setf visual-chunks (append visual-chunks `((isa (spatial-feature spatial-visual) screen-x ,(first screen-center) screen-y ,(second screen-center) kind ,(list kind nil) class ,(list nil kind) type whole color black points ,(list trans-positions trans-positions) width ,(list nil (* scale 5)) height ,(list nil (* scale 5))))))

		visual-chunks))

;;
; --------------------------------

; Stimuli format:
;
; ( "mr-stim-14" 14 50 T "14_y_0_a" "14_y_50_a")
;
(defun build-trial (stimname type rotation match)
	(setf *response* 0)
	(setf *left-stimulus-position* ;(list (/ *window-width* 4) (/ *window-height* 2)))
	(list -180 0))
	(setf *right-stimulus-position* ;(list (* 3 (/ *window-width* 4)) (/ *window-height* 2)))
	(list 180 0))
	(setf *prompted* nil)
	(setf *done* nil)
	
	(setf *processing-cam-pos* '(0 0 0))
	(setf *processing-cam-dir* '(0 0 0))
	(setf *ref-position* nil) ; if cube is left or right
	(setf *tar-position* nil) ; if folding pattern is left or right
	
	(setf *tar-rotation* nil)

	(setf *left-stimulus-drawn* nil) ; If left window side is already taken
	(setf *mental-display* nil)
	
	;(delete-all-visicon-features)
	
	; Fenster-Maße werden festgelegt
	(if *experiment-window*
		(clear-exp-window *experiment-window*)
		(setf *experiment-window* (open-exp-window "Mental Rotation"
							 :visible *visible-experiment-window* ; change this parameter for "hidden" windows for batch learning
							 :width *window-width*
							 :height *window-height*
							 :x 0
							 :y 0)))
			
	(if *mental-window*
		(clear-exp-window *mental-window*)
		(setf *mental-window* (open-exp-window "Mental Rotation - Mental Spatial Representation"
							 :visible *visible-mental-window* ; change this parameter for "hidden" windows for batch learning
							 :width *window-width*
							 :height *window-height*
							 :x *window-width*
							 :y 0)))
	
	(delete-all-visicon-features)
	
	; Entscheidung über Item-Positionen
	(if (= (act-r-random 2) 0)
		(progn
			(setf *ref-position* *left-stimulus-position*)
			(setf *tar-position* *right-stimulus-position*))
		(progn
			(setf *ref-position* *right-stimulus-position*)
			(setf *tar-position* *left-stimulus-position*)))
			
	(setf *tar-rotation* (+list *initial-rotation* (list 0 0 rotation)))

	(let ((delim (read-from-string (concatenate 'string "delims-" (write-to-string type)))))
	; Lege eine Referenz fest
		(setf *ref-stimulus* (create-stimulus (list (eval (read-from-string stimname)) (eval delim)) *ref-position* *initial-rotation* *scale* 'reference)) ; is reference stim always unrotated?
				
		; Lege ein Target fest
		(let ((target (if match (eval (read-from-string stimname)) (create-mirror-stimulus (eval (read-from-string stimname))))))
			(setf *tar-stimulus* (create-stimulus (list target (eval delim)) *tar-position* *tar-rotation* *scale* 'target))))						; is target always original object (mirrored or not)?
 
	; Lege die korrekte Antwort fest
	(setf *correct-response* match)
)

; Draw Ref
(defun build-trial-draw-ref ()
	(mapcar #'add-visicon-features *ref-stimulus*)
	(draw-stimulus-chunk *ref-stimulus* *initial-rotation* *scale*))

; Draw Tar
(defun build-trial-draw-tar ()
	(mapcar #'add-visicon-features *tar-stimulus*)
	(draw-stimulus-chunk *tar-stimulus* *tar-rotation* *scale*))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mr-key-event-handler (model keypress)
	(case (char keypress 0)
		(#\r (setf *response* t))
		(#\w (setf *response* nil))
		(otherwise (print "Keypress registered but unknown")))
	(if (or (equal *response* nil) (equal *response* t)) (schedule-break-relative 0 :details "Trial finished"))
	(if (equal *response* *correct-response*)
		; if answer is correct 
		(progn
			(model-output "correct")
			(if *enable-utility-learning* (trigger-reward *reward*))
			)
		; if answer is wrong
		(progn
			(model-output "wrong")
			(if *enable-utility-learning* (trigger-reward 0))
			))
	(setf *done* t))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Stimuli format:
;
; ("mr-stim-14" 14 50 T "14_y_0_a" "14_y_50_a")
;
; --------------------------------
(defun do-stimuli (stimuli)
	(if (not *puzzle-identifier*)
		(setq *puzzle-identifier* (format nil "debug-~d" (get-universal-time))))
	(setf *current-stimulus* stimuli)
	(setf *stage-markers* nil)
	(let ((stimname (first stimuli)) (type (second stimuli)) (rotation (third stimuli)) (match (fourth stimuli)))
	
		(build-trial stimname type rotation match)
		(build-trial-draw-ref)
		
		; Fenster wird mit Modell verknüpft
		(if (not (member *experiment-window* (current-devices "vision"))) (install-device *experiment-window*))
		
		(add-act-r-command "mr-key-event-handler" 'mr-key-event-handler "Key monitor for Mental Rotation (Spatial Edition)")
		(monitor-act-r-command "output-key" "mr-key-event-handler")
		
		(schedule-event-relative 1 'build-trial-draw-tar)
		
		(setf *prompted* t)
		(if *actr-enabled-p*
			(do-experiment-model)
			(do-experiment-person))

		; schedule additional second to simulate fixation cross between trials, and let modules cool down

		(remove-act-r-command-monitor "output-key" "mr-key-event-handler")
		(remove-act-r-command "mr-key-event-handler")
		(schedule-clear-buffer 'retrieval 0)
		(schedule-clear-buffer 'spatial 0)
		(schedule-clear-buffer 'spatial-action 0)
		(eq *response* *correct-response*)))
			
; (defun testr ()
	; (let ((sameness (act-r-random 2)) (stimuli nil))
		; (setq stimuli (nth (act-r-random (1+ (length (nth sameness (create-stimuli-block))))) (nth sameness (create-stimuli-block))))	;;;;;;;;;;;;;;;;;; FIX
		; (setq *puzzle-identifier* (format nil "~a-~d" (first stimuli) (get-universal-time)))
		; ;(do-stimuli (second stimuli))))
		; (do-stimuli stimuli)))
		
(defun testtype (n)
	(do-stimuli (create-stimuli-set n (nth (act-r-random 4) '(0 50 100 150)) (nth (act-r-random 2) '(nil t)))))
	
(defun testr ()
	(do-stimuli (create-stimuli-set (+ 1 (act-r-random 16)) (nth (act-r-random 4) '(0 50 100 150)) (nth (act-r-random 2) '(nil t)))))
		
(defun testr-n (runs)
	(let ((results nil))
		(dotimes (i runs) (format t "~a~%" i) (setf results (append results (list (testr)))))
		results))
		
(defun test-n (test runs)
	(let ((results nil))
		(dotimes (i runs) (setf results (append results (list (funcall test)))))
		results))

; Private function! Starts experiment with model
(defun do-experiment-model ()
  (setf *done* nil *response* 0)
  (goal-focus start-goal)
  (mod-focus-fct `(currentpuzzle ,*puzzle-identifier*))
  (run *runtime* *do-in-real-time*))

; Private function! Starts experiment with human
(defun do-experiment-person ()
	(setf *done* nil *response* 0)
	;(while (not *done*)
	;	(allow-event-manager *experiment-window*))
	(sleep 1))

; Simulates fixation cross, for between-trial periods
(defun do-fixation-cross ()
	(run-full-time 1.0 *do-in-real-time*))

(defun tblock ()
	(mr-block (create-stimuli-block) 1 nil))

(defun mr-block (stimuli number &optional (output t))
	(let ((blockdata nil) (counter 0))
		(dolist (i stimuli)
			(let ((puzzledata nil) (degrees (third i)) (puzzletime (mp-time)) (workload nil))
				(setf *puzzle-identifier* (concatenate 'string (first i) "_" (format nil "~3,'0d" degrees) (format nil "_~d" (get-universal-time))))
				(if output
					(format t "~&[(~3,5$) B ~a / T ~a] ~a" (mp-time) number counter *puzzle-identifier*))
				(incf counter)
				(do-stimuli i)
				(if output
					(format t " - RT: ~as - Correct? ~a" (- (mp-time) puzzletime) (if (equal *response* *correct-response*) "yes" "no")))
				(if *include-workload*
					(progn
						(setf workload (compute-workload :start puzzletime :end (mp-time) :step 0.05))
						(setf puzzledata (list *puzzle-identifier* number counter degrees (- (mp-time) puzzletime) *response* (equal *response* *correct-response*) workload)))			
					(setf puzzledata (list *puzzle-identifier* number counter degrees (- (mp-time) puzzletime) *response* (equal *response* *correct-response*))))
				(setf blockdata (append blockdata (list puzzledata)))))
		blockdata))

(defun mr-experiment (&optional (output t))
	(setf *strategy-counter* '(0 0))
	(let ((block (create-stimuli-block)) (exp-data (list (list 'ID 'Block_number 'Trial_number 'angle 'RT 'Response 'Correct))))
		(dotimes (i 6)
			(let ((blockdata nil))
				(setf blockdata (mr-block block (1+ i) output))
				(setf exp-data (append exp-data blockdata))))
		exp-data))

;;; PARAMETER FITTING
(defun combinations (lists)
	(if (listp (car lists))
		(if (car lists)
			(mapcan (lambda (inner-val)
				(mapcar (lambda (outer-val) (cons outer-val inner-val)) (car lists)))
				(apply #'combinations (list (cdr lists))))
			(list nil))
		lists))

;(setf fitting-test '((0.5) (-1.0) (nil) (1) (0.005 0.01)))

; Fitting values for :lf, :rt, :ans, :egs and :s-delay
; for Chris' data:
; (setf fitting-values '((0.1 0.2) (-1.0 -1.5) (0.5) (1 2) (0.03 0.01 0.05)))
; testing s-complexity-factor
;(setf fitting-values '((0.1 0.3) (-1.0 -1.5) (0.5) (1) (0.03 0.01 0.05)))
;(setf fitting-values '((0.1 0.2 0.3) (-1.5 -1.0 -0.5) (0.5) (1) (0.001 0.002 0.003 0.01 0.05) (0.25 0.5 0.75)))
(setf fitting-values '((0.1 0.2) (-1.8 -1.5) (0.5) (1) (0.002 0.003) (0.25 0.5)))

;(setf single-fit '((0.1 0.1 0.1 0.1 0.1) (-1.6) (0.5) (1) (0.003) (0.25)))
(setf single-fit '((0.1 0.1 0.1 0.2 0.2 0.2) (-1.6) (0.5) (1) (0.002) (0.5)))

; for Linda's data:
; (setf fitting-values '((0.2 0.5) (0 -1.0 -2.0) (nil 0.2 0.5) (0 1 2) (0.005 0.01 0.02)))
; (setf fine-fitting-values '((0.1 0.2) (-0.8 -1.0) (0.5) (1 2) (0.02 0.03 0.05)))

; Quick commands:
;(setf *fitting-data* (mr-fitting fitting-values nil 0))
;(setf *fitting-data* (mr-fitting fine-fitting-values nil 0))

; for chris' data:
;(setf fine-values '((0.3) (-1.0 -0.5) (0.2 0.5) (1) (0.005)))
;(setf best-values '((0.3) (-1.0) (0.5) (2) (0.005)))
; Parameters to be fitted: latency factor, retrieval threshold, activation noise, egs, s-delay, s-complexity-factor
; Possible Values: lf[0.5 1], rt[0 -1.0 -2.0], ans[nil 0.2 0.5], egs[0 1 2], s-delay[0.005 0.01 0.02], s-complexity-factor[0.02 0]

; 2020 best parameters for Chris' data set (old values): lf 0.3, rt -1.0, ans 0.5, egs 2, s-delay 0.005
; 2022 best parameters for Linda: lf 0.2, rt -1.0, ans 0.5, egs 1, s-delay 0.03
; 2023 best parameters for Chris' data set: lf: 0.1 rt: -1.6 ans: 0.5 egs: 1 s-delay: 0.003 s-complexity-factor: 0.25
; 2023 best parameters for Mental Folding: ; lf: 0.2 rt: -1.8 ans: nil egs: 1 alpha: 0.2 s-delay: 0.001 s-complexity-factor: 0.5

; (setf *fitting-data* (mr-fitting fitting-test))
; (setf *fitting-data* (mr-fitting fitting-values))
; (setf *fitting-data* (mr-fitting fitting-values nil 0 *human-data*))
; (setf *fitting-data* (mr-fitting single-fit))

(defun mr-fitting (fitting-values &optional (already-combi? nil) (start-at 0) (humandata *human-data*))
	(let ((parameters (subseq (if already-combi? fitting-values (combinations fitting-values)) start-at)) (all-aggregates nil))
		(dotimes (n (length parameters))
			(let* ((params (nth n parameters)) (exp-data nil) (blockdata nil) (aggregated-exp-data nil) (block (create-stimuli-block))
					; define wanted parameters here and pick them out of params list, e.g. (rt (third params))
					(lf (first params))
					(rt (second params))
					(ans (third params))
					(egs (fourth params))
					(s-delay (fifth params))
					(s-complexity-factor (sixth params))
					)
				(format t "~&[Run ~a of ~a] Parameters used: lf - ~a; rt - ~a; ans - ~a; egs - ~a; s-delay - ~a; s-compl.-factor - ~a~%" (+ 1 n) (length parameters) lf rt ans egs s-delay s-complexity-factor) ; add parameters here for output
				(reset)
				(sgp-fct `(:lf ,lf :rt ,rt :ans ,ans :egs ,egs :s-delay ,s-delay :s-complexity-factor ,s-complexity-factor))
				
				(dotimes (i 6)
					(setf blockdata (mr-block block (+ 1 i) nil))
					(dolist (trial blockdata)
						(let* ((degreepos (+ 1 (position '_ (first trial) :test #'string-equal))) (degree (subseq (first trial) degreepos (+ degreepos 3)))
								(key (parse-integer (concatenate 'string (write-to-string (+ i 1)) degree))) (entry (assoc key exp-data)))
							(if entry
								;(rplacd entry (append (cdr entry) (list (second trial))))
								(rplacd entry (append (cdr entry) (list (fifth trial))))
								;(setf exp-data (acons key (list (second trial)) exp-data))))))
								(setf exp-data (acons key (list (fifth trial)) exp-data))))))
								
				(setf exp-data (stable-sort exp-data #'< :key #'car))
				(dolist (aggregate exp-data)
					(setf aggregated-exp-data (append aggregated-exp-data (list (/ (apply #'+ (cdr aggregate)) (length (cdr aggregate)))))))
				(setf all-aggregates (append all-aggregates (list aggregated-exp-data)))

				(correlation aggregated-exp-data humandata)
				(mean-deviation aggregated-exp-data humandata)))
		(list parameters all-aggregates)))

;;; MODULE ACTIVITY SIMULATION

; Put in activity simulation:
; start
; (record-history "buffer-trace" "goal" "production" "imaginal" "imaginal-action" "retrieval" "manual" "vocal" "aural" "aural-location" "visual" "visual-location" "temporal" "spatial" "spatial-action")
; end
; (stop-recording-history)

; Gets Module Activity and resamples it for comparison with EEG data
(defun resample-trial-activity (t-start t-end human-trialtime samplerate)
	(let* ((model-trialtime (- t-end t-start)) (model-human-ratio (/ model-trialtime human-trialtime)) (stretched-samplerate (/ samplerate model-human-ratio)) (stretched-stepsize (/ 1 stretched-samplerate)))
		;(module-demand-functions (get-history-data "buffer-trace") :start t-start :end t-end :step stretched-stepsize)))
		(module-demand-functions :start t-start :end t-end :step stretched-stepsize)))

;;; Simulates a number of participants in the mental folding task with their actual order of trials, for comparison with EEG data
;;; Requires a csv with the words "SUBJ", "trials", "block", "*****stim", "*****degrees", "*****match" and "*****correct" in the first line.
;;; Outputs a list of results for each trial: Subject, Block, Trial, Identifier, Model RT, Human RT, Model Correctness, Human Correctness
;;; Quicker reading of csv files if enabled, but incompatible with older ACT-R due to unfulfillable package dependencies:
(ql:quickload :cl-csv) ; using a csv reader package. needs quicklisp, which YOU should have already
;;; Aufruf z.B. mit:
;;; (mr-simulation "~/rotation/simulation_data.csv" 103)

(defun sim ()
	(mr-simulation "~/rotation/simulation_data_NP.csv" 103 1 t))

(defun sim2 ()
	(mr-read-and-simulate "~/rotation/simulation_data_NP.csv" 103))
	
(defun sim-all () ; also, 20Hz sampling seems more than enough for now
	(mr-simulation "~/rotation/simulation_data_NP.csv" t t t t 20))

;;; Individually simulates each listed subject
;;; Test z.B. mit:
; (sim-individually '(4) t t t 100)
;;; Aufruf z.B. mit:
; (sim-individually '(117 118 120 121 122 123 124 125 126 127 128 129 130 131 132 134 136 140 141) t t t 250)
; (sim-individually '(133 137 138 139 142))
; Extra Probanden:
; (sim-individually '(102 112 121 122 125 126 127 128 129 130 131 134 136 140 141))
; (sim-individually '(130 131 134 136 140 141))
; 
(defun sim-individually (&optional (sublist t) (block t) (trial t) (output t) (samplerate 250))
	; already done: 102 103 104 106 107 108 110 111 112 113 114 115 116 117 118 120 121 122 123 124 125 126 127 130 131 132 133 134 136 137 138 139 140 142
	; full list of available participants: 102 103 104 105 106 107 108 110 111 112 113 114 115 116 117 118 120 121 122 123 124 125 126 127 128 129 130 131 132 134 136 140 141
	; .....fuller list of tested participants included in EEG: 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142
	; available: 103 104 105 106 107 108 110 111 113 114 115 116 117 118 120 123 124 132 133 137 138 139 142
	; missing from available list: 102 112 121 122 125 126 127 128 129 130 131 134 136 140 141
	; superfluous (?): 133 137 138 139 142
	; to do: 
	; to do!: re-retrieve participant data from single participant data files

	
	(if (eq sublist t) (setf sublist '(103)))
	(dolist (sub sublist)
		(let* ((subname (write-to-string sub)) (subdata (mr-simulation *source-data* sub block trial output samplerate)))
			(write-to-csv2 subdata (concatenate 'string *sim-directory* "simdata_" subname ".csv"))
			(write-to-csv *stage-markers-output* (concatenate 'string *sim-directory* "simdata_stagemarkers_" subname ".csv")))))
	
(defun mr-simulation (filepath &optional (subject t) (block t) (trial t) (output t) (samplerate 100))
	(record-history "buffer-trace" "goal" "production" "imaginal" "imaginal-action" "retrieval" "manual" "vocal" "aural" "aural-location" "visual" "visual-location" "temporal" "spatial" "spatial-action")
	; Possibilities for CSV loading:
	; (1) (table (cl-csv:read-csv (pathname filepath))) ; for ACT-R versions with up-to-date quicklisp (looking at you, 7.5).
	; (2) (table (read-from-csv filepath)) ; for everything else. Using my custom csv-reader, which is slow, but reliable, like an old horse.
	
	(setf *stage-markers-output* '((Subject Block Trial Trialtime Out Markertime Marker)))
	
	(let* ((table (cl-csv:read-csv (pathname filepath))) (header (first table)) (currentsubject 0)
			(col-subj (position "subject" header :test #'string-equal))
			(col-block (position "block" header :test #'string-equal))
			(col-trial (position "trial" header :test #'string-equal))
			(col-stim (position "Abs_ref_stim" header :test #'string-equal))
			(col-degrees (position "angle" header :test #'string-equal)); TO DO: maybe necessary to fill out to 3 digits (000 / 050 / 100 / 150)
			(col-match (position "match" header :test #'string-equal))
			(col-correct (position "correct" header :test #'string-equal))
			(col-out (position "out" header :test #'string-equal))
			(col-rt (position "RT" header :test #'string-equal))
			;(simdata '((Timestep) (Subject) (Block) (Trial) (ID) (Degrees) (Match) (Model_RT) (Human_RT) (Model_Correctness) (Human_Correctness) (Equal_Correctness) (Out) (Goal) (Production) (Imaginal) (Imaginal-Action) (Retrieval) (Manual) (Vocal) (Aural) (Aural-Location) (Visual) (Visual-Location) (Temporal) (Spatial) (Spatial-Action))))
			(simdata '((Timestep) (Subject) (Block) (Trial) (ID) (Degrees) (Match) (Model_RT) (Human_RT) (Model_Correctness) (Human_Correctness) (Equal_Correctness) (Out) (Imaginal-Action) (Imaginal) (Visual) (Production) (Retrieval) (Visual-Location) (Vocal) (Manual) (Aural-Location) (Aural) (Temporal) (Spatial-Action) (Spatial) (Goal))))
			;;; output of header: (IMAGINAL-ACTION IMAGINAL VISUAL PRODUCTION RETRIEVAL VISUAL-LOCATION VOCAL MANUAL AURAL-LOCATION AURAL TEMPORAL SPATIAL-ACTION SPATIAL GOAL)
			;;; as long as no modules are added or taken from this function, the above list is the correct module order.
			;;; Sadly, there is not really a way to automate it, since it requires module activity to be generated and that happens after the first line to simdata is written, for loop reasons. :(
		(dolist (trialdata (cdr table)) ; each line of the CSV file except the first (header) is now looped over
			; if subject is either t (meaning all subjects) or this specific subject, simulate from CSV, otherwise skip
			(if (and (or (eq subject t) (eq (read-from-string (nth col-subj trialdata)) subject)) (or (eq block t) (eq (read-from-string (nth col-block trialdata)) block))
					(or (eq trial t) (eq (read-from-string (nth col-trial trialdata)) trial)))
				(progn
					(if (not (eq currentsubject (read-from-string (nth col-subj trialdata)))) (progn (setf currentsubject (read-from-string (nth col-subj trialdata))) (reset)))
					(let* ((puzzledata nil) (puzzletime (mp-time)) (stimuli (create-stimuli-set (read-from-string (nth col-stim trialdata)) (read-from-string (nth col-degrees trialdata)) (read-from-string (nth col-match trialdata)))))
						(setf *puzzle-identifier* (concatenate 'string (first stimuli) (format nil "-~d" (get-universal-time))))
						(if output
							(format t "~&[S ~a // B ~a // T ~3a]: ~10a, ~3a, ~3a" (nth col-subj trialdata) (nth col-block trialdata) (nth col-trial trialdata) (first stimuli) (nth col-degrees trialdata) (nth col-match trialdata)))
						(do-stimuli stimuli)
						(let* ((trialtime (- (mp-time) puzzletime)) (humantime (float (/ (read-from-string (nth col-rt trialdata)) 1000)))
								(correctness (equal *response* *correct-response*)) (step (/ 1 samplerate))
								(humancorrectness (read-from-string (nth col-correct trialdata))) (outness (read-from-string (nth col-out trialdata)))
								(raw-activity (module-demand-functions :start puzzletime :end (mp-time) :step step))
								(activity (mapcar #'cdr raw-activity)) (activity-header (mapcar #'car raw-activity)) (act-length (length (car activity)))
								;;;
								;;; above: maybe source of naming confusion of activity output - might implicitly reverse module order, try reverse?
								;;;
								(newcol-step (loop :for n :below act-length :collect (float (* (+ n 1) step)))) ;;;;;;;;;;;;;;;;;;;
								(newcol-subj (make-list act-length :initial-element (read-from-string (nth col-subj trialdata))))
								(newcol-block (make-list act-length :initial-element (read-from-string (nth col-block trialdata)))) 
								(newcol-trial (make-list act-length :initial-element (read-from-string (nth col-trial trialdata))))
								(newcol-stimuli (make-list act-length :initial-element (first stimuli)))
								(newcol-degrees (make-list act-length :initial-element (read-from-string (nth col-degrees trialdata))))
								(newcol-match (make-list act-length :initial-element (read-from-string (nth col-match trialdata))))
								(newcol-trialtime (make-list act-length :initial-element trialtime))
								(newcol-humantime (make-list act-length :initial-element humantime))
								(newcol-correctness (make-list act-length :initial-element correctness))
								(newcol-humancorrectness (make-list act-length :initial-element humancorrectness))
								(newcol-equalresult (make-list act-length :initial-element (eq correctness humancorrectness)))
								(newcol-outness (make-list act-length :initial-element outness))
								;(extraheader '(Subject Block Trial ID Model_RT Human_RT Model_Correctness Human_Correctness Equal_Correctness Out))
								;(newheader (append act-header extraheader))
								(newcolumns (list newcol-step newcol-subj newcol-block newcol-trial newcol-stimuli newcol-degrees newcol-match newcol-trialtime newcol-humantime
										newcol-correctness newcol-humancorrectness newcol-equalresult newcol-outness))
								(trialmarkers (finish-stage-markers puzzletime))
								(extended-trialmarkers (mapcar #'(lambda (marker) (append (list currentsubject (read-from-string (nth col-block trialdata)) (read-from-string (nth col-trial trialdata)) trialtime outness) marker)) trialmarkers)))
							(setf *stage-markers-output* (append *stage-markers-output* extended-trialmarkers))
							(if output (format t " - RT: ~as - Correct? ~a" trialtime (if correctness "yes" "no")))
							;;;
							;;;
							;;;
							(setf puzzledata (append newcolumns activity)))
							;;;
							;;;
							;;;
							(stop-recording-history) ; not sure if necessary or even  D A N G E R O U S  to add this, but I don't want the "fixation cross" activity to mess things up
							;;;
							(do-fixation-cross)
							;;;
							(record-history "buffer-trace" "goal" "production" "imaginal" "imaginal-action" "retrieval" "manual" "vocal" "aural" "aural-location" "visual" "visual-location" "temporal" "spatial" "spatial-action")
							;;;
							;(setf puzzledata (list (read-from-string (nth col-subj trialdata)) (read-from-string (nth col-block trialdata)) (read-from-string (nth col-trial trialdata)) *puzzle-identifier* trialtime humantime correctness humancorrectness outness)))
						(setf simdata (mapcar #'append simdata puzzledata))))))
		simdata))

; (mr-simulation-for "~/rotation/simulation_data_NP.csv" '(103 104) 10)
(defun mr-simulation-for (filepath sublist samplerate)
	(loop for sub in sublist collect (progn (reset) (mr-simulation filepath sub t 66 t samplerate))))

; Aufruf z.B. mit:
; (mr-read-and-simulate "~/rotation/simulation_data.csv" 103)
;;; WARNING: old version, does not include code for cognitive stage marker output!
(defun mr-read-and-simulate (filepath &optional (subject t) (output t))
	(let ((first-line t) (col-subj nil) (col-block nil) (col-trial nil) (col-stim nil) (col-degrees nil) (col-match nil)
			(col-correct nil) (col-out nil) (col-rt nil) (simdata nil))
		(with-open-file (stream filepath)
			(loop for current-line = (read-line stream nil) while current-line do
				(setf current-line
					(mapcar
						(lambda (str)
							(if (not (ignore-errors (parse-integer (string-trim "\"" str))))
								(string-trim "\"" str)
								(parse-integer (string-trim "\"" str))))
						(split-by current-line)))
				(print (write-to-string current-line))
				(if first-line
					(setf col-subj (position "Subject" current-line :test #'string-equal) col-block (position "Block_number" current-line :test #'string-equal)
							col-trial (position "Trial_number" current-line :test #'string-equal) col-stim (position "Abs_ref_stim" current-line :test #'string-equal)
							col-degrees (position "angle" current-line :test #'string-equal) col-match (position "same" current-line :test #'string-equal)
							col-correct (position "Resp_Correct" current-line :test #'string-equal) col-out (position "out" current-line :test #'string-equal)
							col-rt (position "RT" current-line :test #'string-equal) first-line nil)
					(if (or (eq subject t) (eq (nth col-subj current-line) subject))
						(let* ((puzzledata nil) (puzzletime (mp-time)) (stimuli (create-stimuli-set (nth col-stim current-line) (nth col-degrees current-line) (nth col-match current-line)))
								(current-block 1))
							(setf *puzzle-identifier* (concatenate 'string (first stimuli) (format nil "-~d" (get-universal-time))))
							(if output
								(format t "~&[SUBJ ~a // Block ~a // Trial ~a]: ~a" (nth col-subj current-line) (nth col-block current-line) (nth col-trial current-line) *puzzle-identifier*))
							(do-stimuli stimuli)
							(let* ((trialtime (- (mp-time) puzzletime)) (humantime (/ (nth col-rt current-line) 1000)) (correctness (equal *response* *correct-response*))
									(humancorrectness (nth col-correct current-line)) (outness (nth col-out current-line)))
								(if output (format t " - Time needed: ~as - Correct? ~a" trialtime (if correctness "yes" "no")))
								(setf puzzledata (list col-subj col-block col-trial *puzzle-identifier* trialtime humantime correctness humancorrectness outness)))
							(setf simdata (append simdata (list puzzledata))))))))
		simdata))
						

;;;
;;; not completely modified for Mental Rotation yet!
;;;
#|
; nicht-korrekte / nicht mit humandaten übereinstimmende trials verwerfen
; EEG-samplerate von 100Hz -> wegen Nyquist-Geschichten Modulaktivität mit 200Hz samplen (actually true? do i not need to stretch the trials?)
; VP	Block	Trial	Bedingung 	Modulaktivität

; SKETCH - function for logging that to a file (for all subjects and trials)
(defun get-simulation-activity (simulation samplerate)
	(let ((starttime 0) (endtime 0) (activities nil))
		(dolist (trialdata simulation)
			(setf endtime (+ endtime (nth 2 trialdata)))
			(setf activities (append activities (resample-trial-activity starttime endtime (nth 2 trialdata) samplerate))) ; second (nth 2 trialdata) is wrong, should get human trial length instead, but maybe i still have to save that in the simulation output
			(setf starttime (+ starttime (nth 2 trialdata))))
		activities))

|#

;;; MODEL
; Purifies model of past sins
(clear-all)

; --------------------------------

(define-model MentalRotation
	(if *use-seed-value* (sgp-fct `(:seed (,*use-seed-value* 5)))) ;randomizer with seed-value for predictable events)
	(sgp :show-focus t :trace-detail medium ;window parameters
		:er t ;further randomization to break "ties" - most effective if :esc is nil
		; :esc t
		:use-tree t ; generates a decision tree based on production requirements before running. May be quicker, but needs more memory
		;:bll 0.5
		:ncnar nil ; don't normalize chunks. supposed to speed up batch simulations
		:do-not-harvest spatial
	)
	
	(sgp-fct `(:v ,*enable-trace* :s-delay ,*spatial-delay* :s-complexity ,*spatial-complexity* :s-complexity-factor ,*spatial-complexity-factor*
				:declarative-finst-span ,*declarative-finst-span* :rt ,*retrieval-threshold* :ans ,*activation-noise*
				:visual-num-finsts ,*visual-num-finsts* :visual-finst-span ,*visual-finst-span* :esc ,*enable-sc* :lf ,*latency-factor*
				:ul ,*enable-utility-learning* :iu ,(if *enable-utility-learning* *initial-utility* 0) :egs ,*utility-noise*
				:epl ,*enable-production-compilation* :bll ,*enable-bl-learning*))

	(chunk-type-fct `(rotation-task currentpuzzle state substate (similarity-threshold ,*similarity-threshold*)
			(degrees ,*degrees-of-rotation*) totaldegrees rotateonce direction directionchanged last-result))
		
	;(chunk-type spatial-object class points transformations attached-to (origin T))
	(chunk-type (spatial-structure (:include spatial-object)) delimiters trial-id type)

	; visual objects
	;(chunk-type visual-object screen-pos value status color height width)
	(chunk-type (spatial-visual (:include visual-object)) (spatial t) class points type)

	; visual locations
	;(chunk-type visual-location screen-x screen-y distance kind color value height width size)
	(chunk-type (spatial-feature (:include visual-location)) (kind spatial) points type)

	(add-dm (start) (find) (encode) (retrieval) (wholesale) (piecemeal) (find-target) (encode-target) (compare-angle)
		(evaluate-angle) (answered) (try-rotation) (start-test-rotation) (evaluate-test-rotation) (yes) (rotate-object)
		(compare-rotation) (evaluate-rotation) (attend-other)
		(compare-objects) (rotate-around-z)
		(visual) (memory) (target) (reference) (whole) (piece) (none) (extra)
		)
	
	(set-all-base-levels 1000 -1000)
	(set-visual-center-point (/ *window-width* 2) (/ *window-height* 2))

	; necessary?
	;(add-dm-fct (list `(start-goal ISA rotation-task state start similarity-threshold ,*similarity-threshold* currentpuzzle ,*puzzle-identifier*)))
	(add-dm-fct (list `(start-goal ISA rotation-task state start currentpuzzle ,*puzzle-identifier*)))

	(sgp :cycle-hook update-sim-hook)

; ....................

	(P find-reference
		=goal>
			state	start
			substate	nil
		?visual-location>
			state	free
	==>
		=goal>
			substate	find
		+visual-location>
			-	points	nil
			kind	reference
			type	whole
			;spatial		t
	)
	
	(P attend-reference
		=goal>
			state		start
			substate	find
		=visual-location>
		?visual>
			state		free
	==>
		=goal>
			substate	encode
		-visual-location>
		+visual>
			cmd		move-attention
			screen-pos	=visual-location
	)

	#|
	(P DEBUG-OVERRIDE-always-do-wholesale
		=goal>
			state		start
			substate	encode
		=visual>
			type	whole
			points	=points
		?imaginal>
			state		free
	==>
		=goal>
			state		wholesale
			substate	find-target
		+imaginal>
			isa		spatial-object
			points	=points
			type	whole
			origin	memory
			class	reference
		-visual>
		+visual-location>
			spatial		t
			kind	target
			type	whole
	)
	|#

	(P try-wholesale-retrieval
		=goal>
			state		start
			substate	encode
		=visual>
			points	=points
		?retrieval>
			state		free
	==>
		=goal>
			substate	retrieval
		=visual>
		+retrieval>
			type	whole
			points	=points
			origin	memory 
	)

 	(P remembered-whole-find-target
		=goal>
			state		start
			substate	retrieval
			similarity-threshold	=sim
			degrees					=deg
		=visual>
			type	whole
			points	=points
		=retrieval>
			points	=points
		?imaginal>
			state		free
	==>
		!safe-eval!		(add-to-stage-markers "strategy-wholesale")
		+goal>	;;;;;;;;;;;;;;;;;;;; change to +goal> here?
			state		wholesale
			substate	find-target
			similarity-threshold	=sim
			degrees					=deg
		+imaginal>
			isa		spatial-object
			points	=points
			type	whole
			origin	memory
			class	reference
		+visual-location>
			spatial		t
			kind	target
			type	whole
		=visual>
			origin	memory ; encode visual object as a retrievable memory before harvesting
		-visual>
		-retrieval>
	)

		(P waiting-for-whole-target
			=goal>
				state		wholesale
				substate	find-target
			=imaginal>
			?visual-location>
				state	error
		==>
			=imaginal>
			+visual-location>
				;spatial		t
				kind	target
				type	whole
				- points	nil
		)

			;;; If Target has not yet appeared, explore other vis-locs, encode them and strengthen potential activations.
			;;; Nice and noble and cool idea, but adds about 200ms on trials...shelve for now.
			#| (P waiting-for-target-so-explore
				=goal>
					state		wholesale
					substate	find-target
				=imaginal>
				?visual-location>
					state		error
			==>
				=goal>
					substate	explore
				=imaginal>
				+visual-location>
					spatial		t
			)

				(P waiting-for-target-so-attend
					=goal>
						state		wholesale
						substate	explore
					=imaginal>
					=visual-location>
					?visual>
						state		free
				==>
					=goal>
						substate	explore-and-encode
					=imaginal>
					+visual>
						cmd		move-attention
						screen-pos	=visual-location
				)

				(P waiting-for-target-so-encode
					=goal>
						state		wholesale
						substate	explore-and-encode
					=imaginal>
					=visual>
				==>
					=goal>
						substate	find-target
					=imaginal>
					-visual>
					+visual-location>
						spatial		t
						kind	target
						type	whole
				) |#

	(P did-not-remember-whole-find-reference-piece
		=goal>
			state		start
			substate	retrieval
			similarity-threshold	=sim
			degrees					=deg
		=visual>
		?retrieval>
			state		error
	==>
		!safe-eval!		(add-to-stage-markers "strategy-piecemeal")
		+goal>	;;;;;;;;;;;;;;;;;;;; change to +goal> here?
			state		piecemeal
			substate	find
			similarity-threshold	=sim
			degrees					=deg
		=visual>
			origin	memory ; encode visual object as a retrievable memory before harvesting
		-visual>
		+visual-location>
			;spatial		t
			kind	reference
			type	piece
	)
	
	(P attend-reference-piece
		=goal>
			state		piecemeal
			substate	find
		=visual-location>
			;spatial		t
			kind	reference
			type	piece
		?visual>
			state		free
	==>
		=goal>
			substate	encode
		+visual>
			cmd		move-attention
			screen-pos	=visual-location
	)

	;;; *** HIDDEN ASSUMPTION WARNING: the model should randomly pick a companion piece and
	;;;			compare it (not based on points, but...on length? angles?), but that's really complicated.
	;;;			Easier to just take a piece from the same delimiting list position as the reference piece
	(P encode-reference-piece-and-find-matching-target-piece
		=goal>
			state		piecemeal
			substate	encode
		=visual>
			isa		spatial-visual
			points	=points
		?imaginal>
			state		free
	==>
		!safe-bind!		=matched-piece (get-matched-piece =points *ref-stimulus* *tar-stimulus*)
		=goal>
			substate	find-target
		+imaginal>
			isa		spatial-object
			type	piece
			points	=points
			origin	visual
			class		reference
		-visual>
		+visual-location>
			:attended		nil
			;spatial		t
			kind	target
			type	piece
			points	=matched-piece
	)

		(P waiting-for-target-piece
			=goal>
				state		piecemeal
				substate	find-target
			=imaginal>
				points	=points
			?visual-location>
				state	error
		==>
			!safe-bind!		=matched-piece (get-matched-piece =points *ref-stimulus* *tar-stimulus*)
			=imaginal>
			+visual-location>
				:attended		nil
				;spatial		t
				kind	target
				type	piece
				points	=matched-piece
		)

		(P attend-whole-target
			=goal>
				state		wholesale
				substate	find-target
			=imaginal>
			=visual-location>
			?visual>
				state	free
		==>
			=goal>
				substate	encode-target
			=imaginal>
			+visual>
				cmd		move-attention
				screen-pos	=visual-location
		)

		(P attend-target-piece
			!safe-bind!		=matched-piece (get-matched-piece =points *ref-stimulus* *tar-stimulus*)
			=goal>
				state		piecemeal
				substate	find-target
			=imaginal>
				points	=points
			=visual-location>
				points	=matched-piece
			?visual>
				state	free
		==>
			=goal>
				substate	encode-target
			=imaginal>
			+visual>
				cmd		move-attention
				screen-pos	=visual-location
		)

	(P encode-target
		=goal>
		;	state		wholesale
			substate	encode-target
		=imaginal>
			type	=type
		=visual>
			isa			spatial-visual
			points	=points
		?spatial>
			state		free
		?spatial-action>
			state	free
	==>
		=goal>
			substate	compare-angle
		=imaginal>
		+spatial>
			isa			spatial-object
			points	=points
			class		target
			;type		whole
			type	=type
			origin	visual
		-visual>
	)

		(P wholesale-obvious-match
			=goal>
				state		wholesale
				substate	compare-angle
			=imaginal>
				points	=points2
			=spatial>
				points	=points
			?manual>
				state		free
			!safe-eval!		(quick-compare-same =points =points2)
		==>
			!safe-eval!		(progn (add-to-stage-markers "match_wholesale-direct") (setf *debug-match-counter* (1+ *debug-match-counter*)))
			+goal>	;;;;;;;;;;;;;;;;;;;; change to +goal> here?
				state		answered
				substate	nil
			=spatial>
				origin	memory
			-spatial>
			-imaginal>
			+manual>
				cmd		press-key
				key		"r"
		)

		; no piecemeal-obvious-match, as single same pieces can easily give the same result as mirrored pieces (and vice versa)

		(P wholesale-obvious-mismatch
			=goal>
				state		wholesale
				substate	compare-angle
			=imaginal>
			;	points	=points2
			=spatial>
			;	points	=points
			?manual>
				state		free
			!safe-eval!		(quick-compare-mirrored *correct-response* *tar-rotation*)
		==>
			!safe-eval!		(progn (add-to-stage-markers "mismatch_wholesale-direct") (setf *debug-mismatch-counter* (1+ *debug-mismatch-counter*)))
			+goal>	;;;;;;;;;;;;;;;;;;;; change to +goal> here?
				state		answered
				substate	nil
			=spatial>
				origin	memory
			-spatial>
			-imaginal>
			+manual>
				cmd		press-key
				key		"w"
		)

		; no piecemeal-obvious-mismatch, as single mirrored pieces can easily give the same result as same pieces (and vice versa)

	(P compare-objects
		=goal>
		;	state		wholesale
			substate	compare-angle
		=imaginal>
			points	=points2
		=spatial>
		?spatial-action>
			state		free
	==>
		=goal>
			substate	evaluate-angle
		=imaginal>
		=spatial>
		+spatial-action>
			cmd		compare-objects
			value		=points2
	)

		(P wholesale-initial-match ; decision production!
			=goal>
				state		wholesale
				substate	evaluate-angle
				similarity-threshold		=threshold
			=spatial-action>
				< result	=threshold
				result	=result
			=spatial>
			=imaginal>
			?manual>
				state		free
		==>
			!safe-eval!		(add-to-stage-markers "match_wholesale-initial-comparison")
			+goal>	;;;;;;;;;;;;;;;;;;;; change to +goal> here?
				state		answered
				substate	nil
			-spatial-action>
			=spatial>
				origin	memory
			-spatial>
			-imaginal>
			+manual>
				cmd		press-key
				key		"r"
			!output!	=result
		)

		(P piecemeal-initial-match ; piecemeal-specific!
			=goal>
				state		piecemeal
				substate	evaluate-angle
				similarity-threshold		=threshold
				totaldegrees	nil
				rotateonce		nil
			=spatial-action>
				< result	=threshold
				result	=result
			=spatial>
			=imaginal>
		==>
			=goal>
				substate	attend-other
			;	totaldegrees	none
			;	rotateonce	t
			=spatial>
				origin	memory
			-spatial>
			-imaginal>
			+visual-location>
				:attended		nil
				;spatial		t
				kind	reference
				type	piece
			!output!	=result
			; no need to mark as success, just repeat with all pieces
		)
		
		(P piecemeal-force-known-rotation
			=goal>
				state		piecemeal
				substate	evaluate-angle
				similarity-threshold		=threshold
				rotateonce	t
			;	- totaldegrees	none	; should work and exclude this production from firing if an "initial match" occured
			=spatial-action>
			;	>= result	=threshold
				result	=result
			=spatial>
			=imaginal>
		==>
			=goal>
				substate	try-rotation
				last-result		=result
			=spatial>
			=imaginal>
			-spatial-action>
			!output!	=result
		)
		
		(P piecemeal-no-further-initial-match-possible-mismatch ; piecemeal-specific!
			=goal>
				state		piecemeal
				substate	evaluate-angle
				similarity-threshold		=threshold
				totaldegrees	none ; string slot names have made problems in the past - check if it works here
			=spatial-action>
				>= result	=threshold
				result	=result
			=spatial>
			=imaginal>
			?manual>
				state	free
		==>
			!safe-eval!		(add-to-stage-markers "mismatch_piecemeal-direct")
			+goal>	;;;;;;;;;;;;;;;;;;;; change to +goal> here?
				state		answered
				substate	nil
				direction		nil
				directionchanged	nil
				last-result		nil
				totaldegrees	nil
				rotateonce	nil
			-spatial-action>
			=spatial>
				origin	memory
			-spatial>
			-imaginal>
			+manual>
				cmd		press-key
				key		"w"
			!output!	=result
		)

	(P initial-mismatch-try-rotation
		=goal>
		;	state		wholesale
			substate	evaluate-angle
			similarity-threshold		=threshold
			rotateonce	nil
		=spatial-action>
			>= result	=threshold
			result	=result
		=spatial>
		=imaginal>
	==>
		!safe-eval!		(add-to-stage-markers "transformation_first-rotation")
		=goal>
			substate	try-rotation
			last-result		=result
		=spatial>
		=imaginal>
		-spatial-action>
		!output!	=result
	)

		(P direction-known-rotate-object
			=goal>
			;	state		piecemeal
				substate	try-rotation
				totaldegrees	=degrees
				- direction		nil
				directionchanged	nil
			=spatial>
			=imaginal>
			?spatial-action>
				state		free
		==>
			=goal>
				substate	compare-rotation
				rotateonce	t
			=spatial>
			=imaginal>
			+spatial-action>
				cmd		rotate-around-z
				value		=degrees
		)
		
		;;; Participants instructed to only rotate clockwise
		(P initial-direction-fixed-to-clockwise-rotate-object
			=goal>
			;	state		piecemeal
				substate	try-rotation
				degrees		=degrees
			;	totaldegrees	nil
				direction		nil
				directionchanged	nil
			=spatial>
			=imaginal>
			?spatial-action>
				state		free
		==>
			!safe-bind!		=new-degrees (* =degrees -1)
			=goal>
				substate	compare-rotation
				degrees		=new-degrees
				totaldegrees	-45
				direction	clockwise
			=spatial>
			=imaginal>
			+spatial-action>
				cmd		rotate-around-z
				value		=new-degrees
		)

	;
	; rotation explicitly given as clockwise in experiment instructions!
	;
	
	; ; ; (P try-rotation-counterclockwise
		; ; ; =goal>
		; ; ; ;	state		wholesale
			; ; ; substate	try-rotation
			; ; ; ;- direction		counterclockwise
			; ; ; direction		nil
			; ; ; directionchanged	nil
		; ; ; =spatial>
		; ; ; =imaginal>
		; ; ; ?spatial-action>
			; ; ; state		free
	; ; ; ==>
		; ; ; =goal>
			; ; ; substate	start-test-rotation
			; ; ; direction		counterclockwise
			; ; ; totaldegrees	15
		; ; ; =spatial>
		; ; ; =imaginal>
		; ; ; +spatial-action>
			; ; ; cmd		rotate-around-z
			; ; ; value		15
	; ; ; )

	; ; (P try-rotation-clockwise
		; ; =goal>
		; ; ;	state		wholesale
			; ; substate	try-rotation
			; ; ;- direction		clockwise
			; ; direction		nil
			; ; directionchanged	nil
		; ; =spatial>
		; ; =imaginal>
		; ; ?spatial-action>
			; ; state		free
	; ; ==>
		; ; =goal>
			; ; substate	start-test-rotation
			; ; direction		clockwise
			; ; totaldegrees	-15
		; ; =spatial>
		; ; =imaginal>
		; ; +spatial-action>
			; ; cmd		rotate-around-z
			; ; value		-15
	; ; )

	; ; (P evaluate-test-rotation
		; ; =goal>
		; ; ;	state		wholesale
			; ; substate	start-test-rotation
		; ; =spatial>
		; ; =imaginal>
			; ; points	=points2
		; ; ?spatial-action>
			; ; state		free
	; ; ==>
		; ; =goal>
			; ; substate	evaluate-test-rotation
		; ; =spatial>
		; ; =imaginal>
		; ; +spatial-action>
			; ; cmd		compare-objects
			; ; value		=points2
	; ; )

		; ; (P change-direction
			; ; =goal>
			; ; ;	state		wholesale
				; ; substate	evaluate-test-rotation
				; ; last-result		=before
				; ; - direction		nil
				; ; directionchanged	nil
			; ; =spatial>
			; ; =imaginal>
			; ; =spatial-action>
				; ; > result	=before
				; ; result	=result
		; ; ==>
			; ; =goal>
				; ; substate	try-rotation
				; ; directionchanged	yes
			; ; =spatial>
			; ; =imaginal>
			; ; -spatial-action>
			; ; !output!	=result
		; ; )

			; ; (P try-rotation-counterclockwise-after-change
				; ; =goal>
				; ; ;	state		wholesale
					; ; substate	try-rotation
					; ; - direction		counterclockwise
					; ; - directionchanged	nil
				; ; =spatial>
				; ; =imaginal>
				; ; ?spatial-action>
					; ; state		free
			; ; ==>
				; ; =goal>
					; ; substate	start-test-rotation
					; ; direction		counterclockwise
					; ; totaldegrees	15
				; ; =spatial>
				; ; =imaginal>
				; ; +spatial-action>
					; ; cmd		rotate-around-z
					; ; value		30
			; ; )

			; ; (P try-rotation-clockwise-after-change
				; ; =goal>
				; ; ;	state		wholesale
					; ; substate	try-rotation
					; ; - direction		clockwise
					; ; - directionchanged	nil
				; ; =spatial>
				; ; =imaginal>
				; ; ?spatial-action>
					; ; state		free
			; ; ==>
				; ; =goal>
					; ; substate	start-test-rotation
					; ; direction		clockwise
					; ; totaldegrees	-15
				; ; =spatial>
				; ; =imaginal>
				; ; +spatial-action>
					; ; cmd		rotate-around-z
					; ; value		-30
			; ; )

		; ; (P both-directions-fail-mismatch ; decision production!
			; ; =goal>
			; ; ;	state		wholesale
				; ; substate	evaluate-test-rotation
				; ; last-result		=before
				; ; - direction		nil
				; ; - directionchanged	nil
			; ; =spatial>
			; ; =imaginal>
			; ; =spatial-action>
				; ; > result	=before
				; ; result	=result
			; ; ?manual>
				; ; state		free
		; ; ==>
			; ; !safe-eval!		(add-to-stage-markers "mismatch_rotation-test-failed")
			; ; +goal>	;;;;;;;;;;;;;;;;;;;; change to +goal> here?
				; ; state		answered
				; ; substate	nil
				; ; direction		nil
				; ; directionchanged	nil
				; ; last-result		nil
				; ; totaldegrees	nil
				; ; rotateonce	nil
			; ; -spatial-action>
			; ; =spatial>
				; ; origin	memory
			; ; -spatial>
			; ; -imaginal>
			; ; +manual>
				; ; cmd		press-key
				; ; key		"w"
			; ; !output!	=result
		; ; )

	; ; ; start of rotation-comparison-loop
	; ; (P correct-direction
		; ; =goal>
		; ; ;	state		wholesale
			; ; substate	evaluate-test-rotation
			; ; degrees		=degrees
			; ; last-result		=before
			; ; - direction		nil
			; ; direction		=direction
		; ; =spatial>
		; ; =imaginal>
		; ; =spatial-action>
			; ; <= result	=before
			; ; result	=result
	; ; ==>
		; ; !safe-bind!		=new-degrees (if (string-equal =direction "clockwise") (* =degrees -1) =degrees)
		; ; =goal>
			; ; substate	rotate-object
			; ; last-result		=result
			; ; degrees		=new-degrees
			; ; directionchanged	nil
		; ; =spatial>
		; ; =imaginal>
		; ; -spatial-action>
		; ; !output!	=result
	; ; )

	(P rotate-object
		=goal>
		;	state		wholesale
			substate	rotate-object
			degrees		=degrees
			totaldegrees	=total
		=spatial>
		=imaginal>
		?spatial-action>
			state		free
	==>
		!safe-bind!		=newtotal	(+ =total =degrees)
		=goal>
			substate	compare-rotation
			totaldegrees	=newtotal
		=spatial>
		=imaginal>
		+spatial-action>
			cmd		rotate-around-z
			value		=degrees
	)

	(P evaluate-rotation
		=goal>
		;	state		wholesale
			substate	compare-rotation
		=spatial>
		=imaginal>
			points	=points2
		?spatial-action>
			state		free
	==>
		=goal>
			substate	evaluate-rotation
		=spatial>
		=imaginal>
		+spatial-action>
			cmd		compare-objects
			value		=points2
	)

		(P rotation-improved-fit
			=goal>
			;	state		wholesale
				substate	evaluate-rotation
				similarity-threshold		=threshold
				last-result		=before
				- rotateonce	t
				- rotateonce	extra
			=spatial>
			=imaginal>
			=spatial-action>
				<	result	=before
				>= 	result	=threshold
				result	=result
		==>
			!safe-eval!		(add-to-stage-markers "transformation_rotation")
			=goal>
				substate	rotate-object
				last-result		=result
			=spatial>
			=imaginal>
			-spatial-action>
			!output!	=result
		)
		
		(P known-rotation-does-not-work-one-more-try
			=goal>
			;	state		wholesale
				substate	evaluate-rotation
				similarity-threshold		=threshold
				last-result		=before
				degrees		=degrees
				totaldegrees	=total
				rotateonce	t
			=spatial>
			=imaginal>
			=spatial-action>
				<	result	=before
				>= 	result	=threshold
				result	=result
			?spatial-action>
				state	free
		==>
			=goal>
				substate	compare-rotation
				rotateonce	extra
			=spatial>
			=imaginal>
			+spatial-action>
				cmd		rotate-around-z
				value		=degrees
			!output!	=result
		)
				
			(P known-rotation-does-not-work-mismatch
				=goal>
				;	state		wholesale
					substate	evaluate-rotation
					similarity-threshold		=threshold
					last-result		=before
					totaldegrees	=total
					rotateonce	extra
				=spatial>
				=imaginal>
				=spatial-action>
					<	result	=before
					>= 	result	=threshold
					result	=result
				?manual>
					state	free
			==>
			;	!safe-eval!		(format t "~&~a~%" =total)
				!safe-eval!		(add-to-stage-markers "mismatch_piecemeal-inapplicable")
				+goal>	;;;;;;;;;;;;;;;;;;;; change to +goal> here?
					state		answered
					substate	nil
					last-result		nil
					direction		nil
					directionchanged	nil
					totaldegrees	nil
					rotateonce	nil
				=spatial>
					origin	memory
				-spatial>
				-imaginal>
				-spatial-action>
				+manual>
					cmd		press-key
					key		"w"
				!output!	=result	
			)
		
		;;;
		;;;
		;;; TO DO (optional)
		;;; special case might have spatial object rotated too far - add production(s) for backrotating!
		;;;

		(P rotation-no-improvement-mismatch ; decision production!
			=goal>
			;	state		wholesale
				substate	evaluate-rotation
				similarity-threshold		=threshold
				last-result		=before
			=spatial>
			=imaginal>
			=spatial-action>
				>=	result	=before
				>= 	result	=threshold
				result	=result
			?manual>
				state		free
		==>
			!safe-eval!		(add-to-stage-markers "mismatch_after-rotation")
			+goal>	;;;;;;;;;;;;;;;;;;;; change to +goal> here?
				state		answered
				substate	nil
				last-result		nil
				direction		nil
				directionchanged	nil
				totaldegrees	nil
				rotateonce	nil
			=spatial>
				origin	memory
			-spatial>
			-imaginal>
			-spatial-action>
			+manual>
				cmd		press-key
				key		"w"
			!output!	=result
		)

		(P wholesale-rotation-match ; decision production!
			=goal>
				state		wholesale
				substate	evaluate-rotation
				similarity-threshold		=threshold
			=spatial>
			=imaginal>
			=spatial-action>
				< result	=threshold
				result	=result
			?manual>
				state		free
		==>
			!safe-eval!		(add-to-stage-markers "match_wholesale-after-rotation")
			+goal>	;;;;;;;;;;;;;;;;;;;; change to +goal> here?
				state		answered
				substate	nil
				last-result		nil
				direction		nil
				directionchanged	nil
				totaldegrees	nil
				rotateonce	nil
			=spatial>
				origin	memory
			-spatial>
			-imaginal>
			-spatial-action>
			+manual>
				cmd		press-key
				key		"r"
			!output!	=result
		)

		(P piecemeal-rotation-match ; piecemeal-specific!
			=goal>
				state		piecemeal
				substate	evaluate-rotation
				similarity-threshold		=threshold
			=spatial>
			=imaginal>
			=spatial-action>
				< result	=threshold
				result	=result
		==>
			!safe-eval!		(add-to-stage-markers "transformation_piecemeal-rotation")
			=goal>
				substate	attend-other
				rotateonce	t
			=spatial>
				origin	memory
			-spatial>
			-imaginal>
			+visual-location>
				:attended		nil
				;spatial		t
				kind	reference
				type	piece
			!output!	=result
		)

			(P attend-other-reference-piece
				=goal>
					state		piecemeal
					substate	attend-other
				=visual-location>
				?visual>
					state		free
			==>
				=goal>
					substate	encode
				+visual>
					cmd		move-attention
					screen-pos	=visual-location
			)

			(P all-pieces-match ; decision production!
				; no more pieces found, no mismatch so far: solve
				; ...and don't think you can leave this production out and use one with just "find" instead:
				; that way, this production and the generic "wait" production would have the same requirements
				=goal>
					state		piecemeal
					substate	attend-other
				?visual-location>
					state		error
				?manual>
					state		free
			==>
				!safe-eval!		(add-to-stage-markers "match_piecemeal")
				+goal>	;;;;;;;;;;;;;;;;;;;; change to +goal> here?
					state		answered
					substate	nil
					direction		nil
					directionchanged	nil
					totaldegrees	nil
					rotateonce	nil
					last-result		nil
				+manual>
					cmd		press-key
					key		"r"
			)
		;;; end of rotation-comparison-loop
		
	(if *enable-utility-learning* (progn	
		(spp find-reference :reward nil)))

)








; here thar be dragons






;;;
;;;
;;; These can probably be deleted as the old productions already work,
;;; but i'll try that out first to be sure
;;;
#|
	(P attend-target-piece
		=goal>
			state		piecemeal
			substate	find-target
		=imaginal>
		=visual-location>
		?visual>
			state		free
	==>
		=goal>
			substate	encode-target
		=imaginal>
		+visual>
			cmd		move-attention
			screen-pos	=visual-location
	)

	(P encode-target-piece
		=goal>
			state		piecemeal
			substate	encode-target
		=imaginal>
		=visual>
			isa		spatial-visual
			points	=points
		?spatial>
			state		free
		?spatial-action>
			state	free
	==>
		=goal>
			substate	compare-angle
		=imaginal>
		+spatial>
			isa		spatial-object
			type	piece
			class		target
			points	=points
			origin	visual
		-visual>
	)

	(P compare-piece-objects
		=goal>
			state		piecemeal
			substate	compare-angle
		=imaginal>
			points	=points2
		=spatial>
		?spatial-action>
			state		free
	==>
		=goal>
			substate	evaluate-angle
		=imaginal>
		=spatial>
		+spatial-action>
			cmd		compare-objects
			value		=points2
	)

	(P piece-initial-mismatch-try-rotation
		=goal>
			state		piecemeal
			substate	evaluate-angle
			similarity-threshold		=threshold
		=spatial-action>
			>= result	=threshold
			result	=result
		=spatial>
		=imaginal>
	==>
		=goal>
			substate	try-rotation
			last-result		=result
		=spatial>
		=imaginal>
		-spatial-action>
		!output!	=result
	)

		(P piecemeal-try-rotation-counterclockwise
			=goal>
				state		piecemeal
				substate	try-rotation
				- direction		counterclockwise ; if this production fired before, don't try again for this trial
			=spatial>
			=imaginal>
			?spatial-action>
				state		free
		==>
			=goal>
				substate	start-test-rotation
				direction		counterclockwise
			=spatial>
			=imaginal>
			+spatial-action>
				cmd		rotate-around-z
				value		15 ; look if that's actually counterclockwise...
		)

		(P piecemeal-try-rotation-clockwise
			=goal>
				state		piecemeal
				substate	try-rotation
				- direction		clockwise ; if this production fired before, don't try again for this trial
			=spatial>
			=imaginal>
			?spatial-action>
				state		free
		==>
			=goal>
				substate	start-test-rotation
				direction		clockwise
			=spatial>
			=imaginal>
			+spatial-action>
				cmd		rotate-around-z
				value		-15 ; look if that's actually clockwise...
		)

		(P piecemeal-evaluate-test-rotation
			=goal>
				state		piecemeal
				substate	start-test-rotation
			=spatial>
			=imaginal>
				points	=points2
			?spatial-action>
				state		free
		==>
			=goal>
				substate	evaluate-test-rotation
			=spatial>
			=imaginal>
			+spatial-action>
				cmd		compare-objects
				value		=points2
		)

			(P piecemeal-change-direction
				=goal>
					state		piecemeal
					substate	evaluate-test-rotation
					last-result		=before
					- direction		nil
					directionchanged	nil
				=spatial>
				=imaginal>
				=spatial-action>
					> result	=before
					result	=result
			==>
				=goal>
					substate	try-rotation
					directionchanged	yes
				=spatial>
				=imaginal>
				-spatial-action>
				!output!	=result
			)

				;;; HIDDEN ASSUMPTION WARNING: Only works with *perfect* piece matching!
				;;;	 	otherwise:
				;;; 	no improvement after turning both directions: try other piece of target
				;;; 	all pieces of target tried? comparison mismatch
				(P piecemeal-both-directions-fail-mismatch ; decision production!
					=goal>
						state		piecemeal
						substate	evaluate-test-rotation
						last-result		=before
						- direction		nil
						- directionchanged	nil
					=spatial>
					=imaginal>
					=spatial-action>
						> result	=before
						result	=result
					?manual>
						state		free
				==>
					=goal>
						state		answered
						substate	nil
						direction		nil
						directionchanged	nil
						last-result		nil
					-spatial-action>
					=spatial>
						origin	memory
					-spatial>
					-imaginal>
					+manual>
						cmd		press-key
						key		"w"
					!output!	=result
				)

		; start of rotation-comparison-loop
		(P piecemeal-correct-direction
				=goal>
					state		piecemeal
					substate	evaluate-test-rotation
					degrees		=degrees
					last-result		=before
					- direction		nil
					direction		=direction
				=spatial>
				=imaginal>
				=spatial-action>
					<= result	=before
					result	=result
			==>
				!safe-bind!		=new-degrees (if (string-equal =direction "clockwise") (* =degrees -1) =degrees) ; check if clockwise or counterclockwise!!
				=goal>
					substate	rotate-object
					last-result		=result
					degrees		=new-degrees
				=spatial>
				=imaginal>
				-spatial-action>
				!output!	=result
			)

			(P piecemeal-rotate-object
				=goal>
					state		piecemeal
					substate	rotate-object
					degrees		=degrees
				=spatial>
				=imaginal>
				?spatial-action>
					state		free
			==>
				=goal>
					substate	compare-rotation
				=spatial>
				=imaginal>
				+spatial-action>
					cmd		rotate-around-z
					value		=degrees
			)

			(P piecemeal-evaluate-rotation
				=goal>
					state		piecemeal
					substate	compare-rotation
				=spatial>
				=imaginal>
					points	=points2
				?spatial-action>
					state		free
			==>
				=goal>
					substate	evaluate-rotation
				=spatial>
				=imaginal>
				+spatial-action>
					cmd		compare-objects
					value		=points2
			)

				(P piecemeal-rotation-improved-fit
					=goal>
						state		piecemeal
						substate	evaluate-rotation
						similarity-threshold		=threshold
						last-result		=before
					=spatial>
					=imaginal>
					=spatial-action>
						<		result	=before
						>= 	result	=threshold
						result	=result
				==>
					=goal>
						substate	rotate-object
						last-result		=result
					=spatial>
					=imaginal>
					-spatial-action>
					!output!	=result
				)

				;;;
				;;;
				;;; TO DO
				;;; special case might have spatial object rotated too far - add production for backrotating!
				;;;
				;;; HIDDEN ASSUMPTION WARNING: Only works with *perfect* piece matching!
				(P piecemeal-rotation-no-improvement-mismatch ; decision production!
					=goal>
						state		piecemeal
						substate	evaluate-rotation
						similarity-threshold		=threshold
						last-result		=before
					=spatial>
					=imaginal>
					=spatial-action>
						>=	result	=before
						>= 	result	=threshold
						result	=result
					?manual>
						state		free
				==>
					=goal>
						state		answered
						substate	nil
						last-result		nil
						direction		nil
						directionchanged	nil
					=spatial>
						origin	memory
					-spatial>
					-imaginal>
					-spatial-action>
					+manual>
						cmd		press-key
						key		"w"
					!output!	=result
				)
|#




































