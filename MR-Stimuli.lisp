; Stimulus format: 9-10 3D coordinates
; Matches?: t, nil

; Cube Coordinates for Mental Rotation Stimuli
(setf mr-stim-1 '((0 3 3) (0 3 2) (0 3 1) (0 2 1) (0 1 1) (0 0 1) (1 0 1) (2 0 1) (3 0 1) (3 0 0)))
(setf mr-stim-2 '((0 3 2) (0 3 1) (0 2 1) (0 1 1) (0 0 1) (1 0 1) (2 0 1) (3 0 1) (3 0 0) (3 0 -1)))
(setf mr-stim-3 '((0 3 3) (0 3 2) (0 3 1) (0 2 1) (0 1 1) (0 0 1) (1 0 1) (2 0 1) (3 0 1) (3 -1 1)))
(setf mr-stim-4 '((0 3 2) (0 3 1) (0 2 1) (0 1 1) (0 0 1) (1 0 1) (2 0 1) (3 0 1) (3 -1 1) (3 -2 1)))

(setf mr-stim-5 '((0 3 3) (0 2 3) (0 1 3) (0 1 2) (0 1 1) (0 1 0) (1 1 0) (2 1 0) (2 0 0)))
(setf mr-stim-6 '((0 4 3) (0 3 3) (0 2 3) (0 1 3) (0 1 2) (0 1 1) (0 1 0) (1 1 0) (2 1 0) (2 0 0)))
(setf mr-stim-7 '((0 2 3) (0 1 3) (0 1 2) (0 1 1) (0 1 0) (1 1 0) (2 1 0) (2 0 0) (2 -1 0) (2 -2 0)))
(setf mr-stim-8 '((0 3 3) (0 3 2) (0 3 1) (0 2 1) (0 1 1) (0 0 1) (1 0 1) (2 0 1) (2 0 0) (2 0 -1)))

(setf mr-stim-9 '((0 3 3) (0 2 3) (0 1 3) (0 1 2) (0 1 1) (0 1 0) (1 1 0) (2 1 0) (2 0 0) (2 -1 0)))
(setf mr-stim-10 '((0 3 3) (0 2 3) (0 1 3) (0 1 2) (0 1 1) (1 1 1) (2 1 1) (3 1 1) (3 0 1) (3 -1 1)))
(setf mr-stim-11 '((0 4 3) (0 3 3) (0 2 3) (1 2 3) (2 2 3) (2 2 2) (2 2 1) (2 2 0) (2 1 0) (2 0 0)))
(setf mr-stim-12 '((0 3 2) (0 3 1) (0 2 1) (0 1 1) (0 0 1) (0 -1 1) (1 -1 1) (2 -1 1) (3 -1 1) (3 -2 1)))

(setf mr-stim-13 '((0 3 2) (0 3 1) (0 2 1) (0 1 1) (0 0 1) (1 0 1) (2 0 1) (3 0 1) (4 0 1) (4 -1 1)))
(setf mr-stim-14 '((0 2 4) (0 1 4) (0 1 3) (0 1 2) (0 1 1) (0 1 0) (1 1 0) (2 1 0) (3 1 0) (3 0 0)))
(setf mr-stim-15 '((0 2 3) (0 1 3) (0 1 2) (0 1 1) (0 1 0) (1 1 0) (2 1 0) (3 1 0) (4 1 0) (4 0 0)))
(setf mr-stim-16 '((0 5 2) (0 4 2) (0 3 2) (0 2 2) (1 2 2) (2 2 2) (2 2 1) (2 2 0) (2 1 0) (2 0 0)))

; Delimiters to separate Mental Rotation Stimuli into "arms"
(setf delims-1 '(3 3 2 2))
(setf delims-2 '(2 3 3 2))
(setf delims-3 '(3 3 2 2))
(setf delims-4 '(2 3 3 2))

(setf delims-5 '(3 2 2 2))
(setf delims-6 '(3 3 2 2))
(setf delims-7 '(2 3 2 3))
(setf delims-8 '(3 3 2 2))

(setf delims-9 '(3 2 2 3))
(setf delims-10 '(3 2 3 2))
(setf delims-11 '(3 2 3 2))
(setf delims-12 '(2 4 2 2))

(setf delims-13 '(2 3 3 2))
(setf delims-14 '(2 3 3 2))
(setf delims-15 '(2 3 3 2))
(setf delims-16 '(3 3 2 2))



; "Inverses" a list (multiply with -1)
(defun create-mirror-object (points)
	(mapcar #'(lambda (pts) (progn (setf (first pts) (* -1 (first pts))) pts)) points))

; Creates a stimulus with mirrored x-Coordinates, based on an existing stimulus
(defun create-mirror-stimulus (stim)
	(mapcar #'(lambda (pts) (progn (setf (first pts) (* -1 (first pts))) pts)) (copy-tree stim)))
	
; Create a set of MR stimuli (given level, rotation and if trial is a matching trial) - returns the varname and official reference and target names
(defun create-stimuli-set (type rotation match)
	(let* ((targettype (if match "a" "b")) (refstim (concatenate 'string (write-to-string type) "_y_0_a")) (tarstim (concatenate 'string (write-to-string type) "_y_" (write-to-string rotation) "_" targettype))
			(stimname (concatenate 'string "mr-stim-" (write-to-string type))))
		(list stimname type rotation match refstim tarstim)))

; Create a (random) block of 128 trials, consisting of each combination of the 16 types, 4 rotation degrees and match/mismatch (16*4*2 = 128) - returns a list of stimuli sets
(defun create-stimuli-block ()
	(let ((stimlist nil) (rotations '(0 50 100 150)) (types (loop :for n :from 1 :to 16 :collect n)) (matching (list t nil)))
		(dolist (type types)
			(dolist (rotation rotations)
				(dolist (match matching)
					(setf stimlist (append stimlist (list (create-stimuli-set type rotation match)))))))
		(permute-list stimlist)))

; ; Retrieves the actual stimulus object based on its name
; (defun get-stimulus (name match rotation)
	; (if match
		; (mapcar #'(lambda (pts) (rotate-around-z pts rotation)) name)
		; (mapcar #'(lambda (pts) (rotate-around-z pts rotation)) (create-mirror-stimulus name))))