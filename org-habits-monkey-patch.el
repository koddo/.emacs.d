;; severe monkey-patching here
;; there's no org-habit-missed-glyph var originally

;; maybe pr this to org-mode repo
;; taken from https://github.com/bzg/org-mode/blob/master/lisp/org-habit.el

;; beware, there's a bug in the code:
;; when org-habit-following-days < 2 and the glyph is not space (?/s), it behaves weirdly

(defun org-habit-build-graph (habit starting current ending)
  "Build a graph for the given HABIT, from STARTING to ENDING.
CURRENT gives the current time between STARTING and ENDING, for
the purpose of drawing the graph.  It need not be the actual
current time."
  (let* ((all-done-dates (sort (org-habit-done-dates habit) #'<))
	 (done-dates all-done-dates)
	 (scheduled (org-habit-scheduled habit))
	 (s-repeat (org-habit-scheduled-repeat habit))
	 (start (time-to-days starting))
	 (now (time-to-days current))
	 (end (time-to-days ending))
	 (graph (make-string (1+ (- end start)) org-habit-missed-glyph))
	 (index 0)
	 last-done-date)
    (while (and done-dates (< (car done-dates) start))
      (setq last-done-date (car done-dates)
	    done-dates (cdr done-dates)))
    (while (< start end)
      (let* ((in-the-past-p (< start now))
	     (todayp (= start now))
	     (donep (and done-dates (= start (car done-dates))))
	     (faces
	      (if (and in-the-past-p
		       (not last-done-date)
		       (not (< scheduled now)))
		  (if (and all-done-dates (= (car all-done-dates) start))
		      ;; This is the very first done of this habit.
		      '(org-habit-ready-face . org-habit-ready-future-face)
		    '(org-habit-clear-face . org-habit-clear-future-face))
		(org-habit-get-faces
		 habit start
		 (and in-the-past-p
		      last-done-date
		      ;; Compute scheduled time for habit at the time
		      ;; START was current.
		      (let ((type (org-habit-repeat-type habit)))
			(cond
			 ;; At the last done date, use current
			 ;; scheduling in all cases.
			 ((null done-dates) scheduled)
			 ((equal type ".+") (+ last-done-date s-repeat))
			 ((equal type "+")
			  ;; Since LAST-DONE-DATE, each done mark
			  ;; shifted scheduled date by S-REPEAT.
			  (- scheduled (* (length done-dates) s-repeat)))
			 (t
			  ;; Compute the scheduled time after the
			  ;; first repeat.  This is the closest time
			  ;; past FIRST-DONE which can reach SCHEDULED
			  ;; by a number of S-REPEAT hops.
			  ;;
			  ;; Then, play TODO state change history from
			  ;; the beginning in order to find current
			  ;; scheduled time.
			  (let* ((first-done (car all-done-dates))
				 (s (let ((shift (mod (- scheduled first-done)
						      s-repeat)))
				      (+ (if (= shift 0) s-repeat shift)
					 first-done))))
			    (if (= first-done last-done-date) s
			      (catch :exit
				(dolist (done (cdr all-done-dates) s)
				  ;; Each repeat shifts S by any
				  ;; number of S-REPEAT hops it takes
				  ;; to get past DONE, with a minimum
				  ;; of one hop.
				  (cl-incf s (* (1+ (/ (max (- done s) 0)
						       s-repeat))
						s-repeat))
				  (when (= done last-done-date)
				    (throw :exit s))))))))))
		 donep)))
	     markedp face)
	(cond
	 (donep
	  (aset graph index org-habit-completed-glyph)
	  (setq markedp t)
	  (while (and done-dates (= start (car done-dates)))
	    (setq last-done-date (car done-dates))
	    (setq done-dates (cdr done-dates))))
	 (todayp
	  (aset graph index org-habit-today-glyph)))
	(setq face (if (or in-the-past-p todayp)
		       (car faces)
		     (cdr faces)))
	(when (and in-the-past-p
		   (not (eq face 'org-habit-overdue-face))
		   (not markedp))
	  (setq face (cdr faces)))
	(put-text-property index (1+ index) 'face face graph)
	(put-text-property index (1+ index)
			   'help-echo
			   (concat (format-time-string
				    (org-time-stamp-format)
				    (time-add starting (days-to-time (- start (time-to-days starting)))))
				   (if donep " DONE" ""))
			   graph))
      (setq start (1+ start)
	    index (1+ index)))
    graph))
