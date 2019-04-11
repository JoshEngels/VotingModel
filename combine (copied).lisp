; This file creates a full voting model in a new file and runs it (depending on which function is called, it may do this many times and in different ways)

; Functions:
; DONE: first-for-each: a proof of concept function that takes the first file name from each category and creates and runs a model (fast speed)
; TODO: test: takes as input a file name from a category and uses arbitrary other components and runs the model
; TODO: run-all: runs every single combination and collects data

; Global Variables
(defvar base-file-name "C:/Users/Joshua Engels/Desktop/A-Voting-Folder/")
(defvar output-file-name "C:/Users/Joshua Engels/Desktop/A-Voting-Folder/combination.lisp")
(defvar current-strat nil)


; First for each with optional list of files to choose


(defun first-for-each ()
		
	(load (concatenate 'string base-file-name "logging.lisp")) ;load in the logging functions
	(create-new-file) ;create a new logging file
	
	(load (car (directory (concatenate 'string base-file-name "1Ballot/*.lisp")))) ;load in the first ballot found


	(let 
	(
		; get all the first files from each directory
		(parameters-file (open (concatenate 'string base-file-name "parameters.lisp")))
		(memory-file (open (car (directory (concatenate 'string base-file-name "2Memory/*.lisp")))))
		(macronavigation-file (open (car (directory (concatenate 'string base-file-name "3Macronavigation/*.lisp")))))
		(encoding-file (open (car (directory (concatenate 'string base-file-name "4Encoding/*.lisp")))))
		(micronavigation-file (open (car (directory (concatenate 'string base-file-name "5Micronavigation/*.lisp")))))
		(click-file (open (car (directory (concatenate 'string base-file-name "6Click/*.lisp")))))
		(output-file (open output-file-name :direction :output :if-exists :supersede))
		
		; Start of model, to add to
		(model-combination '(define-model combined))
	)
	

			
		;function that takes in a file and returns a list of the s-expressions in it
		(defun get-to-add (file)
			(let ((line (read file nil)) (so-far '()))

			(loop while line 
		
				do 	(progn 
					(setf so-far (append so-far (list line)))
					(setf line (read file nil))
				)
			)
			
			(return-from get-to-add so-far)

		))
		
		
		;Creates and writes the model file
		(setf model-combination (append model-combination (get-to-add parameters-file))) ;Adds parameters to the model
		(setf model-combination (append model-combination (get-to-add memory-file))) ;Adds declaritive memory to the model
		(setf model-combination (append model-combination (get-to-add macronavigation-file))) ;Adds macronavigation to the model
		(setf model-combination (append model-combination (get-to-add encoding-file))) ;Adds encoding to the model
		(setf model-combination (append model-combination (get-to-add micronavigation-file))) ;Adds micronavigation to the model
		(setf model-combination (append model-combination (get-to-add click-file))) ;Adds click to the model
	
		(setf model-combination (append model-combination (list '(goal-focus Vote)))) ;Adds the start goal focus
		
		(pprint '(clear-all) output-file)

		(pprint model-combination output-file)	
		
			
		(close output-file) ;bad practice, but leave for now
	)
	
	; runs the model file
	(load output-file-name) ;hopefully this works
	(vote nil t t) ;runs it in not real time and visible

)

; TODO clean up in general
; TODO combine with first-for-all
; File-names contains a list of file names or nils (if default is desired). Creates the model in the output file and runs it with no display not in real time
(defun create-specific (file-names)
	
	(print file-names)
	
	(if (nth 0 file-names) (load (concatenate 'string base-file-name "1Ballot/" (nth 0 file-names) ".lisp"))
											(load (car (directory (concatenate 'string base-file-name "1Ballot/*.lisp")))))


	(let 
	(
		(parameters-file (open (concatenate 'string base-file-name "parameters.lisp")))
		(memory-file (if (nth 1 file-names) (open (concatenate 'string base-file-name "2Memory/" (nth 1 file-names) ".lisp"))
											(open (car (directory (concatenate 'string base-file-name "2Memory/*.lisp"))))))
		(macronavigation-file (if (nth 2 file-names) (open (concatenate 'string base-file-name "3Macronavigation/" (nth 2 file-names) ".lisp"))
											(open (car (directory (concatenate 'string base-file-name "3Macronavigation/*.lisp"))))))
		(encoding-file (if (nth 3 file-names) (open (concatenate 'string base-file-name "4Encoding/" (nth 3 file-names) ".lisp"))
											(open (car (directory (concatenate 'string base-file-name "4Encoding/*.lisp"))))))
		(micronavigation-file (if (nth 4 file-names) (open (concatenate 'string base-file-name "5Micronavigation/" (nth 4 file-names) ".lisp"))
											(open (car (directory (concatenate 'string base-file-name "5Micronavigation/*.lisp"))))))
		(click-file (if (nth 5 file-names) (open (concatenate 'string base-file-name "6Click/" (nth 5 file-names) ".lisp"))
											(open (car (directory (concatenate 'string base-file-name "6Click/*.lisp"))))))
		(output-file (open output-file-name :direction :output :if-exists :supersede))
		
		; Start of model, to add to
		(model-combination '(define-model combined))
	)
	

			
		;function that takes in a file and returns a list of the s-expressions in it
		(defun get-to-add (file)
			(let ((line (read file nil)) (so-far '()))

			(loop while line 
		
				do 	(progn 
					(setf so-far (append so-far (list line)))
					(setf line (read file nil))
				)
			)
			
			(return-from get-to-add so-far)

		))
		
		
		;Creates and writes the model file
		(setf model-combination (append model-combination (get-to-add parameters-file))) ;Adds parameters to the model
		(setf model-combination (append model-combination (get-to-add memory-file))) ;Adds declaritive memory to the model
		(setf model-combination (append model-combination (get-to-add macronavigation-file))) ;Adds macronavigation to the model
		(setf model-combination (append model-combination (get-to-add encoding-file))) ;Adds encoding to the model
		(setf model-combination (append model-combination (get-to-add micronavigation-file))) ;Adds micronavigation to the model
		(setf model-combination (append model-combination (get-to-add click-file))) ;Adds click to the model
	
		(setf model-combination (append model-combination (list '(goal-focus Vote)))) ;Adds the start goal focus
		
		(pprint '(clear-all) output-file)

		(pprint model-combination output-file)	
		
			
		(close output-file) ;bad practice, but leave for now
	)
	
	; runs the model file
	(load output-file-name) ;hopefully this works
	(vote nil t nil) ;runs it in not real time and invisible

)


(defun test-ballot-human (file-name)


	(let 
	(		
		; File Names
		(base-file-name "C:/Users/Joshua Engels/Desktop/A-Voting-Folder/")
		(output-file-name "C:/Users/Joshua Engels/Desktop/A-Voting-Folder/combination.lisp")
		
		(model-combination '(define-model combined))
	)


		(load (car (directory (concatenate 'string base-file-name "1Ballot/" file-name)))) ;load in ballot specified

		(let 
		(
			(output-file (open output-file-name :direction :output :if-exists :supersede))
		)
		
			(pprint '(clear-all) output-file)
			(pprint model-combination output-file)	
			(close output-file) ;bad practice, but leave for now
		)
		
		; runs the model file
		(load output-file-name) ;hopefully this works
		(vote t nil t) ;human interaction
)
)





(defun run-lists (num-trials)

	(defvar current-dm "TEMPTEMP")
	(defvar current-micro "TEMPTEMP")

	(load (concatenate 'string base-file-name "logging.lisp")) ;load in the logging functions
	(create-new-file) ;create a new logging file
	
	(let ((micros '("VG-Random-Retrieve-Recognize-Party"
				"VG-Serial-Recognize-Party"
				"VG-Serial-Retrieve-Party"
				"VG-Serial-RetrieveParty-Party"
				"VG-Serial-Retrieve-Recognize-Party"
				"VG-Random-Recognize-Party"
				"VG-Random-Retrieve-Party"
				"VG-Random-RetrieveParty-Party"))
		(dms '("all-perfect"
			"all-rolloff"
			"most-rolloff"
			"most-perfect"
			"full-dm")))
		
	
		(loop for micro in micros
		do (loop for dm in dms do (progn
			(setf current-dm dm)
			(setf current-micro micro)
			(reset-count)
			(dotimes (trial num-trials)
				(progn 
				(print trial)
				(create-specific (list nil dm nil nil micro nil)))
				)
			)))
			
))




; (defvar *dm-path* *load-truename*)
; (case *which-dm*
  ; (all-perfect (load (merge-pathnames "all-candidates-perfect.lisp" *dm-path*)))
  ; (all-rolloff (load (merge-pathnames "all-with-rolloff.lisp" *dm-path*)))
  ; (most-rolloff (load (merge-pathnames "most-with-rolloff.lisp" *dm-path*)))
  ; (most-perfect (load (merge-pathnames "most-with-abstention.lisp" *dm-path*)))
  ; (full-dm (load (merge-pathnames "full-dm.lisp" *dm-path*)))
; )