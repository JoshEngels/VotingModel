; This file creates a full voting model in a new file and runs it (depending on which function is called, it may do this many times and in different ways)

; Functions:
; DONE: first-for-each: a proof of concept function that takes the first file name from each category and creates and runs a model (fast speed)
; TODO: test: takes as input a file name from a category and uses arbitrary other components and runs the model
; TODO: run-all: runs every single combination and collects data

(defun first-for-each ()

	(let 
	(
		; Everything in this first let statement can and should be modified
		
		; File Names
		(base-file-name "C:/Users/Joshua Engels/Desktop/A-Voting-Folder/")
		(output-file-name "C:/Users/Joshua Engels/Desktop/A-Voting-Folder/combination.lisp")
		
		
		(model-combination 
		
		'(define-model combined 
		
		
		; Parameters
		(sgp :v t :needs-mouse t :show-focus t :esc t :process-cursor t)
		
		(sgp :visual-finst-span 100) ;neccesary to avoid forgetting where we looked for the recognition strategies

		; Grouping Parameters
		(setf vg-glomming-radius 8)
		(setf vg-collision-type 'box) ;'point is faster, but less plausible (also needs a larger radius to work similarly)
		(setf vg-naming-type 'sequential) 
		
		;********************************
		; Enable forgetting / Activation 
		(sgp :ans 0.3)
		(sgp :rt 0)
		;********************************

		(sgp :act t)

		(setf *actr-enabled-p* t)


		; Declarative Memory
		(chunk-type MakeVote race candidate party button position screen state handpos to-do found default endState left right)
		(chunk-type Candidate name party race)
		(chunk-type VoteParty default)
		(chunk-type Abstain contest)
		(chunk-type VisualGroup race-group candidate-group party-group nextpage-group nextpage-text party-text candidate-text race-text button-group)
		
		; First Goal
		(add-dm (Vote ISA MakeVote state start-voting to-do SelectCandidate default "DEM" endState "nameofrace" left -1 right -1)))
		
		)
		
	)





		(load (car (directory (concatenate 'string base-file-name "1Ballot/*.lisp")))) ;load in the first ballot found

		(let 
		(
			; get all the first files from each directory
			(memory-file (open (car (directory (concatenate 'string base-file-name "2Memory/*.lisp")))))
			(macronavigation-file (open (car (directory (concatenate 'string base-file-name "3Macronavigation/*.lisp")))))
			(encoding-file (open (car (directory (concatenate 'string base-file-name "4Encoding/*.lisp")))))
			(micronavigation-file (open (car (directory (concatenate 'string base-file-name "5Micronavigation/*.lisp")))))
			(click-file (open (car (directory (concatenate 'string base-file-name "6Click/*.lisp")))))
			(output-file (open output-file-name :direction :output :if-exists :supersede))
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
			;(setf declaritive (append declaritive (get-to-add memory-file))) ;creates declaritive memory
			;(setf model-combination (append model-combination (list declaritive))) ;adds declaritive memory to the model

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
		(vote nil t) ;runs it in not real time
	)


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
		(vote t nil) ;human interaction
)
)



