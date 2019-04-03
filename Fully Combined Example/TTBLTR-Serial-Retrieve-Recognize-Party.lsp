

;;; Supporting Lisp code to run the model

(defun rand (n)
    (- (random (1+ (* 2 n))) n))


(defun vote (noise noise_macro realtime &optional use-model)

  
	(reset)
  
	(setq i 0)
	(setq button-map (make-hash-table)) 
	(setq button-state (make-hash-table)) 
	;(setq noise 1)

  
	(let* ((window (open-exp-window "Ballet" :width 550 :height 550)))
	
	
		(setq starting-x 10)
	
		(loop
	
			(setq j 0)
			
			(setq starting-x (+ (* i 180) 10))
		
			;starting x is i * 160 + 10
			
			(loop 

				;starting y is j * 85 + 10		
				(setq starting-y (+ (* j 85) 10))
				
				
				
				(setq randomx (+ starting-x (rand noise_macro)))
				(setq randomy (+ starting-y (rand noise_macro)))

				
				
				(setf candidate-party-object-array (make-array '(6)))
				
				(setq race-num (+ (* i 6) j))

				
				(add-text-to-exp-window :text (concatenate 'string "temporaryracename" (write-to-string race-num)) :x randomx :y randomy)
				
				(setf (aref candidate-party-object-array 0) (add-text-to-exp-window :text (concatenate 'string "candidate" (write-to-string (* race-num 3))) :x (+ randomx 30 (rand noise)) :y (+ randomy 20 (rand noise))))
				(setf (aref candidate-party-object-array 1) (add-text-to-exp-window :text (concatenate 'string "candidate" (write-to-string (+ (* race-num 3) 1))) :x (+ randomx 30 (rand noise)) :y (+ randomy 35 (rand noise))))
				(setf (aref candidate-party-object-array 2) (add-text-to-exp-window :text (concatenate 'string "candidate" (write-to-string (+ (* race-num 3) 2))) :x (+ randomx 30 (rand noise)) :y (+ randomy 50 (rand noise))))
				
				(setf (aref candidate-party-object-array 3) (add-text-to-exp-window :text "party1" :x (+ randomx 118 (rand noise)) :y (+ randomy 20 (rand noise))))
				(setf (aref candidate-party-object-array 4) (add-text-to-exp-window :text "party2" :x (+ randomx 118 (rand noise)) :y (+ randomy 35 (rand noise))))
				(setf (aref candidate-party-object-array 5) (add-text-to-exp-window :text "party3" :x (+ randomx 118 (rand noise)) :y (+ randomy 50 (rand noise))))
				
				(setf button_temp (add-button-to-exp-window :text "" :x randomx :y (+ randomy 22) :width 20 :height 10 :action 
				(lambda (button)
				(if (= (gethash button button-state) 0) (progn
					(modify-text-for-exp-window (aref (gethash button button-map) 0) :color 'blue)
					(modify-text-for-exp-window (aref (gethash button button-map) 3) :color 'blue)
					(setf (gethash button button-state) 1)
					)
					(progn
					(modify-text-for-exp-window (aref (gethash button button-map) 0) :color 'black)
					(modify-text-for-exp-window (aref (gethash button button-map) 3) :color 'black)
					(setf (gethash button button-state) 0)
					)
				)
				)))
				(setf (gethash button_temp button-map) candidate-party-object-array)
				(setf (gethash button_temp button-state) 0)

				
				
				
				(setf button_temp (add-button-to-exp-window :text "" :x randomx :y (+ randomy 38) :width 20 :height 10 :action 
				(lambda (button)
				(if (= (gethash button button-state) 0) (progn
					(modify-text-for-exp-window (aref (gethash button button-map) 1) :color 'blue)
					(modify-text-for-exp-window (aref (gethash button button-map) 4) :color 'blue)
					(setf (gethash button button-state) 1)
					)
					(progn
					(modify-text-for-exp-window (aref (gethash button button-map) 1) :color 'black)
					(modify-text-for-exp-window (aref (gethash button button-map) 4) :color 'black)
					(setf (gethash button button-state) 0)
					)
				)
				)))
				(setf (gethash button_temp button-map) candidate-party-object-array)
				(setf (gethash button_temp button-state) 0)
				
				
				(setf button_temp (add-button-to-exp-window :text "" :x randomx :y (+ randomy 53) :width 20 :height 10 :action 
				(lambda (button)
				(if (= (gethash button button-state) 0) (progn
					(modify-text-for-exp-window (aref (gethash button button-map) 2) :color 'blue)
					(modify-text-for-exp-window (aref (gethash button button-map) 5) :color 'blue)
					(setf (gethash button button-state) 1)
					)
					(progn
					(modify-text-for-exp-window (aref (gethash button button-map) 2) :color 'black)
					(modify-text-for-exp-window (aref (gethash button button-map) 5) :color 'black)
					(setf (gethash button button-state) 0)
					)
				)
				)))
				(setf (gethash button_temp button-map) candidate-party-object-array)
				(setf (gethash button_temp button-state) 0)
			
			
			(setq j (+ j 1))
			
			(when (> j 5) (return j))
		
		)
		
		(setq i (+ i 1))
				
		(when (> i 2) (return i))
				
		
	
	)
	
	
	(if use-model
		(progn
		(install-device window)
		(proc-display)
		(start-hand-at-mouse)
		(if realtime (run 200 :real-time t) (run 200))
		;(run 200 :real-time t)
		(run 200)
		)())

		
))


; Start of the model
(clear-all)
(define-model Top-To-Bottom-Left-To-Right

; Parameters
(sgp :v t :needs-mouse t :show-focus t :esc t)
(sgp :process-cursor t)

; Grouping Parameters
(setf vg-glomming-radius 8)
(setf vg-collision-type 'box) ;'point is faster, but less plausible (also needs a larger radius to work similarly)
(setf vg-naming-type 'sequential) 


; Declarative Memory
(chunk-type MakeVote race candidate party button position screen state handpos to-do found default endState left right)
(chunk-type Candidate name party race)
(chunk-type VoteParty default)
(chunk-type Abstain contest)
(chunk-type VisualGroup race-group candidate-group party-group nextpage-group nextpage-text party-text candidate-text race-text)

(add-dm

(candidate1 ISA Candidate name "candidate1" party "party2" race "temporaryracename0")

(candidate4 ISA Candidate name "candidate4" party "party2" race "temporaryracename1")

(candidate7 ISA Candidate name "candidate7" party "party2" race "temporaryracename2")

(candidate10 ISA Candidate name "candidate10" party "party3" race "temporaryracename3")

(candidate13 ISA Candidate name "candidate13" party "party1" race "temporaryracename4")

(candidate16 ISA Candidate name "candidate16" party "party1" race "temporaryracename5")

(candidate19 ISA Candidate name "candidate19" party "party2" race "temporaryracename6")

(candidate21 ISA Candidate name "candidate21")

(candidate24 ISA Candidate name "candidate24")

(candidate29 ISA Candidate name "candidate29")

(temporaryracename13 ISA Abstain contest "temporaryracename13")

(temporaryracename14 ISA Abstain contest "temporaryracename14")

(temporaryracename15 ISA Abstain contest "temporaryracename15")

(temporaryracename16 ISA Abstain contest "temporaryracename16")

(temporaryracename17 ISA Abstain contest "temporaryracename17")

(Vote ISA MakeVote state start-voting to-do SelectCandidate default "party3" endState "nameofrace" left -1 right -1)

;(Vote ISA MakeVote state start-voting to-do SelectCandidate default "party3" endState "nameofrace" left -1 right -1)

)





;****************************************
;****************************************
; Find the next race to vote on (macronavigation)
;****************************************
;****************************************

;****************************************
; These productions deal with within column navigation
;****************************************


; Attends the top left corner race to start voting and sends control as if this was a new column
; TODO: This is a problem if the x of the top left element is not the most left x of any race.
; To fix, would need to first attend the top left corner, at a point or something, and then find the nearest object
; (P Find-First-Race

; =goal>
	; state			start-voting
	
; ==>

; +visual-location>
	; ISA		visual-location
	; kind		text
	; screen-x	lowest
	; screen-y	lowest
	
; =goal>
	; state		attending-race-next-column

; )

; Finds the first race, but need a guess for right x that is for sure greater than the middle of the first race description
(P Find-First-Race

=goal>
	state			start-voting
	left			=left-bound
	
==>

+visual-location>
	ISA			visual-location
	kind		text
	> screen-x	=left-bound
	< screen-x	150 ;
	screen-y	lowest
	
=goal>
	state		attending-race-next-column
	right		-1 ;a little cheat here, only use the right to find the first race and then delete it like it was never there

)



; Does a visual location request for the next race in this column
(P Find-Race-Same-Column

=goal>
	state			find-next-race
	left			=left-bound
	right			=right-bound

	
=imaginal>
	race-group		=race-group
	candidate-group	=candidate-group
	party-group		=party-group
	
==>

+visual-location>
	ISA			visual-location
	kind		text
	> screen-x	=left-bound
	< screen-x	=right-bound
	> screen-y	current
	- group		=race-group
	- group		=candidate-group
	- group		=party-group
	screen-y	lowest
	kind		text
	
=imaginal>
	
=goal>
	state		attending-race-same-column
	to-do		SelectCandidate


)

; Goes to the column switch logic
(P Find-Race-Same-Column-No-Match

=goal>
	state		attending-race-same-column	
	
?visual-location>
	buffer		failure
	
==>

=goal>
	state		find-top-race

)

; Attends the race header, passes control to the productions that encode the race groups
(P Attend-Race-Same-Column

=goal>
	state		attending-race-same-column	
		
=visual-location>
	ISA			visual-location
	kind		text

?visual>
	state		free
	
?imaginal>
	state		free
	
==>

+imaginal>
	race-group		none
	candidate-group	none
	party-group		none

+visual>
	ISA			move-attention
	screen-pos	=visual-location
	
=visual-location>

=goal>
	state		storing-race-group

)


;****************************************
; These productions switch to the next collumn and attend a new race
;****************************************


(P Find-Top-Race

=goal>
	state		find-top-race
	left		=left-bound
	right		=right-bound
	
==>

+visual-location>
	ISA			visual-location
	> screen-x	=left-bound
	< screen-x	=right-bound
	screen-y	lowest
	kind		text

=goal>
	state		attending-top-race

)

(P Attend-Top-Race

=goal>
	state		attending-top-race
	
=visual-location>
	ISA			visual-location	
	kind		text
	
?visual>
	state		free

==>

+visual>
	ISA			move-attention
	screen-pos	=visual-location
	
=goal>
	state		find-race-next-column
	
)

(P Find-Race-Next-Column

=goal>
	state 		find-race-next-column
	right		=right-bound
	
=visual>

==>

+visual-location>
	ISA			visual-location
	kind		text
	> screen-x	=right-bound
	:nearest	current
	
=goal>
	state		attending-race-next-column

)	

;****************************************
; We have found a new column and so attend it
(P Attend-Race-Next-Column

=goal>
	state		attending-race-next-column
	right		=old-right
	
?imaginal>
	state		free
	
?visual>
	state		free

=visual-location>
	ISA			visual-location	
	kind		text
	
;****************************************

==>

+imaginal>
	race-group		none
	candidate-group	none
	party-group		none
	;button-group	none

+visual>
	ISA			move-attention
	screen-pos	=visual-location
	
=visual-location>
	ISA			visual-location	
	kind		text
	
=goal>
	state		storing-race-group	
	left		=old-right
	
)

;****************************************
;If there is nothing found when looking for a new column, we are at the bottom right corner of the ballet and there are no more races, so we can end the model
(P Find-Race-Next-Column-No-Match

=goal>
	state			attending-race-next-column

?visual-location>
	buffer			failure
		
==>

=goal>
	state  			end
	
)



;****************************************
; These productions encode the groups of this race
; Make sure not to use any references to the right bound here as it still may reference the last column
;****************************************

; Encodes the race and makes a request for a button associated with this race
; No attend production because productions that lead into this have one
(P Encode-Race

 =goal>
	state   	storing-race-group
	left		=left-bound

=visual>
	value		=text
	
=visual-location>
	ISA			visual-location	
	kind		text
	group		=group1
	screen-y	=screen-y


=imaginal>
	race-group	none

==>

+visual-location>
	ISA			visual-location
	> screen-y	current
	> screen-x	=left-bound
	< screen-x	current
	:nearest	current
	kind		oval
	
=imaginal>
	race-group  =group1
 
=goal>
	state		find-button-group
	race-top	=screen-y
	
!output! ("Example of race is: ~s" =text)



)

; Attends the button 
(P Attend-Button

=goal>
	state		find-button-group

=visual-location>
	ISA			visual-location	
	kind		oval

?visual>
	state		free
	
==>

+visual>
	ISA			move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		storing-button-group

)

; Encodes the button's group and makes a request for a candidate
(P Encode-Button

 =goal>
	state   	storing-button-group
	left		=left-bound
	race-top	=top

=visual>
	
=visual-location>
	ISA			visual-location	
	kind		oval
	group		=group


=imaginal>
	- race-group	none
	race-group		=race-group
	;button-group	none


==>

+visual-location>
	ISA			visual-location
	> screen-x	=left-bound
	> screen-y	=top
	:nearest	current
	- group		=race-group
	kind		text
	
=imaginal>
	;button-group  =group
 
=goal>
	state		find-candidate-group

)

; Attends a candidate for this race
(P Attend-Candidate

=goal>
	state		find-candidate-group

=visual-location>
	ISA			visual-location	
	kind		text

?visual>
	state		free
	
==>

+visual>
	ISA			move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		storing-candidate-group

)

; Encodes the candidate group and makes a visual request for another candidate with greater y so we can eventually find the party
(P Encode-Candidate

 =goal>
	state   	storing-candidate-group
	race-top	=top

	
=visual>
	value		=text

=visual-location>
	ISA			visual-location	
	kind		text
	group		=group2

=imaginal>
	- race-group	none
	;- button-group	none
	candidate-group	none
	race-group		=race-group

==>

+visual-location>
	ISA			visual-location
	kind		text
	group		=group2
	> screen-y	current

=imaginal>
	candidate-group  =group2
 
=goal>
	state		find-party-group-part-1
	
!output! ("Example of candidate is: ~s" =text)

	
)

; Attends a different candidate so we can find a party with smaller y
(P Attend-Different-Candidate

=goal>
	state		find-party-group-part-1

=visual-location>
	ISA			visual-location	
	kind		text

?visual>
	state		free	

	
==>

+visual>
	ISA			move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		find-party-group-part-2
	
)

; Makes the request to finnaly find the party
(P Find-Party

=goal>
	state   	find-party-group-part-2
	left		=left-bound
	race-top			=top
	
=visual>

=imaginal>
	- race-group		none
	;- button-group	none
	- candidate-group	none
	candidate-group		=candidate-group
	race-group			=race-group

==>

+visual-location>
	ISA			visual-location
	kind		text
	> screen-x	current
	< screen-y	current
	> screen-y	=top
	- group		=candidate-group
	- group		=race-group
	:nearest	current
 
=goal>
	state		find-party-group-part-3
	
=imaginal>
	

)

; Attends the party
(P Attend-Party

=goal>
	state		find-party-group-part-3

=visual-location>
	ISA			visual-location	
	kind		text
	width		=width
	screen-x	=middle-x

?visual>
	state		free
	
!bind! =new-right (+ (/ =width 2) =middle-x)

	
==>

+visual>
	ISA			move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		storing-party-group
	right		=new-right

)

; Encodes the party group and sends us into voting
(P Encode-Party

 =goal>
	state   	storing-party-group

=visual>
	value		=text

=visual-location>
	ISA			visual-location	
	kind		text
	group		=group3

=imaginal>
	- race-group		none
	- candidate-group	none
	;- button-group		none
	party-group			none
	

==>

=imaginal>
	party-group  =group3
 
=goal>
	state		ready-to-make-choice
	
!output! ("Example of party is: ~s" =text)
	
)


;****************************************
;****************************************
; Vote on the next race (micronavigation)
;****************************************
;****************************************
; We should be able to paste anything into this section

(P Select-Choice_Locate-Contest-Description

=goal>
	ISA       MakeVote
	state     ready-to-make-choice
	to-do     selectCandidate

=imaginal>
	race-group  =val1
	
?visual-location>
	state     free

==>

=imaginal>

+visual-location>
	ISA       visual-location
	kind      text
	group     =val1

=goal>
	state    found-contest-description
	
!eval! (setf current-strat 'retrieval)

)

;****************************************

(P Select-Choice_Attend-Contest-Description

=goal>
	ISA       MakeVote
	state     found-contest-description
	to-do     SelectCandidate

=visual-location>
	ISA       visual-location
	kind      =text
	group     =val1

?visual>
	state     free

==>

+visual>
	ISA          move-attention
	screen-pos   =visual-location

=goal>
	state        attended-contest-description

)

;****************************************
; the following two productions check if model should abstain from contest
(P check-contest

=goal>
	ISA       MakeVote
	state     attended-contest-description
	to-do     SelectCandidate

=visual>
	ISA       text
	value     =textVal

?retrieval>
	state     free

=imaginal>

==>

+retrieval>
	ISA       Abstain
	contest   =textVal

=imaginal> ;encoding contest in imaginal buffer for a later check if it goes past end state
	ISA       MakeVote
	race      =textVal 

=goal>
	state     checking-contest

)

;****************************************

(P abstain

=goal>
	ISA      MakeVote
	state 	 checking-contest
	to-do    SelectCandidate

=retrieval>
	ISA      Abstain
	contest  =this

==>

; sends to navigation production
=goal>
	ISA 	MakeVote
	state	find-next-race
	to-do	Navigate

)

;****************************************

(P Select-Choice_Encode-Contest-Description

=goal>
	ISA       MakeVote
	state     checking-contest
	to-do     SelectCandidate

?retrieval>
	buffer    failure

=imaginal>
	ISA       MakeVote
	race      =textVal

==>

+retrieval>
	ISA       Candidate
	race      =textval  

=imaginal>

=goal>
	state    encoded-contest-description

!output! ("Contest is: ~s" =textVal)
)


;****************************************
; Successful retrieval of candidate to vote for

(P Initial-Retrieval-Success

=goal> 
	ISA     MakeVote
	state   encoded-contest-description
	to-do   SelectCandidate

=retrieval>
	ISA     Candidate
	race    =r
	name    =n

=imaginal>

==>

=imaginal> ;moving the info in retrieval to imaginal buffer for later
	ISA         MakeVote
	race        =r
	candidate   =n

=goal>
	state	   search-screen-retrieval

!output! ("I'm voting for: ~s" =n)

)

;****************************************

(p Select-Choice_Search-Screen-Ordered_Retrieval

=goal>
	ISA     MakeVote
	state   search-screen-retrieval
	to-do   SelectCandidate

?visual-location>
	state     free

=imaginal>
	candidate-group  =val2

==>

=imaginal>

+visual-location>
	ISA          visual-location
	group        =val2
	kind         text
	> screen-y   current
	screen-y     lowest
	screen-x     lowest

=goal>
	state     something-found-retrieval
  
)

;****************************************

(P Select-Choice_Attend-Search_Retrieval

=goal>
	ISA       MakeVote
	state     something-found-retrieval
	to-do     SelectCandidate

=visual-location>
	ISA       visual-location
	kind      text
	group     =val2

?visual>
	state     free

==>

+visual>
	ISA          move-attention
	screen-pos   =visual-location


=goal>
	state     attending-something-found-retrieval

)

;****************************************

(P Select-Choice_Encode-Search_Retrieval

=goal>
	ISA     MakeVote
	state   attending-something-found-retrieval
	to-do   SelectCandidate

=visual>
	ISA          text
	value        =val
	screen-pos   =pos

?visual-location>
	state        free

==>

=visual>

=goal>
	state	 encoded-search-retrieval

!output! ("Looking at candidate: ~s" =val)

)

;****************************************

(P  Select-Choice_Imaginal-Match-Stop_Retrieval

=goal>
	ISA         MakeVote
	state       encoded-search-retrieval
	to-do       SelectCandidate

=imaginal>
	ISA         MakeVote
	candidate   =val

=visual> 
	ISA           text
	value         =val
	screen-pos    =pos

?visual-location>
	state     free

?manual>
	state     free

==>

=imaginal>

=visual>

+manual>
	ISA     move-cursor
	loc     =pos

=goal>
	state    moved-to-candidate

)

;****************************************
;If the name in the visual location does not match
; candidate saved in imaginal, look for another name 

(P  Select-Choice_No-Match_Retrieval

=goal>
	ISA       MakeVote
	state     encoded-search-retrieval
	to-do     SelectCandidate

?visual-location>
	state	  free

==>

=goal>
	state    search-screen-retrieval

)

;****************************************
;
;Deal with retrieval failure, 
;initiates recognition strategy
;
;****************************************

;****************************************
;Model has read contest description but retrieval of candidate has failed 

(P Retrieval-Fails

=goal>
	ISA 	MakeVote
	state 	encoded-contest-description
	to-do	selectCandidate

?retrieval>
	buffer	failure

==>

=goal>
	state	search-screen-recognition

!output! ("Initial retrieval fails, switch to recog strategy.")
!eval! (setf current-strat 'recognition)

)

;****************************************
; Model has read contest description but retrieval of candidate has failed

(P Retrieval-Fails-after-searching

=goal>
	ISA     MakeVote
	state   encoded-search-retrieval
	to-do   SelectCandidate

?visual-location>
	buffer	failure

==>

=goal>
	state  search-screen-recognition

!output! ("Looked at everything and nothing retrieved--switch to recognition")
!eval! (setf current-strat 'recognition)

)

;****************************************
;Read text in order with no specific name in mind

(p Select-Choice_Search-Screen-Ordered_Recognition

=goal>
	ISA     MakeVote
	state   search-screen-recognition
	to-do   selectCandidate

?retrieval>
	state   free

?visual-location>
	state   free
	
?visual>
	state   free	
	
=imaginal> 
	candidate-group  =val2

==>

=imaginal> 

+visual-location>
	ISA          visual-location
	group        =val2
	kind         text
	> screen-y   current
	screen-y     lowest
	screen-x     lowest

=goal>
	state    something-found-recognition
	
)

;****************************************
;Attend that name

(P Select-Choice_Attend-Search_Recognition

=goal>
	ISA       MakeVote
	state     something-found-recognition
	to-do     SelectCandidate

=visual-location>
	ISA       visual-location
	kind      text
	group     =val2 

?visual>
	state     free

==>

+visual>
	ISA           move-attention
	screen-pos    =visual-location

=goal>
	state     attending-something-found-recognition

)

;****************************************
;Search for that name in memory

(P Select-Choice_Encode-Search_Recognition

=goal>
	ISA       MakeVote
	state     attending-something-found-recognition
	to-do     SelectCandidate

=visual>
	ISA     text
	value   =val

?visual-location>
	state     free

?retrieval>
	state     free

==>

=visual>

+retrieval>
	ISA       candidate 
	name      =val

=goal>
	state     encoded-search-recognition

!output! ("Looking at Candidate: ~s" =val)

)

;****************************************
;See if the name in the visual location matches
;the name in memory

(P  Select-Choice_Match-Stop_Recognition

=goal>
	ISA       MakeVote
	state     encoded-search-recognition
	to-do     SelectCandidate

=retrieval>
	ISA       candidate 
	name      =val

=visual> 
	ISA          text
	value        =val
	screen-pos   =pos

?visual-location>
	state      free

?manual>
	state      free

==>

=retrieval>

=visual>

+manual>
	ISA     move-cursor
	loc     =pos

=goal>
	state   moved-to-candidate
	
!output! ("Match! Voting for: ~s" =val)

)

;****************************************
;Deal with retrieval failure
;If the name in the visual location does not match
;anything in memory, read another name

(P  Select-Choice_No-Match_Recognition

=goal>
	ISA       MakeVote
	state     encoded-search-recognition
	to-do     SelectCandidate

?visual-location>
	state     free

?retrieval>
	buffer      failure

==>

=retrieval>

=goal>
	state      search-screen-recognition

!output! ("Name does not match. Read another.")

)


;****************************************

; Productions that handle recognition failure 
; Switches to voting by party
; VBP = Vote By Party

;****************************************
;; Model has read contest description but recognition of candidate has failed as well
; initiates vote-by-party strategy

(P Recognition-Fails

=goal>
	ISA 	MakeVote
	state 	something-found-recognition
	to-do	selectCandidate

?visual-location>
	buffer	failure

==>

=goal>
	state	search-by-party

!output! ("Looked at everything and nothing recognized-- voting by party")
!eval! (setf current-strat 'party)

)

;****************************************
;; current visual location is at the top of the list (Production fires if initial retrieval fails)
;; looking at party

(P VBP-Search-Screen-Top-Down

=goal>
	ISA     MakeVote
	state   search-by-party
	to-do   SelectCandidate

?retrieval>
	state	free

?visual-location>
	state	free
	
=imaginal>
	party-group  =val3

==>

=imaginal>

+visual-location>
	ISA         visual-location
	kind        text
	group       =val3
	:attended    nil
	screen-y     lowest
  
=goal>
	state    vbp-something-found
  
)

; same production that fires if current visual location is at the bottom of the list
; this happens if it initial retrieval succeeds, but fails during search

(P VBP-Search-Screen-Bottom-Up

=goal>
	ISA     MakeVote
	state   search-by-party-at-bottom
	to-do   SelectCandidate

?retrieval>
	state	free

?visual-location>
	state	free

=imaginal>
	party-group  =val3

==>

=imaginal>

+visual-location>
	ISA         visual-location
	kind        text
	group       =val3
	< screen-y  current
 	screen-y    highest
 	screen-x    lowest

=goal>
	state    vbp-something-found
  
)

;****************************************
; looks at party
(P VBP-Select-Choice_Attend-Search

=goal>
	ISA       MakeVote
	state     vbp-something-found
	to-do     SelectCandidate

=visual-location>
	ISA       visual-location
	kind      text
	group     =val3

?visual>
	state     free

==>

+visual>
	ISA          move-attention
	screen-pos   =visual-location

=goal>
	state     vbp-attending-something-found

)

;****************************************
(P VBP-Encode-Search-Match

=goal>
	ISA      MakeVote
	state    vbp-attending-something-found
	to-do    SelectCandidate
	default  =party

=visual> 
	ISA         text
	value       =party
	screen-pos  =pos

?manual>
	state	 free

==>

+manual>
	ISA     move-cursor
	loc     =pos

=goal>
	state   moved-to-candidate

!output! ("Party matches: ~s" =party)

)

;****************************************
;; if visual item doesn't match what we're looking at, then search again
(P VBP-No-Match-Next-Choice

=goal>
	ISA      MakeVote
	state    vbp-attending-something-found
	to-do    SelectCandidate
	default  =party

=visual>
	ISA      text
	- value  =party
	value    =notparty

==>

=goal>
	state	search-by-party

!output! ("Party ~s does not match, search again." =notparty)

)

;****************************************
; if we reach bottom and nothing has been retrieved then vote by party

(P VBP-bottom-of-list-fail

=goal>
	ISA     MakeVote
	state   vbp-something-found
	to-do   SelectCandidate

?visual-location>
	buffer	failure

==>

=goal>
	state	start-voting
	to-do	Navigate

)





;****************************************
;****************************************
; Click on the bubble
;****************************************
;****************************************

; Finds the bubble closest to the currently attended location
; Does not use the todo field (navigate) (include in things to change when pasting in)
(P find-bubble

=goal>
	state		moved-to-candidate
	left		=left-bound
	right		=right-bound
	
==>

+visual-location>
	ISA			visual-location
	kind		oval
	> screen-x	=left-bound
	< screen-x	=right-bound
	:nearest	current

=goal>
	state		attending-bubble

)


; Clicks on the bubble
(P move-mouse-to-bubble

=goal>
	state		attending-bubble
	
?manual>
	state		free
	
=visual-location>
	ISA			visual-location
	kind		oval
	
==>

+manual>
	ISA     move-cursor
	loc     =visual-location


=goal>
	state	moving-mouse-to-bubble
	

)

(P click-bubble

=goal>
	state     moving-mouse-to-bubble

?manual>
	state     free

==>

+manual>
	ISA     click-mouse

=goal>
	state 	find-next-race

)


;****************************************
;****************************************

;Production Parameters


;****************************************
;Selection
;****************************************

(spp Select-Choice_Imaginal-Match-Stop_Retrieval :u 1000)
(spp Select-Choice_Search-Screen-Ordered_Retrieval :u 8)
(spp Select-Choice_Search-Screen-Ordered_Recognition :u 8)

(goal-focus Vote)

)



