;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Mike Byrne [and others]
;;; Copyright   : (c) 2016 Mike Byrne
;;; Address     : Department of Psychology 
;;;             : Rice University
;;;             : Houston, TX 77005
;;;             : byrne@rice.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : VG-Serial-Recognize-Party.lisp
;;; Version     : r1
;;; 
;;; Description : Model of voting on virtual VoteBox DRE.
;;;             : * Uses a recognition-based memory strategy
;;;             : * Uses serial visual search strategy.
;;;             : * Retrieval fails randomly if name is under the activation threshold.
;;;             : * If retrieval fails, performs a serial search by party
;;; 
;;; Bugs        : * None known
;;;
;;; To do       : * 
;;; 
;;; ----- History -----
;;; 2019.1.31   Xianni Wang
;;;				: * updated screen learning code
;;;				: * removed clear-finsts production
;;; 2018.9.5    Xianni Wang
;;;				: * added defparameter and !eval! functions to log strategies for simulation
;;; 2018.5.19   Xianni Wang
;;;				: * added two more activation levels 
;;;				: * added candidate chunks and abstain chunks
;;; 2018.4.25   Xianni Wang
;;;				: * removed unnecessary imaginal buffers
;;;				: * added Clear-Finsts production
;;; 2018.4.14   Xianni Wang
;;;				: * adjusted the format
;;; 2018.4.1    Xianni Wang
;;;				: * adapted file with visual grouping learning model
;;; 2017.04.20  Marita Sailor
;;;				: * duplicated file from Off-by-one folder
;;;				: * replaced Candidate chunks in DM
;;;				: * moved abstain chunks from this file
;;; 2017.04.12  Marita Sailor
;;;				: * duplicated file from Intentional Abstention folder
;;;				: * added cursor-noise and process-cursor
;;;				: * adjust end goal state of P Advance-Screen_Imaginal-Match-Stop
;;;				: * find-cursor and find-cursor-2 chunks
;;;				: * new productions: find-cursor-location, find-cursor-incorrect
;;;				: * tested and works
;;; 2017.03.18  Marita Sailor
;;;				: * duplicated file from other folder
;;;				: * made similar changes that were made in Recog-Random-Party
;;; 2016.11.22  Marita Sailor
;;;				: * added base-level activations for all chunks
;;; 2016.11.14  Marita Sailor
;;;				: * model check-up
;;; 2016.10.18  Marita Sailor
;;;				: * fixed End-Here production
;;;				: * Model will now end at CountyJudge
;;; 2016.10.12  Marita Sailor
;;;				: * added End-Here production, but never fires
;;; 2016.10.11  Marita Sailor
;;;				: * Figured out problems: the model fails on the last race when it abstains or votes by race
;;;				: * continuously loops because it can't see "next page" (on the very last page)
;;;				: * Halt! production wont ever fire because model hasn't retrieved a name
;;; 2016.10.04  Marita Sailor 
;;;				: * Model works! If initial candidate retrieval fails, model performs a
;;;				: random search by party and checks if it matches 
;;;				: the default party in DM. If default party retrieval fails, then 
;;;				: it abstains from the race. 
;;; 2016.10.03  Marita Sailor 
;;;				: * Works better now. Implemented default party but still having issues.
;;; 2016.09.26  Marita Sailor 
;;;				: * More edits, control flow should not skip Encode-Race
;;; 2016.09.23  Marita Sailor 
;;;				: * Added productions to handle infinite loop and voting by party (still unfinished)
;;; 2016.09.22  Marita Sailor 
;;;				: * Added header
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; As noted above, this model searches for candidates by working down the list.
;;; Retrieval will fail randomly and if this happens, the model will select a candidate
;;; based on party affiliation.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Supporting Lisp code to run the model

(defvar *vbw* nil)

(defun do-experiment (&optional wind)
  (cond (wind 
          (setf *vbw* wind))
        (t
         (when *vbw*
           (window-close *vbw*))
         (setf *vbw* (make-instance 'votebox-window :contest-lst cntst-lst 
                                    :out-path (choose-new-file-dialog 
                                                :prompt "Save data in:")))))
  (reset)
  (install-device *vbw*)
  (start-hand-at-mouse)
  (proc-display)
  (run 200 :real-time nil))

(defparameter current-strat nil)
(defparameter final-strats '())

(clear-all)
(define-model BryansVBModel

(sgp :v t :trace-detail high :needs-mouse t :show-focus t :esc t)
(sgp :ul t :egs 2)
(sgp :cursor-noise t)
(sgp :process-cursor t)

; Enable forgetting / Activation 
(sgp :ans 0.3)
(sgp :rt 0)

(sgp :act t)

;(sgp :show-focus t 
;     :visual-num-finsts 100 
;     :visual-finst-span 100)


(setf *actr-enabled-p* t)

;set parameters (visual location)
(setf vg-glomming-radius 25)
(setf vg-collision-type 'box) ;'point is faster, but less plausible (also needs a larger radius to work similarly)
(setf vg-naming-type 'sequential) 

(chunk-type MakeVote race candidate button position screen state handpos to-do found default endState)
(chunk-type Candidate name party race)
(chunk-type VoteParty default)
(chunk-type Abstain contest)
(chunk-type VisualGroup race-group candidate-group party-group nextpage-group nextpage-text party-text candidate-text race-text state)


(add-dm

(Vote ISA MakeVote state ready-to-study to-do SelectCandidate default "Dem" endState "nameofrace") ;the end state is hardcoded
;(Abstain ISA MakeVote state ready-to-advance to-do Navigate endState "countyjudge") ;the end state is hardcoded
; chunk below will stop the model if it proceeds past the countyjudge race
;(TooFar	ISA MakeVote state attended-contest-description to-do SelectCandidate endState "Nameofrace")


;****************************************
;****************************************

;Visual Grouping Chunks

;****************************************
;****************************************

(ready-to-study ISA chunk)
(checking-buffer ISA chunk)
(attending-group ISA chunk)
(looking-for-group ISA chunk)
(looking-for-lr ISA chunk)
(storing-group ISA chunk)
(none ISA chunk)

;****************************************
;****************************************

;Navigation Chunks

;****************************************
;****************************************

(ready-to-advance ISA chunk)
(attending-something-found ISA chunk)
(something-found ISA chunk)
(find-party ISA chunk)
(waiting-for-click ISA chunk)
(start ISA chunk)
(moved-mouse ISA chunk)
;(not-first ISA chunk)
(located-mouse-back ISA chunk)
(ready-to-navigate ISA chunk)
(navigate ISA chunk)
(find-cursor ISA chunk)
(find-cursor-2 ISA chunk)
(cursor ISA chunk)

;****************************************
;****************************************

;Selection Chunks

;****************************************
;****************************************

(stop ISA chunk)
(selectcandidate ISA chunk)
(found-contest-description ISA chunk)
(ready-to-make-choice ISA chunk)
(ready-to-start ISA chunk)
(attended-contest-description ISA chunk)
(encoded-contest-description ISA chunk)
(vbp-locate-contest)
(vbp-attend-name ISA chunk)
(vbp-attended-contest-description ISA chunk)
(read-by-party ISA chunk)
(party-found ISA chunk)
(vbp-moved-to-candidate ISA chunk)
(Rep ISA chunk)
(Dem ISA chunk)
(Ind ISA Chunk)
(PresidentoftheUnitedStates ISA chunk)
(moved-to-candidate ISA chunk)
(clicked-candidate ISA chunk)
(moved-cursor-to-button ISA chunk)
(ready-to-search ISA chunk)
(ready-to-attend ISA chunk)
(ready-to-encode ISA chunk)
)

;****************************************
;****************************************

;Candidate Chunks and Abstain Chunks

;****************************************
;****************************************

(defvar *dm-path* *load-truename*)
(case *which-dm*
  (all-perfect (load (merge-pathnames "all-candidates-perfect.lisp" *dm-path*)))
  (all-rolloff (load (merge-pathnames "all-with-rolloff.lisp" *dm-path*)))
  (most-rolloff (load (merge-pathnames "most-with-rolloff.lisp" *dm-path*)))
  (most-perfect (load (merge-pathnames "most-with-abstention.lisp" *dm-path*)))
  (full-dm (load (merge-pathnames "full-dm.lisp" *dm-path*)))
)

(defvar *dm-path* *load-truename*)
(case *which-dm*
  (all-perfect2 (load (merge-pathnames "all-candidates-perfect-2.lisp" *dm-path*)))
  (all-rolloff2 (load (merge-pathnames "all-with-rolloff-2.lisp" *dm-path*)))
  (most-rolloff2 (load (merge-pathnames "most-with-rolloff-2.lisp" *dm-path*)))
  (most-perfect2 (load (merge-pathnames "most-with-abstention-2.lisp" *dm-path*)))
  (full-dm2 (load (merge-pathnames "full-dm-2.lisp" *dm-path*)))
)

(defvar *dm-path* *load-truename*)
(case *which-dm*
  (all-perfect3 (load (merge-pathnames "all-candidates-perfect-3.lisp" *dm-path*)))
  (all-rolloff3 (load (merge-pathnames "all-with-rolloff-3.lisp" *dm-path*)))
  (most-rolloff3 (load (merge-pathnames "most-with-rolloff-3.lisp" *dm-path*)))
  (most-perfect3 (load (merge-pathnames "most-with-abstention-3.lisp" *dm-path*)))
  (full-dm3 (load (merge-pathnames "full-dm-3.lisp" *dm-path*)))
)

;****************************************
;****************************************
;start to learn
;****************************************
;****************************************
;****************************************
;visual grouping - creating imaginal buffer for learning
(P Create-Imaginal-Buffer

=goal>
	ISA    VisualGroup
	state  ready-to-study

?imaginal>
	state  free
	
==>

+imaginal>
	nextpage-group   none
	party-group      none
	candidate-group  none
	race-group       none
	nextpage-text    "nextpage"
	party-text       "dem"
	candidate-text   "gordonbearce"
	race-text        "presidentoftheunitedstates"

=goal>
	state  looking-for-group

)

;****************************************
;visual grouping - put the visual location on the top-left corner

(P Find-Location-group

=goal>
	ISA     VisualGroup
	state   looking-for-group

=imaginal>
	nextpage-group    =val1
	race-group        =val2
	candidate-group   =val3
	party-group       =val4
	
!eval! (or (equal 'none =val1) (equal 'none =val2) (equal 'none =val3) (equal 'none =val4))

==>

=imaginal>

+visual-location>
	ISA          visual-location
	kind         text
	screen-y     lowest
	screen-x     lowest
	> screen-y   current
	
=goal>
	state  attending-group

)

;****************************************
;visual grouping - put the visual location to the next left-right position

(P Find-Location-lr

=goal>
	ISA     VisualGroup
	state   looking-for-lr

?visual-location>
	state    free
   
==>

+visual-location>
	ISA          visual-location
	kind         text
	> screen-x   current
	screen-x     lowest
	screen-y     current	
	
=goal>
	state  attending-group

)

;****************************************

(P Find-Location-nomatch

=goal>
	ISA     VisualGroup
	state   attending-group

?visual-location>
	buffer  failure

==>

=goal>
	state  looking-for-group

)

;****************************************
;visual grouping - Attend some visual location

(P Attend-Visual-Location-Group

=goal>
	ISA    VisualGroup
	state  attending-group

=visual-location>
	ISA     visual-location
	kind    =text

?visual>
	state   free

==>

=visual-location>

+visual>
	ISA          move-attention
	screen-pos   =visual-location

=goal>
	state  storing-group

)

;****************************************
;visual grouping - encode race group information 

(P Encode-Race-Group

=goal>
	ISA     VisualGroup
	state   storing-group

=visual>
	ISA     text
	value   =race

=visual-location>
	ISA     visual-location
	group   =group1

=imaginal>
;	race-group  none
	race-text   =race

==>

=visual-location>

=imaginal>
	race-group  =group1
 
=goal>
	state  looking-for-lr

!output! ("race group stored: ~s" =group1)

)


;****************************************
;visual grouping - encode party group information 

(P Encode-Party-Group

=goal>
	ISA    VisualGroup
	state  storing-group

=visual>
	ISA     text
	value   =party

=visual-location>
	ISA     visual-location
	group   =group2

=imaginal>
;	party-group  none
	party-text   =party

==>

=visual-location>

=imaginal>
	party-group  =group2
 
=goal>
	state  looking-for-lr

!output! ("party group stored: ~s" =group2)

)

;****************************************
;visual grouping - encode candidate group information 

(P Encode-Candidate-Group

=goal>
	ISA    VisualGroup
	state  storing-group

=visual>
	ISA     text
	value   =candidate

=visual-location>
	ISA     visual-location
	group   =group3

=imaginal>
;	candidate-group  none
	candidate-text   =candidate

==>

=visual-location>

=imaginal>
	candidate-group  =group3
 
=goal>
	state  looking-for-lr

!output! ("candidate group stored: ~s" =group3)

)

;****************************************
;visual grouping - encode nextpage group information 

(P Encode-Nextpage-Group

=goal>
	ISA     VisualGroup
	state   storing-group

=visual>
	ISA     text
	value   =next

=visual-location>
	ISA     visual-location
	group   =group4

=imaginal>
;	nextpage-group  none
	nextpage-text   =next

==>

=visual-location>

=imaginal>
	nextpage-group  =group4
 
=goal>
	state  looking-for-lr

!output! ("nextpage group stored: ~s" =group4)

)


;****************************************
(P Search-Group-No-Match

=goal>
	ISA         VisualGroup
	state       storing-group

=visual>
	ISA          text
	- value      =next
	- value      =party
	- value      =candidate
	- value      =race

=imaginal>
	nextpage-text    =next
	party-text       =party
	candidate-text   =candidate
	race-text        =race

==>

=visual>

=imaginal>

=goal>
	state	looking-for-lr

)

;****************************************
(P Check-Imaginal-Buffer-Full

=goal>
	ISA       VisualGroup
	state     looking-for-group

=imaginal>
	- race-group       none
	- nextpage-group   none
	- candidate-group  none
	- party-group      none

==>

=imaginal>  

=goal>
	state     ready-to-make-choice

)


;****************************************
;****************************************
;start to vote
;****************************************
;****************************************

;****************************************


;****************************************
;Put the visual location somewhere on the screen

(P Select-Choice_Locate-Contest-Description

=goal>
	ISA       MakeVote
	state     ready-to-make-choice
	to-do     selectCandidate

=imaginal>
	race-group 	 =val1
	
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

!eval! (setf current-strat 'recognition)

)


;****************************************
;Attend some visual location
(P Select-Choice_Attend-Contest-Description

=goal>
	ISA       MakeVote
	state     found-contest-description
	to-do     SelectCandidate

=visual-location>
	ISA       visual-location
	kind      text
	group     =val1

?visual>
	state     free

==>

+visual>
	ISA           move-attention
	screen-pos    =visual-location

=goal>
	state     attended-contest-description

)

;****************************************
(P encode-contest-description

=goal>
	ISA     MakeVote
	state   attended-contest-description
	to-do   SelectCandidate

=visual>
	ISA  	text
	value   =textVal

==>

+retrieval>
	ISA       Abstain
	contest   =textVal

=goal>
	state	encoded-contest-description

!output! ("Contest is: ~s" =textVal)

)


;****************************************
; Production that fires only if contest is one to abstain from

(P check-contest

=goal>
	ISA 	MakeVote
	state	encoded-contest-description
	to-do	SelectCandidate

=retrieval>
	ISA      Abstain
	contest  =race

==>

; sends to navigation production
=goal>
	ISA 	MakeVote
	state	ready-to-advance
	to-do	Navigate

)

;****************************************
; Production that recognizes if model has gone past "CountyJudge"

(P Past-End-State

=goal>
	ISA         MakeVote
	state       attended-contest-description
	to-do       SelectCandidate
	endState    =end

=visual>
	ISA      text
	value    =end	

==>

+goal>
	ISA     clear

)

;****************************************
;Read text in order with no specific name in mind
(p Select-Choice_Search-Screen-Ordered

=goal>
	ISA     MakeVote
	state   encoded-contest-description
	to-do   selectCandidate

?retrieval>
	buffer	failure
	
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
	state    something-found
	
)


;****************************************
;Attend that name
(P Select-Choice_Attend-Search

=goal>
	ISA       MakeVote
	state     something-found
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
	state     attending-something-found

)

;****************************************
;Search for that name in memory
(P Select-Choice_Encode-Search

=goal>
	ISA       MakeVote
	state     attending-something-found
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
	state     encoded-contest

!output! ("Looking at Candidate: ~s" =val)

)

;****************************************
;See if the name in the visual location matches
;any name in memory
(P  Select-Choice_Match-Stop

=goal>
	ISA       MakeVote
	state     encoded-contest
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

(P Select-Choice_Click-Candidate

=goal>
	ISA       MakeVote
	state     moved-to-candidate
	to-do     SelectCandidate

=retrieval>
	ISA     candidate 
	name	=val

=visual>
	ISA      text
	value    =val	


?manual>
	state     free

==>

=retrieval>

+manual>
	ISA     click-mouse

=goal>
	state     ready-to-advance
	to-do     Navigate 

)

;****************************************
;Deal with retrieval failure
;If the name in the visual location does not match
;anything in memory, read another name

(P  Select-Choice_No-Match

=goal>
	ISA       MakeVote
	state     encoded-contest
	to-do     SelectCandidate

?visual-location>
	state     free

?retrieval>
	buffer      failure


==>

=retrieval>

=goal>
	state      encoded-contest-description

!output! ("Name does not match. Read another.")

)

;****************************************
;Deal with retrieval failure
;If the name in the visual location does not match
;anything in memory and it is at the bottom of the 
;name list, vote by party
;****************************************

(P  Select-Choice_No-Match-VoteByParty

=goal>
	ISA     MakeVote
	state   something-found
	to-do   selectCandidate

?visual-location>
	buffer    failure

==>

=visual-location>

=goal>
	state      read-by-party

!output! ("Bottom of list and nothing matches. Voting by party.")
!eval! (setf current-strat 'party)

)


;****************************************
; serially searches the screen to find a candidate's party


(P VBP-Select-Choice_Search-Screen-Ordered

=goal>
	ISA	    MakeVote
	state   read-by-party

?retrieval>
	state   free

?visual-location>
	state   free
	
?visual>
	state   free

=imaginal>
	party-group  =val3

==>

=imaginal>

+visual-location>
	ISA          visual-location
	group        =val3
	kind         text
	:attended    nil
	screen-y     lowest

=goal>
	state    vbp-attend-name
  
)
;****************************************
; something found, moves attention to that location

(P VBP-Select-Choice_Attend-Search

=goal>
	ISA     MakeVote
	state   vbp-attend-name
	to-do   selectCandidate

=visual-location>
	ISA     visual-location
	kind    text
	group   =val3

?visual>
	state	free

==> 

+visual>
	ISA          move-attention
	screen-pos   =visual-location

=goal>
	state	party-found

)

;****************************************
; a name was found and now we need to check if it matches with the default party

(P VBP-Encode-Search-Match

=goal>
	ISA         MakeVote
	state       party-found
	to-do       selectCandidate
	default     =party

=visual>
	ISA         text
	value       =party
	screen-pos  =pos

?manual>
	state	free

==>

=visual>

+manual>
	ISA     move-cursor
	loc     =pos

=goal>
	state   vbp-moved-to-candidate

!output! ("Matches party ~s" =party)

)


;****************************************
(P Vote-by_Party-Select-Choice

=goal>
	ISA     MakeVote	
	state   vbp-moved-to-candidate
	to-do   selectCandidate

?manual>
	state	free

==>

+manual>
	ISA     click-mouse

=goal>
	state     ready-to-advance
	to-do     Navigate 

)


;****************************************
; if the name in the visual location doesn't match the default party from memory
; search again

(P Vote-by-Party_No-Match

=goal>
	ISA      MakeVote
	state    party-found
	to-do    selectCandidate
	default  =party

=visual>
	ISA      text
	- value  =party
	value    =notparty
	
==>

=goal>
	state	read-by-party

!output! ("Party ~s does not match default" =notparty)
!output! ("Default party is: ~s" =party)

)


;****************************************
; if the model has looked at the entire list and cannot recall default party 
; abstain

(P Vote-by-Party_No-Match-ABSTAIN

=goal>
	ISA      MakeVote
	state    vbp-attend-name
	to-do    selectCandidate
	
;already attended everything
?visual-location> 
	buffer	failure

==>

=goal>
	state	ready-to-advance
	to-do	Navigate

!output! ("Reached the end of the search and nothing matches-- Abstain")

)

;****************************************
;****************************************

;Navigation

;****************************************
;****************************************

(P Advance-Screen_Search-Screen

=goal>
	ISA    	  MakeVote
	state     ready-to-advance
	to-do     Navigate

?manual>
	state     free

=imaginal>
	nextpage-group  =val4

==>

=imaginal>

+visual-location>
	ISA        visual-location
	group      =val4
	kind       text
	screen-y   highest

=goal>
	state     something-found

)


;****************************************

(P Advance-Screen_Attend-Search

=goal>
	ISA       MakeVote
	state     something-found
	to-do     Navigate

=visual-location>
	ISA       visual-location
	kind      text
	group     =val4

?visual>
	state     free

==>

+visual>
	ISA          move-attention
	screen-pos   =visual-location

=goal>
	state     attending-something-found

)

;****************************************

(P Advance-Screen_Encode-Search

=goal>
	ISA         MakeVote
	state       attending-something-found
	to-do       Navigate

=visual>
	ISA         text
	value       =val
	screen-pos  =pos

==>

=visual>

=goal>
	state     ready-to-advance

)

;****************************************

(P Advance-Screen_Imaginal-Match-Stop

=goal>
	ISA         MakeVote
	state       ready-to-advance
	to-do       Navigate

=imaginal>
	nextpage-group  =grp

=visual>
	ISA          text
	value       "NextPage"
	screen-pos   =pos

?manual>
	state      free

==>

=visual>

=imaginal>

+manual>
	ISA        move-cursor
	loc        =pos

+visual-location>
	ISA        visual-location
	group      =grp
	kind       oval
	:nearest   current

=goal>
	state     find-cursor

)

;****************************************
(P find-cursor-location

=goal>
	ISA       MakeVote
	state     find-cursor
	to-do     Navigate

=visual-location>
	ISA        visual-location
	group      =grp
	kind       oval
	screen-x   =x
	screen-y   =y
	width      =w
	height     =h

!bind! =l (- =x (/ =w 2))
!bind! =r (+ =x (/ =w 2))
!bind! =t (- =y (/ =h 2))
!bind! =b (+ =y (/ =h 2))

?manual>
	state free

==>

; search for cursor location
+visual-location>
	ISA         visual-location
	kind        cursor
	> screen-x  =l
	< screen-x  =r
	> screen-y  =t
	< screen-y  =b
	
=goal>
	ISA       MakeVote
	state     find-cursor-2
	to-do     Navigate

;!output! (=l =r =t =b)

)

;****************************************
(P find-cursor-correct

=goal>
	ISA       MakeVote
	state     find-cursor-2
	to-do     Navigate

=visual-location>
	ISA      visual-location
	kind     cursor

==>

=goal>
	state     moved-cursor-to-button
	
)

;****************************************

(P find-cursor-incorrect

=goal>
	ISA      MakeVote
	state    find-cursor-2
	to-do    Navigate

?visual-location>
	buffer    failure

==>

=goal>
	ISA 	MakeVote
	state	ready-to-advance ;searches again for button        
	to-do 	Navigate

)

;****************************************
;; found cursor in proper location, ready to advance
(P Advance-Screen_Click-Advance

=goal>
	ISA       MakeVote
	state     moved-cursor-to-button
	to-do     Navigate

?manual>
	state     free

?visual>
	state     free

==>

+manual>
	ISA     click-mouse

+visual>
	ISA     clear

=goal>
	state    waiting-for-click
	to-do    Navigate

-retrieval> 

)

;****************************************

(P Advance-Screen_wait-for-click

=goal>
	ISA     MakeVote
	state   waiting-for-click
	to-do   Navigate

?manual>
	state   free

==> 

=goal>
	state     ready-to-make-choice
	to-do     selectCandidate
	
!eval! (setf final-strats (append final-strats (list current-strat)))

)

;****************************************

(P Halt!

=goal>
	ISA       MakeVote
	state     moved-cursor-to-button
	to-do     Navigate
	endState  =end

=retrieval>
	ISA       candidate
	race      =end

==>

+goal>
	ISA     clear

)

;****************************************

;****************************************

;Production Parameters

;****************************************
(spp Past-End-State :at 0 :u 4000)
(spp Halt! :at 0 :u 4000)

;****************************************
;Selection
;****************************************

(spp Select-Choice_Match-Stop :u 1000)
(spp Select-Choice_Search-Screen-Ordered :u 1000)
(spp check-contest :u 1000)

;****************************************
;Navigation
;****************************************

(spp Advance-Screen_Imaginal-Match-Stop :u 1000)
(spp Advance-Screen_Search-Screen :u 8)

;(spp Advance-Screen_Move-Back :at .001)

;****************************************

(goal-focus Vote)

)
