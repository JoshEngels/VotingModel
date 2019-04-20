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
;;; Filename    : VG-Random-Recognize-Party.lisp
;;; Version     : r1
;;; 
;;; Description : Model of voting on virtual VoteBox DRE.
;;;             : * Uses a recognition-based memory strategy
;;;             : * Uses random visual search strategy.
;;;             : * Retrieval fails randomly if name is under the activation threshold.
;;;             : * If retrieval fails, performs a random search by party
;;; 
;;; Bugs       
;;;             : * None known
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
;;; 2018.4.24   Xianni Wang
;;;				: * added Clear-Finsts production
;;; 2018.4.14   Xianni Wang
;;;				: * adjusted the format
;;; 2018.4.4	Xianni Wang
;;;				: * adapted file with visual grouping learning model
;;; 2017.4.27	Marita Sailor
;;;				: * model not properly handling motor noise when tested with run-multiple
;;;				: * I think I forgot to update this one when I got the others to work
;;;				: * replaced the old navigation productions with the new ones
;;; 2017.4.13	Marita Sailor
;;;				: * duplicated file from Off-by-one folder
;;;				: * replaced Candidate chunks in DM
;;;				: * moved abstain chunks from this file
;;; 2017.4.12	Marita Sailor
;;;				: * added sgp process-cursor
;;;				: * added find-cursor production, modified Advance-screen-click-advance
;;; 2017.4.3	Marita Sailor
;;;				: * duplicated file from Intentional Abstention folder
;;;				: * added cursor noise
;;; 2017.2.19	Marita Sailor
;;;				: * duplicated file from other folder
;;;				: * created new chunk type "Abstain" 
;;;				: * made 3 abstain chunks for testing
;;;				: * wrote new production "check-contest" 
;;;				: * slightly modified encode-contest description production
;;;				: * set production parameter for check-contest to avoid skipping
;;;				: * set abstention chunk activation levels
;;; 2016.11.22	Marita Sailor
;;;				: * added base-level activations for all chunks
;;; 2016.11.14	Marita Sailor
;;;				: * model check-up
;;; 2016.11.12	Marita Sailor
;;;				: * fixed issues production Past-End-State (now works every time) 
;;; 2016.11.04	Marita Sailor 
;;;				: * default party stored in MakeVote chunk
;;;				: * other fixes (when to stop, adjusted VBP productions )
;;;				: * added (sgp :visual-finst-span 10) 
;;; 2016.11.03	Marita Sailor 
;;;				: * replaced serial search with random search
;;; 2016.10.26	Marita Sailor 
;;;				: * Created file, duplicated from Recog-Serial-Party and renamed
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;; As noted above, this model searches for candidates using a random search strategy.
;;; Retrieval will fail randomly and if this happens the model will select a candidate
;;; based on party affiliation.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;****************************************
;****************************************
;start to vote
;****************************************
;****************************************

;****************************************
;Put the visual location somewhere on the screen
(P Select-Choice_Locate-Contest-Description

=goal>
	ISA    	  MakeVote
	state     ready-to-make-choice
	

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
	state     found-contest-description
	
!eval! (setf current-strat 'recognition)

)

;****************************************
;Attend some visual location
(P Select-Choice_Attend-Contest-Description

=goal>
	ISA       MakeVote
	state     found-contest-description
	

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
	

=retrieval>
	ISA      Abstain
	contest  =race

==>

; sends to navigation production
=goal>
	ISA 	MakeVote
	state	find-next-race
	


)


;****************************************
;Random search

(P Select-Choice_Search-Screen-Fastest

=goal>
	ISA       MakeVote
	state     encoded-contest-description
	

?retrieval>
	buffer     failure
	
=imaginal> 
	candidate-group  =val2

==>

=imaginal>
 
+visual-location>
	ISA          visual-location
	kind         text
	:attended    nil
	group        =val2


=goal>
	state     something-found

)

;****************************************
;Attend that name
(P Select-Choice_Attend-Search

=goal>
	ISA       MakeVote
	state     something-found
	

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
	state     attending-something-found

)

;****************************************
;Search for that name in memory
(P Select-Choice_Encode-Search

=goal>
	ISA       MakeVote
	state     attending-something-found
	

=visual>
	ISA         text
	value       =val

?visual-location>
	state       free

?retrieval>
	state     free

==>

=visual>

+retrieval>
	ISA       candidate 
	name      =val

=goal>
	state     encoded-contest

!output! ("Looking at ~s" =val)

)

;****************************************
;See if the name in the visual location matches
;any name in memory
(P  Select-Choice_Match-Stop

=goal>
	ISA       MakeVote
	state     encoded-contest
	

=retrieval>
	ISA       candidate 
	name      =val

=visual> 
	ISA         text
	value       =val
	screen-pos  =pos

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
;anything in memory, find another name

(P  Select-Choice_No-Match

=goal>
	ISA       MakeVote
	state     encoded-contest
	

?retrieval>
	buffer      failure

==>

=retrieval>

=goal>
	state      encoded-contest-description

!output! ("Retrieval Fail, redo search")

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
	

?visual-location>
	buffer      failure

==>

=visual-location>

=goal>
	state      read-by-party

!output! ("Visual Buffer failure, voting by party")
!eval! (setf current-strat 'party)

)

;****************************************
; randomly searches the screen to find a candidate's party
(P Vote-by-Party_Search-Screen-Random

=goal>
	ISA       MakeVote
	state     read-by-party
	

?retrieval>
	state     free

?visual-location>
	state     free

=imaginal>
	party-group  =val3

==>

=imaginal>
 
+visual-location>
	ISA         visual-location
	kind        text
	group       =val3
	:attended   nil

=goal>
	state     vbp-attend-name

)

;****************************************
; something found, moves attention to that location

(P Vote-by-Party_Attend-Location

=goal>
	ISA     MakeVote
	state   vbp-attend-name
	

=visual-location>
	ISA     visual-location
	kind    text
	group   =val3

?visual>
	state	free

==> 

+visual>
	ISA            move-attention
	screen-pos     =visual-location

=goal>
	state	party-found

)

;****************************************
; a name was found and matches with the default party

(P Vote-by-Party_Match

=goal>
	ISA      MakeVote
	state    party-found
	
	default  =party

=visual>
	ISA 	    text
	value	    =party
	screen-pos  =pos

?manual>
	state	  free

==>

=visual>

+manual>
	ISA     move-cursor
	loc     =pos

=goal>
	state	moved-to-candidate

!output! ("Party found: ~s" =party)

)

;****************************************
; production fires if default party does not match
; returns goal state to search again

(P Vote-by-Party_No-Match

=goal>
	ISA      MakeVote
	state    party-found
	
	default	 =party

=visual>
	ISA      text
	- value  =party
	value    =notparty

==>

=goal>
	state	read-by-party

!output! ("Party found does not match default: ~s" =notparty)

)


;****************************************
; if the model has looked at the entire list and cannot recall default party 
; abstain

(P Vote-by-Party_No-Match-ABSTAIN

=goal>
	ISA     MakeVote
	state   vbp-attend-name
	

?visual-location>
	buffer	failure

==>

=goal>
	state	find-next-race
	

	

!output! ("Reached the end of the search and nothing matches-- Abstain")
)


;Production Parameters
(spp Select-Choice_Search-Screen-Fastest :u 8)
(spp check-contest :u 1000)



