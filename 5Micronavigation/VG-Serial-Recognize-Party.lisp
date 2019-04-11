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
	!eval! (log-candidate nil nil)	

	

)

;****************************************
; Production that recognizes if model has gone past "CountyJudge"

(P Past-End-State

=goal>
	ISA         MakeVote
	state       attended-contest-description
	
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

(P  Select-Choice_No-Match

=goal>
	ISA       MakeVote
	state     encoded-contest
	

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
	state   moved-to-candidate

!output! ("Matches party ~s" =party)

)




;****************************************
; if the name in the visual location doesn't match the default party from memory
; search again

(P Vote-by-Party_No-Match

=goal>
	ISA      MakeVote
	state    party-found
	
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
	
	
;already attended everything
?visual-location> 
	buffer	failure

==>

=goal>
	state	find-next-race
	!eval! (log-candidate nil nil)	

	

!output! ("Reached the end of the search and nothing matches-- Abstain")

)

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


