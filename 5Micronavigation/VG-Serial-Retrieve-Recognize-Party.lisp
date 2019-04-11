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
;;; Filename    : VG-Serial-Retreive-Recognize-Party.lisp
;;; Version     : r1
;;; 
;;; Description : Model of voting on virtual VoteBox DRE.
;;;             : * Uses a retrieval-based serial strategy to search for candidates by name.
;;;             : * Model abstains if it reaches the bottom and no name has matched
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
;;; 2018.4.4    Xianni Wang
;;;				: * adapted file with visual grouping learning model
;;; 2017.4.20   Marita Sailor
;;;				: * duplicated file from Off-by-one folder
;;;				: * replaced Candidate chunks in DM
;;;				: * moved abstain chunks from this file
;;; 2017.4.16	Marita Sailor
;;;				: * duplicated file from Intentional Abstention folder
;;;				: * added cursor-noise and process-cursor
;;;				: * added find-cursor and find-cursor-2 chunks
;;;				: * adjust end goal state of P Advance-Screen_Imaginal-Match-Stop
;;;				: * new productions: find-cursor-location, find-cursor-incorrect
;;;				: * tested and works
;;; 2017.3.21  Marita Sailor
;;;				: * duplicated file from Updated DM folder
;;;				: * added Abstain chunk type
;;;				: * added Abstain chunks to DM and base-level activations
;;;				: * added checking-contest Selection chunk
;;;				: * added intermediary productions 1) check-contest and 2) abstain
;;;				: * modified Encode-contest production 
;;;				: * adjusted production parameter for check-contest
;;; 2016.11.22 Marita Sailor
;;;				: * added base-level activations for all chunks
;;; 2016.11.14 Marita Sailor
;;;				: * model check-up 
;;; 2016.10.28 Marita Sailor 
;;;				: * Fixed all of the control flow problems. Model works!
;;; 2016.10.27 Marita Sailor 
;;;				: * current issue is sending it back to the ordered search when it knows who
;;;				: to vote for, but that name doesn't match what it's currently looking at
;;; 2016.10.26 Marita Sailor 
;;;				: * Fixed FIND-LOC-FAILURE 
;;;				: * added default VoteParty chunk to DM
;;;				: * added productions to handle retrieval failure
;;;				: * Enabled forgetting
;;;				: * changed control flow, but haven't tested
;;; 2016.09.22 Marita Sailor
;;;				: * Added header
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; Previously named SearchWithRace_InOrder_VBModel.lisp
;;;
;;; This model does a serial search down the list of candidates until it finds the
;;; one that matches a name in memory.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;****************************************
;****************************************
;start to vote
;****************************************
;****************************************

;****************************************

(P Select-Choice_Locate-Contest-Description

=goal>
	ISA       MakeVote
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
	state    found-contest-description
	
!eval! (setf current-strat 'retrieval)

)

;****************************************

(P Select-Choice_Attend-Contest-Description

=goal>
	ISA       MakeVote
	state     found-contest-description
	

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
	

=retrieval>
	ISA      Abstain
	contest  =this

==>

; sends to navigation production
=goal>
	ISA 	MakeVote
	state	find-next-race
	
	

)

;****************************************

(P Select-Choice_Encode-Contest-Description

=goal>
	ISA       MakeVote
	state     checking-contest
	

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
;; Production that halts if it goes beyond CountyJudge
(P Past-End-State

=goal>
	ISA 	   MakeVote
	state	   encoded-contest-description
	
	endState   =end

=imaginal>
	ISA     MakeVote
	race    =end

==>

=imaginal>

+goal>
	ISA     clear

)

;****************************************
; Successful retrieval of candidate to vote for

(P Initial-Retrieval-Success

=goal> 
	ISA     MakeVote
	state   encoded-contest-description
	

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
	state	   search-screen

!output! ("I'm voting for: ~s" =n)

)

;****************************************
;; Model has read contest description but retrieval of candidate has failed
; initiates recognition strategy

(P Initial-Retrieval-Fails

=goal>
	ISA 	MakeVote
	state 	encoded-contest-description
	

?retrieval>
	buffer	failure

==>

=goal>
	state	search-screen

!output! ("Initial retrieval fails, switch to recog strategy.")
!eval! (setf current-strat 'recognition)

)

;****************************************

(p Select-Choice_Search-Screen-Ordered

=goal>
	ISA     MakeVote
	state   search-screen
	

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
	state     something-found
  
)

;****************************************

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
(P Select-Choice_Encode-Search

=goal>
	ISA     MakeVote
	state   attending-something-found
	

=visual>
	ISA         text
	value       =val
	screen-pos  =pos

=imaginal>

==>

+retrieval>
	ISA       Candidate
	name      =val

=imaginal>
	ISA         MakeVote
	candidate   =val
	position    =pos

=goal>
	state	 encoded-search

!output! ("Looking at candidate: ~s" =val)

)

;****************************************
; 

(P  Select-Choice_Imaginal-Match-Stop

=goal>
	ISA         MakeVote
	state       encoded-search
	

=retrieval>
	ISA    	    Candidate 
	name        =val

=imaginal>
	ISA         MakeVote
	candidate   =val
	position    =pos

?manual>
	state      free

==>

=imaginal>

+manual>
	ISA     move-cursor
	loc     =pos

=goal>
	state    moved-to-candidate

)

;****************************************
;If the name in the visual location does not match
; candidate saved in imaginal, look for another name 

(P  Select-Choice_No-Match

=goal>
	ISA       MakeVote
	state     attending-something-found
	

=retrieval>
	ISA         MakeVote 
	candidate   =val

=visual>
	ISA       text
	value     =notval

?visual-location>
	state	  free

==>

=goal>
	state    search-screen

!output! ("Attended candidate doesn't match, redo search: ~s" =notval)

)

;****************************************
; retrieval for voting candidate fails during search

(P VBP-Retrieval-Fails-During-Search

=goal>
	ISA     MakeVote
	state   encoded-search
	

?retrieval>
	buffer	failure

==>

=goal>
	state  search-screen

!output! ("Retrieval fails during search, revisit list")

)


;****************************************

; Productions that handle retrieval failure (i.e. when retrieval of candidate fails)
; Switches to voting by party
; VBP = Vote By Party

;****************************************
;; Model has read contest description but retrieval of candidate has failed
; initiates vote-by-party strategy

(P Retrieval-Fails-After-Search

=goal>
	ISA 	MakeVote
	state 	something-found
	

?visual-location>
	buffer	failure

==>

=goal>
	state	search-by-party

!output! ("Looked at everything and nothing retrieved-- voting by party")
!eval! (setf current-strat 'party)

)

;****************************************
;; current visual location is at the top of the list (Production fires if initial retrieval fails)
;; looking at party

(P VBP-Search-Screen-Top-Down

=goal>
	ISA     MakeVote
	state   search-by-party
	

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
	

?visual-location>
	buffer	failure

==>

=goal>
	state	ready-to-advance
	


)

;****************************************
;Selection
;****************************************

(spp Select-Choice_Imaginal-Match-Stop :u 1000)
(spp Select-Choice_Search-Screen-Ordered :u 8)
(spp check-contest :u 4000)
(spp Past-End-State :u 4000)

