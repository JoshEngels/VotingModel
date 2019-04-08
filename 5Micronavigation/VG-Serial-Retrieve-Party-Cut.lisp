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
;;; Filename    : VG-Serial-Retrieve-Party.lisp
;;; Version     : r1
;;; 
;;; Description : Model of voting on virtual VoteBox DRE.
;;;             : * Uses a retrieval-based serial strategy to search for candidates by name.
;;;             : * If initial retrieval fails, performs a serial search by party
;;; 
;;; Bugs        : * None known
;;;
;;; To do       : * 
;;; 
;;; ----- History -----
;;; 2019.1.31   Xianni Wang
;;;				: * updated screen learning code
;;; 2018.9.5    Xianni Wang
;;;				: * added defparameter and !eval! functions to log strategies for simulation
;;; 2018.5.19   Xianni Wang
;;;				: * added two more activation levels 
;;;				: * added candidate chunks and abstain chunks
;;; 2018.4.25   Xianni Wang
;;;				: * removed unnecessary imaginal buffers
;;;				: * added retrieval failure production
;;; 2018.4.14   Xianni Wang
;;;				: * adjusted the format
;;; 2018.4.1    Xianni Wang
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
;;;				: * added VoteParty chunk type and Abstain chunk type
;;;				: * added checking-contest chunk
;;;				: * added Abstain chunks and base-level activations
;;;				: * added intermediary productions 1) check-contest and 2) abstain
;;;				: * reorganized imaginal buffer requests in Encode-contest production
;;;				: * adjusted production parameter for check-contest		
;;; 2016.11.22 Marita Sailor
;;;				: * added base-level activations for all chunks
;;; 2016.11.14 Marita Sailor
;;;				: * model check-up
;;; 2016.11.03 Marita Sailor
;;;				* Created file (duplicated file Retrieve-Serial-Recog-Party
;;;				: and changed name to Retrieve-Serial-Party)
;;;				: * got rid of the VoteParty chunk that Retrieve-Serial-Recog-Party
;;;				: uses. Now the default party is located in the MakeVote chunk.
;;;				: * Model works successfully
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
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
;Put the visual location on the race group 

(P Select-Choice_Locate-Contest-Description

=goal>
	ISA      MakeVote
	state    ready-to-make-choice
	

=imaginal>
	race-group  =val1

==>

=imaginal>

+visual-location>
	ISA      visual-location
	kind     text
	group    =val1

=goal>
	state    found-contest-description
	
!eval! (setf current-strat 'retrieval)

)

;****************************************
;Attend some visual location
(P Select-Choice_Attend-Contest-Description

=goal>
	ISA       MakeVote
	state     found-contest-description
	

=imaginal>

=visual-location>
	ISA       visual-location
	kind      text
	group     =val1

?visual>
	state     free

==>

=imaginal>

+visual>
	ISA          move-attention
	screen-pos   =visual-location

=goal>
	state     attended-contest-description

)

;****************************************
; the following two productions check if model should abstain from contest
(P check-contest

=goal>
	ISA       MakeVote
	state     attended-contest-description
	

=imaginal>

=visual>
	ISA       text
	value     =textVal

?retrieval>
	state     free

==>

+retrieval>
	ISA       Abstain
	contest   =textVal

=imaginal> ; encoding contest in imaginal buffer for a later check if it goes past end state
	ISA       MakeVote
	race      =textVal 

=goal>
	state     checking-contest

)

;****************************************
; Production that fires only if contest is one to abstain from

(P abstain

=goal>
	ISA      MakeVote
	state    checking-contest
	

=retrieval>
	ISA      Abstain
	contest  =this

;=imaginal> 

==>

;=imaginal> 

; sends to navigation production
=goal>
	ISA     MakeVote
	state   find-next-race
	

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
	state     encoded-contest-description

!output! ("Contest is: ~s" =textVal)
)

;****************************************
; Production that halts if it goes beyond CountyJudge
(P Past-End-State

=goal>
	ISA        MakeVote
	state      encoded-contest-description
	
	endState   =end

=imaginal>
	ISA        MakeVote
	race       =end

==>

=imaginal>

+goal>
	ISA       clear

)

;****************************************
; Successful retrieval of candidate to vote for

(P Retrieval-Success

=goal> 
	ISA     MakeVote
	state   encoded-contest-description
	

=retrieval>
	ISA      Candidate
	race     =r
	name     =n

=imaginal>

==>

=imaginal> ;moving the info in retrieval to imaginal buffer for later
	ISA         MakeVote
	race        =r
	candidate   =n

=goal>
	state    search-screen

!output! ("I'm voting for: ~s" =n)

)

;****************************************
;Read text in order with no specific name in mind

(p Select-Choice_Search-Screen-Ordered

=goal>
	ISA     MakeVote
	state   search-screen
	

?visual-location>
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
	state    attending-something-found

)

;****************************************
;Search for that name in memory

(P Select-Choice_Encode-Search

=goal>
	ISA       MakeVote
	state     attending-something-found
	

=visual>
	ISA          text
	value        =val
	screen-pos   =pos

?visual-location>
	state        free

==>

=visual>

=goal>
	state     encoded-search

!output! ("Looking at Candidate: ~s" =val)

)

;****************************************
;See if the name in the visual location matches
;any name in memory

(P Select-Choice_Imaginal-Match-Stop

=goal>
	ISA       MakeVote
	state     encoded-search
	

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
	state   moved-to-candidate

;!STOP!

)

;****************************************
; works now

(P Select-Choice-No-Match

=goal>
	ISA       MakeVote
	state     encoded-search
	

?visual-location>
	state     free

==>

=goal>
	state     search-screen

)


;****************************************

; Productions that handle retrieval failure (i.e. when retrieval of candidate fails)
; switchces to a serial voting by party
; VBP = Vote By Party

;****************************************
;; Model has read contest description but retrieval of candidate has failed

(P VBP-Retrieval-Fails

=goal>
	ISA      MakeVote
	state    encoded-contest-description
	

?retrieval>
	buffer   failure

==>

=goal>
	state   search-by-party
	
!eval! (setf current-strat 'party)

)

;****************************************
;OR
; Initial retrieval worked but model looked at everything and retrieval match failed

(P VBP-Retrieval-Fails-after-searching

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
;; restarts search from top

(P VBP-Select-Choice_Search-Screen-Ordered

=goal>
	ISA	    MakeVote
	state   search-by-party

?retrieval>
	state   free

?visual-location>
	state   free

=imaginal>
	party-group  =val3

==>

=imaginal>

+visual-location>
	ISA          visual-location
	kind         text
	group        =val3
	> screen-y   current
	screen-y     lowest
	screen-x     lowest

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
	ISA           move-attention
	screen-pos    =visual-location

=goal>
	state     vbp-attending-something-found

)

;****************************************
; attended candidate matches our default party

(P VBP-Encode-Search-Match

=goal>
	ISA        MakeVote
	state      vbp-attending-something-found
	
	default	   =party

=visual> 
	ISA         text
	value       =party
	screen-pos  =pos

?manual>
	state    free

==>

+manual>
	ISA     move-cursor
	loc     =pos

=goal>
	state    moved-to-candidate

!output! ("Matches party ~s" =party)
)

;****************************************
; if default party and attended party don't match
(P VBP-No-Match-Next-Choice

=goal>
	ISA      MakeVote
	state    vbp-attending-something-found
	
	default	 =party

=visual>
	ISA      text
	- value  =party
	value    =notparty

==>

=goal>
	state    search-by-party

!output! ("Party ~s does not match default" =notparty)
)

;****************************************

; if we reach bottom and nothing has been retrieved (no candidates match our default)
; abstain

(P VBP-bottom-of-list-fail

=goal>
	ISA     MakeVote
	state   vbp-something-found
	

?visual-location>
	buffer  failure

==>

=goal>
	state   find-next-race
	

!output! ("VBP reached bottom of list-- abstain from voting")
)



;****************************************
;Selection
;****************************************

(spp Select-Choice_Imaginal-Match-Stop :u 1000)
(spp Select-Choice_Search-Screen-Ordered :u 8)


;(spp Select-Choice_Search-Screen :u 0)
;(spp Select-Choice_Search-Screen-Faster :u 9.5 :reward -.5)
(spp check-contest :u 4000)



