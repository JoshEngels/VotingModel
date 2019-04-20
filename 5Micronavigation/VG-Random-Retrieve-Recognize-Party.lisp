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
;;; Filename    : VG-Random_Retrieve-Recognize-Party.lisp
;;; Version     : r1
;;; 
;;; Description : Model of voting on virtual VoteBox DRE.
;;;             : * Model attends contest and retrieves desired candidate using a random search strategy.
;;;				: * If initial retrieval fails, switches to recognition by name
;;;				: * If no name is recognized, votes by party
;;;				: * Abstains from race if retrieval of default party fails
;;; 
;;; Bugs        : * None known
;;;
;;; To do       : *
;;; 
;;; ----- History -----
;;; 2019.2.14   Xianni Wang
;;;				: * modified the logical flow
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
;;; 2018.4.4    Xianni Wang
;;;				: * adapted file with visual grouping learning model
;;;	2017.4.20   Marita Sailor
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
;;;				: * added Abstain chunks and chunk base-level activations
;;;				: * added intermediary productions 1) check-contest and 2) abstain
;;;				: * reorganized imaginal buffer requests in Encode-contest production
;;;				: * adjusted production parameter for check-contest
;;; 2016.11.22 Marita Sailor
;;;				: * added base-level activations for all chunks
;;; 2016.11.14 Marita Sailor
;;;				: * model check-up
;;; 2016.11.22 Marita Sailor
;;;				: * Fixed so it votes by party if initial retrieval fails
;;; 2016.11.06 Marita Sailor
;;;				: * Created file from duplicate
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;; The model does a single retrieveal from DM for each race and then does a random 
;;; search for the string that was retrieved from memory. Votes by party if retrieval fails.
;;;
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
	state     found-contest-description
	
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
	ISA           move-attention
	screen-pos    =visual-location

=goal>
	state      attended-contest-description

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
	state   checking-contest

)

;****************************************

(P abstain

=goal>
	ISA     MakeVote
	state   checking-contest
	

=retrieval>
	ISA      Abstain
	contest  =this

==>

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
	buffer     failure

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

=retrieval>

=imaginal> ;moving the info in retrieval to imaginal buffer for later
	ISA	       MakeVote
	race       =r
	candidate  =n

=goal>
	state   search-screen-retrieval

!output! ("I'm voting for: ~s" =n)

)

;****************************************

(P Select-Choice_Search-Screen-Fastest_Retrieval

=goal>
	ISA      MakeVote
	state    search-screen-retrieval
	

?visual-location>
	state     free

=imaginal>
	candidate-group  =val2

==>
 
=imaginal>
 
+visual-location>
	ISA          visual-location
	kind         text
	group        =val2
	:attended    nil

=goal>
	state     something-found-retrieval

)

;****************************************

(P Select-Choice_Attend-Search_Retrieval

=goal>
	ISA      MakeVote
	state    something-found-retrieval
	

=visual-location>
	ISA      visual-location
	kind     text
	group    =val2

?visual>
	state    free

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
	ISA	    MakeVote
	state   attending-something-found-retrieval
	

=visual>
	ISA         text
	value       =val
	screen-pos  =pos

?visual-location>
	state        free

==>

=visual>

=goal>
	state    encoded-search-retrieval

!output! ("Looking at candidate: ~s" =val)

)

;**************************************** 

(P  Select-Choice_Imaginal-Match-Stop_Retrieval

=goal>
	ISA       MakeVote
	state     encoded-search-retrieval
	

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
	

?visual-location>
	state    free

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

(P Retrieval-Fails

=goal>
	ISA     MakeVote
	state   encoded-contest-description
	

?retrieval>
	buffer   failure

==>

=goal>
	state   search-screen-recognition

!output! ("Initial retrieval fails, switch to recog strategy.")
!eval! (setf current-strat 'recognition)

)

;****************************************
; Model has read contest description but retrieval of candidate has failed

(P Retrieval-Fails-after-searching

=goal>
	ISA     MakeVote
	state   encoded-search-retrieval
	

?visual-location>
	buffer	failure

==>

=goal>
	state  search-screen-recognition

!output! ("Looked at everything and nothing retrieved--switch to recognition")
!eval! (setf current-strat 'recognition)

)

;****************************************
;Random search

(P Select-Choice_Search-Screen-Fastest_Recognition

=goal>
	ISA       MakeVote
	state     search-screen-recognition
	

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
	kind         text
	:attended    nil
	group        =val2


=goal>
	state     something-found-recognition

)

;****************************************
;Attend that name

(P Select-Choice_Attend-Search_Recognition

=goal>
	ISA       MakeVote
	state     something-found-recognition
	

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
;; Model has read contest description but retrieval of candidate has failed
; initiates vote-by-party strategy

(P Recognition-Fails

=goal>
	ISA     MakeVote
	state   something-found-recognition
	

?visual-location>
	buffer  failure

==>

=goal>
	state   search-by-party

!output! ("Looked at everything and nothing retrieved-- voting by party")
!eval! (setf current-strat 'party)

)

;****************************************
; random searches the screen to find a candidate's party

(P VBP-Select-Choice_Search-Screen-random

=goal>
	ISA     MakeVote
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
	ISA         visual-location
	kind        text
	group       =val3
	:attended   nil
  
=goal>
	state    vbp-attend-name
  
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
	state   free

==> 

+visual>
	ISA          move-attention
	screen-pos   =visual-location

=goal>
	state   party-found

)

;****************************************
; a name was found and matches with the default party

(P Vote-by-Party_Match

=goal>
	ISA      MakeVote
	state    party-found
	
	default  =party

=visual>
	ISA         text
	value       =party
	screen-pos  =pos

?manual>
	state   free

==>

=visual>

+manual>
	ISA    move-cursor
	loc    =pos

=goal>
	state   moved-to-candidate

!output! ("Party found ~s and matches default" =party)

)

;****************************************
; production fires if default party does not match
; returns goal state to search again

(P Vote-by-Party_No-Match

=goal>
	ISA      MakeVote
	state    party-found
	
	default  =p

=visual>
	ISA     text
	- value =p
	value   =q

==>

=goal>
	state    search-by-party

!output! ("Party ~s does not match default, search again" =q)

)


;****************************************
; if the model has looked at the entire list and cannot recall default party 
; abstain

(P Vote-by-Party_No-Match-ABSTAIN

=goal>
	ISA     MakeVote
	state   vbp-attend-name
	

?visual-location>
	buffer   failure

==>

=goal>
	state   find-next-race
	

	

)


;Production Parameters
(spp Select-Choice_Search-Screen-Fastest_Retrieval :u 8)
(spp Select-Choice_Search-Screen-Fastest_recognition :u 8)
(spp check-contest :u 4000)



