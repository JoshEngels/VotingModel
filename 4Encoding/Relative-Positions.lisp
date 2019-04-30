;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Joshua Engels
;;; Copyright   : (c) 2019 Joshua Engels
;;; Address     : Lovett College 
;;;             : Rice University
;;;             : Houston, TX 77005
;;;             : jae4@rice.edu
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : Relative-Positions.lisp
;;; Version     : 1
;;; 
;;; Description : Encoding strategy
;;;				: * Starts off with visual attention at the race header and determines the race header, candidate, and party groups
;;;				: * using only relative positions. This means that there are no cheats or look ups in memory; the groups are encoded by
;;;				: * the structure of a race.
;;;
;;; Bugs        : * If the ballot has noise, the relative movement from candidate to party will not work in a race with only one candidate.
;;;				: * This is because the way the navigation works is kind of a cheat. In order to maintain relative movement, the model finds
;;;				: * the candidate of lowest y and finds text to the right of it with greater y not in its group. This is neccesary because otherwise
;;;				: * the nearest text might be below in the next race. This is not a problem if the ballot had "no noise", i.e. if the candidates
;;;				: * and parties had the exact same y.
;;;
;;; To do       : 
;;;
;;; ----- History -----
;;; 2019.4.28   Joshua Engels
;;;				: * Documented the file
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; See contract.pdf for info on what fields the model has access to from the macronavigation
;;;   
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Encodes the race and makes a request for a button associated with this race
;; No attend production because productions that lead into this have one
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

;; Attends the button 
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

; Makes a request for a candidate (text closest to the bubble and to the right of it and not equal to the race group)
; Does not store its group, but could be added later
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

;; Attends a candidate for this race
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

;; Encodes the candidate group and makes a visual request for the candidate with greatest y
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
	screen-y	highest

=imaginal>
	candidate-group  =group2
 
=goal>
	state		find-party-group-part-1
	
!output! ("Example of candidate is: ~s" =text)

	
)

; Attends the candidate with greatest y so we can find a party with smaller y
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

; Makes the request to finnaly find the party by requesting a party with y less than or equal to this position
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
	<= screen-y	current
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

?visual>
	state		free
	
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
	
+visual>
	ISA     clear-all-finsts
	
!output! ("Example of party is: ~s" =text)
	
)

