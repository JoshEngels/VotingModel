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
;;; Filename    : Left-To-Right-Top-To-Bottom.Lisp
;;; Version     : 1
;;; 
;;; Description : Macronavigation strategy
;;;				: * If starting, finds the race in the top left corner. Otherwise tries to find the next race in the column, or if there is 
;;;				: * no such race the top race in the next column, or if there is no such next column ends the model run.
;;;
;;; Bugs        : * None known
;;;
;;; To do       : * There are a few places where I have had to cheat to make the model work (because we do not have access to relative group 
;;;				: * positios or super and sub groups). These places are documented, but eventually it would be better if they were removed. The
;;;				: * reason these cheats are neccesary is mostly because of navigating a ballot with noise. If one is not using a ballot with noise,
;;;				: * and these cheats are causing problems, they can be removed.
;;;
;;; ----- History -----
;;; 2019.9.27   Joshua Engels
;;;				: * Created the file
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;****************************************
; This production makes a visual location request for the first race on the ballot (the race in the top left corner) by requesting the text
; with lowest y that has an x between -1 (the initial x bound) and 150.
; This is one of the places where we must cheat. The 150 number is hardcoaded in as a guess (it must be greater than the middle of the race header
; for it to work but less than the next column over). Eventually it would be better to be able to say something like "find the group closest to
; the top left corner" 
(P Find-First-Race

=goal>
	state			start-voting
	left			=left-bound
	
==>

+visual-location>
	ISA			visual-location
	kind		text
	> screen-x	=left-bound
	< screen-x	150 ; This is the cheat
	screen-y	lowest
	
=goal>
	state		attending-race-next-column
	right		-1 

)


;****************************************
; This production gets called every time we have finished with the last race, i.e. it is the "first" production for each race most of the time.
; We are guarenteed attention somewhere in the last race (as well as a number of other different conditions, see Contract.pdf).
; The production makes a visual location request for the next text that is below this current race and within the left and right bounds.
(P Find-Race-Same-Column

; This manual check is a sort of hack that might need to be changed eventually. Basically logging only happens when the manual event is 
; processed (because the logic is in the ballot function in the button pressed function), so if this check is not here sometimes a race willgoes
; be falsely marked as an abstension because the logging won't even have time to process.
?manual> 
	state     free

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
	
; Tell logging.lisp that we are done with the last race and to mark it as an abstension if it has not recieved anything since the last time this
; function was called
!eval! (log-finish)

)


;****************************************
; This production being called means that we have reached the end of a column, so we begin the process of finding the top race in the next column
(P Find-Race-Same-Column-No-Match

=goal>
	state		attending-race-same-column	
	
?visual-location>
	buffer		failure
	
==>

=goal>
	state		find-top-race

)

; We have found the next race within this column and so we pass control to encoding process
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
; This is the next of the productions that find the next race if it is in a different column (after find-race-same-column-no-match)
; It finds the top race in this column to prepare for the switch to the next column
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


;****************************************
; This production attends the top race after we have found its location in preperation for the move to the next column.
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


;****************************************
; This production makes the visual location request for the top race in the next column over
; Another big cheat is here: we make a guess (called right-guess) for an x location that is to the right of any text in this column
; and hopefully to the left of any text in the next column
(P Find-Race-Next-Column

=goal>
	state 		find-race-next-column
	right		=right-bound
	left		=left-bound
	
=visual>


; Cheating big time; the guess is the current "right-bound" (a calculation from the last race group that can lead us astray) plus half the width
; of the column. Note this would not be neccesary in the ballot without noise, and if it is causing problems and we are only using a ballot without
; noise it can be removed and the other commented visual location request below used instead
!bind! =right-guess (+ =right-bound (- =right-bound =left-bound))


==>

; +visual-location>
	; ISA			visual-location
	; kind		text
	; > screen-x	=right-bound
	; :nearest	current
	
+visual-location>
	ISA			visual-location
	kind		text
	screen-y	lowest
	> screen-x	=right-bound
	< screen-x	=right-guess
	:nearest	current	
	
=goal>
	state		attending-race-next-column

)	


;****************************************
; We have found a race in the next column, so attend it
; More cheating ensures that the left of the column is to the left of everything, even if there is noise. As mentioned above, this is unnecesary
; in a ballot without noise and so can be removed if it is causing problems
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
	screen-x	=center-x
	width		=width
	kind		text

; More cheating	
!bind! =new-left (/ (+ =old-right (- =center-x (/ =width 2))) 2)

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
	left		=new-left
	
)

;****************************************
; If there is nothing found when looking for a new column, we are at the bottom right corner of the ballet and there are no more races, 
; so we can end the model
(P Find-Race-Next-Column-No-Match

=goal>
	state			attending-race-next-column

?visual-location>
	buffer			failure
		
==>

=goal>
	state  			end
	
)
