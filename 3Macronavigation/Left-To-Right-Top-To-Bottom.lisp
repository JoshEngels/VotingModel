

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
	color		red
	
=goal>
	state		attending-race-next-row

)


;****************************************
; This production gets called every time we have finished with the last race, i.e. it is the "first" production for each race most of the time.
; We are guarenteed attention somewhere in the last race (as well as a number of other different conditions, see Contract.pdf).
; The production makes a visual location request for the next text that is below this current race and within the left and right bounds.
(P Find-Race-Same-Row

; This manual check is a sort of hack that might need to be changed eventually. Basically logging only happens when the manual event is 
; processed (because the logic is in the ballot function in the button pressed function), so if this check is not here sometimes a race willgoes
; be falsely marked as an abstension because the logging won't even have time to process.
?manual> 
	state     free

=goal>
	state			find-next-race
	top				=top-bound
	bottom			=bottom-bound
	right			=right
	left			=left

	
=imaginal>
	race-group		=race-group
	candidate-group	=candidate-group
	party-group		=party-group
	
==>

+visual-location>
	ISA			visual-location
	color		red
	>= screen-x	=right
	<= screen-y	=top-bound
	>= screen-y	=bottom-bound
	- group		=race-group
	- group		=candidate-group
	- group		=party-group
	:nearest	current
	
=imaginal>
	
=goal>
	state		attending-race-same-row
	
; Tell logging.lisp that we are done with the last race and to mark it as an abstension if it has not recieved anything since the last time this
; function was called
!eval! (log-finish)

!output! ("Last race outline top ~s bottom ~s left ~s right~s" =top-bound =bottom-bound =left =right)

)


;****************************************
; This production being called means that we have reached the end of a row, so we begin the process of finding the top race in the next row
(P Find-Race-Same-Row-No-Match

=goal>
	state		attending-race-same-row	
	
?visual-location>
	buffer		failure
	
==>

=goal>
	state		find-left-race

)

; We have found the next race within this row and so we pass control to encoding process
(P Attend-Race-Same-Row

=goal>
	state		attending-race-same-row	
		
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
; This is the next of the productions that find the next race if it is in a different column (after find-race-same-row-no-match)
; It finds the leftmost race header in this row to prepare for the switch to the next row
(P Find-Left-Race

=goal>
	state		find-left-race
	top			=top-bound
	bottom		=bottom-bound
	
==>

+visual-location>
	ISA			visual-location
	>= screen-y	=bottom-bound
	<= screen-y	=top-bound
	screen-x	lowest
	kind		text
	color 		red

=goal>
	state		attending-left-race

)


;****************************************
; This production attends the left race after we have found its location in preperation for the move to the next column.
(P Attend-Left-Race

=goal>
	state		attending-left-race
	
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
	state		find-race-next-row
	
)


;****************************************
; This production makes the visual location request for the leftmost race in the next row over
(P Find-Race-Next-Row

=goal>
	state 		find-race-next-column
	top			=top-bound
	bottom		=bottom-bound
	
=visual>


==>

	
+visual-location>
	ISA			visual-location
	kind		text
	>= screen-y	=top-bound
	:nearest	current	
	color		red
	
=goal>
	state		attending-race-next-row

)	


;****************************************
; We have found a race in the next row, so attend it
(P Attend-Race-Next-Row

=goal>
	state		attending-race-next-row
	
?imaginal>
	state		free
	
?visual>
	state		free

=visual-location>
	ISA			visual-location	
	screen-x	=center-x
	width		=width
	kind		text
	screen-y	=new-bottom


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
	bottom		=new-bottom
	
)

;****************************************
; If there is nothing found when looking for a new row, we are at the bottom right corner of the ballet and there are no more races, 
; so we can end the model
(P Find-Race-Next-Row-No-Match

=goal>
	state			attending-race-next-row

?visual-location>
	buffer			failure
		
==>

=goal>
	state  			end
	
)
