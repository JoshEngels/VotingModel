; Assumes verticles are exactly lines up

; Makes a visual location request for the first race
(P Find-First-Race

=goal>
	state			start-voting
	
==>

+visual-location>
	ISA			visual-location
	kind		text
	color		red
	screen-x	lowest
	screen-y	lowest
	
=goal>
	state		attending-race-next-row
)

; ------------------- These next productions find the race header and request a race in the same row

; Makes a visual location request for the race title
(P Find-Race-Title

=goal>
	state		find-next-race
	
=imaginal>
	race-group	=race-group
	
==>

+visual-location>
	ISA			visual-location
	kind		text
	group		=race-group
	
=imaginal>
	
=goal>
	state		attending-race-title

)

; Attend the race title
(P Attend-Race-Title

=goal>
	state		attending-race-title
	
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
	state		find-race-same-row
	
)


; Makes a visual location request for the race header that is to the right, and nearest to this race header
; Without greater than the current y clause, could lead to a cycle, but for now keep for simplicity
(P Find-Race-Same-Row

=goal>
	state		find-race-same-row
	
==>

+visual-location>
	ISA			visual-location
	kind		text
	color		red
	> screen-x	current
;	>= screen-y current
	:nearest	current
	
=imaginal>
	
=goal>
	state		attending-race-same-row

; !output! ("Last race outline top ~s bottom ~s left ~s right~s" =top-bound =bottom-bound =left =right)

)


;-------------------------------- These next productions either pass control to encoding if a race is found or move to the next row

; ; This production being called means that we have reached the end of a row, so we begin the process of finding the top race in the next row
(P Find-Race-Same-Row-No-Match

=goal>
	state		attending-race-same-row	
	
?visual-location>
	buffer		failure
	
==>

=goal>
	state		find-left-race

)

; ; We have found the next race within this row and so we pass control to encoding process
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



(P Find-Left-Race

=goal>
	state		find-left-race

==>

+visual-location>
	ISA			visual-location
	screen-x	lowest
	color 		red
	:nearest	current

=goal>
	state		attending-left-race

)



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


; ;****************************************
; ; This production makes the visual location request for the leftmost race in the next row down
(P Find-Race-Next-Row

=goal>
	state 		find-race-next-column
	
=visual>


==>

; Order is very important here
+visual-location>
	ISA			visual-location
	kind		text
	> screen-y	current
	screen-x	lowest
	:nearest	current	
	color		red
	
=goal>
	state		attending-race-next-row

)	


; ;****************************************
; ; We have found a race in the next row, so attend it
(P Attend-Race-Next-Row

=goal>
	state		attending-race-next-row
	
?imaginal>
	state		free
	
?visual>
	state		free

=visual-location>
	ISA			visual-location	
	kind		text


==>

+imaginal>
	race-group		none
	candidate-group	none
	party-group		none

+visual>
	ISA			move-attention
	screen-pos	=visual-location
	
=visual-location>
	ISA			visual-location	
	kind		text
	
=goal>
	state		storing-race-group	
	
)

; ;****************************************
; ; If there is nothing found when looking for a new row, we are at the bottom right corner of the ballet and there are no more races, 
; ; so we can end the model
(P Find-Race-Next-Row-No-Match

=goal>
	state			attending-race-next-row

?visual-location>
	buffer			failure
		
==>

=goal>
	state  			end
	
)
