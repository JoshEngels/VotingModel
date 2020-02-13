; Assumes verticles are exactly lined up

; Makes a visual location request for the first race
(P Find-First-Race

=goal>
	state			start-voting
	
==>

+visual-location>
	ISA			visual-location
	kind		text
	color		red
	screen-left lowest
	screen-y	lowest
	
	
=goal>
	state		attending-race-next-col
)

; ------------------- These next productions find the race header and request a race in the same col


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
	
=visual-location>
	
=goal>
	state		find-race-same-col
	
)


; Makes a visual location request for the race header that is the same y and below this race header
(P Find-Race-Same-Col

=goal>
	state		find-race-same-col
	
?visual>
	state		free

=imaginal>
	
==>

+visual-location>
	ISA				visual-location
	kind			text
	color			red
	> screen-y		current
	screen-left		current
	:nearest		current
	
=imaginal>
	
=goal>
	state		attending-race-same-col
)


;-------------------------------- These next productions either pass control to encoding if a race is found or move to the next col

; ; This production being called means that we have reached the end of a col, so we begin the process of finding the top race in the next col
(P Find-Race-Same-Col-No-Match

=goal>
	state		attending-race-same-col	
	
?visual-location>
	buffer		failure
	
==>

=goal>
	state		find-top-race

)

; ; We have found the next race within this col and so we pass control to encoding process
(P Attend-Race-Same-Col

=goal>
	state		attending-race-same-col	
		
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



(P Find-Top-Race

=goal>
	state		find-top-race
	

==>

+visual-location>
	ISA			visual-location
	screen-left	current
	color 		red
	screen-y	lowest

=goal>
	state		attending-top-race

)



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
	state		find-race-next-col
	
=visual-location>
	
)


; ;****************************************
; ; This production makes the visual location request for the closest race in the next col (like LTRTTB)
(P Find-Race-Next-Col

=goal>
	state		find-race-next-col
	
=visual>

=visual-location>
	screen-right	=current-right
	
==>

+visual-location>
	ISA				visual-location
	kind			text
	color			red
	> screen-left	=current-right
	screen-left		lowest
	:nearest		current-y
	
	
=goal>
	state		attending-race-next-col

)

; ;****************************************
; ; We have found a race in the next col, so attend it and launch into encoding
(P Attend-Race-Next-Col

=goal>
	state		attending-race-next-col
	
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
; ; so we can end the mode
(P Find-Race-Next-Col-No-Match

=goal>
	state			attending-race-next-col

?visual-location>
	buffer			failure
		
==>

=goal>
	state  			end
	
)
