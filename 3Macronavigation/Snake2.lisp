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
	screen-left lowest
	screen-y	lowest
	
	
=goal>
	state		attending-race-new-row
;	anchor		nil
	direction	rightwards
)


; ------------------- These next productions deal with cases in which we see a race we have already voted on


; Deals with if we've already seen this race and are at the right edge
(P Deal-With-Already-Voted-First-Col-Right-Edge

=goal> 
	state			already-voted
	direction		leftwards
	first-race-col	true

==>

=goal> 
	direction		rightwards
	state			find-race-new-row
)

; Deals with if we've already seen this race and are at the left edge
(P Deal-With-Already-Voted-First-Col-Left-Edge

=goal> 
	state			already-voted
	direction		rightwards
	first-race-col	true

==>

=goal> 
	direction		leftwards
	state			find-race-new-row
)

; Deals with if we've seen this race and are somewhere in the middle
(P Deal-With-Already-Voted-First-Col-Middle

=goal> 
	state			already-voted
	first-race-col	nil

==>

=goal> 
	state			find-next-race
)


; ------------------- These next productions find the race header and request a race in the same row in the current snake direction

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
	state		find-race-same-row
	
)

; Make a snake move to the left
(P Find-Race-Same-Row-Left

=goal>
	state		find-race-same-row
	direction	leftwards
	anchor		=anchor
	
=visual>

=visual-location>
	screen-left		=current-left

=imaginal>
	
==>

+visual-location>
	ISA				visual-location
	kind			text
	color			red
	< screen-right	=current-left
	screen-left		highest
	:nearest		=anchor
	
=imaginal>
	
=goal>
	state		attending-race-same-row

)


; Make a snake move to the right
(P Find-Race-Same-Row-Right

=goal>
	state		find-race-same-row
	direction	rightwards
	anchor		=anchor
	
=visual>

=visual-location>
	screen-right	=current-right

=imaginal>
	
==>

+visual-location>
	ISA				visual-location
	kind			text
	color			red
	> screen-left	=current-right
	screen-left		lowest
	:nearest		=anchor
	
=imaginal>
	
=goal>
	state		attending-race-same-row

)




;-------------------------------- These next productions either pass control to encoding if a race is found or move to the next row

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
	state			storing-race-group
	first-race-col	nil


)



; This production starts the process of starting a new snake row
(P Find-Race-Same-Row-No-Match

=goal>
	state		attending-race-same-row	
	
?visual-location>
	buffer		failure
	
==>

=goal>
	state		find-race-next-row

)


; TODO: Find race header too

; Finds the next race down and switches directions from right to left
(P Find-Next-Race-Down-Right-Side

=goal>
	state		find-race-new-row
	direction	rightwards

==>

+visual-location>
	ISA			visual-location
	screen-left	current
	color 		red
	>= screen-y	current
	screen-y	highest

=goal>
	state		attending-race-new-row
	direction	leftwards

)


; Finds the next race down and switches directions from left to right
(P Find-Next-Race-Down-Left-Side

=goal>
	state		find-race-new-row
	direction	leftwards

==>

+visual-location>
	ISA			visual-location
	screen-left	current
	color 		red
	>= screen-y	current
	screen-y	highest

=goal>
	state		attending-race-new-row
	direction	rightwards
)



(P Attend-Race-New-Row

=goal>
	state		attending-race-new-row
	
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
	
=goal>
	state			storing-race-group
	first-race-col	true
	anchor			=visual-location
	
=visual-location>


	
)



; ;****************************************
; ; If there is nothing found when looking for a new row, we are at the bottom corner of the ballet and there are no more races, 
; ; so we can end
(P Find-Race-Next-Row-No-Match

=goal>
	state			attending-race-new-row

?visual-location>
	buffer			failure
		
==>

=goal>
	state  			end
	
)
