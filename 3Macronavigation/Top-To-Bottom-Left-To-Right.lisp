


;****************************************
;****************************************
; Find the next race to vote on (macronavigation)
;****************************************
;****************************************

;****************************************
; These productions deal with within column navigation
;****************************************


; Attends the top left corner race to start voting and sends control as if this was a new column
; TODO: This is a problem if the x of the top left element is not the most left x of any race.
; To fix, would need to first attend the top left corner, at a point or something, and then find the nearest object
; (P Find-First-Race

; =goal>
	; state			start-voting
	
; ==>

; +visual-location>
	; ISA		visual-location
	; kind		text
	; screen-x	lowest
	; screen-y	lowest
	
; =goal>
	; state		attending-race-next-column

; )

; Finds the first race, but need a guess for right x that is for sure greater than the middle of the first race description
(P Find-First-Race

=goal>
	state			start-voting
	left			=left-bound
	
==>

+visual-location>
	ISA			visual-location
	kind		text
	> screen-x	=left-bound
	< screen-x	150 ;
	screen-y	lowest
	
=goal>
	state		attending-race-next-column
	right		-1 ;a little cheat here, only use the right to find the first race and then delete it like it was never there

)



; Does a visual location request for the next race in this column
(P Find-Race-Same-Column

; goes back to having everything in a set state each time. need to figure out
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
	
!eval! (log-finish)

)

; Goes to the column switch logic
(P Find-Race-Same-Column-No-Match

=goal>
	state		attending-race-same-column	
	
?visual-location>
	buffer		failure
	
==>

=goal>
	state		find-top-race

)

; Attends the race header, passes control to the productions that encode the race groups
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
; These productions switch to the next collumn and attend a new race
;****************************************


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

(P Find-Race-Next-Column

=goal>
	state 		find-race-next-column
	right		=right-bound
	left		=left-bound
	
=visual>


; Cheating big time
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
; We have found a new column and so attend it
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

	
;****************************************

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
;If there is nothing found when looking for a new column, we are at the bottom right corner of the ballet and there are no more races, so we can end the model
(P Find-Race-Next-Column-No-Match

=goal>
	state			attending-race-next-column

?visual-location>
	buffer			failure
		
==>

=goal>
	state  			end
	
)
