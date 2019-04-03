; This file moves to the nearest bubble and clicks on it
; The entering goal state is moved-to-candidate and the exiting goal state is find-next-race


; Finds the bubble closest to the currently attended location
; Does not use the todo field (navigate) (include in things to change when pasting in)
(P find-bubble

=goal>
	state		moved-to-candidate
	left		=left-bound
	right		=right-bound
	
==>

+visual-location>
	ISA			visual-location
	kind		oval
	> screen-x	=left-bound
	< screen-x	=right-bound
	:nearest	current

=goal>
	state		attending-bubble

)


; Moves the mouse to the bubble
(P move-mouse-to-bubble

=goal>
	state		attending-bubble
	
?manual>
	state		free
	
=visual-location>
	ISA			visual-location
	kind		oval
	
==>

+manual>
	ISA     move-cursor
	loc     =visual-location


=goal>
	state	moving-mouse-to-bubble
	

)

; Clicks the bubble and sets the state to find-next-race
(P click-bubble

=goal>
	state     moving-mouse-to-bubble

?manual>
	state     free

==>

+manual>
	ISA     click-mouse

=goal>
	state 	find-next-race

)