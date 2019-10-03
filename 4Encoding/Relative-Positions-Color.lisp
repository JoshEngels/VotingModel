
;; Encodes the race and makes a request for a button associated with this race
;; No attend production because productions that lead into this have one
(P Encode-Race

=goal>
	state   	storing-race-group

=visual>
	value		=text
	
=visual-location>
	ISA			visual-location	
	kind		text
	group		=group1

=imaginal>
	race-group	none

==>

+visual-location>
	ISA			visual-location
	> screen-y	current
	color		purple
	:nearest	current
	
=imaginal>
	race-group  =group1
 
=goal>
	state		find-candidate-group	
	
!output! ("Example of race is: ~s" =text)



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

;; Encodes the candidate group and makes a visual request for the parties
(P Encode-Candidate

 =goal>
	state   	storing-candidate-group
	top			=top

	
=visual>
	value		=text

=visual-location>
	ISA			visual-location	
	kind		text
	group		=group2

=imaginal>
	- race-group	none
	candidate-group	none
	race-group		=race-group

==>

+visual-location>
	ISA			visual-location
	kind		text
	:nearest	current
	color		blue

=imaginal>
	candidate-group  =group2
 
=goal>
	state		find-party-group
	
!output! ("Example of candidate is: ~s" =text)

	
)

; Attends the party
(P Attend-Party

=goal>
	state		find-party-group

=visual-location>
	ISA			visual-location	
	kind		text
	; width		=width
	; screen-x	=middle-x

?visual>
	state		free
	
==>

+visual>
	ISA			move-attention
	screen-pos	=visual-location
	
=visual-location>
	
=goal>
	state		storing-party-group

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

