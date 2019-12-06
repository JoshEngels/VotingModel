(clear-all)
(reset)

(define-model test

(sgp :v t :esc t :show-focus t :process-cursor nil)


(add-dm
 (one)
 (two)
 (three)
 (done)
 (oldloc)
 (goal state one oldloc nil)
)

    
(P Find-First

=goal>
	state			one
	
==>

+visual-location>
	ISA			visual-location
	kind		text
	:nearest	current
;	:nearest2	current
	
	
=goal>
	state		two
	
)

(P Test-Storage

=goal>
	state			two
	
=visual-location>
	ISA			visual-location	
	kind		text
	
==>

+visual>
	ISA			move-attention
	screen-pos	=visual-location
	
=goal>
	state		three
	oldloc		=visual-location
	
)

(P Find-Second

=goal>
	state			three
	
=visual>

	
==>


+visual-location>
	ISA				visual-location
	kind			text
	> screen-left	current-right

	
=goal>
	state		done
	
)

(goal-focus goal)

)


(let* 
	((wheight 500)
	(wwidth 500)
	(window (open-exp-window "Ballet" :width wwidth :height wheight :visible t)))
	
	(add-text-to-exp-window :text "Testing1!" :x 150 :y 150 :font-size 20) 
	(add-text-to-exp-window :text "Testing2!" :x 200 :y 200 :font-size 20) 

	
	(install-device window)
	(proc-display)
	(start-hand-at-mouse)
	(run 100 :real-time t)
)