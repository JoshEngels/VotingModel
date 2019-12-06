(clear-all)
(reset)

(define-model test

(sgp :v t :esc t :show-focus t :process-cursor nil)


(add-dm
 (start)
 (end)
 (goal state start)
)

    
(P Find-First-Race

=goal>
	state			start
	
==>

+visual-location>
	ISA			visual-location
	kind		text
	color		red
	screen-left lowest
	screen-y	lowest
	:nearest	current
	:closest	current
	
	
=goal>
	state		end
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