(defun rand (n)
    (- (random (1+ (* 2 n))) n))


(defun vote (realtime use-model)

  
	(reset)
  
	(setq i 0)
	(setq button-map (make-hash-table)) 
	(setq button-state (make-hash-table)) 
	;(setq noise 1)
	(setq noise 1)
	(setq noise_macro 6)

  
	(let* ((window (open-exp-window "Ballet" :width 550 :height 550)))
	
	
		(setq starting-x 10)
	
		(loop
	
			(setq j 0)
			
			(setq starting-x (+ (* i 180) 10))
		
			;starting x is i * 160 + 10
			
			(loop 

				;starting y is j * 85 + 10		
				(setq starting-y (+ (* j 85) 10))
				
				
				
				(setq randomx (+ starting-x (rand noise_macro)))
				(setq randomy (+ starting-y (rand noise_macro)))

				
				
				(setf candidate-party-object-array (make-array '(6)))
				
				(setq race-num (+ (* i 6) j))

				
				(add-text-to-exp-window :text (concatenate 'string "temporaryracename" (write-to-string race-num)) :x randomx :y randomy)
				
				(setf (aref candidate-party-object-array 0) (add-text-to-exp-window :text (concatenate 'string "candidate" (write-to-string (* race-num 3))) :x (+ randomx 30 (rand noise)) :y (+ randomy 20 (rand noise))))
				(setf (aref candidate-party-object-array 1) (add-text-to-exp-window :text (concatenate 'string "candidate" (write-to-string (+ (* race-num 3) 1))) :x (+ randomx 30 (rand noise)) :y (+ randomy 35 (rand noise))))
				(setf (aref candidate-party-object-array 2) (add-text-to-exp-window :text (concatenate 'string "candidate" (write-to-string (+ (* race-num 3) 2))) :x (+ randomx 30 (rand noise)) :y (+ randomy 50 (rand noise))))
				
				(setf (aref candidate-party-object-array 3) (add-text-to-exp-window :text "party1" :x (+ randomx 118 (rand noise)) :y (+ randomy 20 (rand noise))))
				(setf (aref candidate-party-object-array 4) (add-text-to-exp-window :text "party2" :x (+ randomx 118 (rand noise)) :y (+ randomy 35 (rand noise))))
				(setf (aref candidate-party-object-array 5) (add-text-to-exp-window :text "party3" :x (+ randomx 118 (rand noise)) :y (+ randomy 50 (rand noise))))
				
				(setf button_temp (add-button-to-exp-window :text "" :x randomx :y (+ randomy 22) :width 20 :height 10 :action 
				(lambda (button)
				(if (= (gethash button button-state) 0) (progn
					(modify-text-for-exp-window (aref (gethash button button-map) 0) :color 'blue)
					(modify-text-for-exp-window (aref (gethash button button-map) 3) :color 'blue)
					(setf (gethash button button-state) 1)
					)
					(progn
					(modify-text-for-exp-window (aref (gethash button button-map) 0) :color 'black)
					(modify-text-for-exp-window (aref (gethash button button-map) 3) :color 'black)
					(setf (gethash button button-state) 0)
					)
				)
				)))
				(setf (gethash button_temp button-map) candidate-party-object-array)
				(setf (gethash button_temp button-state) 0)

				
				
				
				(setf button_temp (add-button-to-exp-window :text "" :x randomx :y (+ randomy 38) :width 20 :height 10 :action 
				(lambda (button)
				(if (= (gethash button button-state) 0) (progn
					(modify-text-for-exp-window (aref (gethash button button-map) 1) :color 'blue)
					(modify-text-for-exp-window (aref (gethash button button-map) 4) :color 'blue)
					(setf (gethash button button-state) 1)
					)
					(progn
					(modify-text-for-exp-window (aref (gethash button button-map) 1) :color 'black)
					(modify-text-for-exp-window (aref (gethash button button-map) 4) :color 'black)
					(setf (gethash button button-state) 0)
					)
				)
				)))
				(setf (gethash button_temp button-map) candidate-party-object-array)
				(setf (gethash button_temp button-state) 0)
				
				
				(setf button_temp (add-button-to-exp-window :text "" :x randomx :y (+ randomy 53) :width 20 :height 10 :action 
				(lambda (button)
				(if (= (gethash button button-state) 0) (progn
					(modify-text-for-exp-window (aref (gethash button button-map) 2) :color 'blue)
					(modify-text-for-exp-window (aref (gethash button button-map) 5) :color 'blue)
					(setf (gethash button button-state) 1)
					)
					(progn
					(modify-text-for-exp-window (aref (gethash button button-map) 2) :color 'black)
					(modify-text-for-exp-window (aref (gethash button button-map) 5) :color 'black)
					(setf (gethash button button-state) 0)
					)
				)
				)))
				(setf (gethash button_temp button-map) candidate-party-object-array)
				(setf (gethash button_temp button-state) 0)
			
			
			(setq j (+ j 1))
			
			(when (> j 5) (return j))
		
		)
		
		(setq i (+ i 1))
				
		(when (> i 2) (return i))
				
		
	
	)
	
	
	(if use-model
		(progn
		(install-device window)
		(proc-display)
		(start-hand-at-mouse)
		(if realtime (run 200 :real-time t) (run 200))
		;(run 200 :real-time t)
		(run 200)
		)())

		
))
