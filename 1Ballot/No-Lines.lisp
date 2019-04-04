(defclass contest ()
  ((office-name :accessor office-name :initarg :office-name :initform nil)

   (cand-lst :accessor cand-lst :initarg :cand-lst :initform nil)
   (selection :accessor selection :initarg :selection :initform nil)
   (office-field :accessor office-field :initarg :office-field)
	)
  )
  
	
(defclass cand-choice ()
  ((cand-name :accessor cand-name :initarg :cand-name :initform nil)
   (party-name :accessor party-name :initarg :party-name :initform "")
   (selected-p :accessor selected-p :initarg :selected-p :initform nil)
   (parent :accessor parent :initarg :parent :initform nil)
   (my-idx :accessor my-idx :initarg :my-idx :initform nil)
   ))

(setf cntst-lst
	  (list
	   
	   (make-instance 'contest
		 :office-name "PresidentoftheUnitedStates"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "GordonBearce" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "VernonStanleyAlbury" :party-name "DEM")
		  (make-instance 'cand-choice
			:cand-name "JanetteFroman" :party-name "LIB")))
	   
	   (make-instance 'contest
		 :office-name "UnitedStatesSenator"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "CecileCadieux" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "FernBrzezinski" :party-name "DEM")
		  (make-instance 'cand-choice
			:cand-name "CoreyDery" :party-name "IND")))
	   
	   (make-instance 'contest
		 :office-name "UnitedStatesRepresentativeDistrict7"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "PedroBrouse" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "RobertMettler" :party-name "DEM")))

	   (make-instance 'contest
		 :office-name "Governor"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "GlenTravisLozier" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "RickStickles" :party-name "DEM")
		  (make-instance 'cand-choice
			:cand-name "MauriceHumble" :party-name "IND")))

	   (make-instance 'contest
		 :office-name "LieutenantGovernor"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "ShaneTerrio" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "CassiePrincipe" :party-name "DEM")))

	   (make-instance 'contest
		 :office-name "AttorneyGeneral"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "TimSpeight" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "RickOrgan" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "ComptrollerofPublicAccounts"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "ThereseGustin" :party-name "IND")
		  (make-instance 'cand-choice
			:cand-name "GregConverse" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "CommissionerofGeneralLandOffice"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "SamSaddler" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "EliseEllzey" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "CommissionerofAgriculture"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "PollyRylander" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "RobertoAron" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "RailroadCommissioner"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "JillianBalas" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "ZacharyMinick" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "StateSenator"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "RicardoNigro" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "WesleyStevenMillette" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "StateRepresentativeDistrict134"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "PetraBencomo" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "SusanneRael" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "MemberStateBoardofEducationDistrict2"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "PeterVarga" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "MarkBaber" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "PresidingJudgeTexasSupremeCourtPlace2"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "TimGrasty" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "PresidingJudgeCourtofCriminalAppeals"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "DanPlouffe" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "DerrickMelgar" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "DistrictAttorney"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "CoreyBehnke" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "JenniferALundeed" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "CountyTreasurer"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "DeanCaffee" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "GordonKallas" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "Sheriff"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "StanleySaari" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "JasonValle" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "CountyTaxAssessor"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "HowardGrady" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "RandyHClemons" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "JusticeofthePeace"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "DeborahKamps" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "ClydeGaytonJr" :party-name "DEM")))

	   
	   (make-instance 'contest
		 :office-name "CountyJudge"
		 :cand-lst
		 (list
		  (make-instance 'cand-choice
			:cand-name "DanAtchley" :party-name "REP")
		  (make-instance 'cand-choice
			:cand-name "LewisShine" :party-name "DEM")))
	))


(defun rand (n)
    (- (random (1+ (* 2 n))) n))


(defun vote (realtime use-model)

  
	(reset)
  
	(setq i 0)
	(setq button-map (make-hash-table)) 
	(setq button-state (make-hash-table)) 
	(setq button-index (make-hash-table)) 
	;(setq noise 1)
	(setq noise 0)
	(setq noise_macro 0)

  
	(let* ((window (open-exp-window "Ballet" :width 900 :height 550)))
	
	
		(setq starting-x 10)
	
		(loop
	
			(setq j 0)
			
			(setq starting-x (+ (* i 300) 10))
		
			;starting x is i * 160 + 10
			
			(loop 

				;starting y is j * 85 + 10		
				(setq starting-y (+ (* j 85) 10))
				
				
				
				(setq randomx (+ starting-x (rand noise_macro)))
				(setq randomy (+ starting-y (rand noise_macro)))

				
				
				(setf candidate-party-object-array (make-array '(6)))
				
				(setq race-num (+ (* i 6) j))


				(setf contest (pop cntst-lst))
				(setf candidates (cand-lst contest))

				
				(add-text-to-exp-window :text (office-name contest) :x randomx :y randomy)
				
				
				; Constructs the ballot
				(let ((candidate (pop candidates))
						(y-offset 20)
						(index 0))
				
				
					(loop while candidate 
			
					do 	(progn 
						
						; Candidates
						(setf (aref candidate-party-object-array index) (add-text-to-exp-window :text (cand-name candidate) :x (+ randomx 30 (rand noise)) :y (+ randomy y-offset (rand noise))))
						
						; Parties
						(setf (aref candidate-party-object-array (+ index 3)) (add-text-to-exp-window :text (party-name candidate) :x (+ randomx 200 (rand noise)) :y (+ randomy y-offset (rand noise))))

						; Buttons
						(setf button_temp (add-button-to-exp-window :text "" :x randomx :y (+ randomy y-offset 2) :width 20 :height 10 :action 
						(lambda (button)
						(if (= (gethash button button-state) 0) 
							(progn
								(modify-text-for-exp-window (aref (gethash button button-map) (gethash button button-index)) :color 'blue)
								(modify-text-for-exp-window (aref (gethash button button-map) (+ (gethash button button-index) 3)) :color 'blue)
								(setf (gethash button button-state) 1))
							(progn
								(modify-text-for-exp-window (aref (gethash button button-map) (gethash button button-index)) :color 'black)
								(modify-text-for-exp-window (aref (gethash button button-map) (+ (gethash button button-index) 3)) :color 'black)
								(setf (gethash button button-state) 0))))))								
						(setf (gethash button_temp button-map) candidate-party-object-array)
						(setf (gethash button_temp button-state) 0)
						(setf (gethash button_temp button-index) index)
						
						; Loop increment operations
						(setf y-offset (+ y-offset 15))
						(setf candidate (pop candidates))
						(setf index (+ index 1)))))
						
						
				
				;(setf (aref candidate-party-object-array 0) (add-text-to-exp-window :text (concatenate 'string "candidate" (write-to-string (* race-num 3))) :x (+ randomx 30 (rand noise)) :y (+ randomy 20 (rand noise))))
				;(setf (aref candidate-party-object-array 1) (add-text-to-exp-window :text (concatenate 'string "candidate" (write-to-string (+ (* race-num 3) 1))) :x (+ randomx 30 (rand noise)) :y (+ randomy 35 (rand noise))))
				;(setf (aref candidate-party-object-array 2) (add-text-to-exp-window :text (concatenate 'string "candidate" (write-to-string (+ (* race-num 3) 2))) :x (+ randomx 30 (rand noise)) :y (+ randomy 50 (rand noise))))
				
				;(setf (aref candidate-party-object-array 3) (add-text-to-exp-window :text "party1" :x (+ randomx 118 (rand noise)) :y (+ randomy 20 (rand noise))))
				;(setf (aref candidate-party-object-array 4) (add-text-to-exp-window :text "party2" :x (+ randomx 118 (rand noise)) :y (+ randomy 35 (rand noise))))
				;(setf (aref candidate-party-object-array 5) (add-text-to-exp-window :text "party3" :x (+ randomx 118 (rand noise)) :y (+ randomy 50 (rand noise))))
				
				; (setf button_temp (add-button-to-exp-window :text "" :x randomx :y (+ randomy 22) :width 20 :height 10 :action 
				; (lambda (button)
				; (if (= (gethash button button-state) 0) (progn
					; (modify-text-for-exp-window (aref (gethash button button-map) 0) :color 'blue)
					; (modify-text-for-exp-window (aref (gethash button button-map) 3) :color 'blue)
					; (setf (gethash button button-state) 1)
					; )
					; (progn
					; (modify-text-for-exp-window (aref (gethash button button-map) 0) :color 'black)
					; (modify-text-for-exp-window (aref (gethash button button-map) 3) :color 'black)
					; (setf (gethash button button-state) 0)
					; )
				; )
				; )))
				; (setf (gethash button_temp button-map) candidate-party-object-array)
				; (setf (gethash button_temp button-state) 0)

				
				
				
				; (setf button_temp (add-button-to-exp-window :text "" :x randomx :y (+ randomy 38) :width 20 :height 10 :action 
				; (lambda (button)
				; (if (= (gethash button button-state) 0) (progn
					; (modify-text-for-exp-window (aref (gethash button button-map) 1) :color 'blue)
					; (modify-text-for-exp-window (aref (gethash button button-map) 4) :color 'blue)
					; (setf (gethash button button-state) 1)
					; )
					; (progn
					; (modify-text-for-exp-window (aref (gethash button button-map) 1) :color 'black)
					; (modify-text-for-exp-window (aref (gethash button button-map) 4) :color 'black)
					; (setf (gethash button button-state) 0)
					; )
				; )
				; )))
				; (setf (gethash button_temp button-map) candidate-party-object-array)
				; (setf (gethash button_temp button-state) 0)
				
				
				; (setf button_temp (add-button-to-exp-window :text "" :x randomx :y (+ randomy 53) :width 20 :height 10 :action 
				; (lambda (button)
				; (if (= (gethash button button-state) 0) (progn
					; (modify-text-for-exp-window (aref (gethash button button-map) 2) :color 'blue)
					; (modify-text-for-exp-window (aref (gethash button button-map) 5) :color 'blue)
					; (setf (gethash button button-state) 1)
					; )
					; (progn
					; (modify-text-for-exp-window (aref (gethash button button-map) 2) :color 'black)
					; (modify-text-for-exp-window (aref (gethash button button-map) 5) :color 'black)
					; (setf (gethash button button-state) 0)
					; )
				; )
				; )))
				; (setf (gethash button_temp button-map) candidate-party-object-array)
				; (setf (gethash button_temp button-state) 0)
			
			
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
