; Parameters
(sgp :v nil :needs-mouse t :esc t :process-cursor t)
		
(sgp :visual-finst-span 10) ;neccesary to avoid forgetting where we looked for the recognition strategies

; Grouping Parameters
(setf vg-glomming-radius 8)
(setf vg-collision-type 'box) ;'point is faster, but less plausible (also needs a larger radius to work similarly)
(setf vg-naming-type 'sequential) 
		
;********************************
; Enable forgetting / Activation 
(sgp :ans 0.3)
(sgp :rt 0)
;********************************

; (sgp :act t)

(setf *actr-enabled-p* t)

; Declarative Memory Chunk Types
(chunk-type MakeVote race candidate party button position screen state handpos to-do found default endState left right)
(chunk-type Candidate name party race)
(chunk-type VoteParty default)
(chunk-type Abstain contest)
(chunk-type VisualGroup race-group candidate-group party-group nextpage-group nextpage-text party-text candidate-text race-text button-group)
		
; First Goal (Must be called vote)
(add-dm (Vote ISA MakeVote state start-voting default "DEM" endState "nameofrace" left -1 right -1))