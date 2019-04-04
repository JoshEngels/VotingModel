
(CLEAR-ALL)
(DEFINE-MODEL COMBINED
              (SGP :V
                   T
                   :NEEDS-MOUSE
                   T
                   :SHOW-FOCUS
                   T
                   :ESC
                   T
                   :PROCESS-CURSOR
                   T)
              (SGP :VISUAL-FINST-SPAN 100)
              (SETF VG-GLOMMING-RADIUS 8)
              (SETF VG-COLLISION-TYPE 'BOX)
              (SETF VG-NAMING-TYPE 'SEQUENTIAL)
              (CHUNK-TYPE MAKEVOTE
                          RACE
                          CANDIDATE
                          PARTY
                          BUTTON
                          POSITION
                          SCREEN
                          STATE
                          HANDPOS
                          TO-DO
                          FOUND
                          DEFAULT
                          ENDSTATE
                          LEFT
                          RIGHT)
              (CHUNK-TYPE CANDIDATE NAME PARTY RACE)
              (CHUNK-TYPE VOTEPARTY DEFAULT)
              (CHUNK-TYPE ABSTAIN CONTEST)
              (CHUNK-TYPE VISUALGROUP
                          RACE-GROUP
                          CANDIDATE-GROUP
                          PARTY-GROUP
                          NEXTPAGE-GROUP
                          NEXTPAGE-TEXT
                          PARTY-TEXT
                          CANDIDATE-TEXT
                          RACE-TEXT
                          BUTTON-GROUP))