
(CLEAR-ALL)
(DEFINE-MODEL COMBINED
              (SGP :V NIL :NEEDS-MOUSE T :ESC T :PROCESS-CURSOR T)
              (SGP :VISUAL-FINST-SPAN 10)
              (SETF VG-GLOMMING-RADIUS 8)
              (SETF VG-COLLISION-TYPE 'BOX)
              (SETF VG-NAMING-TYPE 'SEQUENTIAL)
              (SGP :ANS 0.3)
              (SGP :RT 0)
              (SETF *ACTR-ENABLED-P* T)
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
                          BUTTON-GROUP)
              (ADD-DM (VOTE ISA
                            MAKEVOTE
                            STATE
                            START-VOTING
                            DEFAULT
                            "DEM"
                            ENDSTATE
                            "nameofrace"
                            LEFT
                            -1
                            RIGHT
                            -1))
              (ADD-DM (GORDONBEARCE ISA CANDIDATE NAME "GordonBearce"
                       PARTY "Rep" RACE "PresidentoftheUnitedStates")
                      (VERNONSTANLEYALBURY ISA CANDIDATE NAME
                       "VernonStanleyAlbury" PARTY "Dem" RACE
                       "PresidentoftheUnitedStates")
                      (JANETTEFROMAN ISA CANDIDATE NAME
                       "JanetteFroman" PARTY "Lib" RACE
                       "PresidentoftheUnitedStates")
                      (CECILECADIEUX ISA CANDIDATE NAME
                       "CecileCadieux" PARTY "Rep" RACE
                       "UnitedStatesSenator")
                      (FERNBRZEZINSKI ISA CANDIDATE NAME
                       "FernBrzezinski" PARTY "Dem" RACE
                       "UnitedStatesSenator")
                      (COREYDERY ISA CANDIDATE NAME "CoreyDery" PARTY
                       "Ind" RACE "UnitedStatesSenator")
                      (PEDROBROUSE ISA CANDIDATE NAME "PedroBrouse"
                       PARTY "Rep" RACE
                       "UnitedStatesRepresentativeDistrict7")
                      (ROBERTMETTLER ISA CANDIDATE NAME
                       "RobertMettler" PARTY "Dem" RACE
                       "UnitedStatesRepresentativeDistrict7")
                      (GLENTRAVISLOZIER ISA CANDIDATE NAME
                       "GlenTravisLozier" PARTY "Rep" RACE
                       "Governor")
                      (RICKSTICKLES ISA CANDIDATE NAME "RickStickles"
                       PARTY "Dem" RACE "Governor")
                      (MAURICEHUMBLE ISA CANDIDATE NAME
                       "MauriceHumble" PARTY "Ind" RACE "Governor")
                      (SHANETERRIO ISA CANDIDATE NAME "ShaneTerrio"
                       PARTY "Rep" RACE "LieutenantGovernor")
                      (CASSIEPRINCIPE ISA CANDIDATE NAME
                       "CassiePrincipe" PARTY "Dem" RACE
                       "LieutenantGovernor")
                      (TIMSPEIGHT ISA CANDIDATE NAME "TimSpeight"
                       PARTY "Rep" RACE "AttorneyGeneral")
                      (RICKORGAN ISA CANDIDATE NAME "RickOrgan" PARTY
                       "Dem" RACE "AttorneyGeneral")
                      (THERESEGUSTIN ISA CANDIDATE NAME
                       "theresegustin" PARTY "Ind" RACE
                       "comptrollerofpublicaccounts")
                      (GREGCONVERSE ISA CANDIDATE NAME "GregConverse"
                       PARTY "Dem" RACE
                       "comptrollerofpublicaccounts")
                      (ELISEELLZEY ISA CANDIDATE NAME "eliseellzey"
                       PARTY "Dem" RACE
                       "commissionerofgenerallandoffice")
                      (SAMSADDLER ISA CANDIDATE NAME "SamSaddler"
                       PARTY "Rep" RACE
                       "commissionerofgenerallandoffice")
                      (POLLYRYLANDER ISA CANDIDATE NAME
                       "pollyrylander" PARTY "Rep" RACE
                       "commissionerofagriculture")
                      (ROBERTOARON ISA CANDIDATE NAME "RobertoAron"
                       PARTY "Dem" RACE "commissionerofagriculture")
                      (JILLIANBALAS ISA CANDIDATE NAME "jillianbalas"
                       PARTY "Rep" RACE "railroadcommissioner")
                      (ZACHARYMINICK ISA CANDIDATE NAME
                       "ZacharyMinick" PARTY "Dem" RACE
                       "railroadcommissioner")
                      (WESLEYSTEVEN-MILLETTE ISA CANDIDATE NAME
                       "wesleystevenmillette" PARTY "Dem" RACE
                       "statesenator")
                      (RICARDONIGRO ISA CANDIDATE NAME "RicardoNigro"
                       PARTY "Rep" RACE "statesenator")
                      (SUSANNERAEL ISA CANDIDATE NAME "susannerael"
                       PARTY "Dem" RACE
                       "stateRepresentativedistrict134")
                      (PETRABENCOMO ISA CANDIDATE NAME "PetraBencomo"
                       PARTY "Rep" RACE
                       "stateRepresentativedistrict134")
                      (PETERVARGA ISA CANDIDATE NAME "petervarga"
                       PARTY "Rep" RACE
                       "memberstateboardofeducationdistrict2")
                      (MARKBABER ISA CANDIDATE NAME "MarkBaber" PARTY
                       "Dem" RACE
                       "memberstateboardofeducationdistrict2")
                      (TIMGRASTY ISA CANDIDATE NAME "timgrasty" PARTY
                       "Dem" RACE
                       "presidingjudgetexassupremecourtplace2")
                      (DERRICKMELGAR ISA CANDIDATE NAME
                       "derrickmelgar" PARTY "Dem" RACE
                       "presidingjudgecourtofcriminalappeals")
                      (DANPLOUFFE ISA CANDIDATE NAME "DanPlouffe"
                       PARTY "Rep" RACE
                       "presidingjudgecourtofcriminalappeals")
                      (COREYBEHNKE ISA CANDIDATE NAME "coreybehnke"
                       PARTY "Rep" RACE "districtattorney")
                      (JENNIFERALUNDEED ISA CANDIDATE NAME
                       "JenniferALundeed" PARTY "Dem" RACE
                       "districtattorney")
                      (DEANCAFFEE ISA CANDIDATE NAME "deancaffee"
                       PARTY "Rep" RACE "countytreasurer")
                      (GORDONKALLAS ISA CANDIDATE NAME "GordonKallas"
                       PARTY "Dem" RACE "countytreasurer")
                      (JASONVALLE ISA CANDIDATE NAME "jasonvalle"
                       PARTY "Dem" RACE "sheriff")
                      (STANLEYSAARI ISA CANDIDATE NAME "StanleySaari"
                       PARTY "Rep" RACE "sheriff")
                      (HOWARDGRADY ISA CANDIDATE NAME "howardgrady"
                       PARTY "Rep" RACE "countytaxassessor")
                      (RANDYHCLEMONS ISA CANDIDATE NAME
                       "RandyHClemons" PARTY "Dem" RACE
                       "countytaxassessor")
                      (CLYDEGAYTONJR. ISA CANDIDATE NAME
                       "clydegaytonjr" PARTY "Dem" RACE
                       "justiceofthepeace")
                      (DEBORAHKAMPS ISA CANDIDATE NAME "DeborahKamps"
                       PARTY "Rep" RACE "justiceofthepeace")
                      (LEWISSHINE ISA CANDIDATE NAME "lewisshine"
                       PARTY "Dem" RACE "countyjudge")
                      (DANATCHLEY ISA CANDIDATE NAME "DanAtchley"
                       PARTY "Rep" RACE "countyjudge")
                      (PLACEHOLDER ISA CANDIDATE NAME "Candidate2"
                       PARTY "Dem" RACE "NameofRace")
                      (PLACEHOLDER2 ISA CANDIDATE NAME "Candidate3"
                       PARTY "Dem" RACE "LastRace")
                      (PARTY ISA VOTEPARTY DEFAULT "Dem"))
              (SDP GORDONBEARCE :BASE-LEVEL 0.7)
              (SDP COREYDERY :BASE-LEVEL 0.7)
              (SDP ROBERTMETTLER :BASE-LEVEL 0.7)
              (SDP RICKSTICKLES :BASE-LEVEL 0.7)
              (SDP CASSIEPRINCIPE :BASE-LEVEL 0.7)
              (SDP RICKORGAN :BASE-LEVEL 0.7)
              (SDP THERESEGUSTIN :BASE-LEVEL 0.7)
              (SDP ELISEELLZEY :BASE-LEVEL 0.6)
              (SDP POLLYRYLANDER :BASE-LEVEL 0.6)
              (SDP JILLIANBALAS :BASE-LEVEL 0.6)
              (SDP WESLEYSTEVEN-MILLETTE :BASE-LEVEL 0.6)
              (SDP SUSANNERAEL :BASE-LEVEL 0.6)
              (SDP PETERVARGA :BASE-LEVEL 0.6)
              (SDP TIMGRASTY :BASE-LEVEL 0.6)
              (SDP DERRICKMELGAR :BASE-LEVEL 0.5)
              (SDP COREYBEHNKE :BASE-LEVEL 0.5)
              (SDP DEANCAFFEE :BASE-LEVEL 0.5)
              (SDP JASONVALLE :BASE-LEVEL 0.5)
              (SDP HOWARDGRADY :BASE-LEVEL 0.5)
              (SDP CLYDEGAYTONJR. :BASE-LEVEL 0.5)
              (SDP LEWISSHINE :BASE-LEVEL 0.5)
              (SDP VERNONSTANLEYALBURY :BASE-LEVEL -0.4)
              (SDP JANETTEFROMAN :BASE-LEVEL -0.4)
              (SDP CECILECADIEUX :BASE-LEVEL -0.4)
              (SDP FERNBRZEZINSKI :BASE-LEVEL -0.4)
              (SDP PEDROBROUSE :BASE-LEVEL -0.4)
              (SDP GLENTRAVISLOZIER :BASE-LEVEL -0.4)
              (SDP MAURICEHUMBLE :BASE-LEVEL -0.4)
              (SDP SHANETERRIO :BASE-LEVEL -0.4)
              (SDP TIMSPEIGHT :BASE-LEVEL -0.4)
              (SDP GREGCONVERSE :BASE-LEVEL -0.4)
              (SDP SAMSADDLER :BASE-LEVEL -0.4)
              (SDP ROBERTOARON :BASE-LEVEL -0.4)
              (SDP ZACHARYMINICK :BASE-LEVEL -0.4)
              (SDP RICARDONIGRO :BASE-LEVEL -0.4)
              (SDP PETRABENCOMO :BASE-LEVEL -0.4)
              (SDP MARKBABER :BASE-LEVEL -0.4)
              (SDP DANPLOUFFE :BASE-LEVEL -0.4)
              (SDP JENNIFERALUNDEED :BASE-LEVEL -0.4)
              (SDP GORDONKALLAS :BASE-LEVEL -0.4)
              (SDP STANLEYSAARI :BASE-LEVEL -0.4)
              (SDP RANDYHCLEMONS :BASE-LEVEL -0.4)
              (SDP DEBORAHKAMPS :BASE-LEVEL -0.4)
              (SDP DANATCHLEY :BASE-LEVEL -0.4)
              (SDP VOTE :BASE-LEVEL 0.8)
              (P FIND-FIRST-RACE
                 =GOAL>
                 STATE
                 START-VOTING
                 LEFT
                 =LEFT-BOUND
                 ==>
                 +VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 >
                 SCREEN-X
                 =LEFT-BOUND
                 <
                 SCREEN-X
                 150
                 SCREEN-Y
                 LOWEST
                 =GOAL>
                 STATE
                 ATTENDING-RACE-NEXT-COLUMN
                 RIGHT
                 -1)
              (P FIND-RACE-SAME-COLUMN
                 =GOAL>
                 STATE
                 FIND-NEXT-RACE
                 LEFT
                 =LEFT-BOUND
                 RIGHT
                 =RIGHT-BOUND
                 =IMAGINAL>
                 RACE-GROUP
                 =RACE-GROUP
                 CANDIDATE-GROUP
                 =CANDIDATE-GROUP
                 PARTY-GROUP
                 =PARTY-GROUP
                 ==>
                 +VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 >
                 SCREEN-X
                 =LEFT-BOUND
                 <
                 SCREEN-X
                 =RIGHT-BOUND
                 >
                 SCREEN-Y
                 CURRENT
                 -
                 GROUP
                 =RACE-GROUP
                 -
                 GROUP
                 =CANDIDATE-GROUP
                 -
                 GROUP
                 =PARTY-GROUP
                 SCREEN-Y
                 LOWEST
                 KIND
                 TEXT
                 =IMAGINAL>
                 =GOAL>
                 STATE
                 ATTENDING-RACE-SAME-COLUMN
                 TO-DO
                 SELECTCANDIDATE)
              (P FIND-RACE-SAME-COLUMN-NO-MATCH
                 =GOAL>
                 STATE
                 ATTENDING-RACE-SAME-COLUMN
                 ?VISUAL-LOCATION>
                 BUFFER
                 FAILURE
                 ==>
                 =GOAL>
                 STATE
                 FIND-TOP-RACE)
              (P ATTEND-RACE-SAME-COLUMN
                 =GOAL>
                 STATE
                 ATTENDING-RACE-SAME-COLUMN
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 ?VISUAL>
                 STATE
                 FREE
                 ?IMAGINAL>
                 STATE
                 FREE
                 ==>
                 +IMAGINAL>
                 RACE-GROUP
                 NONE
                 CANDIDATE-GROUP
                 NONE
                 PARTY-GROUP
                 NONE
                 +VISUAL>
                 ISA
                 MOVE-ATTENTION
                 SCREEN-POS
                 =VISUAL-LOCATION
                 =VISUAL-LOCATION>
                 =GOAL>
                 STATE
                 STORING-RACE-GROUP)
              (P FIND-TOP-RACE
                 =GOAL>
                 STATE
                 FIND-TOP-RACE
                 LEFT
                 =LEFT-BOUND
                 RIGHT
                 =RIGHT-BOUND
                 ==>
                 +VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 >
                 SCREEN-X
                 =LEFT-BOUND
                 <
                 SCREEN-X
                 =RIGHT-BOUND
                 SCREEN-Y
                 LOWEST
                 KIND
                 TEXT
                 =GOAL>
                 STATE
                 ATTENDING-TOP-RACE)
              (P ATTEND-TOP-RACE
                 =GOAL>
                 STATE
                 ATTENDING-TOP-RACE
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 ?VISUAL>
                 STATE
                 FREE
                 ==>
                 +VISUAL>
                 ISA
                 MOVE-ATTENTION
                 SCREEN-POS
                 =VISUAL-LOCATION
                 =GOAL>
                 STATE
                 FIND-RACE-NEXT-COLUMN)
              (P FIND-RACE-NEXT-COLUMN
                 =GOAL>
                 STATE
                 FIND-RACE-NEXT-COLUMN
                 RIGHT
                 =RIGHT-BOUND
                 LEFT
                 =LEFT-BOUND
                 =VISUAL>
                 !BIND!
                 =RIGHT-GUESS
                 (+ =RIGHT-BOUND (- =RIGHT-BOUND =LEFT-BOUND))
                 ==>
                 +VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 SCREEN-Y
                 LOWEST
                 >
                 SCREEN-X
                 =RIGHT-BOUND
                 <
                 SCREEN-X
                 =RIGHT-GUESS
                 :NEAREST
                 CURRENT
                 =GOAL>
                 STATE
                 ATTENDING-RACE-NEXT-COLUMN)
              (P ATTEND-RACE-NEXT-COLUMN
                 =GOAL>
                 STATE
                 ATTENDING-RACE-NEXT-COLUMN
                 RIGHT
                 =OLD-RIGHT
                 ?IMAGINAL>
                 STATE
                 FREE
                 ?VISUAL>
                 STATE
                 FREE
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 SCREEN-X
                 =CENTER-X
                 WIDTH
                 =WIDTH
                 KIND
                 TEXT
                 !BIND!
                 =NEW-LEFT
                 (/ (+ =OLD-RIGHT (- =CENTER-X (/ =WIDTH 2))) 2)
                 ==>
                 +IMAGINAL>
                 RACE-GROUP
                 NONE
                 CANDIDATE-GROUP
                 NONE
                 PARTY-GROUP
                 NONE
                 +VISUAL>
                 ISA
                 MOVE-ATTENTION
                 SCREEN-POS
                 =VISUAL-LOCATION
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 =GOAL>
                 STATE
                 STORING-RACE-GROUP
                 LEFT
                 =NEW-LEFT)
              (P FIND-RACE-NEXT-COLUMN-NO-MATCH
                 =GOAL>
                 STATE
                 ATTENDING-RACE-NEXT-COLUMN
                 ?VISUAL-LOCATION>
                 BUFFER
                 FAILURE
                 ==>
                 =GOAL>
                 STATE
                 END)
              (P ENCODE-RACE
                 =GOAL>
                 STATE
                 STORING-RACE-GROUP
                 LEFT
                 =LEFT-BOUND
                 =VISUAL>
                 VALUE
                 =TEXT
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 GROUP
                 =GROUP1
                 SCREEN-Y
                 =SCREEN-Y
                 =IMAGINAL>
                 RACE-GROUP
                 NONE
                 ==>
                 +VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 >
                 SCREEN-Y
                 CURRENT
                 >
                 SCREEN-X
                 =LEFT-BOUND
                 <
                 SCREEN-X
                 CURRENT
                 :NEAREST
                 CURRENT
                 KIND
                 OVAL
                 =IMAGINAL>
                 RACE-GROUP
                 =GROUP1
                 =GOAL>
                 STATE
                 FIND-BUTTON-GROUP
                 RACE-TOP
                 =SCREEN-Y
                 !OUTPUT!
                 ("Example of race is: ~s" =TEXT))
              (P ATTEND-BUTTON
                 =GOAL>
                 STATE
                 FIND-BUTTON-GROUP
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 OVAL
                 ?VISUAL>
                 STATE
                 FREE
                 ==>
                 +VISUAL>
                 ISA
                 MOVE-ATTENTION
                 SCREEN-POS
                 =VISUAL-LOCATION
                 =VISUAL-LOCATION>
                 =GOAL>
                 STATE
                 STORING-BUTTON-GROUP)
              (P ENCODE-BUTTON
                 =GOAL>
                 STATE
                 STORING-BUTTON-GROUP
                 LEFT
                 =LEFT-BOUND
                 RACE-TOP
                 =TOP
                 =VISUAL>
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 OVAL
                 GROUP
                 =GROUP
                 =IMAGINAL>
                 -
                 RACE-GROUP
                 NONE
                 RACE-GROUP
                 =RACE-GROUP
                 ==>
                 +VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 >
                 SCREEN-X
                 =LEFT-BOUND
                 >
                 SCREEN-Y
                 =TOP
                 :NEAREST
                 CURRENT
                 -
                 GROUP
                 =RACE-GROUP
                 KIND
                 TEXT
                 =IMAGINAL>
                 =GOAL>
                 STATE
                 FIND-CANDIDATE-GROUP)
              (P ATTEND-CANDIDATE
                 =GOAL>
                 STATE
                 FIND-CANDIDATE-GROUP
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 ?VISUAL>
                 STATE
                 FREE
                 ==>
                 +VISUAL>
                 ISA
                 MOVE-ATTENTION
                 SCREEN-POS
                 =VISUAL-LOCATION
                 =VISUAL-LOCATION>
                 =GOAL>
                 STATE
                 STORING-CANDIDATE-GROUP)
              (P ENCODE-CANDIDATE
                 =GOAL>
                 STATE
                 STORING-CANDIDATE-GROUP
                 RACE-TOP
                 =TOP
                 =VISUAL>
                 VALUE
                 =TEXT
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 GROUP
                 =GROUP2
                 =IMAGINAL>
                 -
                 RACE-GROUP
                 NONE
                 CANDIDATE-GROUP
                 NONE
                 RACE-GROUP
                 =RACE-GROUP
                 ==>
                 +VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 GROUP
                 =GROUP2
                 SCREEN-Y
                 HIGHEST
                 =IMAGINAL>
                 CANDIDATE-GROUP
                 =GROUP2
                 =GOAL>
                 STATE
                 FIND-PARTY-GROUP-PART-1
                 !OUTPUT!
                 ("Example of candidate is: ~s" =TEXT))
              (P ATTEND-DIFFERENT-CANDIDATE
                 =GOAL>
                 STATE
                 FIND-PARTY-GROUP-PART-1
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 ?VISUAL>
                 STATE
                 FREE
                 ==>
                 +VISUAL>
                 ISA
                 MOVE-ATTENTION
                 SCREEN-POS
                 =VISUAL-LOCATION
                 =VISUAL-LOCATION>
                 =GOAL>
                 STATE
                 FIND-PARTY-GROUP-PART-2)
              (P FIND-PARTY
                 =GOAL>
                 STATE
                 FIND-PARTY-GROUP-PART-2
                 LEFT
                 =LEFT-BOUND
                 RACE-TOP
                 =TOP
                 =VISUAL>
                 =IMAGINAL>
                 -
                 RACE-GROUP
                 NONE
                 -
                 CANDIDATE-GROUP
                 NONE
                 CANDIDATE-GROUP
                 =CANDIDATE-GROUP
                 RACE-GROUP
                 =RACE-GROUP
                 ==>
                 +VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 >
                 SCREEN-X
                 CURRENT
                 <=
                 SCREEN-Y
                 CURRENT
                 >
                 SCREEN-Y
                 =TOP
                 -
                 GROUP
                 =CANDIDATE-GROUP
                 -
                 GROUP
                 =RACE-GROUP
                 :NEAREST
                 CURRENT
                 =GOAL>
                 STATE
                 FIND-PARTY-GROUP-PART-3
                 =IMAGINAL>)
              (P ATTEND-PARTY
                 =GOAL>
                 STATE
                 FIND-PARTY-GROUP-PART-3
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 WIDTH
                 =WIDTH
                 SCREEN-X
                 =MIDDLE-X
                 ?VISUAL>
                 STATE
                 FREE
                 !BIND!
                 =NEW-RIGHT
                 (+ (/ =WIDTH 2) =MIDDLE-X)
                 ==>
                 +VISUAL>
                 ISA
                 MOVE-ATTENTION
                 SCREEN-POS
                 =VISUAL-LOCATION
                 =VISUAL-LOCATION>
                 =GOAL>
                 STATE
                 STORING-PARTY-GROUP
                 RIGHT
                 =NEW-RIGHT)
              (P ENCODE-PARTY
                 =GOAL>
                 STATE
                 STORING-PARTY-GROUP
                 =VISUAL>
                 VALUE
                 =TEXT
                 ?VISUAL>
                 STATE
                 FREE
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 GROUP
                 =GROUP3
                 =IMAGINAL>
                 -
                 RACE-GROUP
                 NONE
                 -
                 CANDIDATE-GROUP
                 NONE
                 PARTY-GROUP
                 NONE
                 ==>
                 =IMAGINAL>
                 PARTY-GROUP
                 =GROUP3
                 =GOAL>
                 STATE
                 READY-TO-MAKE-CHOICE
                 +VISUAL>
                 ISA
                 CLEAR-ALL-FINSTS
                 !OUTPUT!
                 ("Example of party is: ~s" =TEXT))
              (P SELECT-CHOICE_LOCATE-CONTEST-DESCRIPTION
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 READY-TO-MAKE-CHOICE
                 =IMAGINAL>
                 RACE-GROUP
                 =VAL1
                 ?VISUAL-LOCATION>
                 STATE
                 FREE
                 ==>
                 =IMAGINAL>
                 +VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 GROUP
                 =VAL1
                 =GOAL>
                 STATE
                 FOUND-CONTEST-DESCRIPTION
                 !EVAL!
                 (SETF CURRENT-STRAT 'RETRIEVAL))
              (P SELECT-CHOICE_ATTEND-CONTEST-DESCRIPTION
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 FOUND-CONTEST-DESCRIPTION
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 GROUP
                 =VAL1
                 ?VISUAL>
                 STATE
                 FREE
                 ==>
                 +VISUAL>
                 ISA
                 MOVE-ATTENTION
                 SCREEN-POS
                 =VISUAL-LOCATION
                 =GOAL>
                 STATE
                 ATTENDED-CONTEST-DESCRIPTION)
              (P CHECK-CONTEST
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 ATTENDED-CONTEST-DESCRIPTION
                 =VISUAL>
                 ISA
                 TEXT
                 VALUE
                 =TEXTVAL
                 ?RETRIEVAL>
                 STATE
                 FREE
                 =IMAGINAL>
                 ==>
                 +RETRIEVAL>
                 ISA
                 ABSTAIN
                 CONTEST
                 =TEXTVAL
                 =IMAGINAL>
                 ISA
                 MAKEVOTE
                 RACE
                 =TEXTVAL
                 =GOAL>
                 STATE
                 CHECKING-CONTEST)
              (P ABSTAIN
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 CHECKING-CONTEST
                 =RETRIEVAL>
                 ISA
                 ABSTAIN
                 CONTEST
                 =THIS
                 ==>
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 FIND-NEXT-RACE)
              (P SELECT-CHOICE_ENCODE-CONTEST-DESCRIPTION
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 CHECKING-CONTEST
                 ?RETRIEVAL>
                 BUFFER
                 FAILURE
                 =IMAGINAL>
                 ISA
                 MAKEVOTE
                 RACE
                 =TEXTVAL
                 ==>
                 =IMAGINAL>
                 +RETRIEVAL>
                 ISA
                 CANDIDATE
                 RACE
                 =TEXTVAL
                 =GOAL>
                 STATE
                 ENCODED-CONTEST-DESCRIPTION
                 !OUTPUT!
                 ("Contest is: ~s" =TEXTVAL))
              (P PAST-END-STATE
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 ENCODED-CONTEST-DESCRIPTION
                 ENDSTATE
                 =END
                 =IMAGINAL>
                 ISA
                 MAKEVOTE
                 RACE
                 =END
                 ==>
                 =IMAGINAL>
                 +GOAL>
                 ISA
                 CLEAR)
              (P RETRIEVAL-SUCCESS
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 ENCODED-CONTEST-DESCRIPTION
                 =RETRIEVAL>
                 ISA
                 CANDIDATE
                 RACE
                 =R
                 PARTY
                 =P
                 =IMAGINAL>
                 ==>
                 =RETRIEVAL>
                 =IMAGINAL>
                 ISA
                 MAKEVOTE
                 RACE
                 =R
                 PARTY
                 =P
                 =GOAL>
                 STATE
                 SEARCH-SCREEN
                 !OUTPUT!
                 ("I'm voting for: ~s" =P))
              (P SELECT-CHOICE_SEARCH-SCREEN-FASTEST
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 SEARCH-SCREEN
                 ?VISUAL-LOCATION>
                 STATE
                 FREE
                 =IMAGINAL>
                 PARTY-GROUP
                 =VAL
                 ==>
                 =IMAGINAL>
                 +VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 GROUP
                 =VAL
                 :ATTENDED
                 NIL
                 =GOAL>
                 STATE
                 SOMETHING-FOUND)
              (P SELECT-CHOICE_ATTEND-SEARCH
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 SOMETHING-FOUND
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 GROUP
                 =VAL
                 ?VISUAL>
                 STATE
                 FREE
                 ==>
                 +VISUAL>
                 ISA
                 MOVE-ATTENTION
                 SCREEN-POS
                 =VISUAL-LOCATION
                 =GOAL>
                 STATE
                 ATTENDING-SOMETHING-FOUND)
              (P SELECT-CHOICE_ENCODE-SEARCH
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 ATTENDING-SOMETHING-FOUND
                 =VISUAL>
                 ISA
                 TEXT
                 VALUE
                 =VAL
                 SCREEN-POS
                 =POS
                 ?VISUAL-LOCATION>
                 STATE
                 FREE
                 ==>
                 =VISUAL>
                 =GOAL>
                 STATE
                 ENCODED-SEARCH
                 !OUTPUT!
                 ("Looking at Party: ~s" =VAL))
              (P SELECT-CHOICE_IMAGINAL-MATCH-STOP
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 ENCODED-SEARCH
                 =IMAGINAL>
                 ISA
                 MAKEVOTE
                 PARTY
                 =VAL
                 =VISUAL>
                 ISA
                 TEXT
                 VALUE
                 =VAL
                 SCREEN-POS
                 =POS
                 ?VISUAL-LOCATION>
                 STATE
                 FREE
                 ?MANUAL>
                 STATE
                 FREE
                 ==>
                 =IMAGINAL>
                 =VISUAL>
                 +MANUAL>
                 ISA
                 MOVE-CURSOR
                 LOC
                 =POS
                 =GOAL>
                 STATE
                 MOVED-TO-CANDIDATE)
              (P SELECT-CHOICE-NO-MATCH
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 ENCODED-SEARCH
                 ?VISUAL-LOCATION>
                 STATE
                 FREE
                 ==>
                 =GOAL>
                 STATE
                 SEARCH-SCREEN)
              (P VBP-RETRIEVAL-FAILS
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 ENCODED-CONTEST-DESCRIPTION
                 ?RETRIEVAL>
                 BUFFER
                 FAILURE
                 ==>
                 =GOAL>
                 STATE
                 SEARCH-BY-PARTY
                 !OUTPUT!
                 ("Initial retrieval failure, voting by party")
                 !EVAL!
                 (SETF CURRENT-STRAT 'PARTY))
              (P VBP-RETRIEVAL-FAILS-AFTER-SEARCHING
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 SOMETHING-FOUND
                 ?VISUAL-LOCATION>
                 BUFFER
                 FAILURE
                 ==>
                 =GOAL>
                 STATE
                 SEARCH-BY-PARTY
                 !OUTPUT!
                 ("Looked at everything and nothing retrieved-- voting by party")
                 !EVAL!
                 (SETF CURRENT-STRAT 'PARTY))
              (P VBP-SELECT-CHOICE_SEARCH-SCREEN-FASTEST
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 SEARCH-BY-PARTY
                 ?RETRIEVAL>
                 STATE
                 FREE
                 ?VISUAL-LOCATION>
                 STATE
                 FREE
                 =IMAGINAL>
                 PARTY-GROUP
                 =VAL3
                 ==>
                 =IMAGINAL>
                 +VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 GROUP
                 =VAL3
                 :ATTENDED
                 NIL
                 =GOAL>
                 STATE
                 VBP-SOMETHING-FOUND)
              (P VBP-SELECT-CHOICE_ATTEND-SEARCH
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 VBP-SOMETHING-FOUND
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 TEXT
                 GROUP
                 =VAL3
                 ?VISUAL>
                 STATE
                 FREE
                 ==>
                 +VISUAL>
                 ISA
                 MOVE-ATTENTION
                 SCREEN-POS
                 =VISUAL-LOCATION
                 =GOAL>
                 STATE
                 VBP-ATTENDING-SOMETHING-FOUND)
              (P VBP-ENCODE-SEARCH-MATCH
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 VBP-ATTENDING-SOMETHING-FOUND
                 DEFAULT
                 =PARTY
                 =VISUAL>
                 ISA
                 TEXT
                 VALUE
                 =PARTY
                 SCREEN-POS
                 =POS
                 ?MANUAL>
                 STATE
                 FREE
                 ==>
                 =VISUAL>
                 +MANUAL>
                 ISA
                 MOVE-CURSOR
                 LOC
                 =POS
                 =GOAL>
                 STATE
                 MOVED-TO-CANDIDATE
                 !OUTPUT!
                 ("Matches party ~s" =PARTY))
              (P VBP-NO-MATCH-NEXT-CHOICE
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 VBP-ATTENDING-SOMETHING-FOUND
                 DEFAULT
                 =PARTY
                 =VISUAL>
                 ISA
                 TEXT
                 -
                 VALUE
                 =PARTY
                 VALUE
                 =NOTPARTY
                 ==>
                 =GOAL>
                 STATE
                 SEARCH-BY-PARTY
                 !OUTPUT!
                 ("Party ~s does not match default" =NOTPARTY))
              (P VBP-BOTTOM-OF-LIST-FAIL
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 VBP-SOMETHING-FOUND
                 ?VISUAL-LOCATION>
                 BUFFER
                 FAILURE
                 ==>
                 =GOAL>
                 STATE
                 FIND-NEXT-RACE
                 !OUTPUT!
                 ("VBP reached bottom of list-- abstain from voting"))
              (SPP SELECT-CHOICE_IMAGINAL-MATCH-STOP :U 1000)
              (SPP SELECT-CHOICE_SEARCH-SCREEN-FASTEST :U 8)
              (SPP CHECK-CONTEST :U 1000)
              (P FIND-BUBBLE
                 =GOAL>
                 STATE
                 MOVED-TO-CANDIDATE
                 LEFT
                 =LEFT-BOUND
                 RIGHT
                 =RIGHT-BOUND
                 ==>
                 +VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 OVAL
                 >
                 SCREEN-X
                 =LEFT-BOUND
                 <
                 SCREEN-X
                 =RIGHT-BOUND
                 :NEAREST
                 CURRENT
                 =GOAL>
                 STATE
                 ATTENDING-BUBBLE)
              (P MOVE-MOUSE-TO-BUBBLE
                 =GOAL>
                 STATE
                 ATTENDING-BUBBLE
                 ?MANUAL>
                 STATE
                 FREE
                 =VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 KIND
                 OVAL
                 ==>
                 +MANUAL>
                 ISA
                 MOVE-CURSOR
                 LOC
                 =VISUAL-LOCATION
                 =GOAL>
                 STATE
                 MOVING-MOUSE-TO-BUBBLE)
              (P CLICK-BUBBLE
                 =GOAL>
                 STATE
                 MOVING-MOUSE-TO-BUBBLE
                 ?MANUAL>
                 STATE
                 FREE
                 ==>
                 +MANUAL>
                 ISA
                 CLICK-MOUSE
                 =GOAL>
                 STATE
                 FIND-NEXT-RACE)
              (GOAL-FOCUS VOTE))