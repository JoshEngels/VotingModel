
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
                      (COREYDERY ISA CANDIDATE NAME "CoreyDery" PARTY
                       "Ind" RACE "UnitedStatesSenator")
                      (ROBERTMETTLER ISA CANDIDATE NAME
                       "RobertMettler" PARTY "Dem" RACE
                       "UnitedStatesRepresentativeDistrict7")
                      (RICKSTICKLES ISA CANDIDATE NAME "RickStickles"
                       PARTY "Dem" RACE "Governor")
                      (CASSIEPRINCIPE ISA CANDIDATE NAME
                       "CassiePrincipe" PARTY "Dem" RACE
                       "LieutenantGovernor")
                      (RICKORGAN ISA CANDIDATE NAME "RickOrgan" PARTY
                       "Dem" RACE "AttorneyGeneral")
                      (THERESEGUSTIN ISA CANDIDATE NAME
                       "theresegustin" PARTY "Ind" RACE
                       "comptrollerofpublicaccounts")
                      (ELISEELLZEY ISA CANDIDATE NAME "eliseellzey"
                       PARTY "Dem" RACE
                       "commissionerofgenerallandoffice")
                      (POLLYRYLANDER ISA CANDIDATE NAME
                       "pollyrylander" PARTY "Rep" RACE
                       "commissionerofagriculture")
                      (JILLIANBALAS ISA CANDIDATE NAME "jillianbalas"
                       PARTY "Rep" RACE "railroadcommissioner")
                      (WESLEYSTEVEN-MILLETTE ISA CANDIDATE NAME
                       "wesleystevenmillette" PARTY "Dem" RACE
                       "statesenator")
                      (SUSANNERAEL ISA CANDIDATE NAME "susannerael"
                       PARTY "Dem" RACE
                       "stateRepresentativedistrict134")
                      (PETERVARGA ISA CANDIDATE NAME "petervarga"
                       PARTY "Rep" RACE
                       "memberstateboardofeducationdistrict2")
                      (TIMGRASTY ISA CANDIDATE NAME "timgrasty" PARTY
                       "Dem" RACE
                       "presidingjudgetexassupremecourtplace2")
                      (DERRICKMELGAR ISA CANDIDATE NAME
                       "derrickmelgar" PARTY "Dem" RACE
                       "presidingjudgecourtofcriminalappeals")
                      (COREYBEHNKE ISA CANDIDATE NAME "coreybehnke"
                       PARTY "Rep" RACE "districtattorney")
                      (DEANCAFFEE ISA CANDIDATE NAME "deancaffee"
                       PARTY "Rep" RACE "countytreasurer")
                      (JASONVALLE ISA CANDIDATE NAME "jasonvalle"
                       PARTY "Dem" RACE "sheriff")
                      (HOWARDGRADY ISA CANDIDATE NAME "howardgrady"
                       PARTY "Rep" RACE "countytaxassessor")
                      (CLYDEGAYTONJR. ISA CANDIDATE NAME
                       "clydegaytonjr" PARTY "Dem" RACE
                       "justiceofthepeace")
                      (LEWISSHINE ISA CANDIDATE NAME "lewisshine"
                       PARTY "Dem" RACE "countyjudge")
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
                 (SETF CURRENT-STRAT 'RECOGNITION))
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
              (P ENCODE-CONTEST-DESCRIPTION
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
                 ==>
                 +RETRIEVAL>
                 ISA
                 ABSTAIN
                 CONTEST
                 =TEXTVAL
                 =GOAL>
                 STATE
                 ENCODED-CONTEST-DESCRIPTION
                 !OUTPUT!
                 ("Contest is: ~s" =TEXTVAL))
              (P CHECK-CONTEST
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 ENCODED-CONTEST-DESCRIPTION
                 =RETRIEVAL>
                 ISA
                 ABSTAIN
                 CONTEST
                 =RACE
                 ==>
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 FIND-NEXT-RACE)
              (P PAST-END-STATE
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 ATTENDED-CONTEST-DESCRIPTION
                 ENDSTATE
                 =END
                 =VISUAL>
                 ISA
                 TEXT
                 VALUE
                 =END
                 ==>
                 +GOAL>
                 ISA
                 CLEAR)
              (P SELECT-CHOICE_SEARCH-SCREEN-ORDERED
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 ENCODED-CONTEST-DESCRIPTION
                 ?RETRIEVAL>
                 BUFFER
                 FAILURE
                 =IMAGINAL>
                 CANDIDATE-GROUP
                 =VAL2
                 ==>
                 =IMAGINAL>
                 +VISUAL-LOCATION>
                 ISA
                 VISUAL-LOCATION
                 GROUP
                 =VAL2
                 KIND
                 TEXT
                 >
                 SCREEN-Y
                 CURRENT
                 SCREEN-Y
                 LOWEST
                 SCREEN-X
                 LOWEST
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
                 =VAL2
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
                 ?VISUAL-LOCATION>
                 STATE
                 FREE
                 ?RETRIEVAL>
                 STATE
                 FREE
                 ==>
                 =VISUAL>
                 +RETRIEVAL>
                 ISA
                 CANDIDATE
                 NAME
                 =VAL
                 =GOAL>
                 STATE
                 ENCODED-CONTEST
                 !OUTPUT!
                 ("Looking at Candidate: ~s" =VAL))
              (P SELECT-CHOICE_MATCH-STOP
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 ENCODED-CONTEST
                 =RETRIEVAL>
                 ISA
                 CANDIDATE
                 NAME
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
                 =RETRIEVAL>
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
                 ("Match! Voting for: ~s" =VAL))
              (P SELECT-CHOICE_NO-MATCH
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 ENCODED-CONTEST
                 ?VISUAL-LOCATION>
                 STATE
                 FREE
                 ?RETRIEVAL>
                 BUFFER
                 FAILURE
                 ==>
                 =RETRIEVAL>
                 =GOAL>
                 STATE
                 ENCODED-CONTEST-DESCRIPTION
                 !OUTPUT!
                 ("Name does not match. Read another."))
              (P SELECT-CHOICE_NO-MATCH-VOTEBYPARTY
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 SOMETHING-FOUND
                 ?VISUAL-LOCATION>
                 BUFFER
                 FAILURE
                 ==>
                 =VISUAL-LOCATION>
                 =GOAL>
                 STATE
                 READ-BY-PARTY
                 !OUTPUT!
                 ("Bottom of list and nothing matches. Voting by party.")
                 !EVAL!
                 (SETF CURRENT-STRAT 'PARTY))
              (P VBP-SELECT-CHOICE_SEARCH-SCREEN-ORDERED
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 READ-BY-PARTY
                 ?RETRIEVAL>
                 STATE
                 FREE
                 ?VISUAL-LOCATION>
                 STATE
                 FREE
                 ?VISUAL>
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
                 GROUP
                 =VAL3
                 KIND
                 TEXT
                 :ATTENDED
                 NIL
                 SCREEN-Y
                 LOWEST
                 =GOAL>
                 STATE
                 VBP-ATTEND-NAME)
              (P VBP-SELECT-CHOICE_ATTEND-SEARCH
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 VBP-ATTEND-NAME
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
                 PARTY-FOUND)
              (P VBP-ENCODE-SEARCH-MATCH
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 PARTY-FOUND
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
              (P VOTE-BY-PARTY_NO-MATCH
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 PARTY-FOUND
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
                 READ-BY-PARTY
                 !OUTPUT!
                 ("Party ~s does not match default" =NOTPARTY)
                 !OUTPUT!
                 ("Default party is: ~s" =PARTY))
              (P VOTE-BY-PARTY_NO-MATCH-ABSTAIN
                 =GOAL>
                 ISA
                 MAKEVOTE
                 STATE
                 VBP-ATTEND-NAME
                 ?VISUAL-LOCATION>
                 BUFFER
                 FAILURE
                 ==>
                 =GOAL>
                 STATE
                 FIND-NEXT-RACE
                 !OUTPUT!
                 ("Reached the end of the search and nothing matches-- Abstain"))
              (SPP PAST-END-STATE :AT 0 :U 4000)
              (SPP HALT! :AT 0 :U 4000)
              (SPP SELECT-CHOICE_MATCH-STOP :U 1000)
              (SPP SELECT-CHOICE_SEARCH-SCREEN-ORDERED :U 1000)
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