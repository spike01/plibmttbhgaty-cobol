       IDENTIFICATION DIVISION.
       PROGRAM-ID. TICTACTOE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PLAYER-INPUT PIC 9(2).
       01 CURRENT-MOVE PIC A VALUE "X".
       01 TMP-MOVE PIC A.
       01 NEXT-MOVE PIC A VALUE "O".
       01 GAME-OVER PIC 9 VALUE 0.

       01 BOARD.
         05 CELL PIC A OCCURS 9 TIMES.

       01 WINNING-COMBINATIONS.
         05 IDX PIC 9 OCCURS 3 TIMES.
       
       PROCEDURE DIVISION.
           DISPLAY "COBOL TAC TOE v0.1"
           DISPLAY "================="

           PERFORM GAME-LOOP UNTIL GAME-OVER IS EQUAL TO 1

           GOBACK.

       GAME-LOOP.
           PERFORM SHOW-BOARD 
           PERFORM TAKE-INPUT
           PERFORM CHECK-WIN
           PERFORM CHANGE-PLAYER
           .

       SHOW-BOARD.
           DISPLAY "     "
           DISPLAY CELL(1)"|"CELL(2)"|"CELL(3)
           DISPLAY "-----"
           DISPLAY CELL(4)"|"CELL(5)"|"CELL(6)
           DISPLAY "-----"
           DISPLAY CELL(7)"|"CELL(8)"|"CELL(9)
           DISPLAY "     "
           .

       CHECK-WIN.
           MOVE 1 TO IDX(1)
           MOVE 2 TO IDX(2)
           MOVE 3 TO IDX(3)
           PERFORM CHECK-LINE
           MOVE 4 TO IDX(1)
           MOVE 5 TO IDX(2)
           MOVE 6 TO IDX(3)
           PERFORM CHECK-LINE
           MOVE 7 TO IDX(1)
           MOVE 8 TO IDX(2)
           MOVE 9 TO IDX(3)
           PERFORM CHECK-LINE
           MOVE 1 TO IDX(1)
           MOVE 4 TO IDX(2)
           MOVE 7 TO IDX(3)
           PERFORM CHECK-LINE
           MOVE 2 TO IDX(1)
           MOVE 5 TO IDX(2)
           MOVE 8 TO IDX(3)
           PERFORM CHECK-LINE
           MOVE 3 TO IDX(1)
           MOVE 6 TO IDX(2)
           MOVE 7 TO IDX(3)
           PERFORM CHECK-LINE
           MOVE 1 TO IDX(1)
           MOVE 3 TO IDX(2)
           MOVE 9 TO IDX(3)
           PERFORM CHECK-LINE
           MOVE 3 TO IDX(1)
           MOVE 5 TO IDX(2)
           MOVE 7 TO IDX(3)
           PERFORM CHECK-LINE
           .

       TAKE-INPUT.
           ACCEPT PLAYER-INPUT

           IF CELL(PLAYER-INPUT) IS EQUAL TO "X" OR CELL(PLAYER-INPUT) 
             IS EQUAL TO "0" THEN
             DISPLAY "CELL TAKEN. PICK ANOTHER CELL"
             PERFORM TAKE-INPUT
             END-IF

             IF PLAYER-INPUT IS GREATER THAN 9 OR PLAYER-INPUT IS LESS 
               THAN 1 THEN
               DISPLAY "OUT OF RANGE. PICK ANOTHER CELL"
               PERFORM TAKE-INPUT
             END-IF

             SET CELL(PLAYER-INPUT) TO CURRENT-MOVE
             .

       CHANGE-PLAYER.
           MOVE CURRENT-MOVE TO TMP-MOVE
           MOVE NEXT-MOVE TO CURRENT-MOVE
           MOVE TMP-MOVE TO NEXT-MOVE
           .

       CHECK-LINE.
           IF CELL(IDX(1)) IS EQUAL TO CURRENT-MOVE 
             AND CELL(IDX(2)) IS EQUAL TO CURRENT-MOVE 
             AND CELL(IDX(3)) IS EQUAL TO CURRENT-MOVE THEN
             MOVE 1 TO GAME-OVER
             DISPLAY CURRENT-MOVE " IS THE WINNER."
           END-IF
           .

           END PROGRAM TICTACTOE. 
