       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         rsv.

       DATA                DIVISION.
       WORKING-STORAGE     SECTION.
       01  file-handle     PIC X(4)   USAGE COMP-X.
       01  filename        PIC X(256) VALUE "Test.rsv".
       01  offset          PIC X(8)   USAGE COMP-X.
       01  eov-buffer      PIC X(1)   VALUE X"FF".
       01  nv-buffer       PIC X(2)   VALUE X"FE".
       01  eor-buffer      PIC X(1)   VALUE X"FD".

       01  buffer          PIC X(1024).
       01  numBytes        PIC X(4)   USAGE COMP-X.

       PROCEDURE DIVISION.
           PERFORM START-WRITING-RSV.

      *--- Row 1 -------------------------------------------------------
           MOVE "Hello" TO buffer.
           MOVE 5 TO numBytes.
           PERFORM WRITE-STRING-DATA.
           PERFORM WRITE-END-OF-VALUE.

           MOVE "🌎" TO buffer.
           MOVE 4 TO numBytes.
           PERFORM WRITE-STRING-DATA.
           PERFORM WRITE-END-OF-VALUE.

           PERFORM WRITE-NULL-VALUE.
           PERFORM WRITE-END-OF-VALUE.

           PERFORM WRITE-END-OF-VALUE.

           PERFORM WRITE-END-OF-ROW.

      *--- Row 2 -------------------------------------------------------
           MOVE "A" & X"00" & "B" & X"0A" & "C" TO buffer.
           MOVE 5 TO numBytes.
           PERFORM WRITE-STRING-DATA.
           PERFORM WRITE-END-OF-VALUE.

           MOVE "Test 𝄞" TO buffer.
           MOVE 9 TO numBytes.
           PERFORM WRITE-STRING-DATA.
           PERFORM WRITE-END-OF-VALUE.

           PERFORM WRITE-END-OF-ROW.

      *--- Row 3 -------------------------------------------------------
           PERFORM WRITE-END-OF-ROW.

      *--- Row 4 -------------------------------------------------------
           PERFORM WRITE-END-OF-VALUE.

           PERFORM WRITE-END-OF-ROW.

      *--- End of file -------------------------------------------------
           PERFORM FINISH-WRITING-RSV.

           DISPLAY "Done".
           STOP RUN.

       START-WRITING-RSV.
           CALL "CBL_CREATE_FILE"
               USING filename, 2, 0, 0, file-handle.
           IF RETURN-CODE <> 0
               DISPLAY "Could not create file: " RETURN-CODE
           END-IF
           MOVE 0 TO offset.
           EXIT.

       WRITE-STRING-DATA.
           CALL "CBL_WRITE_FILE"
               USING file-handle, offset, numBytes, 0, buffer.
           IF RETURN-CODE <> 0
               DISPLAY "Could not write string data: " RETURN-CODE
           END-IF
           ADD numBytes TO offset.
           EXIT.

       WRITE-END-OF-VALUE.
           MOVE 1 TO numBytes.
           CALL "CBL_WRITE_FILE"
               USING file-handle, offset, numBytes, 0, eov-buffer.
           IF RETURN-CODE <> 0
               DISPLAY "Could not write EOV: " RETURN-CODE
           END-IF
           ADD numBytes TO offset.
           EXIT.

       WRITE-NULL-VALUE.
           MOVE 1 TO numBytes.
           CALL "CBL_WRITE_FILE"
               USING file-handle, offset, numBytes, 0, nv-buffer.
           IF RETURN-CODE <> 0
               DISPLAY "Could not write NV: " RETURN-CODE
           END-IF
           ADD numBytes TO offset.
           EXIT.

       WRITE-END-OF-ROW.
           MOVE 1 TO numBytes.
           CALL "CBL_WRITE_FILE"
               USING file-handle, offset, numBytes, 0, eor-buffer.
           IF RETURN-CODE <> 0
               DISPLAY "Could not write EOR: " RETURN-CODE
           END-IF
           ADD numBytes TO offset.
           EXIT.

       FINISH-WRITING-RSV.
           CALL "CBL_CLOSE_FILE" USING file-handle.
           IF RETURN-CODE <> 0
               DISPLAY "Could not close file: " RETURN-CODE
           END-IF
           EXIT.
