       IDENTIFICATION DIVISION.
       PROGRAM-ID. InventoryManagement.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  UserChoice         PIC 9.

       PROCEDURE DIVISION.
           PERFORM UNTIL UserChoice = 6
               DISPLAY "||=======================================||"
               DISPLAY "||     Inventory Management System       ||"
               DISPLAY "||=======================================||"
               DISPLAY "||   1. Add Item                         ||"
               DISPLAY "||   2. Update Item                      ||"
               DISPLAY "||   3. View Inventory                   ||"
               DISPLAY "||   4. Search Item                      ||"
               DISPLAY "||   5. Delete Item                      ||"
               DISPLAY "||   6. Exit                             ||"         
               DISPLAY "||=======================================||"
               DISPLAY "||   Enter your choice:                  ||"
               DISPLAY "||=======================================||"
               ACCEPT UserChoice
               EVALUATE UserChoice
      *            WHEN 1
      *                
      *            WHEN 2
      *                 
      *            WHEN 3
      *                
      *            WHEN 4
      *            
      *            WHEN 5
      *               
                   WHEN 6
                       DISPLAY "Exiting..."
                   WHEN OTHER
                       DISPLAY "Invalid choice, please try again."
               END-EVALUATE
           END-PERFORM
           STOP RUN.

