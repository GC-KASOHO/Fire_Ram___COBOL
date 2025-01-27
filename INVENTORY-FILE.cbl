       IDENTIFICATION DIVISION.
       PROGRAM-ID. InventoryManagement.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-FILE ASSIGN TO "INVENTORY.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.   
       FD INVENTORY-FILE.
       01 INVENTORY-RECORD.
           05 ITEM-ID PIC 9(5).
           05 ITEM-NAME PIC X(20).
           05 ITEM-PRICE PIC 9(5).
           05 ITEM-QUANTITY PIC 9(5).

       WORKING-STORAGE SECTION.
       01  UserChoice  PIC 9.
       01 FILE-STATUS PIC XX.
       01 I-ID PIC 9(5).
       01 I-NAME PIC X(20).
       01 I-PRICE PIC 9(5).
       01 I-QUANTITY PIC 9(5).

       PROCEDURE DIVISION.
           PERFORM INITIALIZE
           PERFORM UNTIL UserChoice = 6
               CALL "SYSTEM" USING "CLS"
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
               DISPLAY  "Enter your choice: " NO ADVANCING    
               ACCEPT UserChoice
               EVALUATE UserChoice
                  WHEN 1 PERFORM ADD-ITEM
                  WHEN 3 PERFORM VIEW-INVENTORY-ITEMS
                  WHEN 6 DISPLAY "Exiting..."
                  WHEN OTHER DISPLAY "Invalid choice, please try again."
               END-EVALUATE
           END-PERFORM
           CLOSE INVENTORY-FILE
           STOP RUN.

       INITIALIZE.
           OPEN I-O INVENTORY-FILE
           IF FILE-STATUS NOT = "00"
               DISPLAY "Error opening file."
               STOP RUN.

       ADD-ITEM.
           CALL "SYSTEM" USING "CLS"
           DISPLAY "Enter the ID of the product: " NO ADVANCING.
           ACCEPT I-ID.
           DISPLAY "Enter the name of the product: " NO ADVANCING.
           ACCEPT I-NAME.
           DISPLAY "Enter the price of the product: " NO ADVANCING.
           ACCEPT I-PRICE.
           DISPLAY  "Enter the quantity of the product: " NO ADVANCING.
           ACCEPT I-QUANTITY.
           
           MOVE I-ID TO ITEM-ID.
           MOVE I-NAME TO ITEM-NAME.
           MOVE I-PRICE TO ITEM-PRICE.
           MOVE I-QUANTITY TO ITEM-QUANTITY.

           WRITE INVENTORY-RECORD
           IF FILE-STATUS = "00"
               DISPLAY "Item added successfully."
           ELSE
               DISPLAY "Error writing to file."
           END-IF.

       VIEW-INVENTORY-ITEMS.
           CALL "SYSTEM" USING "CLS"
           DISPLAY "||===============================================||"
           DISPLAY "||               List of Items                   ||"
           DISPLAY "||===============================================||"
           
           OPEN INPUT INVENTORY-FILE
           PERFORM UNTIL FILE-STATUS = "10"  *> End of file
               READ INVENTORY-FILE INTO INVENTORY-RECORD
                   AT END
                       EXIT PERFORM
                   NOT AT END
                       DISPLAY "   ID: " ITEM-ID
                       DISPLAY "   Name: " ITEM-NAME
                       DISPLAY "   Price: " ITEM-PRICE
                       DISPLAY "   Quantity: " ITEM-QUANTITY
               END-READ
           END-PERFORM
           CLOSE INVENTORY-FILE
           DISPLAY "||===============================================||"
           DISPLAY "Press any key to return to the menu." 
           ACCEPT UserChoice.