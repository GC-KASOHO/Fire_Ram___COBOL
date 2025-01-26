       IDENTIFICATION DIVISION.
       PROGRAM-ID. InventoryManagement.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-FILE ASSIGN TO "INVENTORY.DAT"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FILE-STATUS.
           SELECT CSV-FILE ASSIGN TO "INVENTORY.CSV"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.   
       FD INVENTORY-FILE.
       01 INVENTORY-RECORD.
           05 ITEM-NAME PIC X(20).
           05 ITEM-PRICE PIC 9(5).
           05 ITEM-QUANTITY PIC 9(5).
           05 ITEM-ID PIC 9(5).
       FD CSV-FILE.
       01 CSV-RECROD PIC X(40).

       WORKING-STORAGE SECTION.
       01  UserChoice  PIC 9.
       01 FILE-STATUS PIC XX.
       01 I-ID PIC 9(5).
       01 I-NAME PIC 9(5).
       01 I-PRICE PIC 9(5).
       01 I-QUANTITY PIC 9(5).

       PROCEDURE DIVISION.
           OPEN I-O INVENTORY-FILE
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
                  WHEN 1
                       PERFORM ADD-ITEM
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
           CLOSE INVENTORY-FILE
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
           DISPLAY "Item's add succesfully".

