       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY-MANAGEMENT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INVENTORY-FILE ASSIGN TO "INVENTORYFILE.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ITEM-CODE
               FILE STATUS IS FILESTATUS.

           SELECT CSV-FILE ASSIGN TO "INVENTORYFILE.CSV"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INVENTORY-FILE.
       01 INVENTORY-RECORD.
           05 ITEM-CODE         PIC X(10).
           05 ITEM-NAME         PIC X(30).
           05 ITEM-CATEGORY     PIC X(20).
           05 ITEM-SIZE         PIC X(10).
           05 ITEM-COLOR        PIC X(15).
           05 STOCK-QUANTITY    PIC 9(5).
           05 UNIT-PRICE        PIC 9(7)V99.
       
       FD CSV-FILE.
       01 CSV-RECORD            PIC X(100).

       WORKING-STORAGE SECTION.
       01 FILESTATUS            PIC X(2).
       01 WS-OPTION             PIC 9.
       01 WS-EndOfFile          PIC X VALUE 'N'.
       01 WS-WAITFORINPUT       PIC X.

       PROCEDURE DIVISION.
           PERFORM MAIN-MENU.
           STOP RUN.

       MAIN-MENU.
           PERFORM UNTIL WS-EndOfFile = 'Y'
               CALL "SYSTEM" USING "CLS"
               DISPLAY "||=======================================||"
               DISPLAY "||     Inventory Management System       ||"
               DISPLAY "||=======================================||"
               DISPLAY "||   1. Add Item                         ||"
               DISPLAY "||   2. Update Item                      ||"
               DISPLAY "||   3. View Inventory                   ||"
               DISPLAY "||   4. Search Item                      ||"
               DISPLAY "||   5. Delete Item                      ||"
               DISPLAY "||   6. Export to CSV                    ||"
               DISPLAY "||   7. Exit                             ||"
               DISPLAY "||=======================================||"
               DISPLAY "Enter your desired option: " NO ADVANCING
               ACCEPT WS-OPTION
              
               EVALUATE WS-OPTION
                   WHEN 1 PERFORM ADD-ITEM
                   WHEN 2 PERFORM UPDATE-ITEM
                   WHEN 3 PERFORM VIEW-INVENTORY
                   WHEN 4 PERFORM SEARCH-ITEM
                   WHEN 5 PERFORM DELETE-ITEM
                   WHEN 6 PERFORM EXPORT-TO-CSV
                   WHEN 7
                       DISPLAY " "
                       DISPLAY "[SYSTEM] TERMINATING PROGRAM..."
                       CLOSE INVENTORY-FILE
                       MOVE 'Y' TO WS-EndOfFile
                   WHEN OTHER DISPLAY "INVALID OPTION"
               END-EVALUATE
           END-PERFORM.

       ADD-ITEM.
           OPEN I-O INVENTORY-FILE
           IF FILESTATUS = "35"
               OPEN OUTPUT INVENTORY-FILE
               CLOSE INVENTORY-FILE
               OPEN I-O INVENTORY-FILE
           END-IF.

           CALL "SYSTEM" USING "CLS"
           DISPLAY "======================================="
           DISPLAY "           ADD NEW ITEM"
           DISPLAY "======================================="
           DISPLAY "Enter Item Code: " NO ADVANCING
           ACCEPT ITEM-CODE.
           
           READ INVENTORY-FILE
               INVALID KEY 
                   CONTINUE
               NOT INVALID 
                   DISPLAY "Item Code already exists!"
                   CLOSE INVENTORY-FILE
                   DISPLAY "[SYSTEM] Press any key to continue..."
                   ACCEPT WS-WAITFORINPUT
                   EXIT PARAGRAPH
           END-READ.

           DISPLAY "Enter Item Name: " NO ADVANCING
           ACCEPT ITEM-NAME.
           DISPLAY "Enter Category: " NO ADVANCING
           ACCEPT ITEM-CATEGORY.
           DISPLAY "Enter Size: " NO ADVANCING
           ACCEPT ITEM-SIZE.
           DISPLAY "Enter Color: " NO ADVANCING
           ACCEPT ITEM-COLOR.
           DISPLAY "Enter Stock Quantity: " NO ADVANCING
           ACCEPT STOCK-QUANTITY.
           DISPLAY "Enter Unit Price: " NO ADVANCING
           ACCEPT UNIT-PRICE.

           WRITE INVENTORY-RECORD
               INVALID KEY 
                   DISPLAY "Error writing item to inventory!"
               NOT INVALID 
                   DISPLAY "Item successfully added!"
           END-WRITE.
           
           CLOSE INVENTORY-FILE.
           DISPLAY "[SYSTEM] Press any key to continue..."
           ACCEPT WS-WAITFORINPUT.

       UPDATE-ITEM.
           OPEN I-O INVENTORY-FILE.
           CALL "SYSTEM" USING "CLS"
           DISPLAY "======================================="
           DISPLAY "         UPDATE ITEM DETAILS"
           DISPLAY "======================================="
           DISPLAY "Enter Item Code to update: " NO ADVANCING
           ACCEPT ITEM-CODE.
           
           READ INVENTORY-FILE KEY IS ITEM-CODE
               INVALID KEY 
                   DISPLAY "Item not found!"
                   CLOSE INVENTORY-FILE
                   DISPLAY "[SYSTEM] Press any key to continue..."
                   ACCEPT WS-WAITFORINPUT
                   EXIT PARAGRAPH
               NOT INVALID KEY 
                   DISPLAY "Current Item Details:"
                   DISPLAY "Name: " ITEM-NAME
                   DISPLAY "Category: " ITEM-CATEGORY
                   DISPLAY "Size: " ITEM-SIZE
                   DISPLAY "Color: " ITEM-COLOR
                   DISPLAY "Stock Quantity: " STOCK-QUANTITY
                   DISPLAY "Unit Price: " UNIT-PRICE
                   
                   DISPLAY "Enter new details "NO ADVANCING
                   DISPLAY "(press enter to keep current): "
                   DISPLAY "New Item Name: " NO ADVANCING
                   ACCEPT ITEM-NAME
                   DISPLAY "New Category: " NO ADVANCING
                   ACCEPT ITEM-CATEGORY
                   DISPLAY "New Size: " NO ADVANCING
                   ACCEPT ITEM-SIZE
                   DISPLAY "New Color: " NO ADVANCING
                   ACCEPT ITEM-COLOR
                   DISPLAY "New Stock Quantity: " NO ADVANCING
                   ACCEPT STOCK-QUANTITY
                   DISPLAY "New Unit Price: " NO ADVANCING
                   ACCEPT UNIT-PRICE
                   
                   REWRITE INVENTORY-RECORD
                       INVALID KEY 
                           DISPLAY "Error updating item!"
                       NOT INVALID 
                           DISPLAY "Item successfully updated!"
                   END-REWRITE
           END-READ.
           
           CLOSE INVENTORY-FILE.
           DISPLAY "[SYSTEM] Press any key to continue..."
           ACCEPT WS-WAITFORINPUT.

       SEARCH-ITEM.
           OPEN INPUT INVENTORY-FILE.
           CALL "SYSTEM" USING "CLS"
           DISPLAY "======================================="
           DISPLAY "           SEARCH ITEM"
           DISPLAY "======================================="
           DISPLAY "Enter Item Code to search: " NO ADVANCING
           ACCEPT ITEM-CODE.
           
           READ INVENTORY-FILE KEY IS ITEM-CODE
               INVALID KEY 
                   DISPLAY "Item not found!"
               NOT INVALID KEY 
                   DISPLAY "Item Details:"
                   DISPLAY "Name: " ITEM-NAME
                   DISPLAY "Category: " ITEM-CATEGORY
                   DISPLAY "Size: " ITEM-SIZE
                   DISPLAY "Color: " ITEM-COLOR
                   DISPLAY "Stock Quantity: " STOCK-QUANTITY
                   DISPLAY "Unit Price: " UNIT-PRICE
           END-READ.
           
           CLOSE INVENTORY-FILE.
           DISPLAY "[SYSTEM] Press any key to continue..."
           ACCEPT WS-WAITFORINPUT.

       DELETE-ITEM.
           OPEN I-O INVENTORY-FILE.
           CALL "SYSTEM" USING "CLS"
           DISPLAY "======================================="
           DISPLAY "           DELETE ITEM"
           DISPLAY "======================================="
           DISPLAY "Enter Item Code to delete: " NO ADVANCING
           ACCEPT ITEM-CODE.
           
           READ INVENTORY-FILE KEY IS ITEM-CODE
               INVALID KEY 
                   DISPLAY "Item not found!"
                   CLOSE INVENTORY-FILE
                   DISPLAY "[SYSTEM] Press any key to continue..."
                   ACCEPT WS-WAITFORINPUT
                   EXIT PARAGRAPH
               NOT INVALID KEY 
                   DELETE INVENTORY-FILE
                       INVALID KEY 
                           DISPLAY "Error deleting item!"
                       NOT INVALID 
                           DISPLAY "Item successfully deleted!"
                   END-DELETE
           END-READ.
           
           CLOSE INVENTORY-FILE.
           DISPLAY "[SYSTEM] Press any key to continue..."
           ACCEPT WS-WAITFORINPUT.

       VIEW-INVENTORY.
           OPEN INPUT INVENTORY-FILE.
           CALL "SYSTEM" USING "CLS"
           DISPLAY "======================================="
           DISPLAY "         INVENTORY LISTING"
           DISPLAY "======================================="
           
           PERFORM UNTIL FILESTATUS = "10"
               READ INVENTORY-FILE
                   AT END
                       MOVE "10" TO FILESTATUS
                   NOT AT END
                       DISPLAY "Item Code: " ITEM-CODE
                       DISPLAY "Name: " ITEM-NAME
                       DISPLAY "Category: " ITEM-CATEGORY
                       DISPLAY "Size: " ITEM-SIZE
                       DISPLAY "Color: " ITEM-COLOR
                       DISPLAY "Stock Quantity: " STOCK-QUANTITY
                       DISPLAY "Unit Price: " UNIT-PRICE
                       DISPLAY "===================================="
               END-READ
           END-PERFORM.

           CLOSE INVENTORY-FILE.
           DISPLAY "[SYSTEM] Press any key to continue..."
           ACCEPT WS-WAITFORINPUT.

       EXPORT-TO-CSV.
           OPEN OUTPUT CSV-FILE.
           OPEN INPUT INVENTORY-FILE.
           DISPLAY "[SYSTEM] Exporting Inventory to CSV File..."

           PERFORM UNTIL FILESTATUS = "10"
               READ INVENTORY-FILE
                   AT END
                       MOVE "10" TO FILESTATUS
                       EXIT PERFORM
                   NOT AT END
                       STRING 
                           ITEM-CODE "," 
                           ITEM-NAME "," 
                           ITEM-CATEGORY "," 
                           ITEM-SIZE "," 
                           ITEM-COLOR "," 
                           STOCK-QUANTITY "," 
                           UNIT-PRICE 
                       DELIMITED BY SIZE INTO CSV-RECORD
                       WRITE CSV-RECORD
               END-READ
           END-PERFORM.

           CLOSE INVENTORY-FILE.
           CLOSE CSV-FILE.
           DISPLAY "[SYSTEM] Data exported to CSV Successfully!"
           DISPLAY "[SYSTEM] Press any key to continue..."
           ACCEPT WS-WAITFORINPUT.

           