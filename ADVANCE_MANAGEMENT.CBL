       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADVANCED-INVENTORY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-INVENTORY 
               ASSIGN TO "MASTER_INVENTORY.DAT"
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS DYNAMIC
               RELATIVE KEY IS INVENTORY-SEQUENCE
               FILE STATUS IS FILE-STATUS-CODE.

           SELECT SALES-LOG 
               ASSIGN TO "SALES_HISTORY.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS SALE-ID
               FILE STATUS IS SALES-STATUS-CODE.

           SELECT SUPPLIER-FILE
               ASSIGN TO "SUPPLIER_CONTACTS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS SUPPLIER-ID
               FILE STATUS IS SUPPLIER-STATUS-CODE.

       DATA DIVISION.
       FILE SECTION.
       FD MASTER-INVENTORY.
       01 INVENTORY-ENTRY.
           05 PRODUCT-ID         PIC X(12).
           05 PRODUCT-NAME       PIC X(40).
           05 PRODUCT-CATEGORY   PIC X(20).
           05 CURRENT-STOCK      PIC 9(5).
           05 MINIMUM-STOCK      PIC 9(5).
           05 MAXIMUM-STOCK      PIC 9(5).
           05 UNIT-COST          PIC 9(7)V99.
           05 SELLING-PRICE      PIC 9(7)V99.

       FD SALES-LOG.
       01 SALES-RECORD.
           05 SALE-ID            PIC X(12).
           05 SALE-DATE          PIC X(10).
           05 PRODUCT-SOLD-ID     PIC X(12).
           05 QUANTITY-SOLD       PIC 9(5).
           05 TOTAL-SALE-VALUE    PIC 9(7)V99.

       FD SUPPLIER-FILE.
       01 SUPPLIER-RECORD.
           05 SUPPLIER-ID        PIC X(10).
           05 SUPPLIER-NAME      PIC X(40).
           05 CONTACT-PERSON     PIC X(30).
           05 SUPPLIER-PHONE     PIC X(15).
           05 SUPPLIER-EMAIL     PIC X(50).

       WORKING-STORAGE SECTION.
       01 FILE-STATUS-CODE        PIC XX.
       01 SALES-STATUS-CODE       PIC XX.
       01 SUPPLIER-STATUS-CODE    PIC XX.
       01 INVENTORY-SEQUENCE      PIC 9(5).
       01 WS-MENU-CHOICE          PIC X.
       01 WS-CONTINUE             PIC X VALUE 'Y'.
       
       *> Advanced tracking variables
       01 WS-TOTAL-INVENTORY-VALUE PIC 9(10)V99.
       01 WS-TOTAL-SALES-VALUE     PIC 9(10)V99.
       01 WS-PROFIT-MARGIN         PIC 9(5)V99.
       01 WS-TEMP-PRODUCT-NAME     PIC X(40).
       01 WS-REORDER-QUANTITY      PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM OPEN-FILES
           PERFORM ADVANCED-MENU-NAVIGATION UNTIL WS-CONTINUE = 'N'
           PERFORM TERMINATE-SYSTEM
           STOP RUN.

       OPEN-FILES.
           OPEN I-O MASTER-INVENTORY
           OPEN I-O SALES-LOG
           OPEN I-O SUPPLIER-FILE.

       ADVANCED-MENU-NAVIGATION.
           DISPLAY "======= INTELLIGENT INVENTORY MANAGEMENT ======="
           DISPLAY "1. Product Management"
           DISPLAY "2. Sales Tracking"
           DISPLAY "3. Supplier Management"
           DISPLAY "4. Financial Analytics"
           DISPLAY "5. Inventory Optimization"
           DISPLAY "6. Exit System"
           DISPLAY "Select Option: " NO ADVANCING
           ACCEPT WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
               WHEN '1' PERFORM PRODUCT-MANAGEMENT-SUBMENU
               WHEN '2' PERFORM SALES-TRACKING-SUBMENU
               WHEN '3' PERFORM SUPPLIER-MANAGEMENT-SUBMENU
               WHEN '4' PERFORM FINANCIAL-ANALYTICS
               WHEN '5' PERFORM INVENTORY-OPTIMIZATION
               WHEN '6' MOVE 'N' TO WS-CONTINUE
               WHEN OTHER DISPLAY "Invalid Option"
           END-EVALUATE.

       PRODUCT-MANAGEMENT-SUBMENU.
           DISPLAY "--- PRODUCT MANAGEMENT ---"
           DISPLAY "A. Add New Product"
           DISPLAY "B. Update Product Details"
           DISPLAY "C. Remove Product"
           DISPLAY "D. View Product Details"
           DISPLAY "Select Action: " NO ADVANCING
           ACCEPT WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
               WHEN 'A' PERFORM ADD-NEW-PRODUCT
               WHEN 'B' PERFORM UPDATE-PRODUCT-DETAILS
               WHEN 'C' PERFORM REMOVE-PRODUCT
               WHEN 'D' PERFORM VIEW-PRODUCT-DETAILS
           END-EVALUATE.

       SALES-TRACKING-SUBMENU.
           DISPLAY "--- SALES TRACKING ---"
           DISPLAY "A. Record New Sale"
           DISPLAY "B. View Sales History"
           DISPLAY "C. Generate Sales Report"
           DISPLAY "Select Action: " NO ADVANCING
           ACCEPT WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
               WHEN 'A' PERFORM RECORD-NEW-SALE
               WHEN 'B' PERFORM VIEW-SALES-HISTORY
               WHEN 'C' PERFORM GENERATE-SALES-REPORT
           END-EVALUATE.

       SUPPLIER-MANAGEMENT-SUBMENU.
           DISPLAY "--- SUPPLIER MANAGEMENT ---"
           DISPLAY "A. Add New Supplier"
           DISPLAY "B. Update Supplier Details"
           DISPLAY "C. View Supplier List"
           DISPLAY "Select Action: " NO ADVANCING
           ACCEPT WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
               WHEN 'A' PERFORM ADD-NEW-SUPPLIER
               WHEN 'B' PERFORM UPDATE-SUPPLIER-DETAILS
               WHEN 'C' PERFORM VIEW-SUPPLIER-LIST
           END-EVALUATE.

       ADD-NEW-PRODUCT.
           DISPLAY "Enter Product Details:"
           DISPLAY "Product ID: " NO ADVANCING
           ACCEPT PRODUCT-ID
           DISPLAY "Product Name: " NO ADVANCING
           ACCEPT PRODUCT-NAME
           DISPLAY "Category: " NO ADVANCING
           ACCEPT PRODUCT-CATEGORY
           DISPLAY "Initial Stock: " NO ADVANCING
           ACCEPT CURRENT-STOCK
           DISPLAY "Minimum Stock Level: " NO ADVANCING
           ACCEPT MINIMUM-STOCK
           DISPLAY "Maximum Stock Level: " NO ADVANCING
           ACCEPT MAXIMUM-STOCK
           DISPLAY "Unit Cost: " NO ADVANCING
           ACCEPT UNIT-COST
           DISPLAY "Selling Price: " NO ADVANCING
           ACCEPT SELLING-PRICE

           WRITE INVENTORY-ENTRY
               INVALID KEY
                   DISPLAY "Error adding product!"
               NOT INVALID
                   DISPLAY "Product added successfully!".

       UPDATE-PRODUCT-DETAILS.
           DISPLAY "Enter Product ID to Update: " NO ADVANCING
           ACCEPT PRODUCT-ID
           
           READ MASTER-INVENTORY 
               INVALID KEY
                   DISPLAY "Product not found!"
               NOT INVALID KEY
                   DISPLAY "Current Product Name: " PRODUCT-NAME
                   DISPLAY "Enter New Product Name " NO ADVANCING
                   DISPLAY "(or press enter to keep): " NO ADVANCING
                   ACCEPT WS-TEMP-PRODUCT-NAME
                   IF WS-TEMP-PRODUCT-NAME NOT = SPACES
                       MOVE WS-TEMP-PRODUCT-NAME TO PRODUCT-NAME
                   END-IF
                   
                   REWRITE INVENTORY-ENTRY
                       INVALID KEY 
                           DISPLAY "Error updating product!"
                       NOT INVALID KEY
                           DISPLAY "Product updated successfully!"
           END-READ.

       REMOVE-PRODUCT.
           DISPLAY "Enter Product ID to Remove: " NO ADVANCING
           ACCEPT PRODUCT-ID
           
           DELETE MASTER-INVENTORY 
               INVALID KEY
                   DISPLAY "Product not found!"
               NOT INVALID 
                   DISPLAY "Product removed successfully!".

       VIEW-PRODUCT-DETAILS.
           DISPLAY "Enter Product ID to View: " NO ADVANCING
           ACCEPT PRODUCT-ID
           
           READ MASTER-INVENTORY 
               INVALID KEY
                   DISPLAY "Product not found!"
               NOT INVALID KEY
                   DISPLAY "Product Details:"
                   DISPLAY "Name: " PRODUCT-NAME
                   DISPLAY "Category: " PRODUCT-CATEGORY
                   DISPLAY "Current Stock: " CURRENT-STOCK
                   DISPLAY "Unit Cost: " UNIT-COST
                   DISPLAY "Selling Price: " SELLING-PRICE
           END-READ.

       RECORD-NEW-SALE.
           DISPLAY "Enter Sale Details:"
           DISPLAY "Sale ID: " NO ADVANCING
           ACCEPT SALE-ID
           DISPLAY "Product ID Sold: " NO ADVANCING
           ACCEPT PRODUCT-SOLD-ID
           DISPLAY "Quantity Sold: " NO ADVANCING
           ACCEPT QUANTITY-SOLD

           *> Calculate total sale value
           COMPUTE TOTAL-SALE-VALUE = QUANTITY-SOLD * SELLING-PRICE

           *> Update inventory
           PERFORM UPDATE-INVENTORY-AFTER-SALE.

       UPDATE-INVENTORY-AFTER-SALE.
           MOVE PRODUCT-NAME TO WS-TEMP-PRODUCT-NAME
           SUBTRACT QUANTITY-SOLD FROM CURRENT-STOCK
           IF CURRENT-STOCK <= MINIMUM-STOCK
               DISPLAY "ALERT: Stock below minimum " NO ADVANCING
               DISPLAY "level for " WS-TEMP-PRODUCT-NAME
               PERFORM GENERATE-REORDER-SUGGESTION
           END-IF.

       GENERATE-REORDER-SUGGESTION.
           COMPUTE WS-TOTAL-INVENTORY-VALUE = 
               CURRENT-STOCK * UNIT-COST
           
           COMPUTE WS-REORDER-QUANTITY = 
               MAXIMUM-STOCK - CURRENT-STOCK
           
           DISPLAY "Suggested Reorder Quantity: " WS-REORDER-QUANTITY
           DISPLAY "Estimated Reorder Cost: " WS-TOTAL-INVENTORY-VALUE.

       VIEW-SALES-HISTORY.
           DISPLAY "Sales History:"
           PERFORM VARYING INVENTORY-SEQUENCE FROM 1 BY 1 
           UNTIL SALES-STATUS-CODE = '10'
               READ SALES-LOG NEXT
                   AT END 
                       MOVE '10' TO SALES-STATUS-CODE
                   NOT AT END
                       DISPLAY "Sale ID: " SALE-ID
                       DISPLAY "Product: " PRODUCT-SOLD-ID
                       DISPLAY "Quantity: " QUANTITY-SOLD
                       DISPLAY "Total Value: " TOTAL-SALE-VALUE
               END-READ
           END-PERFORM.

       GENERATE-SALES-REPORT.
           DISPLAY "Generating Sales Report..."
           MOVE ZERO TO WS-TOTAL-SALES-VALUE
           PERFORM VARYING INVENTORY-SEQUENCE FROM 1 BY 1 
           UNTIL SALES-STATUS-CODE = '10'
               READ SALES-LOG NEXT
                   AT END 
                       MOVE '10' TO SALES-STATUS-CODE
                   NOT AT END
                       ADD TOTAL-SALE-VALUE TO WS-TOTAL-SALES-VALUE
               END-READ
           END-PERFORM
           
           DISPLAY "Total Sales Value: " WS-TOTAL-SALES-VALUE.

       ADD-NEW-SUPPLIER.
           DISPLAY "Enter Supplier Details:"
           DISPLAY "Supplier ID: " NO ADVANCING
           ACCEPT SUPPLIER-ID
           DISPLAY "Supplier Name: " NO ADVANCING
           ACCEPT SUPPLIER-NAME
           DISPLAY "Contact Person: " NO ADVANCING
           ACCEPT CONTACT-PERSON
           DISPLAY "Phone Number: " NO ADVANCING
           ACCEPT SUPPLIER-PHONE
           DISPLAY "Email: " NO ADVANCING
           ACCEPT SUPPLIER-EMAIL

           WRITE SUPPLIER-RECORD
               INVALID KEY
                   DISPLAY "Error adding supplier!"
               NOT INVALID
                   DISPLAY "Supplier added successfully!".

       UPDATE-SUPPLIER-DETAILS.
           DISPLAY "Enter Supplier ID to Update: " NO ADVANCING
           ACCEPT SUPPLIER-ID
           
           READ SUPPLIER-FILE
               INVALID KEY
                   DISPLAY "Supplier not found!"
               NOT INVALID
                   DISPLAY "Current Supplier Name: " SUPPLIER-NAME
                   DISPLAY "(Press enter to keep)"
                   DISPLAY "Enter New Name : " NO ADVANCING
                   ACCEPT WS-TEMP-PRODUCT-NAME
                   IF WS-TEMP-PRODUCT-NAME NOT = SPACES
                       MOVE WS-TEMP-PRODUCT-NAME TO SUPPLIER-NAME
                   END-IF
                   
                   REWRITE SUPPLIER-RECORD
                       INVALID KEY 
                           DISPLAY "Error updating supplier!"
                       NOT INVALID KEY
                           DISPLAY "Supplier updated successfully!"
           END-READ.

       VIEW-SUPPLIER-LIST.
           DISPLAY "Supplier List:"
           PERFORM VARYING INVENTORY-SEQUENCE FROM 1 BY 1 
           UNTIL SUPPLIER-STATUS-CODE = '10'
               READ SUPPLIER-FILE NEXT
                   AT END 
                       MOVE '10' TO SUPPLIER-STATUS-CODE
                   NOT AT END
                       DISPLAY "ID: " SUPPLIER-ID
                       DISPLAY "Name: " SUPPLIER-NAME
                       DISPLAY "Contact: " CONTACT-PERSON
               END-READ
           END-PERFORM.

       FINANCIAL-ANALYTICS.
           MOVE ZERO TO WS-TOTAL-INVENTORY-VALUE
           MOVE ZERO TO WS-TOTAL-SALES-VALUE

           PERFORM VARYING INVENTORY-SEQUENCE FROM 1 BY 1 
           UNTIL FILE-STATUS-CODE = '10'
               READ MASTER-INVENTORY NEXT
                   AT END 
                       MOVE '10' TO FILE-STATUS-CODE
                   NOT AT END
                       COMPUTE WS-TOTAL-INVENTORY-VALUE = 
                           WS-TOTAL-INVENTORY-VALUE + 
                           (CURRENT-STOCK * UNIT-COST)
               END-READ
           END-PERFORM

           DISPLAY "Total Inventory Value: " WS-TOTAL-INVENTORY-VALUE
           DISPLAY "Total Sales Value: " WS-TOTAL-SALES-VALUE.

       INVENTORY-OPTIMIZATION.
           DISPLAY "Inventory Optimization Report:"
           PERFORM GENERATE-STOCK-HEALTH-REPORT
           PERFORM IDENTIFY-SLOW-MOVING-ITEMS.

       GENERATE-STOCK-HEALTH-REPORT.
           DISPLAY "Stock Health Analysis:"
           PERFORM VARYING INVENTORY-SEQUENCE FROM 1 BY 1 
           UNTIL FILE-STATUS-CODE = '10'
               READ MASTER-INVENTORY NEXT
                   AT END 
                       MOVE '10' TO FILE-STATUS-CODE
                   NOT AT END
                       IF CURRENT-STOCK < MINIMUM-STOCK
                           DISPLAY PRODUCT-NAME ": CRITICALLY LOW"
                       ELSE 
                           IF CURRENT-STOCK > MAXIMUM-STOCK
                               DISPLAY PRODUCT-NAME ": OVERSTOCKED"
                           END-IF
                       END-IF
               END-READ
           END-PERFORM.

       IDENTIFY-SLOW-MOVING-ITEMS.
           DISPLAY "Slow-Moving Items:".
           *> Placeholder for detailed analysis

       TERMINATE-SYSTEM.
           CLOSE MASTER-INVENTORY
           CLOSE SALES-LOG
           CLOSE SUPPLIER-FILE
           DISPLAY "Advanced Inventory System Closed".
           