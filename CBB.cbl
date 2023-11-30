           >>SOURCE FORMAT FREE
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Flowershop.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 PROGRAM-OPTIONS PIC X.
           88 PURCHASE-REQUEST VALUE "Y" "y".
           88 EXIT-REQUEST VALUE "N" "n".
       01 START-ORDER PIC X(999999).
       01 EXIT-PROGRAM PIC X(999999).
       01 DISPLAY-FLOWER-LIST PIC X(999999).
       01 GET-ORDER PIC X(999999).
       01 ASK-QUANTITY PIC X(999999).
       01 QUANTITY PIC X(999999).
       01 CUSTOMER-RESPONSE PIC X(2).
       01 DISPLAY-TOTAL-PRICE PIC X(999999).

       01 FLOWER-TYPES.
           05 CHRYSANTHEMUM-PRICE PIC 9(4)V99 VALUE 50.00.
           05 DAISY-PRICE PIC 9(4)V99 VALUE 200.00.
           05 GARDENIA-PRICE PIC 9(4)V99 VALUE 150.00.
           05 ORCHID-PRICE PIC 9(4)V99 VALUE 1000.00.
           05 POINSETTIA-PRICE PIC 9(4)V99 VALUE 500.00.
           05 ROSE-PRICE PIC 9(4)V99 VALUE 100.00.
           05 SUNFLOWER-PRICE PIC 9(4)V99 VALUE 250.00.
           05 TULIP-PRICE PIC 9(4)V99 VALUE 300.00.
       
       01 TOTAL-PRICE PIC 9(6)V99 VALUE 0.00.

       PROCEDURE DIVISION.

           DISPLAY "HELLO AND WELCOME TO COBOL BLOOMS BOUTIQUE!".
           DISPLAY "WOULD YOU LIKE TO MAKE A PURCHASE?".
           ACCEPT PROGRAM-OPTIONS.
           
           EVALUATE TRUE
               WHEN PURCHASE-REQUEST PERFORM START-ORDER
               WHEN EXIT-REQUEST PERFORM EXIT-PROGRAM
               WHEN OTHER DISPLAY "Incorrect input. Try again."
           END-EVALUATE.

      *>    IF PROGRAM-OPTIONS = "Y" OR PROGRAM-OPTIONS = "y" THEN
      *>        PERFORM START-ORDER.
      *>     ELSE 
      *>     PERFORM EXIT-PROGRAM.

       START-ORDER.
           DISPLAY "What would you like to order?".
           PERFORM DISPLAY-FLOWER-LIST. 
           PERFORM GET-ORDER UNTIL GET-ORDER >= 1. 
           PERFORM ASK-QUANTITY.
           PERFORM CHECKOUT OR PERFORM EXIT-PROGRAM.



       EXIT-PROGRAM.
           DISPLAY "Thank you and have a great day ahead!".

       
       DISPLAY-FLOWER-LIST.
           DISPLAY "FLOWER LIST:".
           DISPLAY "CHRYSANTHEMUM = 50".
           DISPLAY "DAISY = 50".
           DISPLAY "GARDENIA = 50".
           DISPLAY "ORCHID = 50".
           DISPLAY "POINSETTIA = 50".
           DISPLAY "ROSE = 50".
           DISPLAY "SUNFLOWER = 50".
           DISPLAY "TULIP = 50".

       GET-ORDER.
           ACCEPT ORDER-CHOICE.

           EVALUATE ORDER-CHOICE
               WHEN "CHR" PERFORM ADD-TO-TOTAL-PRICE(CHRYSANTHEMUM-PRICE)
               WHEN "DAI" PERFORM ADD-TO-TOTAL-PRICE(DAISY-PRICE)
               WHEN "GAR" PERFORM ADD-TO-TOTAL-PRICE(GARDENIA-PRICE)
               WHEN "ORC" PERFORM ADD-TO-TOTAL-PRICE(ORCHID-PRICE)
               WHEN "POI" PERFORM ADD-TO-TOTAL-PRICE(POINSETTIA-PRICE)
               WHEN "ROS" PERFORM ADD-TO-TOTAL-PRICE(ROSE-PRICE)
               WHEN "SUN" PERFORM ADD-TO-TOTAL-PRICE(SUNFLOWER-PRICE)
               WHEN "TUL" PERFORM ADD-TO-TOTAL-PRICE(TULIP-PRICE)
               WHEN OTHER DISPLAY "Invalid order choice. Please check and try again.".
           END EVALUATE.
       
       ASK-QUANTITY.
           IF QUANTITY <= 0 THEN
               DISPLAY "Quantity must be greater than 0. Please try again.".
           END-IF.

       ADD-TO-TOTAL-PRICE (FLOWER-PRICE).
           DISPLAY "Enter the quantity you want to order: ".
           ACCEPT QUANTITY.
           COMPUTE TOTAL-PRICE = TOTAL-PRICE + (FLOWER-PRICE * QUANTITY).

       CHECKOUT OR EXIT-PROGRAM.
           DISPLAY "Would you like to select another product? (Y/N)".
           ACCEPT CUSTOMER-RESPONSE.
           IF CUSTOMER-RESPONSE = 'Y' OR CUSTOMER-RESPONSE = 'y' THEN
               PERFORM DISPLAY-FLOWER-LIST.
               PERFORM ASK-PRODUCT-CHOICE UNTIL QUANTITY > 0.
               PERFORM ASK-QUANTITY.
           ELSE
               PERFORM ASK-MODE-OF-PAYMENT.
           END-IF.

       DISPLAY-TOTAL-PRICE.
           DISPLAY "Total Price: $", TOTAL-PRICE.
       STOP RUN.
      *