*>****************************************************************
*> Authors: Aditya Hirpara, Meet Maheta & Patel Meet
*> Date: 25/07/2024
*> Purpose: Project 3
*> Tectonics: cobc
*>****************************************************************
IDENTIFICATION DIVISION.
       PROGRAM-ID. ConvertPortfolioToIndexed.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           *> Select the input portfolio file
           SELECT PORTFOLIO-FILE-IN ASSIGN TO "PORTFOLIO.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

           *> Select the output indexed investment file
           SELECT INDEXED-INVESTMENT-FILE-OUT ASSIGN TO "INVESTMENTFILE.dat"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS STOCK-SYMBOL-OUT.

       DATA DIVISION.
       FILE SECTION.
       *> Define the structure of the input portfolio file
       FD PORTFOLIO-FILE-IN.
       01 PORTFOLIO-RECORD-IN.
           05 STOCK-SYMBOL-IN       PIC X(7).
           05 SHARES-IN             PIC 9(5).
           05 AVG-COST-PER-SHARE-IN PIC 9(4)V99.

       *> Define the structure of the output indexed investment file
       FD INDEXED-INVESTMENT-FILE-OUT.
       01 INVESTMENT-RECORD-OUT.
           05 STOCK-SYMBOL-OUT       PIC X(7).
           05 TOTAL-SHARES-OUT       PIC 9(5).
           05 AVG-COST-PER-SHARE-OUT PIC 9(4)V99.

       WORKING-STORAGE SECTION.
       *> Define control fields for the program
       01 CONTROL-FIELDS.
           05 EOF-FLAG              PIC X VALUE "N".
           05 READ-COUNT            PIC 99 VALUE 0.
           05 WRITE-COUNT           PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       100-CREATE-INDEXED-INVESTMENT.
           PERFORM 201-INITIALIZE-FILES
           PERFORM 202-CREATE-PORTFOLIO-FILE UNTIL EOF-FLAG = "Y"
           PERFORM 203-CLOSE-FILES
           STOP RUN.

       *> Initialize files and prepare for reading and writing
       201-INITIALIZE-FILES.
           PERFORM 301-OPEN-FILES
           PERFORM 302-READ-PORTFOLIO-FILE.

       *> Main processing loop to create the indexed file
       202-CREATE-PORTFOLIO-FILE.
           PERFORM 303-WRITE-INDEXED-FILE
           PERFORM 302-READ-PORTFOLIO-FILE.

       *> Close the files after processing
       203-CLOSE-FILES.
           PERFORM 304-CLOSE-FILES
           PERFORM 305-DISPLAY-AUDIT-TRAIL.

       *> Open the input and output files
       301-OPEN-FILES.
           OPEN INPUT PORTFOLIO-FILE-IN
           OPEN OUTPUT INDEXED-INVESTMENT-FILE-OUT.

       *> Read a record from the portfolio file
       302-READ-PORTFOLIO-FILE.
           READ PORTFOLIO-FILE-IN AT END
               MOVE "Y" TO EOF-FLAG
           NOT AT END
               ADD 1 TO READ-COUNT.

       *> Write a record to the indexed investment file
       303-WRITE-INDEXED-FILE.
           MOVE STOCK-SYMBOL-IN TO STOCK-SYMBOL-OUT
           MOVE SHARES-IN TO TOTAL-SHARES-OUT
           MOVE AVG-COST-PER-SHARE-IN TO AVG-COST-PER-SHARE-OUT
           WRITE INVESTMENT-RECORD-OUT
               INVALID KEY PERFORM 401-ERROR
               NOT INVALID KEY
                   ADD 1 TO WRITE-COUNT.

       *> Close the input and output files
       304-CLOSE-FILES.
           CLOSE PORTFOLIO-FILE-IN
           CLOSE INDEXED-INVESTMENT-FILE-OUT.

       *> Display the audit trail with counts of records read and written
       305-DISPLAY-AUDIT-TRAIL.
           DISPLAY "TOTAL RECORDS READ: " READ-COUNT
           DISPLAY "TOTAL RECORDS WRITTEN: " WRITE-COUNT.

       *> Handle errors that occur during writing to the indexed file
       401-ERROR.
           DISPLAY "ERROR WRITING RECORD FOR: " STOCK-SYMBOL-OUT.
       END PROGRAM ConvertPortfolioToIndexed.
