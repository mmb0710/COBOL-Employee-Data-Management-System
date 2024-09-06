*>****************************************************************
*> Authors: Aditya Hirpara, Meet Maheta & Patel Meet
*> Date: 25/07/2024
*> Purpose: Project 3
*> Tectonics: cobc
*>****************************************************************
IDENTIFICATION DIVISION.
       PROGRAM-ID. InvestmentReport.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    *> Select the input stock file
    SELECT STOCKS-FILE ASSIGN TO 'STOCKS.txt'
        ORGANIZATION IS LINE SEQUENTIAL.
    *> Select the indexed portfolio file
    SELECT PORTFOLIO-FILE ASSIGN TO 'INVESTMENTFILE.dat'
        ORGANIZATION IS INDEXED
        ACCESS MODE IS SEQUENTIAL
        RECORD KEY IS PORT-STOCK-SYMBOL
        FILE STATUS IS WS-FILE-STATUS.
    *> Select the output report file
    SELECT REPORT-FILE ASSIGN TO 'REPORT.txt'
        ORGANIZATION IS LINE SEQUENTIAL.

DATA DIVISION.
FILE SECTION.
FD STOCKS-FILE.
    *> Define the structure of the input stock file
    01 STOCKS-RECORD.
        05 STOCK-SYMBOL    PIC X(7).
        05 STOCK-NAME      PIC X(25).
        05 CLOSING-PRICE   PIC 9(4)V99.

FD PORTFOLIO-FILE.
    *> Define the structure of the indexed portfolio file
    01 PORTFOLIO-RECORD.
        05 PORT-STOCK-SYMBOL  PIC X(7).
        05 NUMBER-OF-SHARES   PIC 9(5).
        05 AVG-COST           PIC 9(4)V99.

FD REPORT-FILE.
    *> Define the structure of the output report file
    01 REPORT-RECORD.
        05 REPORT-LINE       PIC X(132).

WORKING-STORAGE SECTION.
COPY 'STOCK-TABLE.CPY.txt'.

01 WS-FILE-STATUS      PIC XX.
01 WS-INDEX            PIC 9(2) VALUE 1.
01 WS-MATCH-INDEX      PIC 9(2) VALUE 1.
01 EOF-PORTFOLIO       PIC X VALUE 'N'.
01 WS-COUNTERS.
    05 WS-READ-COUNT    PIC 9(5) VALUE 0.
    05 WS-WRITE-COUNT   PIC 9(5) VALUE 0.

01 ADJUSTED-COST-BASE  PIC 9(9)V99.
01 MARKET-VALUE        PIC 9(9)V99.
01 GAIN-LOSS           PIC S9(9)V99.

01 WS-REPORT-STOCK-NAME  PIC X(25).
01 WS-REPORT-NUM-SHARES  PIC Z(5).
01 WS-REPORT-AVG-COST    PIC $$$$,$$9.99.
01 WS-REPORT-CLOSING-PRICE PIC $$$$,$$9.99.
01 WS-REPORT-ADJUSTED-COST PIC $$$,$$,$$9.99.
01 WS-REPORT-MARKET-VALUE  PIC $$$,$$,$$9.99.
01 WS-REPORT-GAIN-LOSS     PIC $$$,$$,$$9.99-.

01 WS-REPORT-SUMMARY   PIC X(132).

01 COLUMN-HEADERS      PIC X(132) VALUE "=================================================================================================".
01 COLUMN-TITLES       PIC X(132) VALUE "STOCK NAME                 #SHARES UNIT-COST AT-CLOSING    COST-BASE   MARKET-VALUE   GAIN/LOSS".

PROCEDURE DIVISION.
0000-MAIN-PARA.
    *> Main procedure to control the program flow
    PERFORM 1000-INITIALIZATION.
    PERFORM 2000-PROCESS-FILES.
    PERFORM 3000-FINALIZATION.
    STOP RUN.

1000-INITIALIZATION.
    *> Initialize the program, open files, and prepare for processing
    OPEN INPUT STOCKS-FILE PORTFOLIO-FILE.
    IF WS-FILE-STATUS NOT = '00'
        DISPLAY "Error opening portfolio file, status: " WS-FILE-STATUS
        STOP RUN
    END-IF
    OPEN OUTPUT REPORT-FILE.
    WRITE REPORT-RECORD FROM COLUMN-HEADERS.
    WRITE REPORT-RECORD FROM COLUMN-TITLES.
    WRITE REPORT-RECORD FROM COLUMN-HEADERS.
    *> Load stock data into the table from the stock file
    PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 20
        READ STOCKS-FILE INTO STOCKS-RECORD
        AT END
            MOVE ' ' TO WS-STOCK-SYMBOL (WS-INDEX)
            EXIT PERFORM
        NOT AT END
            MOVE STOCK-SYMBOL TO WS-STOCK-SYMBOL (WS-INDEX)
            MOVE STOCK-NAME TO WS-STOCK-NAME (WS-INDEX)
            MOVE CLOSING-PRICE TO WS-CLOSING-PRICE (WS-INDEX)
    END-PERFORM.

2000-PROCESS-FILES.
    *> Process the portfolio file and generate the report
    PERFORM UNTIL EOF-PORTFOLIO = 'Y'
        READ PORTFOLIO-FILE INTO PORTFOLIO-RECORD
        AT END
            MOVE 'Y' TO EOF-PORTFOLIO
        NOT AT END
            ADD 1 TO WS-READ-COUNT
            DISPLAY "Processing PORTFOLIO record: " PORT-STOCK-SYMBOL " " NUMBER-OF-SHARES " " AVG-COST
            PERFORM 2100-PROCESS-RECORD
    END-PERFORM.

2100-PROCESS-RECORD.
    *> Process each record from the portfolio file
    MOVE 1 TO WS-MATCH-INDEX
    *> Search for a matching stock symbol in the stock table
    PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 20
        IF WS-STOCK-SYMBOL (WS-INDEX) = PORT-STOCK-SYMBOL
            MOVE WS-INDEX TO WS-MATCH-INDEX
            EXIT PERFORM
        END-IF
    END-PERFORM

    IF WS-STOCK-SYMBOL (WS-MATCH-INDEX) = PORT-STOCK-SYMBOL
        DISPLAY "Match found for: " PORT-STOCK-SYMBOL " with " WS-STOCK-SYMBOL (WS-MATCH-INDEX)
        *> Call the subroutine to calculate adjusted cost base, market value, and gain/loss
        CALL 'CalculateValues' USING NUMBER-OF-SHARES AVG-COST WS-CLOSING-PRICE (WS-MATCH-INDEX)
                                    ADJUSTED-COST-BASE MARKET-VALUE GAIN-LOSS
        *> Prepare the report line with calculated values
        MOVE WS-STOCK-NAME (WS-MATCH-INDEX) TO WS-REPORT-STOCK-NAME
        MOVE NUMBER-OF-SHARES TO WS-REPORT-NUM-SHARES
        MOVE AVG-COST TO WS-REPORT-AVG-COST
        MOVE WS-CLOSING-PRICE (WS-MATCH-INDEX) TO WS-REPORT-CLOSING-PRICE
        MOVE ADJUSTED-COST-BASE TO WS-REPORT-ADJUSTED-COST
        MOVE MARKET-VALUE TO WS-REPORT-MARKET-VALUE
        MOVE GAIN-LOSS TO WS-REPORT-GAIN-LOSS
        *> Construct the report line string
        STRING WS-REPORT-STOCK-NAME DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               WS-REPORT-NUM-SHARES DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               WS-REPORT-AVG-COST DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               WS-REPORT-CLOSING-PRICE DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               WS-REPORT-ADJUSTED-COST DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               WS-REPORT-MARKET-VALUE DELIMITED BY SIZE
               " " DELIMITED BY SIZE
               WS-REPORT-GAIN-LOSS DELIMITED BY SIZE
               INTO REPORT-LINE
        DISPLAY "Writing REPORT record: " REPORT-LINE
        WRITE REPORT-RECORD FROM REPORT-LINE
        ADD 1 TO WS-WRITE-COUNT
    ELSE
        *> Display a message if no match is found
        DISPLAY "No match found for: " PORT-STOCK-SYMBOL
    END-IF.

3000-FINALIZATION.
    *> Finalize the program, write summary, and close files
    MOVE "Records read: " TO WS-REPORT-SUMMARY
    STRING WS-READ-COUNT DELIMITED BY SPACE
           "   Records written: " DELIMITED BY SIZE
           WS-WRITE-COUNT DELIMITED BY SPACE
           INTO WS-REPORT-SUMMARY
    WRITE REPORT-RECORD FROM COLUMN-HEADERS.
    WRITE REPORT-RECORD FROM WS-REPORT-SUMMARY.
    CLOSE STOCKS-FILE PORTFOLIO-FILE REPORT-FILE.
