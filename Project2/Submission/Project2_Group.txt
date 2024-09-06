*>****************************************************************
*> Authors: Aditya Hirpara & Meet Maheta
*> Date: 12/07/2024
*> Purpose: Project 2
*> Tectonics: cobc
*>****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. InvestmentReport.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT STOCKS-FILE ASSIGN TO 'STOCKS.txt'
        ORGANIZATION IS LINE SEQUENTIAL.     *> Assign STOCKS-FILE to 'STOCKS.txt' with line sequential organization
    SELECT PORTFOLIO-FILE ASSIGN TO 'PORTFOLIO.txt'
        ORGANIZATION IS LINE SEQUENTIAL.     *> Assign PORTFOLIO-FILE to 'PORTFOLIO.txt' with line sequential organization
    SELECT REPORT-FILE ASSIGN TO 'REPORT.txt'
        ORGANIZATION IS LINE SEQUENTIAL.     *> Assign REPORT-FILE to 'REPORT.txt' with line sequential organization

DATA DIVISION.
FILE SECTION.
FD  STOCKS-FILE.
01  STOCKS-RECORD.
    05 STOCK-SYMBOL    PIC X(7).             *> Stock symbol, 7 characters
    05 STOCK-NAME      PIC X(25).            *> Stock name, 25 characters
    05 CLOSING-PRICE   PIC 9(4)V99.          *> Closing price, 4 digits and 2 decimal places

FD  PORTFOLIO-FILE.
01  PORTFOLIO-RECORD.
    05 PORT-STOCK-SYMBOL  PIC X(7).          *> Portfolio stock symbol, 7 characters
    05 NUMBER-OF-SHARES   PIC 9(5).          *> Number of shares, 5 digits
    05 AVG-COST           PIC 9(4)V99.       *> Average cost, 4 digits and 2 decimal places

FD  REPORT-FILE.
01  REPORT-RECORD.
    05 REPORT-LINE       PIC X(132).         *> Report line, 132 characters

WORKING-STORAGE SECTION.
01  WS-STOCK-TABLE.
    05 WS-STOCK-ENTRY OCCURS 20 TIMES.       *> Stock table with 20 entries
        10 WS-STOCK-SYMBOL    PIC X(7).      *> Stock symbol
        10 WS-STOCK-NAME      PIC X(25).     *> Stock name
        10 WS-CLOSING-PRICE   PIC 9(4)V99.   *> Closing price

01  WS-INDEX            PIC 9(2) VALUE 1.    *> Index for stock table
01  WS-MATCH-INDEX      PIC 9(2) VALUE 1.    *> Index for matching stock
01  EOF-PORTFOLIO       PIC X VALUE 'N'.     *> End-of-file flag for portfolio file
01  WS-COUNTERS.
    05 WS-READ-COUNT    PIC 9(5) VALUE 0.    *> Counter for read records
    05 WS-WRITE-COUNT   PIC 9(5) VALUE 0.    *> Counter for written records

01  ADJUSTED-COST-BASE  PIC 9(9)V99.         *> Adjusted cost base
01  MARKET-VALUE        PIC 9(9)V99.         *> Market value
01  GAIN-LOSS           PIC S9(9)V99.        *> Gain or loss

01  WS-REPORT-STOCK-NAME  PIC X(25).         *> Reported stock name
01  WS-REPORT-NUM-SHARES  PIC Z(5).          *> Reported number of shares
01  WS-REPORT-AVG-COST    PIC $$$$,$$9.99.   *> Reported average cost
01  WS-REPORT-CLOSING-PRICE PIC $$$$,$$9.99. *> Reported closing price
01  WS-REPORT-ADJUSTED-COST PIC $$$,$$,$$9.99. *> Reported adjusted cost base
01  WS-REPORT-MARKET-VALUE  PIC $$$,$$,$$9.99. *> Reported market value
01  WS-REPORT-GAIN-LOSS     PIC $$$,$$,$$9.99. *> Reported gain or loss

01  WS-REPORT-SUMMARY   PIC X(132).          *> Report summary line

01  COLUMN-HEADERS      PIC X(132) VALUE "=================================================================================================". *> Column headers line
01  COLUMN-TITLES       PIC X(132) VALUE "STOCK NAME                 #SHARES UNIT-COST AT-CLOSING    COST-BASE   MARKET-VALUE   GAIN/LOSS". *> Column titles line

PROCEDURE DIVISION.
0000-MAIN-PARA.
    PERFORM 1000-INITIALIZATION.              *> Perform initialization
    PERFORM 2000-PROCESS-FILES.               *> Perform file processing
    PERFORM 3000-FINALIZATION.                *> Perform finalization
    STOP RUN.                                 *> End of program

1000-INITIALIZATION.
    OPEN INPUT STOCKS-FILE PORTFOLIO-FILE.    *> Open input files
    OPEN OUTPUT REPORT-FILE.                  *> Open output file
    WRITE REPORT-RECORD FROM COLUMN-HEADERS.  *> Write column headers
    WRITE REPORT-RECORD FROM COLUMN-TITLES.   *> Write column titles
    WRITE REPORT-RECORD FROM COLUMN-HEADERS.  *> Write column headers again
    PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 20
        READ STOCKS-FILE INTO STOCKS-RECORD
        AT END
            MOVE ' ' TO WS-STOCK-SYMBOL (WS-INDEX) *> End of stocks file
            EXIT PERFORM
        NOT AT END
            MOVE STOCK-SYMBOL TO WS-STOCK-SYMBOL (WS-INDEX)     *> Move stock symbol to table
            MOVE STOCK-NAME TO WS-STOCK-NAME (WS-INDEX)         *> Move stock name to table
            MOVE CLOSING-PRICE TO WS-CLOSING-PRICE (WS-INDEX)   *> Move closing price to table
    END-PERFORM.

2000-PROCESS-FILES.
    PERFORM UNTIL EOF-PORTFOLIO = 'Y'
        READ PORTFOLIO-FILE INTO PORTFOLIO-RECORD
        AT END
            MOVE 'Y' TO EOF-PORTFOLIO              *> End of portfolio file
        NOT AT END
            ADD 1 TO WS-READ-COUNT                 *> Increment read count
            DISPLAY "Processing PORTFOLIO record: " PORT-STOCK-SYMBOL " " NUMBER-OF-SHARES " " AVG-COST
            PERFORM 2100-PROCESS-RECORD            *> Process the record
    END-PERFORM.

2100-PROCESS-RECORD.
    MOVE 1 TO WS-MATCH-INDEX                       *> Initialize match index
    PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 20
        IF WS-STOCK-SYMBOL (WS-INDEX) = PORT-STOCK-SYMBOL
            MOVE WS-INDEX TO WS-MATCH-INDEX        *> Set match index if symbols match
            EXIT PERFORM
        END-IF
    END-PERFORM

    IF WS-STOCK-SYMBOL (WS-MATCH-INDEX) = PORT-STOCK-SYMBOL
        DISPLAY "Match found for: " PORT-STOCK-SYMBOL " with " WS-STOCK-SYMBOL (WS-MATCH-INDEX)
        COMPUTE ADJUSTED-COST-BASE = NUMBER-OF-SHARES * AVG-COST             *> Calculate adjusted cost base
        COMPUTE MARKET-VALUE = NUMBER-OF-SHARES * WS-CLOSING-PRICE (WS-MATCH-INDEX) *> Calculate market value
        COMPUTE GAIN-LOSS = MARKET-VALUE - ADJUSTED-COST-BASE                *> Calculate gain or loss
        MOVE WS-STOCK-NAME (WS-MATCH-INDEX) TO WS-REPORT-STOCK-NAME          *> Prepare report data
        MOVE NUMBER-OF-SHARES TO WS-REPORT-NUM-SHARES
        MOVE AVG-COST TO WS-REPORT-AVG-COST
        MOVE WS-CLOSING-PRICE (WS-MATCH-INDEX) TO WS-REPORT-CLOSING-PRICE
        MOVE ADJUSTED-COST-BASE TO WS-REPORT-ADJUSTED-COST
        MOVE MARKET-VALUE TO WS-REPORT-MARKET-VALUE
        MOVE GAIN-LOSS TO WS-REPORT-GAIN-LOSS
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
        WRITE REPORT-RECORD FROM REPORT-LINE          *> Write report record
        ADD 1 TO WS-WRITE-COUNT                       *> Increment write count
    ELSE
        DISPLAY "No match found for: " PORT-STOCK-SYMBOL
    END-IF.

3000-FINALIZATION.
    MOVE "Records read: " TO WS-REPORT-SUMMARY
    STRING WS-READ-COUNT DELIMITED BY SPACE
           "   Records written: " DELIMITED BY SIZE
           WS-WRITE-COUNT DELIMITED BY SPACE
           INTO WS-REPORT-SUMMARY
    WRITE REPORT-RECORD FROM COLUMN-HEADERS.          *> Write column headers
    WRITE REPORT-RECORD FROM WS-REPORT-SUMMARY.       *> Write summary line
    CLOSE STOCKS-FILE PORTFOLIO-FILE REPORT-FILE.     *> Close all files
