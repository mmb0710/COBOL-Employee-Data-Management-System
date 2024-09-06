*>****************************************************************
*> Authors: Aditya Hirpara, Meet Maheta & Patel Meet
*> Date: 25/07/2024
*> Purpose: Project 3
*> Tectonics: cobc
*>****************************************************************
IDENTIFICATION DIVISION.
PROGRAM-ID. CalculateValues.

DATA DIVISION.
LINKAGE SECTION.
    *> Define variables passed from the calling program
    01 LNK-NUMBER-OF-SHARES  PIC 9(5).
        *> Number of shares owned
    01 LNK-AVG-COST          PIC 9(4)V99.
        *> Average cost per share
    01 LNK-CLOSING-PRICE     PIC 9(4)V99.
        *> Closing price of the stock
    01 LNK-ADJUSTED-COST-BASE PIC 9(9)V99.
        *> Adjusted cost base, calculated as (Number of shares * Average cost)
    01 LNK-MARKET-VALUE       PIC 9(9)V99.
        *> Market value, calculated as (Number of shares * Closing price)
    01 LNK-GAIN-LOSS          PIC S9(9)V99.
        *> Gain or loss, calculated as (Market value - Adjusted cost base)

PROCEDURE DIVISION USING LNK-NUMBER-OF-SHARES LNK-AVG-COST LNK-CLOSING-PRICE
                         LNK-ADJUSTED-COST-BASE LNK-MARKET-VALUE LNK-GAIN-LOSS.
    *> Calculate the adjusted cost base
    COMPUTE LNK-ADJUSTED-COST-BASE = LNK-NUMBER-OF-SHARES * LNK-AVG-COST.
    *> Calculate the market value
    COMPUTE LNK-MARKET-VALUE = LNK-NUMBER-OF-SHARES * LNK-CLOSING-PRICE.
    *> Calculate the gain or loss
    COMPUTE LNK-GAIN-LOSS = LNK-MARKET-VALUE - LNK-ADJUSTED-COST-BASE.
    *> Exit the subroutine
    EXIT PROGRAM.
