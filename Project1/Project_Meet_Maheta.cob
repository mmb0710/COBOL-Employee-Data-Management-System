       *>  Author: Meet Maheta
       *>  Date: 11-06-2024
       *>  Purpose: Project
       *>  Compiler: cobc
        IDENTIFICATION DIVISION.
       PROGRAM-ID. EmpDataManager.
       *> This program handles employee information, allowing for data entry, storage, and retrieval.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EmpFile ASSIGN TO "C:\Users\mmb07\Desktop\BP\Project\EmployeeData.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       *> Specifies the EmpFile and links it to a physical file 'EmployeeData.txt' organized sequentially by lines.

       DATA DIVISION.
       FILE SECTION.
       FD  EmpFile.
       01  EmpRecord.
           05  EmployeeID         PIC 9(6).
           05  DepartmentCode     PIC 9(3).
           05  Surname            PIC X(20).
           05  GivenName          PIC X(20).
           05  YearsOfService     PIC 9(2).
       *> Defines the structure of an employee record with fields for ID, department code, surname, given name, and service years.

       WORKING-STORAGE SECTION.
       01  WS-EndOfFile               PIC X(1)       VALUE 'N'.
           88  EndOfFile              VALUE 'Y'.
           88  ContinueProcessing     VALUE 'N'.
       01  WS-UserResponse            PIC X(1).
       *> Working storage variables for end-of-file indicator and user input.

       PROCEDURE DIVISION.
       000-START.
           OPEN OUTPUT EmpFile
           PERFORM UNTIL EndOfFile
                DISPLAY 'Do you want to enter a new record? (Y/N) '
                ACCEPT WS-UserResponse
                IF WS-UserResponse = 'Y'
                    PERFORM 100-COLLECT-DATA
                    PERFORM 200-SAVE-RECORD
                ELSE
                    SET EndOfFile TO TRUE
                END-IF
           END-PERFORM
           CLOSE EmpFile
       *> Initializes the program by opening the file for output and prompts the user to enter new records until they decline.

           OPEN INPUT EmpFile
           SET ContinueProcessing TO TRUE
           DISPLAY "EmployeeID      DeptCode     FirstName           LastName            ServiceYears"
           DISPLAY "-------------------------------------------------------------------------------------------------------"
           PERFORM 300-READ-DISPLAY-RECORD UNTIL EndOfFile
           DISPLAY "-------------------------------------------------------------------------------------------------------"
           CLOSE EmpFile
       *> After data entry, reopens the file for reading and displays all records.

       STOP RUN.
       *> Ends the program.

       100-COLLECT-DATA.
           DISPLAY "Enter Employee ID (6 digits): ".
           ACCEPT EmployeeID OF EmpRecord.
           DISPLAY "Enter Department Code (3 digits): ".
           ACCEPT DepartmentCode OF EmpRecord.
           DISPLAY "Enter First Name (max 20 chars): ".
           ACCEPT GivenName OF EmpRecord.
           DISPLAY "Enter Last Name (max 20 chars): ".
           ACCEPT Surname OF EmpRecord.
           DISPLAY "Enter Years of Service (2 digits): ".
           ACCEPT YearsOfService OF EmpRecord.
       *> Collects data for a new employee record from the user.

       200-SAVE-RECORD.
           IF YearsOfService OF EmpRecord >= 5
               WRITE EmpRecord
                   AFTER ADVANCING 1 LINE
               END-WRITE
               DISPLAY "Record saved successfully."
           ELSE
               DISPLAY "Record not saved: Less than 5 years of service."
           END-IF.
       *> Writes the entered employee record to the file if years of service are 5 or more and confirms to the user.

       300-READ-DISPLAY-RECORD.
           PERFORM UNTIL EndOfFile
               READ EmpFile INTO EmpRecord
               AT END
                   SET EndOfFile TO TRUE
               NOT AT END
                   DISPLAY EmployeeID OF EmpRecord
                   "      " DepartmentCode OF EmpRecord
                   "      " GivenName OF EmpRecord
                   " " Surname OF EmpRecord
                   " " YearsOfService OF EmpRecord
           END-READ
           END-PERFORM.
       *> Reads and displays each employee record from the file until the end of the file is reached.

       END PROGRAM EmpDataManager.
       *> Marks the end of the program.
