       PROGRAM-ID.   FAZDISTRIB.
       AUTHOR.       GANADE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-CLIENTE   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS COD-CLIENTE
                  ALTERNATE RECORD KEY IS CNPJ
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-CLIENTE.

           SELECT ARQ-VENDEDOR  ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS COD-VENDEDOR
                  ALTERNATE RECORD KEY IS CPF
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-VENDEDOR.

           SELECT ARQ-DISTRIB ASSIGN TO DISK
                  ORGANIZATION     IS LINE SEQUENTIAL
                  FILE STATUS      IS FS-DISTRIB.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-CLIENTE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'CADCLIENTE'.

       COPY "CADCLIENTE.CPY".
       
       FD  ARQ-VENDEDOR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'CADVENDEDOR'.

       COPY "CADVENDEDOR.CPY".
       
       FD  ARQ-DISTRIB
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'CADDISTRIB'.

       COPY "CADDISTRIB.CPY".

       WORKING-STORAGE SECTION.

       77 FS-VENDEDOR            PIC  X(002) VALUE "00".
       77 FS-CLIENTE             PIC  X(002) VALUE "00".
       77 FS-DISTRIB             PIC  X(002) VALUE "00".

       77 W-MENOR-DISTANCIA      PIC  9(009)V9(002) VALUE 999999999.
       77 W-CALC-DISTANCIA       PIC  9(009)V9(002) VALUE ZEROS.
       77 W-LATITUDE-1           PIC S9(003)V9(008) VALUE ZEROS.
       77 W-LATITUDE-2           PIC S9(003)V9(008) VALUE ZEROS.
       77 W-LONGITUDE-1          PIC S9(003)V9(008) VALUE ZEROS.
       77 W-LONGITUDE-2          PIC S9(003)V9(008) VALUE ZEROS.
       77 W-DLA                  PIC S9(003)V9(008) VALUE ZEROS.        
       77 W-DLO                  PIC S9(003)V9(008) VALUE ZEROS.        
       77 W-A                    PIC S9(003)V9(008) VALUE ZEROS.        
       77 W-C                    PIC S9(003)V9(008) VALUE ZEROS.        

       PROCEDURE DIVISION.

       000-INICIO.

           OPEN INPUT ARQ-CLIENTE
           OPEN OUTPUT ARQ-DISTRIB
           
           READ ARQ-CLIENTE NEXT
           
           PERFORM 100-LER-CLIENTE UNTIL FS-CLIENTE NOT EQUAL "00"      
           
           CLOSE ARQ-CLIENTE
           CLOSE ARQ-VENDEDOR
           CLOSE ARQ-DISTRIB
           
           CALL "RELDISTRIB".
       
       100-LER-CLIENTE.
           MOVE COD-CLIENTE TO D-COD-CLIENTE

           OPEN INPUT ARQ-VENDEDOR
           READ ARQ-VENDEDOR NEXT
           
           PERFORM 200-LER-VENDEDOR UNTIL FS-VENDEDOR NOT EQUAL "00"    
           
           MOVE  W-MENOR-DISTANCIA TO DISTANCIA
           MOVE  999999999         TO W-MENOR-DISTANCIA 
           WRITE ARQ-DISTRIB-REG

           CLOSE ARQ-VENDEDOR
           
           READ ARQ-CLIENTE NEXT.
       
       200-LER-VENDEDOR.
           COMPUTE W-LATITUDE-1 = LATITUDE-CLIENTE
                                * FUNCTION PI
                                / 180
       
           COMPUTE W-LATITUDE-2 = LATITUDE-VENDEDOR
                                * FUNCTION PI
                                / 180

           COMPUTE W-LONGITUDE-1 = LONGITUDE-CLIENTE
                                * FUNCTION PI
                                / 180
                              
           COMPUTE W-LONGITUDE-2 = LONGITUDE-VENDEDOR
                                * FUNCTION PI
                                / 180

           COMPUTE W-DLA = W-LATITUDE-2 - (W-LATITUDE-1) 

           COMPUTE W-DLO = W-LONGITUDE-2 - (W-LONGITUDE-1) 

           COMPUTE W-A = FUNCTION SIN(W-DLA / 2)
                       * FUNCTION SIN(W-DLA / 2)
                       + FUNCTION COS(W-LATITUDE-1)
                       * FUNCTION COS(W-LATITUDE-2)
                       * FUNCTION SIN(W-DLO / 2)
                       * FUNCTION SIN(W-DLO / 2)
           
           COMPUTE W-C = 2 * FUNCTION ATAN(FUNCTION SQRT(W-A) /
                                           FUNCTION SQRT(1 - W-A))

           COMPUTE W-CALC-DISTANCIA = 6731 * W-C * 1000

           IF W-CALC-DISTANCIA < W-MENOR-DISTANCIA
              MOVE W-CALC-DISTANCIA TO W-MENOR-DISTANCIA
              MOVE COD-VENDEDOR     TO D-COD-VENDEDOR
           END-IF
           
           READ ARQ-VENDEDOR NEXT.

       END PROGRAM FAZDISTRIB.