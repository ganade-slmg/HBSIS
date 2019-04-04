       PROGRAM-ID.   RELDISTRIB.
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
                  ACCESS MODE      IS SEQUENTIAL
                  FILE STATUS      IS FS-DISTRIB.

           SELECT REL-DISTRIB ASSIGN TO "RELDISTRIB.CSV"
           ORGANIZATION IS LINE SEQUENTIAL.

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

       FD  REL-DISTRIB.
       01  REG-DISTRIB PIC X(110).

       WORKING-STORAGE SECTION.
       
       77 FS-VENDEDOR             PIC  X(002)         VALUE "00".
       77 FS-CLIENTE              PIC  X(002)         VALUE "00".
       77 FS-DISTRIB              PIC  X(002)         VALUE "00".

       01  WCAB.
           03 FILLER PIC X(100) VALUE "CODIGO CLIENTE;RAZAO SOCIAL CLIEN
      -"TE;CODIGO VENDEDOR;DISTANCIA CLIENTE X VENDEDOR;".
           
       01  WDET.
           03 WDET-COD-CLIENTE    PIC 9(007)          VALUE ZEROS.
           03 FILLER              PIC X(001)          VALUE ";".
           03 WDET-RAZAO-SOCIAL   PIC X(040)          VALUE SPACES.
           03 FILLER              PIC X(001)          VALUE ";".
           03 WDET-COD-VENDEDOR   PIC 9(003)          VALUE ZEROS.
           03 FILLER              PIC X(001)          VALUE ";".
           03 WDET-NOME-VENDEDOR  PIC X(040)          VALUE SPACES.
           03 FILLER              PIC X(001)          VALUE ";".
           03 WDET-DISTANCIA      PIC ZZZZZZZZ9,99    VALUE ZEROS.
           03 FILLER              PIC X(001)          VALUE ";".

       PROCEDURE DIVISION.

       000-INCIIO.

           OPEN INPUT  ARQ-CLIENTE
           OPEN INPUT  ARQ-VENDEDOR
           OPEN INPUT  ARQ-DISTRIB
           OPEN OUTPUT REL-DISTRIB
           
           READ ARQ-DISTRIB

           IF FS-DISTRIB EQUAL TO "00"
              WRITE REG-DISTRIB FROM WCAB
           END-IF
           
           PERFORM 100-IMPRIMIR
             UNTIL FS-DISTRIB NOT EQUAL "00"

           CLOSE ARQ-CLIENTE
           CLOSE ARQ-VENDEDOR
           CLOSE ARQ-DISTRIB
           CLOSE REL-DISTRIB

           GOBACK.

       100-IMPRIMIR.
           MOVE D-COD-CLIENTE  TO COD-CLIENTE
           READ ARQ-CLIENTE RECORD KEY IS COD-CLIENTE
           
           MOVE COD-CLIENTE    TO WDET-COD-CLIENTE
           MOVE RAZAO-SOCIAL   TO WDET-RAZAO-SOCIAL
       
           MOVE D-COD-VENDEDOR TO COD-VENDEDOR
           READ ARQ-VENDEDOR RECORD KEY IS COD-VENDEDOR

           MOVE COD-VENDEDOR   TO WDET-COD-VENDEDOR
           MOVE NOME-VENDEDOR  TO WDET-NOME-VENDEDOR   
           MOVE DISTANCIA      TO WDET-DISTANCIA
           
           WRITE REG-DISTRIB FROM WDET

           READ ARQ-DISTRIB.
       
       END PROGRAM RELDISTRIB.