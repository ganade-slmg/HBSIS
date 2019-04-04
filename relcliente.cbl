       PROGRAM-ID.   RELCLIENTE.
       AUTHOR.       GANADE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-CLIENTE   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS SEQUENTIAL
                  RECORD  KEY   IS COD-CLIENTE
                  ALTERNATE RECORD KEY IS CNPJ
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-CLIENTE.

           SELECT ARQ-TEMP      ASSIGN TO "RELCLIENTE.TMP"
                  FILE STATUS   IS FS-TEMP.

           SELECT REL-CLIENTE ASSIGN TO "RELCLIENTE.CSV"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-CLIENTE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'CADCLIENTE'.

       COPY "CADCLIENTE.CPY".

       SD  ARQ-TEMP.
       01  REG-TEMP.
           03 TMP-COD-CLIENTE          PIC  9(007).
           03 TMP-CNPJ                 PIC  9(014).
           03 TMP-RAZAO-SOCIAL         PIC  X(040).
           03 TMP-LATITUDE-CLIENTE     PIC S9(003)V9(008).
           03 TMP-LONGITUDE-CLIENTE    PIC S9(003)V9(008).
       
       FD  REL-CLIENTE.
       01  REL-REGISTRO PIC X(100).

       WORKING-STORAGE SECTION.
       77  FS-CLIENTE             PIC  X(002)         VALUE "00".
       77  FS-TEMP                PIC  X(002)         VALUE "00".
       
       01  WCAB                   PIC  X(100)         VALUE
           "COD CLIENTE;CNPJ;RAZAO SOCIAL;LATITUDE;LONGITUDE".

       01  WDET.
           03 WDET-COD-CLIENTE    PIC 9(007)          VALUE ZEROS.
           03 FILLER              PIC X               VALUE ";".
           03 WDET-CNPJ           PIC 9(014)          VALUE ZEROS.
           03 FILLER              PIC X               VALUE ";".
           03 WDET-RAZAO-SOCIAL   PIC X(040)          VALUE SPACES.
           03 FILLER              PIC X               VALUE ";".
           03 WDET-LATITUDE       PIC -ZZ9,99999999   VALUE ZEROS.
           03 FILLER              PIC X               VALUE ";".
           03 WDET-LONGITUDE      PIC -ZZ9,99999999   VALUE ZEROS.
       
       LINKAGE SECTION.
       01  LPARAMETROS.
           03  LORDENACAO         PIC X VALUE SPACES.
           03  LCLASSIFIC         PIC 9               VALUE ZEROS.
           03  LCODVENDEDOR       PIC 9(003)          VALUE ZEROS.
           03  LCODCLIENTE        PIC 9(007)          VALUE ZEROS.
           03  LRAZAONOME         PIC X(040)          VALUE SPACES.
           03  LMSG               PIC X(040)          VALUE SPACES.

       PROCEDURE DIVISION USING LPARAMETROS.
       
       000-INICIO.
           IF LORDENACAO EQUAL "A"
              IF LCLASSIFIC EQUAL 1
                 SORT ARQ-TEMP
                      ON ASCENDING KEY TMP-COD-CLIENTE
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              ELSE
                 SORT ARQ-TEMP
                      ON ASCENDING KEY TMP-RAZAO-SOCIAL
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              END-IF
           ELSE
              IF LCLASSIFIC EQUAL 1
                 SORT ARQ-TEMP
                      ON DESCENDING KEY TMP-COD-CLIENTE
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              ELSE
                 SORT ARQ-TEMP
                      ON DESCENDING KEY TMP-RAZAO-SOCIAL               
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              END-IF
           END-IF
       
           MOVE "RELATORIO GERADO COM SUCESSO" TO LMSG.

           GOBACK.
       
       100-SORT.
           OPEN INPUT ARQ-CLIENTE 
       
           READ ARQ-CLIENTE
           
           PERFORM 110-GERA-TMP 
             UNTIL FS-CLIENTE NOT EQUAL "00"
             
           CLOSE ARQ-CLIENTE.
       
       110-GERA-TMP.
           IF LCODCLIENTE EQUAL ZEROS
              IF LRAZAONOME EQUAL SPACES
                 RELEASE REG-TEMP FROM ARQ-CLIENTE-REG
              ELSE
                 IF RAZAO-SOCIAL EQUAL LRAZAONOME
                    RELEASE REG-TEMP FROM ARQ-CLIENTE-REG
                 END-IF
              END-IF
           ELSE
              IF COD-CLIENTE EQUAL LCODCLIENTE
                 RELEASE REG-TEMP FROM ARQ-CLIENTE-REG
              END-IF
           END-IF
       
           READ ARQ-CLIENTE.

       200-RELAT.
           OPEN OUTPUT REL-CLIENTE
           
           RETURN ARQ-TEMP
           
           WRITE REL-REGISTRO FROM WCAB

           PERFORM 210-IMP-REL 
             UNTIL FS-TEMP NOT EQUAL "00"
           
           CLOSE REL-CLIENTE.
           
       210-IMP-REL.
           MOVE TMP-COD-CLIENTE       TO WDET-COD-CLIENTE
           MOVE TMP-CNPJ              TO WDET-CNPJ
           MOVE TMP-RAZAO-SOCIAL      TO WDET-RAZAO-SOCIAL
           MOVE TMP-LATITUDE-CLIENTE  TO WDET-LATITUDE
           MOVE TMP-LONGITUDE-CLIENTE TO WDET-LONGITUDE               

           WRITE REL-REGISTRO FROM WDET

           RETURN ARQ-TEMP.
       
       END PROGRAM RELCLIENTE.
