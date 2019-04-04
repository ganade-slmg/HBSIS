       PROGRAM-ID.   RELVENDEDOR.
       AUTHOR.       GANADE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQ-VENDEDOR   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS SEQUENTIAL
                  RECORD  KEY   IS COD-VENDEDOR
                  ALTERNATE RECORD KEY IS CPF
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-VENDEDOR.

           SELECT ARQ-TEMP      ASSIGN TO "RELVENDEDOR.TMP"
                  FILE STATUS   IS FS-TEMP.

           SELECT REL-VENDEDOR ASSIGN TO "RELVENDEDOR.CSV"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ARQ-VENDEDOR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'CADVENDEDOR'.

       COPY "CADVENDEDOR.CPY".

       SD  ARQ-TEMP.
       01  REG-TEMP.
           03 TMP-COD-VENDEDOR         PIC  9(003).
           03 TMP-CPF                  PIC  9(011).
           03 TMP-NOME-VENDEDOR        PIC  X(040).
           03 TMP-LATITUDE             PIC S9(003)V9(008).
           03 TMP-LONGITUDE            PIC S9(003)V9(008).
       
       FD  REL-VENDEDOR.
       01  REL-REGISTRO PIC X(100).

       WORKING-STORAGE SECTION.
       77  FS-VENDEDOR            PIC  X(002)         VALUE "00".
       77  FS-TEMP                PIC  X(002)         VALUE "00".
       
       01  WCAB                   PIC  X(100)         VALUE
           "COD VENDEDOR;CPF;NOME VENDEDOR;LATITUDE;LONGITUDE".

       01  WDET.
           03 WDET-COD-VENDEDOR   PIC 9(003)          VALUE ZEROS.
           03 FILLER              PIC X               VALUE ";".
           03 WDET-CPF            PIC 9(011)          VALUE ZEROS.
           03 FILLER              PIC X               VALUE ";".
           03 WDET-NOME-VENDEDOR  PIC X(040)          VALUE SPACES.
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
                      ON ASCENDING KEY TMP-COD-VENDEDOR
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              ELSE
                 SORT ARQ-TEMP
                      ON ASCENDING KEY TMP-NOME-VENDEDOR
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              END-IF
           ELSE
              IF LCLASSIFIC EQUAL 1
                 SORT ARQ-TEMP
                      ON DESCENDING KEY TMP-COD-VENDEDOR
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              ELSE
                 SORT ARQ-TEMP
                      ON DESCENDING KEY TMP-NOME-VENDEDOR               
                      INPUT  PROCEDURE IS 100-SORT
                      OUTPUT PROCEDURE IS 200-RELAT
              END-IF
           END-IF
       
           MOVE "RELATORIO GERADO COM SUCESSO" TO LMSG.

           GOBACK.
       
       100-SORT.
           OPEN INPUT ARQ-VENDEDOR 
       
           READ ARQ-VENDEDOR
           
           PERFORM 110-GERA-TMP 
             UNTIL FS-VENDEDOR NOT EQUAL "00"
             
           CLOSE ARQ-VENDEDOR.
       
       110-GERA-TMP.
           IF LCODVENDEDOR EQUAL ZEROS
              IF LRAZAONOME EQUAL SPACES
                 RELEASE REG-TEMP FROM ARQ-VENDEDOR-REG
              ELSE
                 IF NOME-VENDEDOR EQUAL LRAZAONOME
                    RELEASE REG-TEMP FROM ARQ-VENDEDOR-REG
                 END-IF
              END-IF
           ELSE
              IF COD-VENDEDOR EQUAL LCODVENDEDOR
                 RELEASE REG-TEMP FROM ARQ-VENDEDOR-REG
              END-IF
           END-IF
       
           READ ARQ-VENDEDOR.

       200-RELAT.
           OPEN OUTPUT REL-VENDEDOR
           
           RETURN ARQ-TEMP
           
           WRITE REL-REGISTRO FROM WCAB

           PERFORM 210-IMP-REL 
             UNTIL FS-TEMP NOT EQUAL "00"
           
           CLOSE REL-VENDEDOR.
           
       210-IMP-REL.
           MOVE TMP-COD-VENDEDOR       TO WDET-COD-VENDEDOR
           MOVE TMP-CPF                TO WDET-CPF
           MOVE TMP-NOME-VENDEDOR      TO WDET-NOME-VENDEDOR
           MOVE TMP-LATITUDE           TO WDET-LATITUDE
           MOVE TMP-LONGITUDE          TO WDET-LONGITUDE               

           WRITE REL-REGISTRO FROM WDET

           RETURN ARQ-TEMP.
       
       END PROGRAM RELVENDEDOR.
