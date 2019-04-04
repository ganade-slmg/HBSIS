       IDENTIFICATION DIVISION.
       PROGRAM-ID.   CADVENDEDOR.
       AUTHOR.       GANADE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
          
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ARQ-VENDEDOR   ASSIGN TO DISK
                  ORGANIZATION  IS INDEXED
                  ACCESS MODE   IS DYNAMIC
                  RECORD  KEY   IS COD-VENDEDOR
                  ALTERNATE RECORD KEY IS CPF
                  LOCK MODE     IS MANUAL
                  FILE STATUS   IS FS-VENDEDOR.

           SELECT IMP-VENDEDOR   ASSIGN TO W-LABEL-IMP
                  ORGANIZATION  IS LINE SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL
                  FILE STATUS   IS FS-IMP-VENDEDOR.

       DATA DIVISION.
       FILE SECTION.

       FD  ARQ-VENDEDOR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'CADVENDEDOR'.

       COPY "CADVENDEDOR.CPY".
       
       FD  IMP-VENDEDOR
           RECORD CONTAINS 100
           LABEL RECORD IS STANDARD.

       01  IMP-VENDEDOR-REG.
           03 IMP-COD-VENDEDOR     PIC  9(003).
           03 IMP-CPF              PIC  9(011).
           03 IMP-NOME-VENDEDOR    PIC  X(040).
           03 IMP-SINAL-LAT        PIC  X.
           03 IMP-LATITUDE         PIC  9(011).
           03 IMP-SINAL-LON        PIC  X.
           03 IMP-LONGITUDE        PIC  9(011).

       WORKING-STORAGE SECTION.

       77 FS-VENDEDOR              PIC X(002) VALUE "00".               

       77 WSAIR                   PIC 9               VALUE ZEROS.
       77 WOPCAO                  PIC 9               VALUE ZEROS.
       77 W-CONFIRMA              PIC X               VALUE SPACES.

       77 FS-IMP-VENDEDOR         PIC X(002)          VALUE "00".      
       77 W-RETORNO               PIC 9(001)          VALUE ZEROS.
       77 W-CPF                   PIC 9(011)          VALUE ZEROS.
       77 W-LABEL-IMP             PIC X(020)          VALUE SPACES.     
       77 WMSG                    PIC X(040)          VALUE SPACES.
       
       01 WCONTADORES.
          03 WLIDOS               PIC 9(009)          VALUE ZEROS.
          03 WGRAVADOS            PIC 9(009)          VALUE ZEROS.

       01  W-ARQ-VENDEDOR-REG.
           03 W-CODIGO-VEN        PIC  9(003)         VALUE ZEROS.
           03 W-CPF-VEN           PIC  9(011)         VALUE ZEROS.
           03 W-NOME-VENDEDOR     PIC  X(040)         VALUE SPACES.    
           03 W-LATITUDE-VEN      PIC S9(003)V9(008)  VALUE ZEROS.
           03 W-LONGITUDE-VEN     PIC S9(003)V9(008)  VALUE ZEROS.
       
       01  LKS-PARAMETRO.
           05 COMPRIMENTO         PIC S9(04) COMP.
           05 LKS-NUMERO-I        PIC 9(015).
           05 FILLER              PIC X(001).
           05 LKS-NUMERO-F        PIC 9(015).
           05 FILLER              PIC X(001).
           05 LKS-TIPO-CALCULO    PIC X(003).
           05 FILLER              PIC X(001).
           05 LKS-ACAO            PIC X(001).
           05 LKS-RETORNO         PIC 9(001).

       SCREEN SECTION.

       01 MENU.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO VENDEDORS x
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "CADASTRO DE VENDEDORS - MENU GERAL". 
          02 LINE 07 COL 10 VALUE "(1) INCLUIR".
          02 LINE 08 COL 10 VALUE "(2) ALTERAR".
          02 LINE 09 COL 10 VALUE "(3) EXCLUIR".
          02 LINE 10 COL 10 VALUE "(4) IMPORTAR".
          02 LINE 11 COL 10 VALUE "(9) VOLTAR AO MENU".
          02 LINE 15 COL 10 "OPCAO DESEJADA: (.)".
          02 LINE 15 COL 27 PIC 9 TO WOPCAO AUTO.              
          02 LINE 19 COL 10, PIC X(040) FROM WMSG.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".

       01 INCLUSAO AUTO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO VENDEDORS x
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "INCLUSAO DE VENDEDOR".
          02 LINE 07 COL 10 VALUE "CODIGO VENDEDOR:".
          02 LINE 07 COL 27, PIC ZZ9 TO W-CODIGO-VEN.
          02 LINE 08 COL 10 VALUE "CPF            :".
          02 LINE 08 COL 27, PIC 99999999999 TO W-CPF-VEN.
          02 LINE 09 COL 10 VALUE "RAZAO SOCIAL   :".
          02 LINE 09 COL 27, PIC X(040) TO W-NOME-VENDEDOR.
          02 LINE 10 COL 10 VALUE "LATITUDE       :".
          02 LINE 10 COL 27, PIC -ZZ9,99999999 TO W-LATITUDE-VEN.
          02 LINE 11 COL 10 VALUE "LONGITUDE      :".
          02 LINE 11 COL 27, PIC -ZZ9,99999999 TO W-LONGITUDE-VEN.
          02 LINE 15 COL 10 "CONFIRMA A INCLUSAO? (S/N): (.)".
          02 LINE 15 COL 39, PIC X TO W-CONFIRMA.          
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".

       01 ALTERACAO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO VENDEDORS x
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "ALTERACAO DE VENDEDOR".
          02 LINE 07 COL 10 VALUE "CODIGO VENDEDOR:".
          02 LINE 07 COL 27, PIC ZZ9 FROM W-CODIGO-VEN.
          02 LINE 08 COL 10 VALUE "CPF            :".
          02 LINE 08 COL 27, PIC 99999999999 FROM W-CPF-VEN AUTO.       
          02 LINE 09 COL 10 VALUE "RAZAO SOCIAL   :".
          02 LINE 09 COL 27, PIC X(040) USING W-NOME-VENDEDOR AUTO.     
          02 LINE 10 COL 10 VALUE "LATITUDE       :".
          02 LINE 10 COL 27, PIC -ZZ9,99999999 USING W-LATITUDE-VEN
                                              AUTO.
          02 LINE 11 COL 10 VALUE "LONGITUDE      :".
          02 LINE 11 COL 27, PIC -ZZ9,99999999 USING W-LONGITUDE-VEN
                                              AUTO.
          02 LINE 15 COL 10 "CONFIRMA A ALTERACAO? (S/N): (.)".
          02 LINE 15 COL 40, PIC X TO W-CONFIRMA AUTO.          
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".

       01 EXCLUSAO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO VENDEDORS x
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "EXCLUSAO DE VENDEDOR".
          02 LINE 07 COL 10 VALUE "CODIGO VENDEDOR:".
          02 LINE 07 COL 27, PIC ZZ9 FROM W-CODIGO-VEN.
          02 LINE 08 COL 10 VALUE "CPF            :".
          02 LINE 08 COL 27, PIC 99999999999 FROM W-CPF-VEN.
          02 LINE 09 COL 10 VALUE "RAZAO SOCIAL   :".
          02 LINE 09 COL 27, PIC X(040) FROM W-NOME-VENDEDOR.           
          02 LINE 10 COL 10 VALUE "LATITUDE       :".
          02 LINE 10 COL 27, PIC -ZZ9,99999999 FROM W-LATITUDE-VEN.
          02 LINE 11 COL 10 VALUE "LONGITUDE      :".
          02 LINE 11 COL 27, PIC -ZZ9,99999999 FROM W-LONGITUDE-VEN.
          02 LINE 15 COL 10 "CONFIRMA A EXCLUSAO? (S/N): (.)".
          02 LINE 15 COL 39, PIC X TO W-CONFIRMA AUTO.      
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".

       01 BUSCAR AUTO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO VENDEDORS x
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "CONSULTAR VENDEDOR PARA ALTERACAO/EXC
      -"LUSAO".
          02 LINE 07 COL 10 VALUE "CODIGO VENDEDOR: (...)".
          02 LINE 07 COL 28, PIC ZZ9 TO W-CODIGO-VEN.
          02 LINE 15 COL 10, PIC X(040) FROM WMSG.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".

       01 MENSAGEM AUTO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO VENDEDORS x
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 10 COL 10 VALUE "MSG:".
          02 LINE 10 COL 15, PIC X(040) FROM WMSG.
          02 LINE 15 COL 10, "FAZER OUTRA CONSULTA? (S/N): (.)".
          02 LINE 15 COL 40, PIC X TO W-CONFIRMA.          
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".

       01 IMPORTACAO AUTO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO VENDEDORS x
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "IMPORTACAO VENDEDOR".
          02 LINE 07 COL 10 VALUE "INFORMAR ARQUIVO PARA IMPORTACAO:".
          02 LINE 07 COL 44, PIC X(020) TO W-LABEL-IMP.
          02 LINE 15 COL 10 VALUE "CONFIRMA A IMPORTACAO? (S/N): (.)".
          02 LINE 15 COL 41, PIC X TO W-CONFIRMA.      
          02 LINE 19 COL 10 VALUE "MSG:".
          02 LINE 19 COL 15, PIC X(040) FROM WMSG.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".

       PROCEDURE DIVISION.

       000-INICIO.

           DISPLAY MENU
           ACCEPT  MENU

           EVALUATE WOPCAO
             WHEN 1
                PERFORM 100-INCLUSAO
             WHEN 2
                MOVE ZEROS TO WSAIR
                PERFORM 200-ALTERACAO
                  UNTIL WSAIR NOT EQUAL ZEROS
             WHEN 3
                MOVE ZEROS TO WSAIR
                PERFORM 300-EXCLUSAO
                  UNTIL WSAIR NOT EQUAL ZEROS
             WHEN 4
                PERFORM 400-IMPORTACAO
             WHEN 9
                GOBACK
           END-EVALUATE

           PERFORM 000-INICIO.

       100-INCLUSAO.

           DISPLAY  INCLUSAO
           ACCEPT   INCLUSAO

           IF FUNCTION UPPER-CASE(W-CONFIRMA) EQUAL "S"
              OPEN I-O ARQ-VENDEDOR

              MOVE W-CPF-VEN TO W-CPF
              MOVE W-ARQ-VENDEDOR-REG TO ARQ-VENDEDOR-REG

              PERFORM 500-VALIDAR

              IF LKS-RETORNO EQUAL 0
                 WRITE ARQ-VENDEDOR-REG
              END-IF

              CLOSE ARQ-VENDEDOR
           END-IF

           PERFORM 000-INICIO.

       200-ALTERACAO.

           OPEN I-O ARQ-VENDEDOR

           DISPLAY BUSCAR
           ACCEPT  BUSCAR

           MOVE    W-CODIGO-VEN    TO COD-VENDEDOR

           READ ARQ-VENDEDOR RECORD INTO W-ARQ-VENDEDOR-REG
             KEY IS COD-VENDEDOR
           
           IF FS-VENDEDOR NOT EQUAL "00"
              MOVE "CODIGO DE VENDEDOR NAO LOCALIZADO"
                TO WMSG
              DISPLAY MENSAGEM
              ACCEPT  MENSAGEM

              IF FUNCTION UPPER-CASE(W-CONFIRMA) EQUAL "N"
                 MOVE 9 TO WSAIR
              ELSE
                 MOVE "INFORME NOVO CODIGO PARA CONSULTA"
                   TO WMSG
              END-IF
           ELSE
              DISPLAY ALTERACAO
              ACCEPT  ALTERACAO
           
              IF FUNCTION UPPER-CASE(W-CONFIRMA) EQUAL "S"
                 INITIALIZE ARQ-VENDEDOR-REG

                  MOVE W-CODIGO-VEN      TO COD-VENDEDOR 
                  MOVE W-CPF-VEN         TO CPF
                  MOVE W-NOME-VENDEDOR   TO NOME-VENDEDOR
                  MOVE W-LATITUDE-VEN    TO LATITUDE-VENDEDOR
                  MOVE W-LONGITUDE-VEN   TO LONGITUDE-VENDEDOR
           
                  REWRITE ARQ-VENDEDOR-REG
              ELSE
                 MOVE 9 TO WSAIR
              END-IF
           END-IF

           CLOSE ARQ-VENDEDOR.

       300-EXCLUSAO.

           OPEN I-O ARQ-VENDEDOR

           DISPLAY BUSCAR
           ACCEPT  BUSCAR

           MOVE W-CODIGO-VEN TO COD-VENDEDOR
       
           READ ARQ-VENDEDOR RECORD INTO W-ARQ-VENDEDOR-REG
                KEY IS COD-VENDEDOR
           
           IF FS-VENDEDOR NOT EQUAL "00"
              MOVE "CODIGO DE VENDEDOR NAO LOCALIZADO"
                TO WMSG
              DISPLAY MENSAGEM
              ACCEPT  MENSAGEM

              IF FUNCTION UPPER-CASE(W-CONFIRMA) EQUAL "N"
                 MOVE 9 TO WSAIR
              ELSE
                 MOVE "INFORME NOVO CODIGO PARA CONSULTA"
                   TO WMSG
              END-IF
           ELSE
              DISPLAY EXCLUSAO
              ACCEPT  EXCLUSAO
           
              IF FUNCTION UPPER-CASE(W-CONFIRMA) EQUAL "S"
                 DELETE ARQ-VENDEDOR RECORD
              ELSE
                 MOVE 9 TO WSAIR
              END-IF
           END-IF

           CLOSE ARQ-VENDEDOR.

       400-IMPORTACAO.

           INITIALIZE WCONTADORES

           DISPLAY IMPORTACAO
           ACCEPT  IMPORTACAO
           
           MOVE SPACES TO WMSG

           IF FUNCTION UPPER-CASE(W-CONFIRMA) EQUAL "S"
              IF W-LABEL-IMP EQUAL SPACES
                 MOVE "INFORME NOME DO ARQUIVO" TO WMSG
                 PERFORM 400-IMPORTACAO
              END-IF

              OPEN INPUT IMP-VENDEDOR

              IF FS-IMP-VENDEDOR NOT EQUAL "00"
                 MOVE "ARQUIVO NAO LOCALIZADO" TO WMSG
              ELSE
                 OPEN I-O ARQ-VENDEDOR        
              
                 PERFORM UNTIL FS-IMP-VENDEDOR NOT EQUAL "00"
                    READ IMP-VENDEDOR

                    IF FS-IMP-VENDEDOR EQUAL ZEROS
                       ADD 1 TO WLIDOS
                       MOVE IMP-CPF           TO W-CPF
                       MOVE IMP-COD-VENDEDOR  TO COD-VENDEDOR
                       MOVE IMP-CPF           TO CPF
                       MOVE IMP-NOME-VENDEDOR TO NOME-VENDEDOR          

                       COMPUTE LATITUDE-VENDEDOR  =
                               IMP-LATITUDE  / 100000000
                       COMPUTE LONGITUDE-VENDEDOR =
                               IMP-LONGITUDE / 100000000

                       IF IMP-SINAL-LAT = "-"
                          COMPUTE LATITUDE-VENDEDOR =
                                  LATITUDE-VENDEDOR * -1
                       END-IF
                       IF IMP-SINAL-LON = "-"
                          COMPUTE LONGITUDE-VENDEDOR =
                                  LONGITUDE-VENDEDOR * -1
                       END-IF
       
                       PERFORM 500-VALIDAR

                       IF LKS-RETORNO EQUAL 0
                          WRITE ARQ-VENDEDOR-REG
                          ADD 1 TO WGRAVADOS
                       END-IF
                    END-IF
                 END-PERFORM
           
                 STRING "LIDOS: "
                        WLIDOS
                        " / GRAVADOS: "
                        WGRAVADOS
                   INTO WMSG

                 CLOSE ARQ-VENDEDOR
                 CLOSE IMP-VENDEDOR
              END-IF
           END-IF.   

       500-VALIDAR.

           MOVE 0 TO LKS-RETORNO   
           MOVE W-CPF TO LKS-NUMERO-I
           MOVE "CPF" TO LKS-TIPO-CALCULO
           MOVE "V" TO LKS-ACAO

           CALL 'CALCDIGITO' USING LKS-PARAMETRO
           
           IF LKS-RETORNO EQUAL 0
              READ ARQ-VENDEDOR RECORD KEY IS COD-VENDEDOR

              IF FS-VENDEDOR EQUAL "00"
                 MOVE 1 TO LKS-RETORNO
              ELSE
                 READ ARQ-VENDEDOR RECORD KEY IS CPF

                 IF FS-VENDEDOR EQUAL "00"
                    MOVE 1 TO LKS-RETORNO
                 END-IF
              END-IF
           END-IF.
      
       END PROGRAM CADVENDEDOR.