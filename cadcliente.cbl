       IDENTIFICATION DIVISION.
       PROGRAM-ID.   CADCLIENTE.
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

           SELECT IMP-CLIENTE   ASSIGN TO W-LABEL-IMP
                  ORGANIZATION  IS LINE SEQUENTIAL
                  ACCESS MODE   IS SEQUENTIAL
                  FILE STATUS   IS FS-IMP-CLIENTE.

       DATA DIVISION.
       FILE SECTION.

       FD  ARQ-CLIENTE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'CADCLIENTE'.

       COPY "CADCLIENTE.CPY".
       
       FD  IMP-CLIENTE
           RECORD CONTAINS 100
           LABEL RECORD IS STANDARD.

       01  IMP-CLIENTE-REG.
           03 IMP-COD-CLIENTE     PIC  9(007).
           03 IMP-CNPJ            PIC  9(014).
           03 IMP-RAZAO-SOCIAL    PIC  X(040).
           03 IMP-SINAL-LAT       PIC  X.
           03 IMP-LATITUDE        PIC  9(011).
           03 IMP-SINAL-LON       PIC  X.
           03 IMP-LONGITUDE       PIC  9(011).

       WORKING-STORAGE SECTION.

       77 FS-CLIENTE              PIC X(002) VALUE "00".                

       77 WSAIR                   PIC 9               VALUE ZEROS.
       77 WOPCAO                  PIC 9               VALUE ZEROS.
       77 W-CONFIRMA              PIC X               VALUE SPACES.

       77 FS-IMP-CLIENTE          PIC X(002)          VALUE "00".       
       77 W-RETORNO               PIC 9(001)          VALUE ZEROS.
       77 W-CNPJ                  PIC 9(014)          VALUE ZEROS.
       77 W-LABEL-IMP             PIC X(020)          VALUE SPACES.     
       77 WMSG                    PIC X(040)          VALUE SPACES.
       
       01 WCONTADORES.
          03 WLIDOS               PIC 9(009)          VALUE ZEROS.
          03 WGRAVADOS            PIC 9(009)          VALUE ZEROS.

       01  W-ARQ-CLIENTE-REG.
           03 W-CODIGO-CLI        PIC  9(007)         VALUE ZEROS.
           03 W-CNPJ-CLI          PIC  9(014)         VALUE ZEROS.
           03 W-RAZAO-SOCIAL      PIC  X(040)         VALUE SPACES.
           03 W-LATITUDE-CLI      PIC S9(003)V9(008)  VALUE ZEROS.
           03 W-LONGITUDE-CLI     PIC S9(003)V9(008)  VALUE ZEROS.
       
       01  LKS-PARAMETRO.
           05 COMPRIMENTO                PIC S9(04) COMP.
           05 LKS-NUMERO-I               PIC 9(015).
           05 FILLER                     PIC X(001).
           05 LKS-NUMERO-F               PIC 9(015).
           05 FILLER                     PIC X(001).
           05 LKS-TIPO-CALCULO           PIC X(003).
           05 FILLER                     PIC X(001).
           05 LKS-ACAO                   PIC X(001).
           05 LKS-RETORNO                PIC 9(001).

       SCREEN SECTION.

       01 MENU.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO CLIENTES x 
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "CADASTRO DE CLIENTES - MENU GERAL".
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
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO CLIENTES x 
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "INCLUSAO DE CLIENTE".
          02 LINE 07 COL 10 VALUE "CODIGO CLIENTE:".
          02 LINE 07 COL 26, PIC ZZZZZZ9 TO W-CODIGO-CLI.
          02 LINE 08 COL 10 VALUE "CNPJ          :".
          02 LINE 08 COL 26, PIC 99999999999999 TO W-CNPJ-CLI.
          02 LINE 09 COL 10 VALUE "RAZAO SOCIAL  :".
          02 LINE 09 COL 26, PIC X(040) TO W-RAZAO-SOCIAL.
          02 LINE 10 COL 10 VALUE "LATITUDE      :".
          02 LINE 10 COL 26, PIC -ZZ9,99999999 TO W-LATITUDE-CLI.
          02 LINE 11 COL 10 VALUE "LONGITUDE     :".
          02 LINE 11 COL 26, PIC -ZZ9,99999999 TO W-LONGITUDE-CLI.
          02 LINE 15 COL 10 "CONFIRMA A INCLUSAO? (S/N): (.)".
          02 LINE 15 COL 39, PIC X TO W-CONFIRMA.          
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".

       01 ALTERACAO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO CLIENTES x 
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "ALTERACAO DE CLIENTE".
          02 LINE 07 COL 10 VALUE "CODIGO CLIENTE:".
          02 LINE 07 COL 26, PIC ZZZZZZ9 FROM W-CODIGO-CLI.
          02 LINE 08 COL 10 VALUE "CNPJ          :".
          02 LINE 08 COL 26, PIC 99999999999999 FROM W-CNPJ-CLI AUTO.   
          02 LINE 09 COL 10 VALUE "RAZAO SOCIAL  :".
          02 LINE 09 COL 26, PIC X(040) USING W-RAZAO-SOCIAL AUTO.
          02 LINE 10 COL 10 VALUE "LATITUDE      :".
          02 LINE 10 COL 26, PIC -ZZ9,99999999 USING W-LATITUDE-CLI
                                              AUTO.
          02 LINE 11 COL 10 VALUE "LONGITUDE     :".
          02 LINE 11 COL 26, PIC -ZZ9,99999999 USING W-LONGITUDE-CLI
                                              AUTO.
          02 LINE 15 COL 10 "CONFIRMA A ALTERACAO? (S/N): (.)".
          02 LINE 15 COL 40, PIC X TO W-CONFIRMA AUTO.          
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".

       01 EXCLUSAO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO CLIENTES x 
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "EXCLUSAO DE CLIENTE".
          02 LINE 07 COL 10 VALUE "CODIGO CLIENTE:".
          02 LINE 07 COL 26, PIC ZZZZZZ9 FROM W-CODIGO-CLI.
          02 LINE 08 COL 10 VALUE "CNPJ          :".
          02 LINE 08 COL 26, PIC 99999999999999 FROM W-CNPJ-CLI.
          02 LINE 09 COL 10 VALUE "RAZAO SOCIAL  :".
          02 LINE 09 COL 26, PIC X(040) FROM W-RAZAO-SOCIAL.
          02 LINE 10 COL 10 VALUE "LATITUDE      :".
          02 LINE 10 COL 26, PIC -ZZ9,99999999 FROM W-LATITUDE-CLI.
          02 LINE 11 COL 10 VALUE "LONGITUDE     :".
          02 LINE 11 COL 26, PIC -ZZ9,99999999 FROM W-LONGITUDE-CLI.
          02 LINE 15 COL 10 "CONFIRMA A EXCLUSAO? (S/N): (.)".
          02 LINE 15 COL 39, PIC X TO W-CONFIRMA AUTO.      
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".

       01 BUSCAR AUTO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO CLIENTES x 
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "CONSULTAR CLIENTE PARA ALTERACAO/EXCL
      -"USAO".
          02 LINE 07 COL 10 VALUE "CODIGO CLIENTE: (.......)".
          02 LINE 07 COL 27, PIC ZZZZZZ9 TO W-CODIGO-CLI.
          02 LINE 15 COL 10, PIC X(040) FROM WMSG.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".

       01 MENSAGEM AUTO.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO CLIENTES x 
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
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO CLIENTES x 
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "IMPORTACAO CLIENTE".
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
              OPEN I-O ARQ-CLIENTE

              MOVE W-CNPJ-CLI TO W-CNPJ
              MOVE W-ARQ-CLIENTE-REG TO ARQ-CLIENTE-REG

              PERFORM 500-VALIDAR

              IF LKS-RETORNO EQUAL 0
                 WRITE ARQ-CLIENTE-REG
              END-IF

              CLOSE ARQ-CLIENTE
           END-IF

           PERFORM 000-INICIO.

       200-ALTERACAO.

           OPEN I-O ARQ-CLIENTE

           DISPLAY BUSCAR
           ACCEPT  BUSCAR

           MOVE    W-CODIGO-CLI    TO COD-CLIENTE

           READ ARQ-CLIENTE RECORD INTO W-ARQ-CLIENTE-REG
             KEY IS COD-CLIENTE
           
           IF FS-CLIENTE NOT EQUAL "00"
              MOVE "CODIGO DE CLIENTE NAO LOCALIZADO"
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
                 INITIALIZE ARQ-CLIENTE-REG

                  MOVE W-CODIGO-CLI      TO COD-CLIENTE 
                  MOVE W-CNPJ-CLI        TO CNPJ
                  MOVE W-RAZAO-SOCIAL    TO RAZAO-SOCIAL
                  MOVE W-LATITUDE-CLI    TO LATITUDE-CLIENTE
                  MOVE W-LONGITUDE-CLI   TO LONGITUDE-CLIENTE
           
                  REWRITE ARQ-CLIENTE-REG
              ELSE
                 MOVE 9 TO WSAIR
              END-IF
           END-IF

           CLOSE ARQ-CLIENTE.

       300-EXCLUSAO.

           OPEN I-O ARQ-CLIENTE

           DISPLAY BUSCAR
           ACCEPT  BUSCAR

           MOVE W-CODIGO-CLI TO COD-CLIENTE
       
           READ ARQ-CLIENTE RECORD INTO W-ARQ-CLIENTE-REG
                KEY IS COD-CLIENTE
           
           IF FS-CLIENTE NOT EQUAL "00"
              MOVE "CODIGO DE CLIENTE NAO LOCALIZADO"
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
                 DELETE ARQ-CLIENTE RECORD
              ELSE
                 MOVE 9 TO WSAIR
              END-IF
           END-IF

           CLOSE ARQ-CLIENTE.

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

              OPEN INPUT IMP-CLIENTE

              IF FS-IMP-CLIENTE NOT EQUAL "00"
                 MOVE "ARQUIVO NAO LOCALIZADO" TO WMSG
              ELSE
                 OPEN I-O ARQ-CLIENTE        
              
                 PERFORM UNTIL FS-IMP-CLIENTE NOT EQUAL "00"
                    READ IMP-CLIENTE

                    IF FS-IMP-CLIENTE EQUAL ZEROS
                       ADD 1 TO WLIDOS
                       MOVE IMP-CNPJ TO W-CNPJ
                       MOVE IMP-COD-CLIENTE  TO COD-CLIENTE
                       MOVE IMP-CNPJ         TO CNPJ
                       MOVE IMP-RAZAO-SOCIAL TO RAZAO-SOCIAL

                       COMPUTE LATITUDE-CLIENTE  =
                               IMP-LATITUDE  / 100000000
                       COMPUTE LONGITUDE-CLIENTE =
                               IMP-LONGITUDE / 100000000

                       IF IMP-SINAL-LAT = "-"
                          COMPUTE LATITUDE-CLIENTE =
                                  LATITUDE-CLIENTE * -1
                       END-IF
                       IF IMP-SINAL-LON = "-"
                          COMPUTE LONGITUDE-CLIENTE =
                                  LONGITUDE-CLIENTE * -1
                       END-IF
       
                       PERFORM 500-VALIDAR

                       IF LKS-RETORNO EQUAL 0
                          WRITE ARQ-CLIENTE-REG
                          ADD 1 TO WGRAVADOS
                       END-IF
                    END-IF
                 END-PERFORM
           
                 STRING "LIDOS: "
                        WLIDOS
                        " / GRAVADOS: "
                        WGRAVADOS
                   INTO WMSG

                 CLOSE ARQ-CLIENTE
                 CLOSE IMP-CLIENTE
              END-IF
           END-IF.   

       500-VALIDAR.

           MOVE 0 TO LKS-RETORNO   
           MOVE W-CNPJ TO LKS-NUMERO-I
           MOVE "CGC" TO LKS-TIPO-CALCULO
           MOVE "V" TO LKS-ACAO

           CALL 'CALCDIGITO' USING LKS-PARAMETRO
           
           IF LKS-RETORNO EQUAL 0
              READ ARQ-CLIENTE RECORD KEY IS COD-CLIENTE

              IF FS-CLIENTE EQUAL "00"
                 MOVE 1 TO LKS-RETORNO
              ELSE
                 READ ARQ-CLIENTE RECORD KEY IS CNPJ

                 IF FS-CLIENTE EQUAL "00"
                    MOVE 1 TO LKS-RETORNO
                 END-IF
              END-IF
           END-IF.
      
       END PROGRAM CADCLIENTE.