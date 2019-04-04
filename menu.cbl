       PROGRAM-ID.   MENU.
       AUTHOR.       GANADE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       77 WERRO                   PIC 9          VALUE ZERO.
       77 WOPCAO                  PIC 9          VALUE ZERO.
       77 WCONFIRMA               PIC X          VALUE SPACES.

       01 PARAMETROS.
          03 WORDENACAO           PIC X          VALUE SPACES.
          03 WCLASSIFIC           PIC 9          VALUE ZERO.
          03 WCODVENDEDOR         PIC 9(003)     VALUE ZEROS.
          03 WCODCLIENTE          PIC 9(007)     VALUE ZEROS.
          03 WRAZAONOME           PIC X(040)     VALUE SPACES.
          03 WMSG                 PIC X(040)     VALUE SPACES.

       SCREEN SECTION.

       01 MENU.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO CLIENTES x 
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "CADASTROS".
          02 LINE 06 COL 15 VALUE "(1) CLIENTES".
          02 LINE 07 COL 15 VALUE "(2) VENDEDORES".
          02 LINE 09 COL 10 VALUE "RELATORIOS".
          02 LINE 10 COL 15 VALUE "(3) CLIENTES".
          02 LINE 11 COL 15 VALUE "(4) VENDEDORES".
          02 LINE 13 COL 10 VALUE "DISTRIBUICAO".
          02 LINE 14 COL 15 VALUE "(5) EXECUTAR".
          02 LINE 16 COL 10 VALUE "SAIR DO SISTEMA".
          02 LINE 17 COL 15 VALUE "(9) SAIR".
          02 LINE 19 COL 10 VALUE "OPCAO DESEJADA (.)".
          02 LINE 19 COL 26, PIC 9 TO WOPCAO AUTO.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".

       01 REL-CLIENTE.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO CLIENTES x 
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "RELATORIO DE CLIENTES".
          02 LINE 07 COL 10 VALUE
             "ORDENAR (A)SCENDENTE OU (D)ESCENDENTE.....: (.)".
          02 LINE 07 COL 55, PIC X TO WORDENACAO AUTO.
          02 LINE 08 COL 10 VALUE
             "CLASSIFICAR (1)-CODIGO OU (2)-RAZAO SOCIAL: (.)".
          02 LINE 08 COL 55, PIC 9 TO WCLASSIFIC AUTO.        
          02 LINE 10 COL 10 VALUE "FILTRAR CODIGO CLIENTE: (.......)".
          02 LINE 10 COL 35, PIC ZZZZZZ9 TO WCODCLIENTE AUTO.      
          02 LINE 11 COL 10 VALUE
          "FILTRAR RAZAO SOCIAL..: (....................................
      -"....)".
          02 LINE 11 COL 35, PIC X(040) TO WRAZAONOME AUTO.         
          02 LINE 15 COL 10 VALUE 
          "CONFIRMA GERACAO RELATORIO? (S/N): (.)".
          02 LINE 15 COL 46, PIC X TO WCONFIRMA AUTO.
          02 LINE 19 COL 10 VALUE "MSG:".
          02 LINE 19 COL 15, PIC X(040) FROM WMSG.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".

       01 REL-VENDEDOR.
          02 BLANK SCREEN.
          02 LINE 01 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 02 COL 15 VALUE "LOGISTICA DE DISTRIBUICAO CLIENTES x 
      -"VENDEDORES".
          02 LINE 03 COL 01 VALUE "=====================================
      -"========================================".
          02 LINE 05 COL 10 VALUE "RELATORIO DE VENDEDORES".
          02 LINE 07 COL 10 VALUE
             "ORDENAR (A)SCENDENTE OU (D)ESCENDENTE......: (.)".
          02 LINE 07 COL 56, PIC X TO WORDENACAO AUTO.
          02 LINE 08 COL 10 VALUE
             "CLASSIFICAR (1)-CODIGO OU (2)-NOME VENDEDOR: (.)".
          02 LINE 08 COL 56, PIC 9 TO WCLASSIFIC AUTO.        
          02 LINE 10 COL 10 VALUE "FILTRAR CODIGO VENDEDOR: (...)".
          02 LINE 10 COL 36, PIC ZZ9 TO WCODVENDEDOR AUTO.      
          02 LINE 11 COL 10 VALUE
             "FILTRAR NOME VENDEDOR..: (................................
      -"........)".
          02 LINE 11 COL 36, PIC X(40) TO WRAZAONOME AUTO.
          02 LINE 15 COL 10 VALUE 
          "CONFIRMA GERACAO RELATORIO? (S/N): (.)".
          02 LINE 15 COL 46, PIC X TO WCONFIRMA AUTO.
          02 LINE 19 COL 10 VALUE "MSG:".
          02 LINE 19 COL 15, PIC X(040) FROM WMSG.
          02 LINE 21 COL 01 VALUE "=====================================
      -"========================================".

       PROCEDURE DIVISION.

       000-MENU.

           DISPLAY MENU
           ACCEPT  MENU
       
           EVALUATE WOPCAO
              WHEN 1
                 CALL "CADCLIENTE"
              WHEN 2
                 CALL "CADVENDEDOR"
              WHEN 3
                 PERFORM 100-REL-CLIENTE
              WHEN 4
                 PERFORM 200-REL-VENDEDOR
              WHEN 5
                 CALL "FAZDISTRIB"
              WHEN 9
                 STOP RUN
           END-EVALUATE

           PERFORM 000-MENU.

       100-REL-CLIENTE.

           DISPLAY REL-CLIENTE
           ACCEPT  REL-CLIENTE

           MOVE 0 TO WERRO        

           IF FUNCTION UPPER-CASE(WORDENACAO) NOT EQUAL "A" AND
              FUNCTION UPPER-CASE(WORDENACAO) NOT EQUAL "D"
              MOVE "ORDENACAO INVALIDA. INFORME A OU D" TO WMSG
              MOVE 9 TO WERRO        
           END-IF

           IF WCLASSIFIC NOT EQUAL 1 AND                     
              WCLASSIFIC NOT EQUAL 2
              MOVE "CLASSIFICACAO INVALIDA. INFORME 1 OU 2" TO WMSG
              MOVE 9 TO WERRO        
           END-IF

           IF FUNCTION UPPER-CASE(WCONFIRMA) = "S"
              IF WERRO EQUAL 0     
                 MOVE FUNCTION UPPER-CASE(WORDENACAO) 
                   TO WORDENACAO
                 CALL "RELCLIENTE" USING PARAMETROS
              END-IF
              PERFORM 100-REL-CLIENTE
           END-IF

           PERFORM 000-MENU.

       200-REL-VENDEDOR.

           DISPLAY REL-VENDEDOR
           ACCEPT  REL-VENDEDOR

           MOVE 0 TO WERRO        

           IF FUNCTION UPPER-CASE(WORDENACAO) NOT EQUAL "A" AND
              FUNCTION UPPER-CASE(WORDENACAO) NOT EQUAL "D"
              MOVE "ORDENACAO INVALIDA. INFORME A OU D" TO WMSG
              MOVE 9 TO WERRO        
           END-IF

           IF WCLASSIFIC NOT EQUAL 1 AND                     
              WCLASSIFIC NOT EQUAL 2
              MOVE "CLASSIFICACAO INVALIDA. INFORME 1 OU 2" TO WMSG
              MOVE 9 TO WERRO        
           END-IF

           IF FUNCTION UPPER-CASE(WCONFIRMA) = "S"
              IF WERRO EQUAL 0     
                 MOVE FUNCTION UPPER-CASE(WORDENACAO) 
                   TO WORDENACAO
                 CALL "RELVENDEDOR" USING PARAMETROS
              END-IF
              PERFORM 200-REL-VENDEDOR
           END-IF

           PERFORM 000-MENU.
       
       END PROGRAM MENU.