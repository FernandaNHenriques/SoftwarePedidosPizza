       IDENTIFICATION DIVISION.
       PROGRAM-ID. 0807_ProjetoFinal.
       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       77  NUMPEDIDO                                PIC 9(03).
       77  CONTADORPEDIDOS                          PIC 9(03) VALUE 1.
       01  DATA-ATUAL.
           05 ANO                                   PIC 9(04).
           05 MES                                   PIC 9(02).
           05 DIA                                   PIC 9(02).
       01 HORA-ATUAL.
           05 HORA                                  PIC 9(02).
           05 MINUTO                                PIC 9(02).
           05 SEGUNDO                               PIC 9(02).
       77  CLIENTE                                  PIC A.
       77  CONTATO                                  PIC 9(09).
       77  CONTADOR                                 PIC 9(03).
       77  TEMP                                     PIC X(10).
       01  TABELAS.
           05 NUMERO                               PIC 9(02).
           05 TAMANHO                              PIC 9(02).
           88 VALIDARTAMANHO                  VALUES 1 THRU 3.
       77  MAXINGREDIENTES                          PIC 9(02).
           88 VALIDARING                      VALUES 1 THRU 5.
       01  ESCOLHAING                               PIC 9(02).
       77  TOTALINGREDIENTES                        PIC 9(02)V99.
       77  LINHA                          PIC 9(03) VALUES 15.
       77  SAIDA                                    PIC ZZ9.99.
       77  SAIDAIVA                                 PIC ZZ9.99.
       77  TOTAL                                    PIC 9(03)V99.
       77  TOTALIVA                                 PIC 9(02)V99.
       77  TOTAL-VALORPIZZA                         PIC 9(03)V99.
       77  VALORPIZZA                               PIC 9(02).
       01  EXTRAS.
           05 CODIGO                PIC 9(02) OCCURS 10 TIMES.
           05 NOME                  PIC X(30) OCCURS 10 TIMES.
           05 VALOR                 PIC 9(02)V99 OCCURS 10 TIMES.
           05 REPETIDO              PIC 9(02)V99 OCCURS 10 TIMES.
       77  REPETIR                                   PIC A.
           88 VALIDAR-REPETIR                 VALUES "S", "s", "N", "n".
.


       SCREEN SECTION.
       01 LIMPAR-ENCRAN BLANK SCREEN.

       01 TITULO.
           05 COL 01 VALUES
           "                                                           "
           BACKGROUND-COLOR 4 HIGHLIGHT                         LINE 01.
           05 COL 01 VALUES
           "         PIZZARIA RAMALHO, Software de Pedidos             "
           BACKGROUND-COLOR 2 HIGHLIGHT FOREGROUND-COLOR 6
           LINE 02.
           05 COL 01 VALUES
           "                Pizzas Deliciosas, Lda.                    "
           BACKGROUND-COLOR 2 HIGHLIGHT FOREGROUND-COLOR 6
           LINE 03.
           05 COL 01 VALUES
           "                                                           "
           BACKGROUND-COLOR 4 HIGHLIGHT                         LINE 04.

       01 INGREDIENTES.
           05 COL 60 VALUES     "INGREDIENTES EXTRAS: "
           FOREGROUND-COLOR 2 HIGHLIGHT                         LINE 10.
           05 COL 60 VALUES
           "--------------------------------------------------" LINE 11.
           05 COL 60 VALUES " 1 - FIAMBRE................(0.5)" LINE 12.
           05 COL 60 VALUES " 2 - ATUM...................(0.7)" LINE 13.
           05 COL 60 VALUES " 3 - ANCHOVAS...............(0.4)" LINE 14.
           05 COL 60 VALUES " 4 - CAMARAO................(0.8)" LINE 15.
           05 COL 60 VALUES " 5 - BACON........... ......(0.9)" LINE 16.
           05 COL 60 VALUES " 6 - BANANA.................(0.6)" LINE 17.
           05 COL 60 VALUES " 7 - ANANAS.................(0.4)" LINE 18.
           05 COL 60 VALUES " 8 - AZEITONAS..............(0.3)" LINE 19.
           05 COL 60 VALUES " 9 - COGUMELOS..............(0.6)" LINE 20.
           05 COL 60 VALUES "10 - MILHO..................(0.5)" LINE 21.
           05 COL 60 VALUES
           "--------------------------------------------------" LINE 22.

       PROCEDURE DIVISION.

       REGISTO.
           DISPLAY LIMPAR-ENCRAN.
           DISPLAY TITULO.
           DISPLAY FUNCTION CONCATENATE ("Numero do Pedido: ",
           CONTADORPEDIDOS)                                     AT 0501.
           DISPLAY "Cliente: "                                  AT 0535.
           ACCEPT TEMP                                          AT 0544.
           MOVE TEMP TO CLIENTE.
           DISPLAY "Contato: "                                  AT 0555.
           ACCEPT TEMP                                          AT 0564.
           MOVE TEMP TO CONTATO.
           ACCEPT DATA-ATUAL FROM DATE YYYYMMDD.
           DISPLAY FUNCTION CONCATENATE ("Data:" , DIA, "/", MES, "/",
           ANO, ".")                                            AT 0601.
           ACCEPT HORA-ATUAL FROM TIME.
           DISPLAY FUNCTION CONCATENATE ("Hora: ", HORA, ":", MINUTO,
           ":", SEGUNDO, " h.")                                 AT 0625.

       VALIDAR-TAMANHO.
           DISPLAY "[1] Pequena [2] Media [3] Grande|"          AT 0901.
           DISPLAY FUNCTION CONCATENATE ("Tipo de pizza pretendido: ",
           TAMANHO)                                             AT 1001.
           ACCEPT TEMP                                          AT 1028.
           MOVE TEMP TO TAMANHO
           IF NOT VALIDARTAMANHO THEN
               DISPLAY "OPCAO INVALIDA!" FOREGROUND-COLOR 4 HIGHLIGHT
                                                                 AT 1015
               GO VALIDAR-TAMANHO
           END-IF.

           EVALUATE TAMANHO
               WHEN 1
                  DISPLAY "Pequena - 3 Euros"                    AT 0935
                  ADD 3 TO VALORPIZZA
               WHEN 2
                  DISPLAY "Media - 4 Euros"                      AT 0935
                  ADD 4 TO VALORPIZZA
               WHEN 3

                   DISPLAY "Grande - 5 Euros"                    AT 0935
                   ADD 5 TO VALORPIZZA
           END-EVALUATE.

           DISPLAY INGREDIENTES.

       ARRAY-EXTRAS.
           MOVE 1 TO CODIGO(1).
           MOVE "Fiambre" TO NOME (1).
           MOVE 0.5 TO VALOR(1).
           MOVE 0 TO REPETIDO(1).

           MOVE 2 TO CODIGO(2).
           MOVE "Atum" TO NOME (2).
           MOVE 0.7 TO VALOR(2).
           MOVE 0 TO REPETIDO(2).

           MOVE 3 TO CODIGO(3).
           MOVE "Anchovas" TO NOME (3).
           MOVE 0.4 TO VALOR(3).
           MOVE 0 TO REPETIDO(3).

           MOVE 4 TO CODIGO(4).
           MOVE "Camarao" TO NOME (4).
           MOVE 0.8 TO VALOR(4).
           MOVE 0 TO REPETIDO(4).

           MOVE 5 TO CODIGO(5).
           MOVE "Bacon" TO NOME (5).
           MOVE 0.9 TO VALOR(5).
           MOVE 0 TO REPETIDO(5).

           MOVE 6 TO CODIGO(6).
           MOVE "Banana" TO NOME (6).
           MOVE 0.6 TO VALOR(6).
           MOVE 0 TO REPETIDO(6).

           MOVE 7 TO CODIGO(7).
           MOVE "Ananas" TO NOME (7).
           MOVE 0.4 TO VALOR(7).
           MOVE 0 TO REPETIDO(7).

           MOVE 8 TO CODIGO(8).
           MOVE "Azeitonas" TO NOME (8).
           MOVE 0.3 TO VALOR(8).
           MOVE 0 TO REPETIDO(8).

           MOVE 9 TO CODIGO(9).
           MOVE "Cogumelos" TO NOME (9).
           MOVE 0.6 TO VALOR(9).
           MOVE 0 TO REPETIDO(9).

           MOVE 10 TO CODIGO(10).
           MOVE "Milho" TO NOME (10).
           MOVE 0.5 TO VALOR(10).
           MOVE 0 TO REPETIDO(10).

       VALIDAR-INGREDIENTE.
           DISPLAY
           "Ingredientes Extras (1 a 5): "                      AT 1101.
           ACCEPT TEMP                                          AT 1130.
           MOVE TEMP TO MAXINGREDIENTES.

           IF NOT VALIDARING THEN
               DISPLAY "OPCAO INVALIDA! Escolha 1 a 5."
               FOREGROUND-COLOR 4 HIGHLIGHT                      AT 1125
               GO TO VALIDAR-INGREDIENTE
           END-IF.

       ESCOLHER-INGREDIENTES.
           PERFORM VARYING CONTADOR FROM 1 BY 1 UNTIL
               CONTADOR > MAXINGREDIENTES
               DISPLAY "Escolha o ingrediente (1 a 10): "        AT 1201
               ACCEPT TEMP                                       AT 1232
               MOVE TEMP TO ESCOLHAING
                   IF ESCOLHAING < 1 OR ESCOLHAING > 10 THEN
                       DISPLAY "Ingrediente invalido!"
                       FOREGROUND-COLOR 4 HIGHLIGHT              AT 1301
                       SUBTRACT 1 FROM CONTADOR
                   ELSE
                       IF REPETIDO(ESCOLHAING) = 1 THEN
                    DISPLAY "INGREDIENTE REPETIDO!"
                    FOREGROUND-COLOR 4 HIGHLIGHT                 AT 1301
                    SUBTRACT 1 FROM CONTADOR
                    ELSE
                    DISPLAY
                    "                               "            AT 1301
                    DISPLAY
                    "|Codigo-------------Ingrediente------------Valor|"
                   FOREGROUND-COLOR 2 HIGHLIGHT                  AT 1401
                   DISPLAY CODIGO(ESCOLHAING) LINE LINHA POSITION 10
                   DISPLAY NOME(ESCOLHAING) LINE LINHA POSITION 25
                   MOVE VALOR(ESCOLHAING) TO SAIDA
                   DISPLAY SAIDA LINE LINHA POSITION 40
                   ADD VALOR(ESCOLHAING) TO TOTALINGREDIENTES
                   ADD 1 TO REPETIDO(ESCOLHAING)
                   ADD 1 TO LINHA
                   END-IF
                   END-sIF
           END-PERFORM.

       CALCULO-TOTAL.
           COMPUTE TOTAL-VALORPIZZA = TOTALINGREDIENTES + VALORPIZZA.
           COMPUTE TOTALIVA = TOTAL-VALORPIZZA * 0.23
           COMPUTE TOTAL-VALORPIZZA = TOTAL-VALORPIZZA + TOTALIVA
           MOVE TOTALINGREDIENTES TO SAIDA.
           DISPLAY FUNCTION CONCATENATE
           ("Valor Ingredientes: ", SAIDA, " Euros")
           BACKGROUND-COLOR 6 FOREGROUND-COLOR 0                AT 2201.
           MOVE VALORPIZZA TO SAIDA.
           DISPLAY FUNCTION CONCATENATE ("    Valor da Pizza: ", SAIDA,
           " Euros") BACKGROUND-COLOR 6  FOREGROUND-COLOR 0     AT 2301.
           MOVE TOTALIVA TO SAIDAIVA.
           DISPLAY FUNCTION CONCATENATE
           ("           IVA 23%: ", SAIDAIVA, "Euros.")
           BACKGROUND-COLOR 6  FOREGROUND-COLOR 0               AT 2401.
           MOVE TOTAL-VALORPIZZA TO SAIDA.
           DISPLAY FUNCTION CONCATENATE ("     Total a pagar: ", SAIDA,
           " Euros") BACKGROUND-COLOR 2 FOREGROUND-COLOR 0      AT 2501.

       LER-REPETIR.
           DISPLAY "DIGITE S PARA CONTINUAR OU N PARA TERMINAR: "
           AT 2701 FOREGROUND-COLOR 2
           ACCEPT REPETIR                                        AT 2745
           IF NOT VALIDAR-REPETIR THEN
               DISPLAY "OPCAO INVALIDA!"
               FOREGROUND-COLOR 4 HIGHLIGHT
           ELSE
           IF (REPETIR = "S" OR REPETIR = "s") THEN
           ADD 1 TO CONTADORPEDIDOS
           MOVE 0 TO TAMANHO
           MOVE 0 TO VALORPIZZA
           MOVE 16 TO LINHA
           MOVE 0 TO TOTAL
           MOVE 0 TO TOTAL-VALORPIZZA
           MOVE 0 TO SAIDA
           MOVE 0 TO SAIDAIVA
           GO TO REGISTO
           END-IF.

            STOP RUN.
       END PROGRAM 0807_ProjetoFinal.
