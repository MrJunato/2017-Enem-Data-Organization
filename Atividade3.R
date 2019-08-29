#Inserindo os pacotes
require(dplyr)
require(data.table)
require(plyr)
library(readr)

#Inserindo os dados brutos
enem <- read_csv("2017.csv")
enem <- as.data.frame(enem)

#Criando uma nova tabela usando apenas as colunas que interessam
tabela_renda <- enem[,c(113,118:120,122,123:129,132,136)]

#Criando uma nova coluna que será preenchida mais para frente
renda <- ("")
tabela_renda$Classe_de_Renda <- renda

#Organizando as colunas na ordem desejada
tabela_renda <- tabela_renda[, c(15,2,1,3:14)]

#Renomeando as colunas
names(tabela_renda) <- c("Classe Social",
                       "Renda(R$)",
                       "Escolaridade Pessoa de Referência",
                       "Empregado Doméstico",
                       "Banheiro",
                       "Carro",
                       "Motocicleta",
                       "Geladeira",
                       "Freezer",
                       "Máquina de Lavar Roupa",
                       "Máquina de Secar Roupa",
                       "Forno Micro-ndas",
                       "Máquina de lavar louça",
                       "DVD",
                       "Computador")

#Criando uma nova tabela que ao invés de ser descritiva será feita de pontos, para serem usados depois
tabela_renda_pontos <- tabela_renda
tabela_renda_pontos <- as.data.frame(tabela_renda_pontos)

#Renomeando os as células de cada coluna da tabela
tabela_renda_pontos$`Escolaridade Pessoa de Referência` <- revalue(tabela_renda_pontos$`Escolaridade Pessoa de Referência`, c("A"=0,
                                                                                                                              "B"=0,
                                                                                                                              "H"=0,
                                                                                                                              "C"=1,
                                                                                                                              "D"=2,
                                                                                                                              "E"=4,
                                                                                                                              "F"=7,
                                                                                                                              "G"=7))

tabela_renda_pontos$`Empregado Doméstico` <- revalue(tabela_renda_pontos$`Empregado Doméstico`, c("A"=0,
                                                                                                  "B"=3,
                                                                                                  "C"=7,
                                                                                                  "D"=10))

tabela_renda_pontos$Banheiro <- revalue(tabela_renda_pontos$Banheiro, c("A"=0,
                                                                        "B"=3,
                                                                        "C"=7,
                                                                        "D"=10,
                                                                        "E"=14))

tabela_renda_pontos$Carro <- revalue(tabela_renda_pontos$Carro, c("A"=0,
                                                                  "B"=3,
                                                                  "C"=5,
                                                                  "D"=8,
                                                                  "E"=11))

tabela_renda_pontos$Motocicleta <- revalue(tabela_renda_pontos$Motocicleta, c("A"=0,
                                                                              "B"=1,
                                                                              "C"=3,
                                                                              "D"=3,
                                                                              "E"=3))

tabela_renda_pontos$Geladeira <- revalue(tabela_renda_pontos$Geladeira, c("A"=0,
                                                                          "B"=2,
                                                                          "C"=3,
                                                                          "D"=5,
                                                                          "E"=5))

tabela_renda_pontos$Freezer <- revalue(tabela_renda_pontos$Freezer, c("A"=0,
                                                                      "B"=2,
                                                                      "C"=4,
                                                                      "D"=6,
                                                                      "E"=6))

tabela_renda_pontos$`Máquina de Lavar Roupa` <- revalue(tabela_renda_pontos$`Máquina de Lavar Roupa`, c("A"=0,
                                                                                                        "B"=2,
                                                                                                        "C"=4,
                                                                                                        "D"=6,
                                                                                                        "E"=6))

tabela_renda_pontos$`Máquina de Secar Roupa` <- revalue(tabela_renda_pontos$`Máquina de Secar Roupa`, c("A"=0,
                                                                                                        "B"=2,
                                                                                                        "C"=2,
                                                                                                        "D"=2,
                                                                                                        "E"=2))

tabela_renda_pontos$`Forno Micro-ndas` <- revalue(tabela_renda_pontos$`Forno Micro-ndas`, c("A"=0,
                                                                                            "B"=2,
                                                                                            "C"=4,
                                                                                            "D"=4,
                                                                                            "E"=4))

tabela_renda_pontos$`Máquina de lavar louça` <- revalue(tabela_renda_pontos$`Máquina de lavar louça`, c("A"=0,
                                                                                                        "B"=3,
                                                                                                        "C"=6,
                                                                                                        "D"=6,
                                                                                                        "E"=6))

tabela_renda_pontos$DVD <- revalue(tabela_renda_pontos$DVD, c("A"=1,
                                                              "B"=0))

tabela_renda_pontos$Computador <- revalue(tabela_renda_pontos$Computador, c("A"=0,
                                                                            "B"=3,
                                                                            "C"=6,
                                                                            "D"=8,
                                                                            "E"=11))
#Transformando as colunas que serão somadas em valores numericos
tabela_renda_pontos[, c(3:15)] <- sapply(tabela_renda_pontos[, c(3:15)], as.numeric)

#Criando uma variável para armazenar a quantidade de linhas da tabela
qtd_linhas <- nrow(tabela_renda_pontos)

#Laço que soma todos os pontos da linha que estiver e adiciona na coluna Classe Social na linha que estiver sendo trabalhada
for (contador in 1:qtd_linhas) {
  soma_linha <- rowSums(tabela_renda_pontos[contador, c(3:15)])
  tabela_renda_pontos[contador,1] = soma_linha
}

#Transformando a nova coluna classe social em númerica
tabela_renda_pontos$`Classe Social` <- as.numeric(tabela_renda_pontos$`Classe Social`)

#Classificando as classes sociais
tabela_renda_pontos$`Classe Social` <- ifelse(tabela_renda_pontos$`Classe Social` <= 16,"D-E",
                                              ifelse(tabela_renda_pontos$`Classe Social`<= 22,"C2",
                                              ifelse(tabela_renda_pontos$`Classe Social`<= 28,"C1",
                                              ifelse(tabela_renda_pontos$`Classe Social`<= 37,"B2",
                                              ifelse(tabela_renda_pontos$`Classe Social`<= 44,"B1","A")))))

###############################################################################################################################################################################################
################################################ Renomeando os as células de cada coluna da tabela descritiva ########################################################################
###############################################################################################################################################################################################

tabela_renda$`Renda(R$)` <- revalue(tabela_renda$`Renda(R$)`, c("A"="Nenhuma renda",
                                                    "B"="Até 937,00",
                                                    "C"="937,01 até  1.405,50",
                                                    "D"="1.405,51 até 1.874,00",
                                                    "E"="1.874,01 até 2.342,50",
                                                    "F"="2.342,51 até 2.811,00",
                                                    "G"="2.811,01 até 3.748,00",
                                                    "H"="3.748,01 até 4.685,00",
                                                    "I"="4.685,01 até 5.622,00",
                                                    "J"="5.622,01 até 6.559,00",
                                                    "K"="6.559,01 até 7.496,00",
                                                    "L"="7.496,01 até 8.433,00",
                                                    "M"="8.433,01 até 9.370,00",
                                                    "N"="9.370,01 até 11.244,00",
                                                    "O"="11.244,01 até 14.055,00",
                                                    "P"="14.055,01 até 18.740,00",
                                                    "Q"="Mais de 18.740,00"))

tabela_renda$`Escolaridade Pessoa de Referência` <- revalue(tabela_renda$`Escolaridade Pessoa de Referência`, c("A"="Nunca estudou",
                                                                                                                "B"="Nunca estudou",
                                                                                                                "H"="Nunca estudou",
                                                                                                                "C"="Fundamental I completo / Fundamental II incompleto",
                                                                                                                "D"="Fundamental II completo / Médio incompleto",
                                                                                                                "E"="Médio completo / Superior incompleto",
                                                                                                                "F"="Superior completo",
                                                                                                                "G"="Superior completo"))

tabela_renda$`Empregado Doméstico` <- revalue(tabela_renda$`Empregado Doméstico`, c("A"="Não",
                                                                                    "B"="Sim",
                                                                                    "C"="Sim",
                                                                                    "D"="Sim"))

tabela_renda$Banheiro <- revalue(tabela_renda$Banheiro, c("A"="0",
                                                          "B"="1",
                                                          "C"="2",
                                                          "D"="3",
                                                          "E"="4+"))

tabela_renda$Carro <- revalue(tabela_renda$Carro, c("A"="0",
                                                    "B"="1",
                                                    "C"="2",
                                                    "D"="3",
                                                    "E"="4+"))

tabela_renda$Motocicleta <- revalue(tabela_renda$Motocicleta, c("A"="0",
                                                                "B"="1",
                                                                "C"="2",
                                                                "D"="3",
                                                                "E"="4+"))

tabela_renda$Geladeira <- revalue(tabela_renda$Geladeira, c("A"="0",
                                                            "B"="1",
                                                            "C"="2",
                                                            "D"="3",
                                                            "E"="4+"))

tabela_renda$Freezer <- revalue(tabela_renda$Freezer, c("A"="0",
                                                       "B"="1",
                                                       "C"="2",
                                                       "D"="3",
                                                       "E"="4+"))

tabela_renda$`Máquina de Lavar Roupa` <- revalue(tabela_renda$`Máquina de Lavar Roupa`, c("A"="0",
                                                                                          "B"="1",
                                                                                          "C"="2",
                                                                                          "D"="3",
                                                                                          "E"="4+"))

tabela_renda$`Máquina de Secar Roupa` <- revalue(tabela_renda$`Máquina de Secar Roupa`, c("A"="0",
                                                                                          "B"="1",
                                                                                          "C"="2",
                                                                                          "D"="3",
                                                                                          "E"="4+"))

tabela_renda$`Forno Micro-ndas` <- revalue(tabela_renda$`Forno Micro-ndas`, c("A"="0",
                                                                              "B"="1",
                                                                              "C"="2",
                                                                              "D"="3",
                                                                              "E"="4+"))

tabela_renda$`Máquina de lavar louça` <- revalue(tabela_renda$`Máquina de lavar louça`, c("A"="0",
                                                                                          "B"="1",
                                                                                          "C"="2",
                                                                                          "D"="3",
                                                                                          "E"="4+"))

tabela_renda$DVD <- revalue(tabela_renda$DVD, c("A"="Sim",
                                                "B"="Não"))

tabela_renda$Computador <- revalue(tabela_renda$Computador, c("A"="0",
                                                              "B"="1",
                                                              "C"="2",
                                                              "D"="3",
                                                              "E"="4+"))
#Trocando a coluna de Classe sociais da tabela_renda pela coluna da tabela_renda_pontos
tabela_renda$`Classe Social` <- tabela_renda_pontos$`Classe Social`
View(tabela_renda)