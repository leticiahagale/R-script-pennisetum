#### Trabalho final ####

#diretorio
setwd("C:/Displina_R_2022.1")
dir()

#importando dados
dados=read.csv(file= "trabfinal.csv" ,header = TRUE, sep = ";")

#inspecao dados
str(dados)
dados$tratamento=as.factor(dados$tratamento)
summary(dados)
med=tapply(dados$multibroto,dados$tratamento,mean)
med

#transformando dados em objetos
attach(dados)

#hipoteses
#h0=nao ha diferenca significativa entre a inducao de multibrotos com a variacao de tratamentos
#h1=ha diferenca significativa entre a inducao de multibrotos de acordo com o tratamento

#variaveis
#tratamento=variavel qualitativa,categorica -> preditora,independente
#multibro=variavel quantitativa -> resposta,dependente

#o teste realizado -> ANOVA

#TESTE DE PRESSUPOSTOS
#h0=os dados sao normais

library(car)

##Homogeneidade 
leveneTest(multibroto~tratamento)
#df=4 , F-value=  2.5593 p-value=0.05623
#aceita h0,normal pois p>0.05


#anova (necessaria para o proximo pressuposto)
resultado=aov(multibroto~tratamento) #roda o teste primeiro e olha o resultado após a análise dos resisuos gerados aqui.
ls(resultado) #de onde pega os residuos

#Análise gráfica

qqnorm(resultado$residuals,col = "black",pch=16)
qqline(resultado$residuals,col = "red",lty=2)

hist(resultado$residuals,col = "blue")

#Analise estatistica

#normlidade dos residuos
shapiro.test(resultado$residuals)#H0=A distribuicao é normal,desejamos o p>0,05
#O valor de p=0.0591,ou seja, maior que 0,05. Aceita H0.
##Portanto aceita a normalidade 

###Analisando o teste ANOVA
summary(resultado)
#rejeita h0,existe diferenca significativa entre os tratamentos (geral)

resultado2=lm(multibroto~tratamento) #para o valor de p em relacao ao tratamento controle comparado com cada um

summary(resultado2)
#Adjusted R-squared:  0.3863 ,SIGNIFICA QUE 38% É JUSTIFICADO PELO TRATAMENTO.
#todos os tratamentos tem diferenca signficativa com o controle(esperado)

boxplot(multibroto~tratamento)
points(1:3,med, 1, col = "blue", pch = 19, type = "p")

###Teste de post hoc, Teste de Tukey mostra os resultados comparando cada um dos tratamentos entre si

TukeyHSD(resultado)
##Tukey multiple comparisons of means
#95% family-wise confidence level
#p deve ser menor que 0.05

#TRAT07-TRAT01 p=0.0007954
#TRAT06-TRAT01 p=0.0191949
#A diferenca sifnificativa foi encontrada entre o tratamento 7 e o controle e o tratamento 6 e o controle.