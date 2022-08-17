#### Trabalho final Discplina R ####
###Analise de inducao de multibroto de capim elefante apos 28 dias###

#diretorio
setwd("C:/Displina_R_2022.1")
dir()

#importando dados
data=read.csv(file="trabfinal.csv",header = TRUE, sep = ";")

#inspecao do banco de dados
str(data)
data$treatment=as.factor(data$treatment)
summary(data)

#Inspecao grafica dos dados
boxplot(multisprout~treatment)

#transformando dados em objetos
attach(data)

##variaveis estatisticas
#tratamento=variavel qualitativa,categorica -> preditora,independente
#multibro=variavel quantitativa -> resposta,dependente

#o teste realizado -> ANOVA
#necessario realizar o teste antes do teste de normalidade dos residuos

result=aov(multisprout~treatment) 
ls(result) 

###TESTE DE PRESSUPOSTOS

library(car)

##Homogeneidade das amostras
leveneTest(multisprout~treatment)
#df=4 , F-value=  2.142  p-value=0.0969
#aceita h0,homogeneo pois p>0.05

##Normlidade dos residuos
#Análise gráfica

qqnorm(result$residuals,col = "black",pch=16)
qqline(result$residuals,col = "red",lty=2)

hist(result$residuals,col = "blue")

#Analise estatistica

shapiro.test(result$residuals)
#W=0.97649 e p-value=0.5773
#aceita H0,normal pois p>0.05


###Analisando o teste ANOVA
summary(result)
#F value=11.9 p-value=3.74e-06 ***
#rejeita h0,existe diferenca significativa entre os tratamentos (geral)

second_result=lm(multisprout~treatment) #para o valor de p de cada tratamento em relacao ao controle 

summary(second_result)
#Adjusted R-squared:  0.5344 ,SIGNIFICA QUE 53% É JUSTIFICADO PELO TRATAMENTO.
#todos os tratamentos tem diferenca signficativa com o controle(esperado)


##Teste a posteriori _> Teste de Tukey 
#p deve ser menor que 0.05

TukeyHSD(result)

#A diferenca sifnificativa foi encontrada entre os tratamentos e o controle,sendo o menos p entre o TRAT2 e o TRAT0. Comparando os demais concluimos que os TRAT3 E 4 nao possuem diferenca significativa em relacao ao 1 e o 2 com excecao ao TRAT4 E O 2(0.0341707) e que entre o TRAT1 E 2 ha uma diferenca significativa.

citation() #referenciar o R em artigo

#####FIM DE ANALISE ###

