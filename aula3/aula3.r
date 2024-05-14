####Intervalo de Confiança e Teste de Hipótese####

dados=read.table("D:/LearnR/EsAc/aula3/datas/IC_TH.txt",h=TRUE)
dados
attach(dados)

##Intervalo de 95% de confiança para proporção de homens

fa=table(SEXO)
fr=fa/sum(fa)
fr
SEXO
p=fr[2] ##proporção de homens
p
q=1-p
q
n=sum(fa)
p.ic=p+qnorm(c(0.025,0.975))*sqrt(q*p/n)
p.ic

##Vamos testar se a proporção de homens pode ser considerada igual a 50% e com hipótese alternativa
##se ela é diferente de 50%, considerando uma significância de 0,05 "H1:p#0.5"

binom.test(12,29, p = 0.5, conf.level = 0.95)

##Se mudar a hipótese alternativa testar para proporção de homens menor que 50%, considerando uma
##significância de 0,05 "H1:p<0.5"

binom.test(12,29, p = 0.5, conf.level = 0.95, alternative ="less")

###Normalidade

shapiro.test(IDADE)
shapiro.test(P)
shapiro.test(P1)
shapiro.test(A)

##Intervalo de 95% confiança para altura

XB=mean(A)
XB
Sigma=0.5
n=length(A)
n
XB.ic=XB+qnorm(c(0.025,0.975))*(Sigma/sqrt(n))
XB.ic

##Testar se média de altura é igual a 1,72 e com hipótese alternativa se ela é diferente de 1,72, considerando
##uma significância de 0,05. "H1 : mu # 1,72"

t.test(A,mu=1.72)

##Testar se média de idade é igual a 36 anos e com hipótese alternativa se ela é diferente de 36, considerando
##uma significância de 0,05. "H1 : mu # 36"

wilcox.test(IDADE,mu=36)

##Os dados pareado quando é medido a mesma variável sobre o mesmo individuo após um tempo ou
##um tratamento. No caso da variável peso, temos um exemplo de dados pareados, nesse caso poderíamos
##estar interessados em ver teve diferença no peso antes e depois do estudo. Nesse caso teríamos um teste
##de hipótese da seguinte forma. "H1 : m1 # m2 ou m1-m2= 0"

t.test(P,P1,m=0,paired=TRUE)

mean(P)
mean(P1)

##Vamos verificar se a peso inicial de homens e mulheres são iguais. Antes de aplicar o teste t, é
##necessário verificar se as variâncias entre os grupos são iguais.

P
summary(P)

x=F=c(51.5, 55.7, 63.8, 66.8, 69.6, 71.8, 74.5, 75.0, 75.8, 76.3, 80.3, 82.0, 83.0, 83.8, 86.7, 87.2, 90.6)
y=M=c(60.2, 63.7, 66.2, 66.5, 68.4, 73.2, 73.9, 88.6, 89.2, 93.8, 100.3, 102.9)

var.test(x,y)

##então não rejeita-se H0, assim as variâncias são iguais.

t.test(x,y,m=0,var.equal=TRUE)

##então não rejeita-se H0. Assim o peso inicial
##médio de homens e mulheres são iguais, ao nível de 5% de significância.

##Obter os valores da idade por sexo

Grupos=tapply(IDADE,SEXO,sort)
Grupos

##valores de idade para mulheres
x=Grupos$F
x

##valores de idade para homens
y=Grupos$M
y

wilcox.test(x,y,m=0)

##então não rejeita-se H0. Assim a idade médio
##de homens e mulheres são iguais, ao nível de 5% de significância.

####Regressão Linear Simples

polpa<-c(9.02, 13.10, 14.76, 21.54, 15.62, 18.34, 20.23, 8.88,
14.06, 23.59, 16.62, 21.93, 10.56, 12.28, 20.68, 9.53, 13.73,
5.73, 15.08, 21.57)
agua<-c(17.87, 13.75, 12.72, 6.98, 11.01, 10.48, 10.19, 19.11,
12.72, 0.45, 10.67, 1.59, 14.91, 14.14, 9.40, 16.23, 12.74,
20.64, 12.34, 6.44)

cor.test(agua,polpa)

modelo=lm(polpa~agua)
modelo
summary(modelo)

###Polpa = 26,36 - 0,94*Agua###

x=seq(1:30)
reta=26.36-0.94*x
plot(agua,polpa,xlab="Água",ylab="Polpa")
lines(x,reta)

##análise de variância para o modelo ajustado
anova(modelo)

##############################################

dados=read.table("D:/LearnR/EsAc/aula3/datas/exercicio.txt",h=TRUE)
dados
attach(dados)

cor.test(aptidao,produtividade)

modelo1=lm(produtividade~aptidao)
modelo1
summary(modelo1)

###Produtividade= -12.619 + 2.614*Aptidão###

x=seq(1:30)
reta=-12.619 + 2.614*x
plot(aptidao,produtividade,xlab="Aptidão",ylab="Produtividade")
lines(x,reta)


