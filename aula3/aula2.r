###Sorteio no software R: Suponha uma população com 200 elementos da qual se deseje sortear
###15 elementos, de forma inteiramente ao acaso, e sem reposição. Então

###Amostragem Aleatória Simples###

N<-200
n<-15
sample(1:N, n, replace=FALSE)

###No R, suponhamos o mesmo exemplo onde se desejam 15 elementos em uma população de
###tamanho 200. Então

###Amostragem Aleatória Sistemática###

N<-200
n<-15
k<-round(N/n)
amostra<-numeric(n)
amostra[1]<-sample(k,1)
for(i in 2:n) amostra[i]<-amostra[i-1]+k
amostra

###Média e Mediana###

ex<-c(3,5,6,8,9)
mean(ex) #média
median(ex) #mediana

###Variância, Desvio padrão e coeficiente de variação###

c1<-c(100,100,100,100)
c2<-c(80,100,100,120)
c3<-c(10,100,100,190)

mean(c1)
mean(c1)
mean(c1)

mean(c2)
mean(c2)
mean(c2)

mean(c3)
mean(c3)
mean(c3)

median(c1)
median(c1)
median(c1)

median(c2)
median(c2)
median(c2)

median(c3)
median(c3)
median(c3)

Ac1<-range(c2)[2] - range(c2)[1] #Amplitude
var(c2) #Variância
sd(c2) #Desvio padrão
CVc3<-sd(c3)/mean(c3)*100 #Coeficiente de variação
CVc3

####Boxplot####

require(readxl) ##ler arquivos do excel

dados=read_excel("peso.xls",sheet="dados1")
dados
attach(dados)

#Boxplot para variável peso.

boxplot(P,main="Peso")

#Boxplot para variável altura por sexo

boxplot(A~SEXO,main="Boxplot",xlab="Sexo",ylab="Altura")

boxplot(A~EC,main="Boxplot")

par(mfrow=c(1,2)) ##uma linha e duas colunas

boxplot(P,main="Peso")
boxplot(A~SEXO,main="Boxplot",xlab="Sexo",ylab="Altura")

###Rotina R da simulação do lançamento de uma moeda honesta 500 vezes.

cara<-0
fr<-vector("numeric",500)
for (i in 1:500) {
moeda<-runif(1,0,1)
if (moeda>0.5) {cara<-cara+1}
fr[i]<-cara/i }
x<-seq(1:500)
plot(x,fr,"l",xlab="Número de lançamentos",ylab="Frequência Relativa",
ylim=c(0.3,0.6))
abline(h=.5,lty=3)

###Exemplo Binomial##

dbinom(7,9,0.5)

###choose(n,p)###
choose(9,7)*0.5^(7)*0.5^2

####distribuição de probabilidades dessa situação pode ser calculada e
####plotada em umgráfico de agulhas

x<-0:9
p<-dbinom(x,9,0.5)
plot(x,p,"h",ylab="P[X=x]")
axis(1,0:9,0:9)

###Exemplo Poison###

x<-0:6
p<-dpois(x,1.5)
plot(x,p,"h",ylab="P[X=x]")

###Exemplo Normal###

pnorm(100,60,20,lower.tail=FALSE)
pnorm(100,60,20)

pnorm(70,60,20) - pnorm(40,60,20)

