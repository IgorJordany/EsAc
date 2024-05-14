# 1) Uma empresa petroquímica diz que se em 80% de uma determinada região for encontrado petróleo então existe viabilidade econômica da exploração de petróleo. Para verificar a viabilidade econômica da exploração foram tomadas 30 amostras de uma determinada região e em 21 foram encontradas petróleo. Utilizando um teste bilateral com nível de significância de 5%, verifique se existe viabilidade econômica nessa região.

binom.test(21, 30, p = 0.5, conf.level = 0.95)

# 5) Os pesos dos bois de uma fazenda na idade de abate apresentam os seguintes dados

# 545,92 532,29 472,92 533,68 479,48 476,38 497,51 504,02 543,66 522,27 502,90 532,45 538,84 548,86 506,67 508,43 520,23 466,14 521,67 482,56 429,72 476,62 509.82 501,61 477,28 530,99 485,24 515,24 544,60 460,53

# b) Calcule a média, mediana e moda.

moda <- function(x) {
    modal <- unique(x)
    modal[which.max(tabulate(match(x, modal)))]
}

peso <- c(545.92, 532.29, 472.92, 533.68, 479.48, 476.38, 497.51, 504.02, 543.66, 522.27, 502.90, 532.45, 538.84, 548.86, 506.67, 508.43, 520.23, 466.14, 521.67, 482.56, 429.72, 476.62, 509.82, 501.61, 477.28, 530.99, 485.24, 515.24, 544.60, 460.53)
sort(peso)
peso
mean(peso)
xp=median(peso)
moda(peso)
summary(peso)

# c) Obtenha o gráfico de boxplot;
boxplot(peso)

# c) Encontre o intervalo de confiança a 95% para a média.
shapiro.test(peso)
Sigma=sd(peso)
n=length(peso)
n
XB.ic=XB+qnorm(c(0.025,0.975))*(Sigma/sqrt(n))
XB.ic

t.test(peso,mu=500.00)


#2. (**) Foi feito um estudo sobre o peso final (Y) de peixes tratados com doses extras de ração (X). Os resultados obtidos foram os seguintes:

#Ração (g/peixe) 0   5   10  15  20
#Peso (g)        495 560 590 620 615

#a) Atribua as notações X e Y para as variáveis independente e dependente desse problema.
#b) Faça um diagrama de dispersão para os dados.
#c) Estime o coeficiente de correlação linear de Pearson (p) e interprete-o.
#d) Estime a equação de regressão que melhor se ajusta aos dados.
#e) Plote a equação estimada no gráfico de dispersão.
#f) Qual é o peso esperado para um peixe que se alimente com 8g extras de ração?
#g) Qual é o peso esperado para um peixe que se alimente com 60g extras de ração?


dados=read.table("D:/LearnR/EsAc/aula3/datas/peixes.txt",h=TRUE)
dados
attach(dados)

cor.test(racao,pesog)

modelo1=lm(pesog~racao)
modelo1
summary(modelo1)

x=seq(0:20)
reta=516 + 6*x
plot(racao,pesog,xlab="Ração",ylab="Peso(g)")
lines(x,reta)

#f) Qual é o peso esperado para um peixe que se alimente com 8g extras de ração?
f=516 + 6*28
f

#g) Qual é o peso esperado para um peixe que se alimente com 60g extras de ração?
g=516 + 6*80
g