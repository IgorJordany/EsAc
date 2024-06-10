
#4) Para estimar a diferença de tempos médios de vida (em anos) entre fumantes e não fumantes, foram recolhidas obtidos as seguintes observações

#Fumantes
fumantes<-c(52.4,55.0,55.2,55.2,55.5,56.2,57.0,57.4,58.3,59.2,59.3,59.6,59.7,60.0,60.5,60.6,61.2,61.6,61.9,62.1,62.2,62.4,62.7,63.5,64.1,64.7,64.8,64.9,65.0,65.4,66.0,66.9,69.1,69.2,69.8)
#Não Fumantes
naofumantes<-c(63.8,65.7,66.2,66.2,66.2,66.8,67.5,67.7,67.9,68.0,68.1,68.3,68.6,68.7,68.8,69.2,69.3,69.4,69.5,70.1,70.1,70.2,70.2,70.3,70.4,70.7,70.8,70.8,71.0,71.4,71.5,71.6,72.7,72.7,72.9,73.3,73.3,73.9,74.1,75.8,75.9,77.5)

#a) Obtenha o intervalo de confiança a 95% para tempo médio de vida fumantes e não fumantes


XF=mean(fumantes)
SigmaF=sd(fumantes)
nF=length(fumantes)
XF.ic=XF+qnorm(c(0.025,0.975))*(SigmaF/sqrt(nF))
XF.ic

XNF=mean(naofumantes)
Sigma=sd(naofumantes)
nNF=length(naofumantes)
XNF.ic=XNF+qnorm(c(0.025,0.975))*(Sigma/sqrt(nNF))
XNF.ic

#b) Por meio do intervalo de confiança é possível dizer que não fumantes vivem mais que fumantes? 
#Sim


#c) Obtenha o intervalo de confiança a 95% para proporção de fumantes e não fumantes
lenFumantes =length(fumantes)
lenFumantes
lenNaoFumantes = length(naofumantes)
lenNaoFumantes
p = length(fumantes)/(length(fumantes)+length(naofumantes))
p
q=1-p
q
n=length(fumantes)+length(naofumantes)
p.ic=p+qnorm(c(0.025,0.975))*sqrt(q*p/n)
p.ic

#d) Por meio do intervalo de confiança é possível dizer que a proporção de fumantes e não fumantes são iguais?

binom.test(35,77, p = 0.5, conf.level = 0.95)

#SIM


#3 exercicio

# Carregar os dados
X <- c(14, 15, 23, 31, 38, 40, 53, 51)
Y <- c(2.0, 4.6, 5.7, 7.3, 9.8, 10.9, 12.6, 13.2)
data <- data.frame(X, Y)

# Instalar e carregar o pacote ggplot2 se ainda não estiver instalado
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Diagrama de dispersão
ggplot(data, aes(x = X, y = Y)) +
  geom_point() +
  labs(title = "Diagrama de Dispersão", x = "Número de acessos ao disco (X)", y = "Tempo de CPU (Y)")

# Calcular o coeficiente de correlação
cor_coef <- cor(X, Y)
cor_coef

# Testar a significância da correlação
cor_test <- cor.test(X, Y)
cor_test

# Ajuste do modelo de regressão linear
modelo <- lm(Y ~ X, data = data)
summary(modelo)

# Teste de significância do modelo de regressão
summary(modelo)

# Gráfico da regressão
ggplot(data, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Diagrama de Dispersão com Linha de Regressão", x = "Número de acessos ao disco (X)", y = "Tempo de CPU (Y)")

# Coeficiente de determinação (R²)
r_squared <- summary(modelo)$r.squared
r_squared

# Previsão usando o modelo de regressão
nova_entrada <- data.frame(X = c(20))  # Substitua 20 pelo valor desejado
previsao <- predict(modelo, nova_entrada)
previsao

#4 exercicio

# Carregar os dados
idade <- c(71, 64, 43, 67, 56, 73, 68, 56, 76, 65, 45, 58, 45, 53, 49, 78, 73, 68)
massa_muscular <- c(82.0, 91.0, 100.0, 68.0, 87.0, 73.0, 78.0, 80.0, 65.0, 84.0, 116.0, 76.0, 97.0, 100.0, 105.0, 77.0, 73.0, 78.0)
data <- data.frame(Idade = idade, MassaMuscular = massa_muscular)

# Instalar e carregar o pacote ggplot2 se ainda não estiver instalado
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Diagrama de dispersão
ggplot(data, aes(x = Idade, y = MassaMuscular)) +
  geom_point() +
  labs(title = "Diagrama de Dispersão", x = "Idade (anos)", y = "Massa Muscular (kg)")

# Calcular o coeficiente de correlação
cor_coef <- cor(data$Idade, data$MassaMuscular)
cor_coef

# Testar a significância da correlação
cor_test <- cor.test(data$Idade, data$MassaMuscular)
cor_test

# Ajuste do modelo de regressão linear
modelo <- lm(MassaMuscular ~ Idade, data = data)
summary(modelo)

# Gráfico da regressão
ggplot(data, aes(x = Idade, y = MassaMuscular)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Diagrama de Dispersão com Linha de Regressão", x = "Idade (anos)", y = "Massa Muscular (kg)")

# Coeficiente de determinação (R²)
r_squared <- summary(modelo)$r.squared
r_squared

#5 exercicio 

# Carregar os dados
semana <- 1:10
horas_prop <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
vendas <- c(180, 200, 210, 220, 230, 240, 250, 260, 270, 280)
data <- data.frame(Semana = semana, HorasPropaganda = horas_prop, Vendas = vendas)

# Instalar e carregar o pacote ggplot2 se ainda não estiver instalado
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Calcular o coeficiente de correlação
cor_coef <- cor(data$HorasPropaganda, data$Vendas)
print(paste("Coeficiente de Correlação: ", cor_coef))

# Diagrama de dispersão
ggplot(data, aes(x = HorasPropaganda, y = Vendas)) +
  geom_point() +
  labs(title = "Diagrama de Dispersão", x = "Horas de Propaganda na TV", y = "Vendas do Produto")

# Ajuste do modelo de regressão linear
modelo <- lm(Vendas ~ HorasPropaganda, data = data)
summary(modelo)

# Coeficiente de determinação (R²)
r_squared <- summary(modelo)$r.squared
print(paste("Coeficiente de Determinação (R²): ", r_squared))

# Coeficientes da regressão
coeficientes <- coef(modelo)
print("Coeficientes da Regressão: ")
print(coeficientes)

# Gráfico da regressão
ggplot(data, aes(x = HorasPropaganda, y = Vendas)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Diagrama de Dispersão com Linha de Regressão", x = "Horas de Propaganda na TV", y = "Vendas do Produto")

