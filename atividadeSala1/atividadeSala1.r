## 9

at <- c("G", "A", "G", "G", "A", "G", "G", "P", "A", "A", "P", "P", "PT", "A", "PT", "P", "A", "A", "P", "PT")
tab.at <- table(at)
df <- matrix(0, 5, 3)
colnames(df) <- c("fa", "fr", "fp")
rownames(df) <- c("Granito", "Ardósia", "Pedra Sabão", "Pedra Talco", "Total")
df[1, 1] <- tab.at["G"]
df[2, 1] <- tab.at["A"]
df[3, 1] <- tab.at["P"]
df[4, 1] <- tab.at["PT"]
df[5, 1] <- length(at)
for (i in 1:5) {
    df[i, 2] <- df[i, 1] / length(at)
}
for (i in 1:5) {
    df[i, 3] <- df[i, 2] * 100
}
df

gc <- barplot(df[1:4, 1], xlab = "Atividade", ylab = "frequência Absoluta", col = gray(seq(0.4, 1.0, length = 4)))
gc


## 10

at <- c("0", "0", "0", "0", "0", "0", "0", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2", "2", "2", "2", "2", "2", "2", "2", "2", "2", "2", "2", "2", "3", "3", "3", "3", "3", "3", "3", "4", "4", "4", "4", "5", "5", "6")
tab.at <- table(at)
df <- matrix(0, 8, 3)
colnames(df) <- c("fa", "fr", "fp")
rownames(df) <- c("0", "1", "2", "3", "4", "5", "6", "Total")
df[1, 1] <- tab.at["0"]
df[2, 1] <- tab.at["1"]
df[3, 1] <- tab.at["2"]
df[4, 1] <- tab.at["3"]
df[5, 1] <- tab.at["4"]
df[6, 1] <- tab.at["5"]
df[7, 1] <- tab.at["6"]
df[8, 1] <- length(at)
for (i in 1:8) {
    df[i, 2] <- df[i, 1] / length(at)
}
for (i in 1:8) {
    df[i, 3] <- df[i, 2] * 100
}
df

gc <- barplot(df[1:7, 3], xlab = "Atividade", ylab = "frequência Percentual", col = gray(seq(0.4, 1.0, length = 7)))
gc


## 12
# Os pesos em kg, de 80 estudantes de um colégio foram os seguintes:
# a) Qual o tipo de variável? Quantitativa Continua
#

at <- c(85.4, 93.7, 103.1, 109.9, 120.2, 124.7, 137.9, 147.2, 89.3, 94.2, 105.2, 110.4, 121.3, 126.2, 138.2, 151.3, 89.8, 94.9, 107.6, 113.2, 122.2, 128.9, 138.7, 153.9, 90.7, 95.7, 108.1, 114.7, 123.3, 137.1, 139.1, 157.2, 91.4, 99.2, 109.0, 119.3, 123.7, 135.5, 139.4, 165.4)
sort(at)
tab.at <- table(at)
df <- matrix(0, 8, 5)
colnames(df) <- c("fa", "fr", "fp", "FA", "FP")
rownames(df) <- c("78,4 |-- 92,4", "92,4 |-- 106,4", "106,4 |-- 120,4", "120,4 |-- 134,4", "134,4 |-- 148,4", "148,4 |-- 162,4", "162,4 |-- 176,4", "Total")
tab.at <- table(cut(at, breaks = c(78.4, 92.4, 106.4, 120.4, 134.4, 148.4, 162.4, 176.4)))
df[1:7, 1] <- tab.at
df[8, 1] <- length(at)
for (i in 1:8) {
    df[i, 2] <- df[i, 1] / length(at)
}
for (i in 1:8) {
    df[i, 3] <- df[i, 2] * 100
}
for (i in 1:8) {
    if (i == 1) {
        df[i, 4] <- df[i, 1]
    }
    if(i>1){
        v <- i-1
        df[i,4] <- df[i, 1] + df[v, 4]
    }
    if(i>7){
        df[i,4] <- 0
    }
}
for (i in 1:8) {
    if (i == 1) {
        df[i, 5] <- df[i, 2]
    }
    if(i>1){
        v <- i-1
        df[i,5] <- df[i, 2] + df[v, 5]
    }
    if(i>7){
        df[i,5] <- 0
    }
}
df

#### Histograma da pressão arterial####

h <- hist(at,
    breaks = c(78.4, 92.4, 106.4, 120.4, 134.4, 148.4, 162.4, 176.4),
    freq = TRUE, ylab = "Dfa", xlab = "pressão arterial", main = "",
    col = topo.colors(15)
)

#### histograma com polígono de frequência####

h <- hist(at,
    breaks = c(78.4, 92.4, 106.4, 120.4, 134.4, 148.4, 162.4, 176.4),
    freq = FALSE, ylab = "Dfr", xlab = "pressão arterial", main = "",
    col = topo.colors(15)
)
points(h$mids, h$density, "l", lwd = 2)




#21 

at <- c("AL", "VL", "AL", "AL", "VL", "AL", "AL", "AR", "VL", "VL", "AR", "AR", "VR", "VL", "VR", "AR", "VL", "VL", "VR", "AR")
tab.at <- table(at)
df <- matrix(0, 5, 3)
colnames(df) <- c("fa", "fr", "fp")
rownames(df) <- c("AL", "VL", "AR", "VR", "Total")
df[1, 1] <- tab.at["AL"]
df[2, 1] <- tab.at["VL"]
df[3, 1] <- tab.at["AR"]
df[4, 1] <- tab.at["VR"]
df[5, 1] <- length(at)
for (i in 1:5) {
    df[i, 2] <- df[i, 1] / length(at)
}
for (i in 1:5) {
    df[i, 3] <- df[i, 2] * 100
}
df

gc <- barplot(df[1:4, 1], xlab = "Especies", ylab = "frequência Absoluta", col = gray(seq(0.4, 1.0, length = 4)))
gc