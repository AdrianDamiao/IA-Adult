# Exibir o dataset 
library(readr)
adultos <- read.csv(file = "adultos.csv", stringsAsFactors = TRUE)
View(adultos)

# Exibir o atributo de saída
adultos <- read.csv(file = "adultos.csv", stringsAsFactors = FALSE)
atributo.Saida <- subset(adultos, select = Above.Limit)
View(atributo.Saida)

# Exibir medidad de localidade
library(ggplot2)

adultos_com_indice <- cbind(Indice = 1:nrow(adultos), adultos)
View(adultos_com_indice)

boxplot(adultos$Age)
ggplot(adultos_com_indice) + geom_point(aes(x = Age, y = Indice))
mean(adultos$Age)

summary(adultos$Workclass)

boxplot(adultos$Fnlwgt)
ggplot(adultos_com_indice) + geom_point(aes(x = Fnlwgt, y = Indice))
median(adultos$Fnlwgt)

summary(adultos$Education)

boxplot(adultos$Education.num)
ggplot(adultos_com_indice) + geom_point(aes(x = Education.num, y = Indice))
mean(adultos$Education.num)

summary(adultos$Marital.status)

summary(adultos$Occupation)

summary(adultos$Relationship)

summary(adultos$Race)

summary(adultos$Sex)

boxplot(adultos$Capital.gain)
ggplot(adultos_com_indice) + geom_point(aes(x = Capital.gain, y = Indice))
median(adultos$Capital.gain)

boxplot(adultos$Capital.loss)
ggplot(adultos_com_indice) + geom_point(aes(x = Capital.loss, y = Indice))
median(adultos$Capital.loss)

boxplot(adultos$Hours.per.week)
ggplot(adultos_com_indice) + geom_point(aes(x = Hours.per.week, y = Indice))
median(adultos$Hours.per.week)

summary(adultos$Native.country)

summary(adultos$Above.Limit)

# Exibir medidas de espalhamento

var(adultos$Age)
var(adultos$Education.num)
sd(adultos$Fnlwgt)
sd(adultos$Capital.gain)
sd(adultos$Capital.loss)
sd(adultos$Hours.per.week)

# Exibir medidas de distribuição

hist(adultos$Age, xlab = "Age", ylab = "Frequência", main = "Histograma de Age")
hist(adultos$Fnlwgt, xlab = "Fnlwgt", ylab = "Frequência", main = "Histograma de Fnlwgt")
hist(adultos$Education.num, xlab = "Educationnum", ylab = "Frequência", main = "Histograma de Education.num")
hist(adultos$Capital.gain, xlab = "Capitalgain", ylab = "Frequência", main = "Histograma de Capitalgain")
hist(adultos$Capital.loss, xlab = "Capitalloss", ylab = "Frequência", main = "Histograma de Capitalloss")
hist(adultos$Hours.per.week, xlab = "Hoursperweek", ylab = "Frequência", main = "Histograma de Hours per Week")

basecompleta <- read.csv(file="basecompleta.csv", stringsAsFactors = TRUE)
conjuntoDeTeste <- read.csv(file="adultosTeste.csv", stringsAsFactors = TRUE)

summary(adultos$Above.Limit)
summary(conjuntoDeTeste$Above.Limit)


# Realizar amostragem

proporcao_classe1 <- 0.76
proporcao_classe2 <- 0.24

library(dplyr)

amostra <- adultos %>%
  group_by(32561) %>%
  sample_frac(size = min(proporcao_classe1, proporcao_classe2))

summary(amostra$Above.Limit)

# Balanceamento da base

proporcao_classe1 <- 0.76
proporcao_eliminar <- 0.06

adultosBalanceado <- adultos %>%
  filter(adultos$Above.Limit == " <=50K") %>%
  sample_frac(size = proporcao_classe1 - proporcao_eliminar) %>%
  bind_rows(adultos %>% filter(adultos$Above.Limit == " >50K"))

summary(adultosBalanceado$Above.Limit)

## Eliminação de outliers

mean_value <- mean(adultos_com_indice$Age)
sd_value <- sd(adultos_com_indice$Age)
threshold <- 1.8

adultosComMenosOutliers <- adultos_com_indice[adultos_com_indice$Age <= mean_value + threshold * sd_value & adultos_com_indice$Age >= mean_value - threshold * sd_value, ]

boxplot(adultosComMenosOutliers$Age)

mean_value <- mean(adultos_com_indice$Fnlwgt)
sd_value <- sd(adultos_com_indice$Fnlwgt)
threshold <- 1.8

adultosComMenosOutliers <- adultos_com_indice[adultos_com_indice$Fnlwgt <= mean_value + threshold * sd_value & adultos_com_indice$Fnlwgt >= mean_value - threshold * sd_value, ]

boxplot(adultosComMenosOutliers$Fnlwgt)

mean_value <- mean(adultos_com_indice$Capital.gain)
sd_value <- sd(adultos_com_indice$Capital.gain)
threshold <- 1.8

adultosComMenosOutliers <- adultos_com_indice[adultos_com_indice$Capital.gain <= mean_value + threshold * sd_value & adultos_com_indice$Capital.gain >= mean_value - threshold * sd_value, ]

boxplot(adultosComMenosOutliers$Capital.gain)

mean_value <- mean(adultos_com_indice$Capital.loss)
sd_value <- sd(adultos_com_indice$Capital.loss)
threshold <- 1.8

adultosComMenosOutliers <- adultos_com_indice[adultos_com_indice$Capital.loss <= mean_value + threshold * sd_value & adultos_com_indice$Capital.loss >= mean_value - threshold * sd_value, ]

boxplot(adultosComMenosOutliers$Capital.loss)

mean_value <- mean(adultos_com_indice$Hours.per.week)
sd_value <- sd(adultos_com_indice$Hours.per.week)
threshold <- 1.8

adultosComMenosOutliers <- adultos_com_indice[adultos_com_indice$Hours.per.week <= mean_value + threshold * sd_value & adultos_com_indice$Hours.per.week >= mean_value - threshold * sd_value, ]

boxplot(adultosComMenosOutliers$Hours.per.week)

## Eliminação de dados inconsistentes

max_values <- apply(adultosComMenosOutliers, 2, max)
min_values <- apply(adultosComMenosOutliers, 2, min)

print(max_values)
print(min_values)

# Eliminação de dados redundantes
adultosComMenosOutliersESemRedundantes <- subset(adultosComMenosOutliers, select = -c(Indice))
sum(duplicated(adultosComMenosOutliersESemRedundantes))

adultosComMenosOutliersESemRedundantes <- unique(adultosComMenosOutliersESemRedundantes)

# Eliminação de dados ausentes

sapply(adultosComMenosOutliersESemRedundantes, function(x)all(is.na(x)))

# Conversão de tipos
adultosComMenosOutliersESemRedundantes$Workclass <- as.factor(adultosComMenosOutliersESemRedundantes$Workclass)
adultosComMenosOutliersESemRedundantes$Education <- as.factor(adultosComMenosOutliersESemRedundantes$Education)
adultosComMenosOutliersESemRedundantes$Marital.status <- as.factor(adultosComMenosOutliersESemRedundantes$Marital.status)
adultosComMenosOutliersESemRedundantes$Occupation <- as.factor(adultosComMenosOutliersESemRedundantes$Occupation)
adultosComMenosOutliersESemRedundantes$Relationship <- as.factor(adultosComMenosOutliersESemRedundantes$Relationship)
adultosComMenosOutliersESemRedundantes$Race <- as.factor(adultosComMenosOutliersESemRedundantes$Race)
adultosComMenosOutliersESemRedundantes$Sex <- as.factor(adultosComMenosOutliersESemRedundantes$Sex)
adultosComMenosOutliersESemRedundantes$Native.country <- as.factor(adultosComMenosOutliersESemRedundantes$Native.country)
adultosComMenosOutliersESemRedundantes$Above.Limit <- as.factor(adultosComMenosOutliersESemRedundantes$Above.Limit)
adultosComMenosOutliersESemRedundantes$Workclass <- as.numeric(adultosComMenosOutliersESemRedundantes$Workclass)
adultosComMenosOutliersESemRedundantes$Education <- as.numeric(adultosComMenosOutliersESemRedundantes$Education)
adultosComMenosOutliersESemRedundantes$Marital.status <- as.numeric(adultosComMenosOutliersESemRedundantes$Marital.status)
adultosComMenosOutliersESemRedundantes$Occupation <- as.numeric(adultosComMenosOutliersESemRedundantes$Occupation)
adultosComMenosOutliersESemRedundantes$Relationship <- as.numeric(adultosComMenosOutliersESemRedundantes$Relationship)
adultosComMenosOutliersESemRedundantes$Race <- as.numeric(adultosComMenosOutliersESemRedundantes$Race)
adultosComMenosOutliersESemRedundantes$Sex <- as.numeric(adultosComMenosOutliersESemRedundantes$Sex)
adultosComMenosOutliersESemRedundantes$Native.country <- as.numeric(adultosComMenosOutliersESemRedundantes$Native.country)
adultosComMenosOutliersESemRedundantes$Above.Limit <- as.numeric(adultosComMenosOutliersESemRedundantes$Above.Limit)
View(adultosComMenosOutliersESemRedundantes)

# Aplicando o PCA
pca <- prcomp(adultosComMenosOutliersESemRedundantes, scale = TRUE)
varianciaExplicada <- pca$sdev^2 / sum(pca$sdev^2)

plot(cumsum(varianciaExplicada), xlab = "Número de Componentes", ylab = "Variância Explicada Acumulada", type = "b")

quantidadeDeComponentes <- which(cumsum(varianciaExplicada) >= 0.95)[1]

summary(pca$x[, 1:quantidadeDeComponentes])


