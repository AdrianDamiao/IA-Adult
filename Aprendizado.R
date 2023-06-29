library(class)

ConjuntoDeTreinamento <- read.csv(file = "baseTreinamentoTrab3.csv", stringsAsFactors = TRUE)
ConjuntoDeTeste <- read.csv(file = "baseTesteTrab3.csv", stringsAsFactors = TRUE)
BaseCompleta <- read.csv(file = "baseCompletaTrab3.csv", stringsAsFactors = TRUE)

# Remover atributos que não serão utilizados
ConjuntoDeTreinamento <- subset(ConjuntoDeTreinamento, select = -c(6, 8, 9, 10))
ConjuntoDeTeste <- subset(ConjuntoDeTeste, select = -c(6, 8, 9, 10))

# Converte a saída para 0 e 1
ConjuntoDeTreinamento$Above.Limit <- ifelse(ConjuntoDeTreinamento$Above.Limit == 1, 0, 1)
ConjuntoDeTeste$Above.Limit <- ifelse(ConjuntoDeTeste$Above.Limit == 1, 0, 1)

# Determinar o tamanho de cada Fold
TamanhoDosFolds <- ceiling(nrow(ConjuntoDeTreinamento) / 5)

# Dividir o dataframe em partes iguais
folds <- split(ConjuntoDeTreinamento, rep(1:5, each = TamanhoDosFolds, length.out = nrow(ConjuntoDeTreinamento)))

# Baseline
# Contar a quantidade acima e abaixo de 50k para o baseline
QuantidadeAcimaDe50K <- table(folds[[1]]$Above.Limit)["1"]
QuantidadeAbaixoDe50K <- table(folds[[1]]$Above.Limit)["0"]

# Calcular as iterações
# Iteração 1
Iteracao1 <- rbind(folds[[1]], folds[[2]], folds[[3]], folds[[4]])
TargetTrain1 <- Iteracao1$Above.Limit
Iteracao1$Above.Limit <- NULL

TesteIteracao1 <- folds[[5]]
TargetTest1 <- TesteIteracao1$Above.Limit
TesteIteracao1$Above.Limit <- NULL

# Iteração 2
Iteracao2 <- rbind(folds[[1]], folds[[2]], folds[[3]], folds[[5]])
TargetTrain2 <- Iteracao2$Above.Limit
Iteracao2$Above.Limit <- NULL

TesteIteracao2 <- folds[[4]]
TargetTest2 <- TesteIteracao2$Above.Limit
TesteIteracao2$Above.Limit <- NULL

# Iteracao 3
Iteracao3 <- rbind(folds[[1]], folds[[2]], folds[[4]], folds[[5]])
TargetTrain3 <- Iteracao3$Above.Limit
Iteracao3$Above.Limit <- NULL

TesteIteracao3 <- folds[[3]]
TargetTest3 <- TesteIteracao3$Above.Limit
TesteIteracao3$Above.Limit <- NULL

# Iteração 4
Iteracao4 <- rbind(folds[[1]], folds[[3]], folds[[4]], folds[[5]])
TargetTrain4 <- Iteracao4$Above.Limit
Iteracao4$Above.Limit <- NULL

TesteIteracao4 <- folds[[2]]
TargetTest4 <- TesteIteracao4$Above.Limit
TesteIteracao4$Above.Limit <- NULL

# Iteração 5
Iteracao5 <- rbind(folds[[2]], folds[[3]], folds[[4]], folds[[5]])
TargetTrain5 <- Iteracao5$Above.Limit
Iteracao5$Above.Limit <- NULL

TesteIteracao5 <- folds[[1]]
TargetTest5 <- TesteIteracao5$Above.Limit
TesteIteracao5$Above.Limit <- NULL

# Iteração 1
limiar <- 0.5

x <- data.frame (Iteracao1, y = as.factor(TargetTrain1))
model <- class::knn(train = Iteracao1, test = TesteIteracao1, cl = TargetTrain1, k = 3)
predsVal <- as.numeric(as.character(model))
predVal <- ifelse(predsVal > limiar, 1, 0)

print(predVal)

tp <- sum((TargetTest1 == 1) & (predVal == 1))
fp <- sum((TargetTest1 == 0) & (predVal == 1))
tn <- sum((TargetTest1 == 0) & (predVal == 0))
fn <- sum((TargetTest1 == 1) & (predVal == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

Acuracia1 <- ifelse(is.nan((tp+tn)/(tp+fp+tn+fn)), 0, (tp+tn)/(tp+fp+tn+fn))
Precisao1 <- ifelse(is.nan(tp/(tp+fp)), 0, tp/(tp+fp))
Recall1 <- ifelse(is.nan(tp/(tp+fn)), 0, tp/(tp+fn))
MedidaF1 <- ifelse(is.nan(2/((1/Precisao1)+(1/Recall1))), 0, 2/((1/Precisao1)+(1/Recall1)))

# Iteração 2
limiar <- 0.5

x <- data.frame (Iteracao2, y = as.factor(TargetTrain2))
model <- class::knn(train = Iteracao2, test = TesteIteracao2, cl = TargetTrain2, k = 3)
predsVal <- as.numeric(as.character(model))
predVal <- ifelse(predsVal > limiar, 1, 0)

print(predVal)

tp <- sum((TargetTest2 == 1) & (predVal == 1))
fp <- sum((TargetTest2 == 0) & (predVal == 1))
tn <- sum((TargetTest2 == 0) & (predVal == 0))
fn <- sum((TargetTest2 == 1) & (predVal == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

Acuracia2 <- ifelse(is.nan((tp+tn)/(tp+fp+tn+fn)), 0, (tp+tn)/(tp+fp+tn+fn))
Precisao2 <- ifelse(is.nan(tp/(tp+fp)), 0, tp/(tp+fp))
Recall2 <- ifelse(is.nan(tp/(tp+fn)), 0, tp/(tp+fn))
MedidaF2 <- ifelse(is.nan(2/((1/Precisao2)+(1/Recall2))), 0, 2/((1/Precisao2)+(1/Recall2)))

# Iteracao 3
limiar <- 0.5

x <- data.frame (Iteracao3, y = as.factor(TargetTrain3))
model <- class::knn(train = Iteracao3, test = TesteIteracao3, cl = TargetTrain3, k = 3)
predsVal <- as.numeric(as.character(model))
predVal <- ifelse(predsVal > limiar, 1, 0)

print(predVal)

tp <- sum((TargetTest3 == 1) & (predVal == 1))
fp <- sum((TargetTest3 == 0) & (predVal == 1))
tn <- sum((TargetTest3 == 0) & (predVal == 0))
fn <- sum((TargetTest3 == 1) & (predVal == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

Acuracia3 <- ifelse(is.nan((tp+tn)/(tp+fp+tn+fn)), 0, (tp+tn)/(tp+fp+tn+fn))
Precisao3 <- ifelse(is.nan(tp/(tp+fp)), 0, tp/(tp+fp))
Recall3 <- ifelse(is.nan(tp/(tp+fn)), 0, tp/(tp+fn))
MedidaF3 <- ifelse(is.nan(2/((1/Precisao3)+(1/Recall3))), 0, 2/((1/Precisao3)+(1/Recall3)))

# Iteração 4
limiar <- 0.5

x <- data.frame (Iteracao4, y = as.factor(TargetTrain4))
model <- class::knn(train = Iteracao4, test = TesteIteracao4, cl = TargetTrain4, k = 3)
predsVal <- as.numeric(as.character(model))
predVal <- ifelse(predsVal > limiar, 1, 0)

print(predVal)

tp <- sum((TargetTest4 == 1) & (predVal == 1))
fp <- sum((TargetTest4 == 0) & (predVal == 1))
tn <- sum((TargetTest4 == 0) & (predVal == 0))
fn <- sum((TargetTest4 == 1) & (predVal == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

Acuracia4 <- ifelse(is.nan((tp+tn)/(tp+fp+tn+fn)), 0, (tp+tn)/(tp+fp+tn+fn))
Precisao4 <- ifelse(is.nan(tp/(tp+fp)), 0, tp/(tp+fp))
Recall4 <- ifelse(is.nan(tp/(tp+fn)), 0, tp/(tp+fn))
MedidaF4 <- ifelse(is.nan(2/((1/Precisao4)+(1/Recall4))), 0, 2/((1/Precisao4)+(1/Recall4)))

# Iteração 5
limiar <- 0.5

x <- data.frame (Iteracao5, y = as.factor(TargetTrain5))
model <- class::knn(train = Iteracao5, test = TesteIteracao5, cl = TargetTrain5, k = 3)
predsVal <- as.numeric(as.character(model))
predVal <- ifelse(predsVal > limiar, 1, 0)

print(predVal)

tp <- sum((TargetTest5 == 1) & (predVal == 1))
fp <- sum((TargetTest5 == 0) & (predVal == 1))
tn <- sum((TargetTest5 == 0) & (predVal == 0))
fn <- sum((TargetTest5 == 1) & (predVal == 0))
confusionMat <- matrix(c(tn, fn, fp, tp), nrow = 2, ncol = 2, dimnames = list(c("0","1"), c("0","1")))

Acuracia5 <- ifelse(is.nan((tp+tn)/(tp+fp+tn+fn)), 0, (tp+tn)/(tp+fp+tn+fn))
Precisao5 <- ifelse(is.nan(tp/(tp+fp)), 0, tp/(tp+fp))
Recall5 <- ifelse(is.nan(tp/(tp+fn)), 0, tp/(tp+fn))
MedidaF5 <- ifelse(is.nan(2/((1/Precisao5)+(1/Recall5))), 0, 2/((1/Precisao5)+(1/Recall5)))

MediaAcuracia <- (Acuracia1+Acuracia2+Acuracia3+Acuracia4+Acuracia5)/5
MediaPrecisao <- (Precisao1+Precisao2+Precisao3+Precisao4+Precisao5)/5
MediaRecall <- (Recall1+Recall2+Recall3+Recall4+Recall5)/5
MediaMedidaF <- (MedidaF1+MedidaF2+MedidaF3+MedidaF4+MedidaF5)/5


