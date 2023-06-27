ConjuntoDeTreinamento <- read.csv(file = "baseTreinamentoTrab3.csv", stringsAsFactors = TRUE)
ConjuntoDeTeste <- read.csv(file = "baseTesteTrab3.csv", stringsAsFactors = TRUE)
BaseCompleta <- read.csv(file = "baseCompletaTrab3.csv", stringsAsFactors = TRUE)

# Remover atributos que não serão utilizados
ConjuntoDeTreinamento <- subset(ConjuntoDeTreinamento, select = -c(1, 7, 9, 10, 11))
ConjuntoDeTeste <- subset(ConjuntoDeTeste, select = -c(1, 7, 9, 10, 11))

# Determinar o tamanho de cada Fold
TamanhoDosFolds <- ceiling(nrow(ConjuntoDeTreinamento) / 5)

# Dividir o dataframe em partes iguais
folds <- split(ConjuntoDeTreinamento, rep(1:5, each = TamanhoDosFolds, length.out = nrow(ConjuntoDeTreinamento)))

# Contar a quantidade acima e abaixo de 50k para o baseline
QuantidadeAcimaDe50K <- table(folds[[1]]$Above.Limit)["2"]
QuantidadeAbaixoDe50K <- table(folds[[1]]$Above.Limit)["1"]