# Definindo a semente
set.seed(1588)

# Parâmetros
m <- 1898
n <- 12
p <- 0.33

# Simulando m amostras de dimensão n de uma população normal de média nula e variância unitária
samples <- vector("list", m)

# PQ EQ ISTO N TA A DAR MEAN = 0 E SD = 1 ???? (mas ta a dar perto)
for (i in 1:m) {
  sample <- rnorm(n, mean = 0, sd = 1)
  samples[[i]] <- sample
}

# Calculando a soma dos quadrados dos valores observados para cada amostra
sum_squares <- sapply(samples, function(x) sum(x^2))

# Calculando o quantil de probabilidade da amostra das somas dos quadrados dos valores observados
sample_quantile <- quantile(sum_squares, p, type = 2)

# Calculando o quantil correspondente à distribuição teórica da soma de quadrados de variáveis normais reduzidas independentes
theoretical_quantile <- qchisq(p, df = n)

# Calculando a diferença em valor absoluto entre os quantis
absolute_difference <- abs(sample_quantile - theoretical_quantile)

# Apresentando a diferença em valor absoluto arredondado a 4 casas decimais
cat("Diferença em valor absoluto: ", round(absolute_difference, 4))

# VALS MARTIM 0.0176