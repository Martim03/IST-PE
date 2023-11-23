set.seed(1187)

m <- 200
n <- 39
sigma <- sqrt(4)
mu0 <- 50.1
mu1 <- 51.3
alpha <- 0.1

# Simulando m amostras
samples <- vector("list", m)


# PQ EQ ISTO N TA A DAR MEAN = 0 E SD = 1 ???? (mas ta a dar perto)
for (i in 1:m) {
  sample <- rnorm(n, mean = mu1, sd = sigma)
  samples[[i]] <- sample
}


# Calculando a estatística de teste e o valor-p para cada amostra
test_statistics <- sapply(samples, function(x) (mean(x) - mu0) / (sigma / sqrt(n)))
p_values <- 2 * pnorm(-abs(test_statistics))

# Contando o número de vezes que H0 não é rejeitado (valor-p > alpha)
non_rejections <- sum(p_values > alpha)

# Estimando a probabilidade do teste conduzir à não rejeição de H0
non_rejection_prob <- non_rejections / m

# Apresentando o resultado com 3 casas decimais
cat("Probabilidade de não rejeição de H0: ", round(non_rejection_prob, 3))


# VALS MARTIM 0.02