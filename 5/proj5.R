# Definindo a semente
set.seed(1394)

# Parâmetros
n <- 1092
p <- 0.35

# Função de distribuição acumulada de X
FX <- function(x) {
  if (x > 1) {
    return(1 - (1 - p)^floor(x))
  } else {
    return(0)
  }
}

# Método de transformação inversa
inverse_transform <- function(u) {
  x <- 0
  while (TRUE) {
    if (FX(x - 1) < u && u <= FX(x)) {
      return(x)
    }
    x <- x + 1
  }
}

# Simulação estocástica
samples <- numeric(n)
for (i in 1:n) {
  u <- runif(1)
  samples[i] <- inverse_transform(u)
}

# Cálculo da média e do desvio padrão amostrais
mean_sample <- mean(samples)
sd_sample <- sd(samples)

# Cálculo da proporção
above_mean <- samples[samples > mean_sample]
above_mean_sd <- above_mean[above_mean > mean_sample + sd_sample]
proportion <- length(above_mean_sd) / length(above_mean)
cat("Proporção: ", round(proportion, 4), "")

# VALORES DO MARTIM 0.4537