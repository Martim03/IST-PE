p1 <- 5
p2 <- 6

exp1 <- 7
exp2 <- 30


# Função de probabilidade de Benford
P_Benford <- function(x) {
  return(log10(1 + 1 / x))
}

# 1. Probabilidade de X ser igual a 3 ou 6
P_X_p1_p2 <- P_Benford(p1) + P_Benford(p2)

# 2. Fração de potências de dois no intervalo [2^exp1, 2^exp2] cujo primeiro algarismo é igual a 3 ou 6
power_of_two <- 2^(exp1:exp2)

first_digit <- floor(power_of_two / 10^(floor(log10(power_of_two))))

count_p1_p2 <- sum(first_digit %in% c(p1, p2))
total <- length(power_of_two)
fraction_p1_p2 <- count_p1_p2 / total

# Desvio absoluto entre os valores calculados em 1. e 2.
absolute_deviation <- abs(P_X_p1_p2 - fraction_p1_p2)

# Apresentando o desvio absoluto arredondado a 4 casas decimais
cat("Desvio absoluto: ", round(absolute_deviation, 4))

# VALORES DO MARTIM DAO 0.0622