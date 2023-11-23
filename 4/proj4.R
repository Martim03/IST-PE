# Definindo a semente
set.seed(3960)

# Parâmetros
k <- 4314
lambda <- 28

# Gerando uma amostra de dimensão k a partir de uma distribuição Exponencial de parâmetro λ
sample_exp <- rexp(k, rate = lambda)

# Calculando a soma sucessiva das observações
s <- cumsum(sample_exp)
# Encontrando o menor número inteiro maior ou igual ao instante de ocorrência do último acontecimento
T <- ceiling(s[k])

# Dividindo o intervalo ]0,T] em intervalos de amplitude unitária
bins <- seq(0, T, by = 1)

# Contabilizando o número de acontecimentos em cada subintervalo
events_per_bin <- hist(s, breaks = bins, plot = FALSE)$counts

# Calculando a média do número de acontecimentos por subintervalo
mean_events <- mean(events_per_bin)

# Calculando o valor esperado (teórico) do número de acontecimentos num subintervalo
expected_value_of_events_in_interval <- lambda

# Calculando o desvio absoluto entre a média e o valor esperado (teórico)
absolute_deviation <- abs(mean_events - expected_value_of_events_in_interval)


# Apresentando o desvio absoluto arredondado a 4 casas decimais
cat("Desvio absoluto: ", round(absolute_deviation, 4))


# VALORES DO MARTIM DAO 0.3462
