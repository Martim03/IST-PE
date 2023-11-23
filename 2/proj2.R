library(readr)
library(dplyr)
library(ggplot2)


time_use_data <- read_csv("TIME_USE_24092022.csv")

filtered_data <- time_use_data %>%
  filter(País != "África do Sul",
         Ocupação %in% c("Cuidados pessoais", "Trabalho não remunerado"),
         Sexo == "Total")

gg_boxplot <- ggplot(filtered_data, aes(x = Ocupação, y = Tempo)) +
  geom_boxplot() +
  labs(title = "Comparação de Tempo Médio Diário em ocupações",
       x = "Ocupação",
       y = "Tempo (minutos)") +
  theme_minimal()

ggsave("ocupations.png", gg_boxplot)