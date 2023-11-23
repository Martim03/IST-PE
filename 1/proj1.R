library(readxl)
library(ggplot2)
library(dplyr)

dados_econ <- read_xlsx("econ.xlsx")

dados_selecionados <- dados_econ %>%
  filter(format(as.Date(tempo, format="%d/%m/%Y"), "%Y")>= 1991) %>%
  select(tempo, gcp, ndesemp)

dados_transformados <- dados_selecionados %>%
  mutate(z_gcp = (gcp - mean(gcp)) / sd(gcp),
         z_ndesemp = (ndesemp - mean(ndesemp)) / sd(ndesemp))

grafico <- ggplot(data = dados_transformados) +
  geom_line(aes(x = tempo, y = z_gcp, color = "gcp"), linewidth = 1) +
  geom_line(aes(x = tempo, y = z_ndesemp, color = "ndesemp"), linewidth = 1) +
  scale_color_manual(values = c("gcp" = "blue", "ndesemp" = "red")) +
  labs(title = "Evolução das variáveis gcp e ndesemp a partir de 1991",
       y = "Valores", x = "Tempo") +
  theme_minimal()

ggsave("grafico.png", grafico)
