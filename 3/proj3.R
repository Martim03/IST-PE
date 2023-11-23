library(readr)
library(dplyr)
library(ggplot2)

gender_emp_data <- read_delim("GENDER_EMP_19032023152556091.txt", delim = "\t")

filtered_data <- gender_emp_data %>%
  filter(Country == "Finland",
         IND == "EMP2",
         Time == "2019",
         `Age Group` %in% c("15-24", "25-54", "55-64"),
         Sex %in% c("Men", "Women"))

bar_plot <- ggplot(filtered_data, aes(x = `Age Group`, y = Value, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Employment/Population Ratio in Finland, 2019",
       x = "Age Group",
       y = "Employment/Population Ratio (%)",
       fill = "Sex") +
  theme_minimal()

ggsave("data.png", bar_plot)