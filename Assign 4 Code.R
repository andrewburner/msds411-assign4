library(tidyverse)
library(GGally)
emp <- read_csv('EuropeanEmployment.csv')

basic_eda <- function(data)
{
  glimpse(data)
  print(status(data))
  freq(data) 
  print(profiling_num(data))
  plot_num(data)
  describe(data)
}

basic_eda(emp)
head(emp)
dim(emp)
str(emp)


View(emp)


ggpairs(emp, columns=3:11, aes(color = Group))

ggplot(emp, aes(x = MAN, y = SER, color = Group)) +
  geom_point() +
  scale_fill_viridis(discrete = TRUE) +
  geom_text(aes(label = Country), hjust = 0, vjust = 0)


ggplot(emp, aes(x = FIN, y = SER, color = Group)) +
  geom_point() +
  scale_fill_viridis(discrete = TRUE) +
  geom_text(aes(label = Country), hjust = 0, vjust = 0)



