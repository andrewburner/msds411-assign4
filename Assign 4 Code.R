library(tidyverse)
library(GGally)
library(viridis)
library(recipes)
library(maptree)
library(ggdendro)
library(dendextend)
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

emp_pca <- emp %>%
  select(-Country, -Group) %>%
  princomp(cor = FALSE)

str(emp_pca)
emp_pca$scores


# PCA
emp_v2 <- emp %>%
  mutate(PCA1 = emp_pca$scores[,1],
         PCA2 = emp_pca$scores[,2])

head(emp_v2)

ggplot(emp_v2, aes(x = PCA1, y = PCA2, color = Group)) +
  geom_point() +
  scale_fill_viridis(discrete = TRUE) +
  geom_text(aes(label = Country), hjust = 0, vjust = 0)


#Scaled PCA (uses recipes package for easy scaling)
pca_recipe <- recipe(~., data = emp)

pca_trans <- pca_recipe %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_pca(all_numeric())

pca_estimates <- prep(pca_trans)
emp_v3 <- juice(pca_estimates)

ggplot(emp_v3, aes(x = PC1, y = PC2, color = Group)) +
  geom_point() +
  scale_fill_viridis(discrete = TRUE) +
  geom_text(aes(label = Country), hjust = 0, vjust = 0)

# 4

# Heirarchical Clustering
hcluster <- emp %>%
  select(-Country, -Group) %>%
  dist() %>%
  hclust(method = 'complete')

plot(hcluster,labels=emp$Country)


ggdendrogram(hcluster) +
  ggtitle("European Employment Dendrogram") +
  label(emp$Country)

dend <- as.dendrogram(hcluster)

d3 <- color_branches(dend, k=3)
plot(d3) +
  title("K=3")

d6 <- color_branches(dend, k=6)
plot(d6) +
  title("K=6")


# PCA Heirarchical Clustering

hcluster_pca <- emp_v3 %>%
  select(PC1, PC2) %>%
  dist() %>%
  hclust(method = 'complete')

plot(hcluster_pca, labels=emp$Country)







