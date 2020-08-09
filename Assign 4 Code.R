library(tidyverse)
library(GGally)
library(viridis)
library(recipes)
library(maptree)
library(ggdendro)
library(dendextend)
library(fpc)
library(cluster)

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

hcluster$labels <- emp$Country

ggdendrogram(hcluster, labels = TRUE) +
  ggtitle("European Employment Dendrogram")

subdat.3 <- emp %>%
  select(-Country, -Group)
TSS.3 <- (nrow(subdat.3)-1)*sum(apply(subdat.3,2,var))
TSS.3

complete3 <- cutree(hcluster, k = 3)
WSS.3 <- cluster.stats(emp %>%
                         select(-Country, -Group) %>%
                         dist(),
                       complete3, alt.clustering=NULL)$within.cluster.ss
WSS.3
BetSSPer.3 <- (TSS.3-WSS.3)/TSS.3
BetSSPer.3

subdat.6 <- emp %>%
  select(-Country, -Group)
TSS.6 <- (nrow(subdat.6)-1)*sum(apply(subdat.6,2,var))
TSS.6

complete6 <- cutree(hcluster, k = 6)
WSS.6 <- cluster.stats(emp %>%
                         select(-Country, -Group) %>%
                         dist(),
                       complete6, alt.clustering=NULL)$within.cluster.ss
WSS.6
BetSSPer.6 <- (TSS.6-WSS.6)/TSS.6
BetSSPer.6


# PCA Heirarchical Clustering
subdat.3_pca <- emp_v3 %>%
  select(PC1, PC2)
TSS.3_pca <- (nrow(subdat.3_pca)-1)*sum(apply(subdat.3_pca,2,var))
TSS.3_pca

complete3_pca <- cutree(hcluster, k = 3)
WSS.3_pca <- cluster.stats(emp_v3 %>%
                             select(PC1, PC2) %>%
                             dist(),
                           complete3_pca, alt.clustering=NULL)$within.cluster.ss
WSS.3_pca
BetSSPer.3_pca <- (TSS.3_pca-WSS.3_pca)/TSS.3_pca
BetSSPer.3_pca



subdat.6_pca <- emp_v3 %>%
  select(PC1, PC2)
TSS.6_pca <- (nrow(subdat.6_pca)-1)*sum(apply(subdat.6_pca,2,var))
TSS.6_pca

complete6_pca <- cutree(hcluster, k = 6)
WSS.6_pca <- cluster.stats(emp_v3 %>%
                         select(PC1, PC2) %>%
                         dist(),
                       complete6_pca, alt.clustering=NULL)$within.cluster.ss
WSS.6_pca
BetSSPer.6_pca <- (TSS.6_pca-WSS.6_pca)/TSS.6_pca
BetSSPer.6_pca

# 5
# K Means Clustering
k3_results <- kmeans(subdat.3, 3)
k3pca_results <- kmeans(subdat.3_pca, 3)

subdat.3 <- subdat.3 %>% mutate(k3 = k3_results$cluster,
                                k3_pca = k3pca_results$cluster)


k6_results <- kmeans(subdat.6, 6)
k6pca_results <- kmeans(subdat.6_pca, 6)

subdat.6 <- subdat.6 %>% mutate(k6 = k6_results$cluster,
                                k6_pca = k6pca_results$cluster)


BetSSPerk3 <- k3_results$betweenss/k3_results$totss
BetSSPerk3
BetSSPerk6 <- k6_results$betweenss/k6_results$totss
BetSSPerk6

clusplot(subdat.3, k3_results$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)




