library(tidyverse)
library(GGally)
library(viridis)
library(recipes)
library(maptree)
library(ggdendro)
library(dendextend)
library(fpc)
library(cluster)
library(factoextra)

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
clusplot(subdat.6, k6_results$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

subdat.3 <- subdat.3 %>% as.data.frame(row.names = emp$Country)
fviz_cluster(k3_results, data=subdat.3, geom=c("point", "text"),
             show.clust.cent = TRUE, label = "name")

subdat.6 <- subdat.6 %>% as.data.frame(row.names = emp$Country)
fviz_cluster(k6_results, data=subdat.6, geom=c("point", "text"),
             show.clust.cent = TRUE, label = "name")


fviz_cluster(k3pca_results, data=subdat.3, geom=c("point", "text"),
             show.clust.cent = TRUE, label = "name")
fviz_cluster(k6pca_results, data=subdat.6, geom=c("point", "text"),
             show.clust.cent = TRUE, label = "name")

BetSSPerk3pca <- k3pca_results$betweenss/k3pca_results$totss
BetSSPerk3pca
BetSSPerk6pca <- k6pca_results$betweenss/k6pca_results$totss
BetSSPerk6pca


wssplot <- function(subdat, nc=20, seed=1234) {
  wss <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(subdat, centers=i)$withinss)
    }
  rs <- (wss[1] - wss)/wss[1]
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  plot(1:nc, rs, type="b", xlab="Number of Clusters",
       ylab="% of Between SS")
  }
wssplot(select(emp, -Country, -Group))


# US States
states <- readxl::read_xlsx('USStates.xlsx')

summary(states)
str(states)

ggpairs(states, columns=3:14, aes(color = Region))

states <- states %>% mutate(logPop = log(Population))


ggplot(states) +
  geom_point(aes(x = HouseholdIncome, y = Insured, color = Region))


pca_recipe <- recipe(~., data = states)

pca_trans <- pca_recipe %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_pca(all_numeric())

pca_estimates <- prep(pca_trans)
states_pca <- juice(pca_estimates)

stand_recipe <- recipe(~., data = states)

stand_trans <- stand_recipe %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

stand_vars <- prep(stand_trans)
states_stand <- juice(stand_vars)


ggplot(states_pca, aes(x = PC1, y = PC2, color = Region)) +
  geom_point() +
  scale_fill_viridis(discrete = TRUE) +
  geom_text(aes(label = State), hjust = 0, vjust = 0)

# Heirarchical Clustering
state_hcluster <- states %>%
  select(-State, -Region) %>%
  dist() %>%
  hclust(method = 'complete')

state_hcluster$labels <- states$State

ggdendrogram(state_hcluster, labels = TRUE) +
  ggtitle("State Dendrogram")

TSS <- (nrow(states)-1)*sum(apply(states[,3:ncol(states)],2,var))
state3 <- cutree(state_hcluster, k = 8)
WSS <- cluster.stats(states %>%
                         select(-State, -Region) %>%
                         dist(),
                     state3, alt.clustering=NULL)$within.cluster.ss
BetSSPer <- (TSS-WSS)/TSS
BetSSPer

wssplot <- function(subdat, nc=20, seed=1234) {
  wss <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- cluster.stats(subdat %>%
                            dist(),
                            cutree(stand_hcluster, k = i),
                            alt.clustering=NULL)$within.cluster.ss
  }
  rs <- (wss[1] - wss)/wss[1]
  plot(1:nc, wss, type="b", xlab="Number of Cuts",
       ylab="Within groups sum of squares")
  plot(1:nc, rs, type="b", xlab="Number of Cuts",
       ylab="% of Between SS")
}
wssplot(select(states, -State, -Region, -logPop))



plot(color_branches(as.dendrogram(state_hcluster), k = 8))



# Standardized variables for heirachrical clustering
stand_hcluster <- states_stand %>%
  select(-State, -Region) %>%
  dist() %>%
  hclust(method = 'complete')

stand_hcluster$labels <- states_stand$State

wssplot(select(states_stand, -State, -Region, -logPop))
states <- states %>% mutate(cluster = cutree(stand_hcluster, k = 8))


#RECIDIVISM
recid <- readxl::read_xlsx('recidivism.xlsx')
str(recid)
summary(recid)
ggpairs(recid)

wssplot <- function(subdat, nc=20, seed=1234) {
  wss <- (nrow(subdat)-1)*sum(apply(subdat,2,var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(subdat, centers=i)$withinss)
  }
  rs <- (wss[1] - wss)/wss[1]
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  plot(1:nc, rs, type="b", xlab="Number of Clusters",
       ylab="% of Between SS")
}
wssplot(recid)


k4_results <- kmeans(recid, 4)

pca_recipe <- recipe(~., data = recid)

pca_trans <- pca_recipe %>%
  step_center(age, tserved, follow, durat) %>%
  step_scale(age, tserved, follow, durat) %>%
  step_pca(all_numeric())

pca_estimates <- prep(pca_trans)
recid_pca <- juice(pca_estimates)

wssplot(recid_pca)

recid_pca <- recid_pca %>% mutate(cluster = k4_results$cluster)

ggplot(recid_pca) +
  geom_point(aes(PC1, PC2, color = as.factor(cluster)))
