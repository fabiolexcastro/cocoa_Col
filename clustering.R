# Mesa y Castro 
# Target: Analysys cluster and PCA to cocoa Points in Colombia
# Clustering climate data


## Libraries necessary 
library(tidyverse)
library(FactoMineR)

df <- read_csv("D:/_clustering/_points/cacao_swd.csv")

# Es necesario investigar acerca del analisis de componentes principales dual que es mejor pero en este momento
# no recuerdomos que ñoña hace

# Analisis componentes principales simple


# res.pca = PCA(decathlon[,1:10], scale.unit = TRUE, graph = F)

novar <- c("X1", "FID", "Long", "Lat", "ID")

vartoacp <- df %>%
  select(bio_1:bio_29)

ncpnumber <- dim(vartoacp)[2]

res.pca = PCA(vartoacp, scale.unit = TRUE, graph = T, ncp = ncpnumber)
datax <- data.frame(res.pca$eig)

ggplot(aes(x = row.names(datax), y = eigenvalue), data = datax) +
  geom_bar(stat = 'identity') +
  labs(x='ID')

barplot(res.pca$eig$eigenvalue)
res.pca$var
