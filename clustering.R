# Mesa y Castro 
# Target: Analysys cluster and PCA to cocoa Points in Colombia
# Clustering climate data


## Libraries necessary 
library(tidyverse)
library(FactoMineR)
library(gtools)


df <- read_csv("C:/Jeison/Github/cocoa_Col/data_base/cacao_swd.csv")

# Es necesario investigar acerca del analisis de componentes principales dual que es mejor pero en este momento
# no recuerdomos que ñoña hace

# Analisis componentes principales simple


# res.pca = PCA(decathlon[,1:10], scale.unit = TRUE, graph = F)

## Quiza pueda ser mejor tener un vector que indique que variables quiero seleccionar para el analisis

vars_acp  <- df %>%
  select(bio_1:bio_29)

## num_vars me indica el numero de variables para calcular luego todas las componentes en el analisis de componentes principales

num_vars <- dim(vars_acp)[2]

res.pca = PCA(vartoacp, scale.unit = TRUE, graph = T, ncp = num_vars)

eigen_df <- tbl_df(data.frame(res.pca$eig))

ggplot(aes(x = reorder(row.names(eigen_df), -eigenvalue), y = eigenvalue), data = eigen_df) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "Componentes") +
  ggtitle("Varianza por componente") +
  geom_hline(aes(yintercept=1), colour = "red")

## 


