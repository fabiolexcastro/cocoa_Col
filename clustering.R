# Seleccionar el lenguaje a trabajar, es mejor ingles o español ?
# Realizar tanto presentacion en R como el informe, la idea es que sirva todo lo que vamos a escribir como propuesta para
# el articulo a realizar (seleccionar zonas idoneas, independiente del cultivo).
# La idea es como tener un tutorial de las recomendaciones al realizar la seleccion de areas idoneas de cultivos
# haciendo uso de las tecnicas multivariantes y random forest
# Mesa y Castro 
# Target: Analysys cluster and PCA to cocoa Points in Colombia
# Clustering climate dataç
# la idea es mejorar algunos analisis que Crhistian Bunn ha creado (desde el punto de vista estadistico y la opinion experta entre Castro y Bunn)


## Libraries necessary 
library(tidyverse)
library(FactoMineR)
library(gtools)


df <- read_csv("C:/Jeison/Github/cocoa_Col/data_base/cacao_swd.csv")

# Es necesario investigar acerca del analisis de componentes principales dual que es mejor pero en este momento
# no recuerdomos que ñoña hace

# Analisis componentes principales simple




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


## buscar la funcion en R que haga lo siguiente
## Generar codigo que automaticamente me genere el numero de componentes a utilizar (Previo a un analisis)
## data es la base de datos que contiene solo aquellas variables que deseo ingresar en el analisis
## scale para definir si se desea escalar las variables a 1 es decir todas las variables ingresan con el mismo peso en el analisis scale = T, scale = F no
## generar parametro para definir si se quiere el numero de componentes por valores propios mayor a 1 o eleccion por porcentaje
## by es un parametro que me definie si deseo sacar las componentes por valores propios o por porcentaje de varianza acumulada
## by = lambdas selecciona las componentes que superen en sus valores propios 1
## by = percentage selecciona el porcentaje de varianza explicada el cual deseo

number_comp <- function(data, scale = T, by = "lambdas", percentage = 75){
  
  # data <- vartoacp
  
  num_vars <- ncol(data)
  res.pca = PCA(data, scale.unit = scale, graph = F, ncp = num_vars)
  res.pca.eigen <- tbl_df(res.pca$eig)
  
  
  if(by == "lambdas"){
    
    num_comp <- res.pca.eigen %>%
                  filter(eigenvalue > 1)
    
  }
  
  if(by == "percentage"){
    
    num_comp <- res.pca.eigen %>%
                  filter(`cumulative percentage of variance` <= percentage)
  
  }
  
  return(ncol(num_comp))
  
}
# number_comp(vars_acp, by = "percentage", percentage = 87)
number_comp(vars_acp)

## 
## averiguar la forma de los metodos
# Numero de cluster
# para definir los cluster y las distancias de los grupos



res.hcpc = HCPC(res.pca)
