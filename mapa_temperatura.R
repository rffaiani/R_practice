#projeto mapa de temperatura em R

#carregando os pacotes
library(ggplot2)
library(geobr)
library(raster)
library(fields)
library(geospatial)

setwd('C:/CENTRAL/ProjetosR/20210127_projeto_temperatura')

dados.temp <- read.csv('dados/dados_temperatura.csv')

relevo.mg <- raster('dados/relevo_minas_gerais.tif')
#plot(relevo.mg)

mg <- read_state(code_state = 'MG')
#plot(mg)
#plot(mg$geom)

modelo <- lm(formula = temp~longitude+latitude+altitude, data = dados.temp)

summary(modelo)

#transformar os dados em dataframe

relevo.df <- as.data.frame(relevo.mg, xy = TRUE)

#removendo valores que estão faltando

relevo.df <- na.omit(relevo.df)
relevo.df

#renomear as colunas de dados
names(relevo.df)
names(relevo.df) <- c('longitude', 'latitude', 'altitude')

relevo.df

#criar coluna temperatura

relevo.df$temp <- 23.49 - 0.25*relevo.df$longitude + 0.48*relevo.df$latitude - 0.0053 * relevo.df$altitude
relevo.df

#criar coluna temperatura com a função predict

relevo.df$temp2 <- predict(modelo, relevo.df)
relevo.df


##ok1



