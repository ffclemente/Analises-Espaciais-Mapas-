#CRIANDO UM MAPA DO BRASIL COM OS IDHS DOS ESTADOS
# Instalação de pacotes necessários

pacotes <- c("rgdal","raster","tmap","maptools","sf","rgeos","sp","adehabitatHR",
             "tidyverse","broom","rayshader","knitr","kableExtra","RColorBrewer",
             "profvis", "rgdal","raster","tmap","maptools","tidyverse","broom","knitr",
             "kableExtra","RColorBrewer")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

rm(pacotes)
# Baixar MAPA do BRASIL no site do IBGE: https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html?=&t=acesso-ao-produto
# IBGE > Malha Municipal > Brasil > Unidades da Federação.
# o arquivo é um zip com 5 itens intitulado "BR_UF_2021".
# o formato do arquivo é SHAPEFILE.

# Ler arquivos "BR_UF_2021" na pasta a qual intitulei de "shapefile_brasil".
# O nome do objeto criado será "shp_br" de shapefile Brasil.

shp_br <- readOGR(dsn = "shapefile_brasil", layer = "BR_UF_2021")


#Informações acerca do Mapa 
summary(shp_br)
class(shp_br)
typeof(shp_br)


# Visualizando informações do Mapa
shp_br@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

shp_br$NM_UF
shp_br$SIGLA
shp_br$NM_REGIAO

# Para acessar os outros componentes do shapefile, utilizaremos o operador @:

shp_br@polygons #Posições geográficas dos polígonos
shp_br@plotOrder #Ordem de plotagem dos polígonos
shp_br@bbox #Eixo X (Longitude Oeste e Leste; Latitude Norte e Sul)
shp_br@proj4string@projargs #Sistema de projeção geográfica do shapefile


# PLOTAGEM do SHAPEFILE
plot(shp_br)

area(shp_br)


# BAIXAR PLANILHA COM IDHs: http://www.atlasbrasil.org.br/ranking
# incluir na planilha uma coluna com a mesma numeração do Shapefile "CD_UF"

#Verificar "CD_UF" do Mapa e criar uma coluna com a mesma numeração na planilha.
shp_br@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


#Lendo planilha baixada com a coluna "Codigo_UF" (idem CD_UF") já incluida.
library(readxl)
data_IDH_EstadosBR <- read_excel("data_IDH_EstadosBR.xlsx")
View(data_IDH_EstadosBR)

#ou
#file > import datset > from Excel

View(data_IDH_EstadosBR)
edit(data_IDH_EstadosBR)


#visualizando informações do Dataset

data_IDH_EstadosBR %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


# MERGE (NOVO OBJETO)
# Para combinar os dados do objeto dados_sp com a base de dados de nosso 
# shapefile, podemos utilizar a função merge():

shp_dados_br <- merge(x = shp_br,
                      y = data_IDH_EstadosBR,
                      by.x = "CD_UF",
                      by.y = "Código_UF")


shp_dados_br@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)



#sALVANDO

writeOGR(obj = shp_dados_sp, 
         layer = "nosso_novo_shapefile", 
         driver = "ESRI Shapefile", 
         dsn = "mbadsa")



#PLOTAGEM HISTOGRAMA
shp_dados_br@data %>% 
  ggplot() +
  geom_histogram(aes(x = IDHM),
                 fill = "deepskyblue4",
                 color = "white") +
  labs(x = "IDHM",
       y = "Frequência") +
  theme_bw()



# Passo 1: Transformar o shapefile num objeto do tipo data frame e, depois,
# importar os dados que já estavam no shapefile para o novo objeto data frame.

shp_dados_br <- tidy(shp_dados_br, region = "CD_UF") %>% 
  rename(CD_UF = id) %>% 
  left_join(shp_dados_br@data,
            by = "CD_UF")


#Passo 2: PLOTAGEM GGPLOT
shp_dados_br %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = IDHM),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       color = "IDHM") +
  scale_fill_viridis_c() +
  theme_bw()


#Passo 3: PLOTAGEM GGPLOT INTERATIVA

plotly::ggplotly ()
shp_dados_br %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = IDHM, label = NM_UF),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "IDHM", "NM_UF") +
  scale_fill_viridis_c()+
  theme_bw()
plot.new()
