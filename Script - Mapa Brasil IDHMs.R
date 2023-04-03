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

# Baixar MAPA do BRASIL no site do IBGE: https://www.ibge.gov.br/geociencias/organizacao-do-territorio/malhas-territoriais/15774-malhas.html?=&t=acesso-ao-produto
# IBGE > Malha Municipal > Brasil > Unidades da Federação.
# o arquivo é um zip com 5 itens intitulado "BR_UF_2021".
# o formato do arquivo é SHAPEFILE.

# Ler arquivos "BR_UF_2021" na pasta a qual intitulei de "shapefile_brasil".
mapa_br <- readOGR(dsn = "shapefile_brasil", layer = "BR_UF_2021")


#Informações acerca do Mapa 
summary(mapa_br)
class(mapa_br)
typeof(mapa_br)


# Visualizando informações do Mapa
mapa_br@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

#É possivel observar que a coluna "CD_UF" são identificadores dos Estados, sendo um dado qualitativo. 
# Usaremos a coluna "CD_UF" como chave para juntar informações a esse mapa.

mapa_br$NM_UF
mapa_br$SIGLA
mapa_br$NM_REGIAO
mapa_br$CD_UF

# Para acessar os outros componentes do shapefile, utilizaremos o operador @:

mapa_br@polygons #Posições geográficas dos polígonos
mapa_br@plotOrder #Ordem de plotagem dos polígonos
mapa_br@bbox #Eixo X (Longitude Oeste e Leste; Latitude Norte e Sul)
mapa_br@proj4string@projargs #Sistema de projeção geográfica do shapefile


# PLOTAGEM do SHAPEFILE
plot(mapa_br)


# BAIXAR PLANILHA COM IDHs: http://www.atlasbrasil.org.br/ranking
# incluir manualmente nessa planilha uma coluna com a mesma numeração idêntica da coluna "CD_UF" do Shapefile do IBGE.
# Eu inclui uma coluna com os mesmos números para cada Estado e intitulei "Codigo_UF" apenas para mostrar que não necessita ter o mesmo nome.
# As variáveis "CD_UF" e "Codigo_UF" são identicas e serão a chave para juntar os dados da tabela de IDHMs ao Mapa do IBGE.
# Após inclusão da coluna "Codigo_UF" podemos abrir a planilha através do R.

mapa_br@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 15)


#Lendo planilha baixada com a coluna "Codigo_UF" (idem CD_UF") já incluida.
library(readxl)
data_IDH_EstadosBR <- read_excel("data_IDH_EstadosBR.xlsx")
View(data_IDH_EstadosBR)

#ou
#file > import datset > from Excel


data_IDH_EstadosBR %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


# MERGE (NOVO OBJETO)
# Para combinar os dados do objeto dados_sp com a base de dados de nosso 
# shapefile, podemos utilizar a função merge():

mapa_br_merge <- merge(x = mapa_br,
                       y = data_IDH_EstadosBR,
                       by.x = "CD_UF",
                       by.y = "Código_UF")


#Visualizando dados da Tabela incluidos no Mapa
mapa_br_merge@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)



#sALVANDO???

writeOGR(obj = mapa_br_merge, 
         layer = "novo_shapefile_brasil", 
         driver = "ESRI Shapefile", 
         dsn = "mbadsa")



# Passo 1: Transformar o shapefile num objeto do tipo data frame e, depois,
# importar os dados que já estavam no shapefile para o novo objeto data frame.

mapa_br_merge <- tidy(mapa_br_merge, region = "CD_UF") %>% 
  rename(CD_UF = id) %>% 
  left_join(mapa_br_merge@data,
            by = "CD_UF")


#Passo 2: PLOTAGEM GGPLOT
mapa_br_merge %>% 
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
mapa_br_merge %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = IDHM, label = NM_UF),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "IDHM", "NM_UF") +
  scale_fill_viridis_c()+
  theme_bw()


#Limpar plotagens
plot.new()
