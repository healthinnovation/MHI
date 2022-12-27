library(readxl)
ancon <- read_excel("Data/Ancon.xlsx")
ate <- read_excel("Data/Ate.xlsx")
barranco <- read_excel("Data/Barranco.xlsx")
brena <- read_excel("Data/Brena.xlsx")
callao <- read_excel("Data/Callao.xlsx")
carabayllo <- read_excel("Data/Carabayllo.xlsx")
chaclacayo <- read_excel("Data/Chaclacayo.xlsx")
chorrillos <- read_excel("Data/Chorrillos.xlsx")
cieneguilla <- read_excel("Data/Cieneguilla.xlsx")
comas <- read_excel("Data/Comas.xlsx")
elagustino <- read_excel("Data/El agustino.xlsx")
independencia <- read_excel("Data/Independencia.xlsx")
jesusmaria <- read_excel("Data/Jesus maria.xlsx")
lamolina <- read_excel("Data/La molina.xlsx")
lavictoria <- read_excel("Data/La victoria.xlsx")
lima  <- read_excel("Data/Lima.xlsx")
lince <- read_excel("Data/Lince.xlsx")
losolivos <- read_excel("Data/Los olivos.xlsx")
lurigancho <- read_excel("Data/Lurigancho.xlsx")
lurin <- read_excel("Data/Lurin.xlsx")
magdalena  <- read_excel("Data/Magdalena.xlsx")
miraflores <- read_excel("Data/Miraflores.xlsx")
pachacamac <- read_excel("Data/Pachacamac.xlsx")
pucusana <- read_excel("Data/Pucusana.xlsx")
pueblolibre <- read_excel("Data/Pueblo libre.xlsx")
puentepiedra <- read_excel("Data/Puente piedra.xlsx")
puntahermosa <- read_excel("Data/Punta hermosa.xlsx")
puntanegra <- read_excel("Data/Punta negra.xlsx")
rimac <- read_excel("Data/Rimac.xlsx")
sanbartolo <- read_excel("Data/San bartolo.xlsx")
sanborja <- read_excel("Data/San borja.xlsx")
sanisIDMANZANAro <- read_excel("Data/San isIDMANZANAro.xlsx")
sanjuanlurigancho <- read_excel("Data/San juan lurigancho.xlsx")
sanjuanmiraflores <- read_excel("Data/San juan miraflores.xlsx")
sanluis <- read_excel("Data/San luis.xlsx")
sanmartinporres <- read_excel("Data/San martin porres.xlsx")
sanmiguel <- read_excel("Data/San miguel.xlsx")
santaanita <- read_excel("Data/Santa anita.xlsx")
santamariamar <- read_excel("Data/Santa maria mar.xlsx")
santarosa <- read_excel("Data/Santa rosa.xlsx")
santiagosurco <- read_excel("Data/Santiago surco.xlsx")
surquillo <- read_excel("Data/Surquillo.xlsx")
villamariatriunfo <- read_excel("Data/Villa maria triunfo.xlsx")
villasalvador <- read_excel("Data/Villa salvador.xlsx")

library(dplyr)
library(tIDMANZANAyverse)

total <- bind_rows(ate, ancon, barranco, brena, callao, carabayllo, 
                    chaclacayo, chorrillos, cieneguilla, comas, elagustino, 
                    independencia, jesusmaria, lamolina, lavictoria, lima, 
                    lince, losolivos, lurigancho, lurin, magdalena, miraflores, 
                    pachacamac, pucusana, pueblolibre, puentepiedra, puntahermosa, 
                    puntanegra, rimac, sanbartolo, sanborja, sanisIDMANZANAro, 
                    sanjuanlurigancho, sanjuanmiraflores, sanluis, 
                    sanmartinporres, sanmiguel, santaanita, santamariamar, 
                    santarosa, santiagosurco, surquillo, villamariatriunfo, 
                    villasalvador, .IDMANZANA = NULL)

total[is.na(total)] <- 0
total$Total <- NULL

total$Total <- (total$`Casa Independiente`
                +total$`Departamento en edificio`
                +total$`Vivienda en quinta`
                +total$`Vivienda en casa de vecindad (Callejón, solar o corralón)`
                +total$`Vivienda improvisada`
                +total$`Local no destinado para habitación humana`
                +total$`Viviendas colectivas`
                +total$`Otro tipo de vivienda particular`)
total <- subset(total, Total != 0)

total$casa <- (100/total$Total)*(total$`Casa Independiente`+total$`Departamento en edificio`)
total$nocasa <- 100-total$casa
total$agua <- (100/total$Total)*(total$`Red pública dentro de la vivienda`+total$`Red pública fuera de la vivienda, pero dentro de la edificación`)
total$noagua <- 100-total$agua
total$luz <- (100/total$Total)*(total$`Si tiene alumbrado eléctrico`)
total$noluz <- 100-total$luz
total$internet <- (100/total$Total)*(total$`Si tiene conexión a internet`)
total$nointernet <- 100-total$internet
total$seguro <- (100/total$Total)*(total$`Solo Seguro Integral de Salud (SIS)` 
                                   +total$`Solo EsSalud`+total$`Solo Seguro de fuerzas armadas o policiales` 
                                   +total$`Solo Seguro privado de salud` 
                                   +total$`Solo Otro seguro`
                                   +total$`Seguro Integral de Salud (SIS) y EsSalud`
                                   +total$`Seguro Integral de Salud (SIS) y Seguro privado de salud`
                                   +total$`Seguro Integral de Salud (SIS) y Otro seguro`
                                   +total$`EsSalud y Seguro de fuerzas armadas o policiales`
                                   +total$`EsSalud y Seguro privado de salud`
                                   +total$`EsSalud y Otro seguro`
                                   +total$`EsSalud, Seguro de fuerzas armadas o policiales y Seguro privado de salud`
                                   +total$`EsSalud, Seguro de fuerzas armadas o policiales y Otro seguro`
                                   +total$`EsSalud, Seguro privado de salud y Otro seguro`
                                   +total$`Seguro de fuerzas armadas o policiales y Seguro privado de salud`
                                   +total$`Seguro de fuerzas armadas o policiales y Otro seguro`
                                   +total$`Seguro de fuerzas armadas o policiales, Seguro privado de salud y Otro seguro`
                                   +total$`Seguro privado de salud y Otro seguro`
                                   +total$`Seguro Integral de Salud (SIS), Seguro privado de salud y Otro seguro`)
total$noseguro <- 100-total$seguro
total$leer <- (100/total$Total)*(total$`Si sabe leer y escribir`)
total$noleer <- 100-total$leer
total$educacion <- (100/total$Total)*(total$`Superior no universitaria incompleta`
                                      +total$`Superior no universitaria completa`
                                      +total$`Superior universitaria incompleta`
                                      +total$`Superior universitaria completa`
                                      +total$`Maestría / Doctorado`)
total$noeducacion <- (100/total$Total)*(total$`Sin Nivel`)
total$escuela <- 100-total$educacion
total$fuerzalab  <- (100/total$Total)*(total$`De 15 a más años`)
total$nofuerzalab <- 100-total$fuerzalab
total$remunerado <- (100/total$Total)*(total$`Si, trabajó por algún pago`)
total$noremunerado <- 100-total$remunerado
total$raza <- (100/total$Total)*(total$Blanco+total$Mestizo)
max(total$agua)
total$agua <- ((total$agua)/(114.2857))*100
max(total$luz)
total$luz <- ((total$luz)/(114.2857))*100
max(total$internet)
total$internet <- ((total$internet)/(103.125))*100
max(total$seguro)
total$seguro <- ((total$seguro)/(104.3478))*100
max(total$leer)
total$leer <- ((total$leer)/(103.2258))*100
max(total$fuerzalab)
total$fuerzalab <- ((total$fuerzalab)/(114.2857))*100
max(total$raza)
total$raza <- ((total$raza)/(102.7778))*100
total$blanco <- (100/total$Total)*(total$Blanco)
total$mestizo <- (100/total$Total)*(total$Mestizo)
total$afro <- (100/total$Total)*(total$`Negro, moreno, zambo, mulato / pueblo afroperuano o afrodescendiente`)
total$nativo <- (100/total$Total)*(total$Quechua
                                   +total$Aimara
                                   +total$`Nativo o indígena de la amazonía`
                                   +total$`Parte de otro pueblo indígena u originario`
                                   )

data <- total %>%
  select(c("Manzana" ,"Total", "casa", "nocasa", "agua", "noagua", "luz", "noluz", "internet", "nointernet", "seguro", "noseguro", "leer", "noleer", "educacion", "noeducacion", "escuela", "fuerzalab", "nofuerzalab", "remunerado", "noremunerado", "raza", "blanco", "mestizo", "afro", "nativo"))

data[data<0] <- 0

library(foreign)
write.dta(data, "datos.dta")

library(readxl)
bd <- read_excel("Basefinal.xlsx")

ubicode <- function(x){
  if(nchar(as.vector(x)) == 19){
    x <- sprintf("%s",x)
  }else{
    x <- sprintf("%s0",x)
  }
}

base <- bd %>% mutate(IDMANZANA=sapply(IDMANZANA, ubicode))

library(sf)
library(dplyr)

shp1 = st_read("data-geogps-2017.gpkg") %>% st_set_geometry(NULL)

shape1 <- select(shp1, c("Mz", "INGR_PER", "CV_INGR")) ## seleccionar variables de centros poblados

shape1[is.na(shape1)] <- 0

df <- merge(x = base, y = shape1, 
               by.x = "IDMANZANA", by.y = "Mz")
max(df$INGR_PER)
df$ingreso <- ((df$INGR_PER)/(5124.494))*100
max(df$CV_INGR)
df$consumo <- ((df$CV_INGR)/(66.79871))*100

basepca <- df %>%
  select(c("IDMANZANA", "casa", "agua", "luz", "internet", "seguro", "leer", "educacion", "fuerzalab", "remunerado", "raza", "INGR_PER", "CV_INGR"))

baseraza <- df %>%
  select(c("IDMANZANA", "blanco", "mestizo", "afro", "nativo"))

baseingreso <- df %>%
  select(c("IDMANZANA", "INGR_PER", "CV_INGR"))

#######################################
###### PCA ############################
basepca2 <- SUHI_Table %>%
  select(c("IDMANZANAMANZANA", "casa", "agua", "luz", "internet", "seguro", "leer", "educacion", "fuerzalab", "remunerado", "raza", "Ingresos_mean"))


library(textshape)
library(FactoMineR)
library(tIDMANZANAyverse)
library(factoextra)

pca <- textshape::column_to_rownames(basepca2, loc = 1) #columnas IDMANZANA como nombre
colnames(data) <- c("Permanent adress", "Water service", "Elerctric energy service", 
                    "Internet service", ">15 years and able to work",
                    "PaIDMANZANA in the last 15 days", "Have a health insurance", "White or mestizo race",
                    "Higher education", "Literacy", "Per capita income")

library(stats)
library(FactoInvestigate)

#prcomp() Forma rápIDMANZANAa de implementar PCA sobre una matriz de datos.
respca<-prcomp(pca, scale = T)
respca2 <- PCA(X = df1, scale = F, ncp = 11, graph = F)
respca3 <- PCA(X = df1, scale = T, ncp = 11, graph = F)
respca4 <- PCA(X = pca, scale = T, ncp = 11, graph = T)
names(respca)

head(respca$rotation)[, 1:5] #las coordenadas de los datos en el nuevo sistema rotado de coordenadas. 
#Estas coordenadas se corresponden con los scores de los componentes principales.

dim(respca$rotation) #Número de distintos componentes

head(respca$x)[,1:5] #los vectores de los scores.

respca$sdev #las desviaciones estándares de cada CP.

respca$sdev^2  ## Varianza explicada por cada componente

summary(respca)

respca$rotation

#comprobemos la importancia del componente 1
xx<-respca$x
xx<-as.data.frame(xx)
pca$PC1<-xx$PC1
pca$PC2<-xx$PC2
pca$PC3<-xx$PC3
pca$PC4<-xx$PC4
pca$PC5<-xx$PC5

pca$Index <- pca$PC1+pca$PC2+pca$PC3+pca$PC4+pca$PC5
head(pca)
cor(pca)

index <- tibble::rownames_to_column(pca, "IDMANZANA")
index <- select(index, c("IDMANZANA", "PC1", "PC2", "PC3", "PC4", "PC5", "Index"))

df<- merge(x = index, y = SUHI_Table, 
               by.x = "IDMANZANA", by.y = "IDMANZANAMANZANA")

library(foreign)
write.csv(df, "df.csv")

library(foreign)
write.dta(data, "datos.dta")


fig1 <- fviz_pca_var(respca, col.var="contrib",
             gradient.cols = c("#fde725","#5ec962","#21918c","#3b528b","#440154"),
             repel = TRUE, # AvoIDMANZANA text overlapping
             title="")
fig2 <- fviz_pca_var(respca2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = ""
)
fig3 <- fviz_pca_var(respca3,
                     col.var = "contrib", # Color by contributions to the PC
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE,
                     title = ""
)
fig4 <- fviz_pca_var(respca4,
                     col.var = "contrib", # Color by contributions to the PC
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE,
                     title = ""
)

fviz_pca_var(respca2)
