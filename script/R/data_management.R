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
sanisidro <- read_excel("Data/San isidro.xlsx")
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
library(tidyverse)

total <- bind_rows(ate, ancon, barranco, brena, callao, carabayllo, 
                    chaclacayo, chorrillos, cieneguilla, comas, elagustino, 
                    independencia, jesusmaria, lamolina, lavictoria, lima, 
                    lince, losolivos, lurigancho, lurin, magdalena, miraflores, 
                    pachacamac, pucusana, pueblolibre, puentepiedra, puntahermosa, 
                    puntanegra, rimac, sanbartolo, sanborja, sanisidro, 
                    sanjuanlurigancho, sanjuanmiraflores, sanluis, 
                    sanmartinporres, sanmiguel, santaanita, santamariamar, 
                    santarosa, santiagosurco, surquillo, villamariatriunfo, 
                    villasalvador, .id = NULL)

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
total$escuela <- total$educacion
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

base <- bd %>% mutate(ID=sapply(ID, ubicode))

library(sf)
library(dplyr)

shp1 = st_read("data-geogps-2017.gpkg") %>% st_set_geometry(NULL)

shape1 <- select(shp1, c("Mz", "INGR_PER", "CV_INGR")) ## seleccionar variables de centros poblados

shape1[is.na(shape1)] <- 0

base1 <- merge(x = base, y = shape1, 
               by.x = "ID", by.y = "Mz", )
max(base1$INGR_PER)
base1$ingreso <- ((base1$INGR_PER)/(5124.494))*100
max(base1$CV_INGR)
base1$consumo <- ((base1$CV_INGR)/(66.79871))*100

basepca <- base1 %>%
  select(c("ID", "casa", "agua", "luz", "internet", "seguro", "leer", "educacion", "fuerzalab", "remunerado", "raza", "ingreso", "consumo"))

baseraza <- base1 %>%
  select(c("ID", "blanco", "mestizo", "afro", "nativo"))

baseingreso <- base1 %>%
  select(c("ID", "INGR_PER", "CV_INGR"))

#######################################
###### PCA ############################

library(textshape)

pca <- textshape::column_to_rownames(basepca, loc = 1) #columnas ID como nombre

library(stats)
#prcomp() Forma rápida de implementar PCA sobre una matriz de datos.
respca<-prcomp(pca, scale = F)

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
pca$Index <- pca$PC1+pca$PC2+pca$PC3+pca$PC4
head(pca)
cor(pca)

############################################
###########dicotomizar######################
baseraza <- base1 %>%
  select(c("blanco", "mestizo", "afro", "nativo", "ID"))

dataraza <- baseraza %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("blanco", "mestizo", "afro", "nativo")))])

names(dataraza)[names(dataraza) == 'row_max'] <- "etnia"

baseingreso <- base1 %>%
  select(c("INGR_PER", "CV_INGR", "ID"))

basecasa <- base1 %>%
  select(c("casa", "nocasa", "ID"))

datacasa <- basecasa %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("casa", "nocasa")))])

names(datacasa)[names(datacasa) == 'row_max'] <- "casacuali"

baseagua <- base1 %>%
  select(c("agua", "noagua", "ID"))

dataagua <- baseagua %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("agua", "noagua")))])

names(dataagua)[names(dataagua) == 'row_max'] <- "aguacuali"

baseluz <- base1 %>%
  select(c("luz", "noluz", "ID"))

dataluz <- baseluz %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("luz", "noluz")))])

names(dataluz)[names(dataluz) == 'row_max'] <- "luzcuali"

baseinternet <- base1 %>%
  select(c("internet", "nointernet", "ID"))

datainternet <- baseinternet %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("internet", "nointernet")))])

names(datainternet)[names(datainternet) == 'row_max'] <- "internetcuali"

baseseguro <- base1 %>%
  select(c("seguro", "noseguro", "ID"))

dataseguro <- baseseguro %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("seguro", "noseguro")))])

names(dataseguro)[names(dataseguro) == 'row_max'] <- "segurocuali"

baseleer <- base1 %>%
  select(c("leer", "noleer", "ID"))

dataleer <- baseleer %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("leer", "noleer")))])

names(dataleer)[names(dataleer) == 'row_max'] <- "leercuali"

baseeducacion <- base1 %>%
  select(c("educacion", "noeducacion", "escuela", "ID"))

dataeducacion <- baseeducacion %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("educacion", "noeducacion", "escuela")))])

names(dataeducacion)[names(dataeducacion) == 'row_max'] <- "educacioncuali"

basefuerzalab <- base1 %>%
  select(c("fuerzalab", "nofuerzalab", "ID"))

datafuerzalab <- basefuerzalab %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("fuerzalab", "nofuerzalab")))])

names(datafuerzalab)[names(datafuerzalab) == 'row_max'] <- "fuerzalabcuali"

baseremunerado <- base1 %>%
  select(c("remunerado", "noremunerado", "ID"))

dataremunerado <- baseremunerado %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("remunerado", "noremunerado")))])

names(dataremunerado)[names(dataremunerado) == 'row_max'] <- "remuneradocuali"

basecasa <- base1 %>%
  select(c("casa", "nocasa", "ID"))

datacasa <- basecasa %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("casa", "nocasa")))])

names(datacasa)[names(datacasa) == 'row_max'] <- "casacuali"

basef1 <- merge(x = datapca, y = dataraza, 
                by.x = "ID", by.y = "ID", )

basef2 <- merge(x = basef1, y = baseingreso, 
                by.x = "ID", by.y = "ID", )

basef3 <- merge(x = basef2, y = dataagua, 
                by.x = "ID", by.y = "ID", )

basef4 <- merge(x = basef3, y = datacasa, 
                by.x = "ID", by.y = "ID", )

basef5 <- merge(x = basef4, y = dataeducacion, 
                by.x = "ID", by.y = "ID", )

basef6 <- merge(x = basef5, y = datafuerzalab, 
                by.x = "ID", by.y = "ID", )

basef7 <- merge(x = basef6, y = datainternet, 
                by.x = "ID", by.y = "ID", )

basef8 <- merge(x = basef7, y = dataleer, 
                by.x = "ID", by.y = "ID", )

basef9 <- merge(x = basef8, y = dataluz, 
                by.x = "ID", by.y = "ID", )

basef10 <- merge(x = basef9, y = dataremunerado, 
                by.x = "ID", by.y = "ID", )

basef11 <- merge(x = basef10, y = dataseguro, 
                by.x = "ID", by.y = "ID", )

DATA <- basef11

#####unir bases#####

library(tibble)
datapca <- tibble::rownames_to_column(pca, "ID")

basef1 <- merge(x = datapca, y = dataraza, 
               by.x = "ID", by.y = "ID", )

basefinal <- merge(x = basef1, y = baseingreso, 
                by.x = "ID", by.y = "ID", )

library(foreign)
write.dta(DATA, "Datacuali.dta")

p1<-ggstatsplot::ggbetweenstats(
  data = final,
  x = etnia,
  y = suhi,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = TRUE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "SUHI",
  title = "SUHI per ethnicity", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = FALSE, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # elegir el paquete asociado a la paleta de colores.
  palette = "Darjeeling1", # cambiar la paleta
  messages = FALSE,
  max.overlaps = 50
)
p1

ggsave(p1,"grafico1.png", height = 5, width = 10)

