library(readr)
library(tidyverse)
library(survival)
library(table1)
library(ggplot2)
library(ggridges)
library(ggstatsplot)
theme_bw
library(sf)
library(dplyr)
library(ggpubr)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridisLite)
library(viridis)
library(PMCMRplus)
if(!require("remotes")) install.packages("remotes")
remotes::install_github("healthinnovation/innovar@v.0.1.0")
library(innovar)
library(cowplot)
library(ggplot2)
library(ggridges)
theme_set(theme_minimal())
library(sf)
library(dplyr)
library(ggpubr)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridisLite)
library(viridis)
df <- read_csv("LimaDB-Salurbal.csv") %>% 
  select(c("IDMANZANA", "hombre", "mujer"))

df1 <- read.csv("SUHI_18_11_2022.csv")

df <- merge(df, df1)
## creando macro distritos basados en la jurisdiccion de diris
df$MACRODISTRITO <- "a"
df$MACRODISTRITO[df$DISTRITO %in% c("BELLAVISTA", 
                                    "CALLAO", 
                                    "CARMEN DE LA LEGUA REYNOSO",
                                    "LA PERLA",
                                    "LA PUNTA",
                                    "MI PERU",
                                    "VENTANILLA")] <- "CALLAO"

df$MACRODISTRITO[df$DISTRITO %in% c("ATE", 
                                    "CHACLACAYO", 
                                    "CIENEGUILLA",
                                    "EL AGUSTINO",
                                    "LA MOLINA",
                                    "LURIGANCHO",
                                    "SANTA ANITA")] <- "ESTE"

df$MACRODISTRITO[df$DISTRITO %in% c("SAN JUAN DE LURIGANCHO", 
                                    "SAN BORJA", 
                                    "LA VICTORIA",
                                    "SAN LUIS",
                                    "SURQUILLO",
                                    "LINCE",
                                    "SAN ISIDRO",
                                    "MIRAFLORES",
                                    "MAGDALENA DEL MAR",
                                    "SAN MIGUEL",
                                    "JESUS MARIA",
                                    "BREÑA",
                                    "LIMA",
                                    "PUEBLO LIBRE")] <- "CENTRO"

df$MACRODISTRITO[df$DISTRITO %in% c("ANCON", 
                                    "SANTA ROSA", 
                                    "PUENTE PIEDRA",
                                    "LOS OLIVOS",
                                    "SAN MARTIN DE PORRES",
                                    "RIMAC",
                                    "INDEPENDENCIA",
                                    "COMAS",
                                    "CARABAYLLO")] <- "NORTE"

df$MACRODISTRITO[df$DISTRITO %in% c("VILLA MARIA DEL TRIUNFO",
                                    "SAN JUAN DE MIRAFLORES",
                                    "VILLA EL SALVADOR",
                                    "LURIN",
                                    "PUCUSANA",
                                    "PUNTA HERMOSA",
                                    "PUNTA NEGRA",
                                    "SAN BARTOLO",
                                    "SANTA MARIA DEL MAR",
                                    "PACHACAMAC",
                                    "CHORRILLOS",
                                    "BARRANCO",
                                    "SANTIAGO DE SURCO")] <- "SUR"

### Tablas resumen
df$hombrep <- (100/df$poblacion)*df$hombre
df$mujerp <- (100/df$poblacion)*df$mujer
df$edad_0to14 <- (100/df$poblacion)*(df$edad_0to4 +
                                       df$edad_5to9 +
                                       df$edad_10to14)
df$edad_15to29 <- (100/df$poblacion)*(df$edad_15to19 +
                                       df$edad_20to24 +
                                       df$edad_25to29)
df$edad_30to64 <- (100/df$poblacion)*(df$edad_30to34 +
                                       df$edad_35to39 +
                                       df$edad_40to44 +
                                        df$edad_45to49 +
                                        df$edad_50to54 +
                                        df$edad_55to59 +
                                        df$edad_60to64)
df$edad_65tomas <- (100/df$poblacion)*(df$edad_65to69 +
                                       df$edad_70to74 +
                                       df$edad_75to79 +
                                         df$edad_80to84 +
                                         df$edad_85to89 +
                                         df$edad_90to94 +
                                         df$edad_95tomás)
df$escuela <- 100-(df$educacion+df$noeducacion)

table1(~ casa + agua + luz + internet +
         seguro + leer + educacion + noeducacion +
         escuela + fuerzalab + remunerado +
         blanco + mestizo + afro + nativo +
         Ingresos_mean + LST2017_mean + LST2021_mean +
         SUHI + delta_2021_2017 +
         mujerp + hombrep + poblacion + edad_0to14 +
         edad_15to29 + edad_30to64 + edad_65tomas | MACRODISTRITO , data = df, overall = T,
       render.continuous = c(.="Mean (SD)", .="Median [Q1, Q3]"))

table1(~ casa + agua + luz + internet +
         seguro + leer + educacion + noeducacion +
         escuela + fuerzalab + remunerado +
         blanco + mestizo + afro + nativo +
         Ingresos_mean + LST2017_mean + LST2021_mean +
         SUHI + delta_2021_2017 +
         mujerp + hombrep + poblacion + edad_0to14 +
         edad_15to29 + edad_30to64 + edad_65tomas | DISTRITO , data = df, overall = T,
       render.continuous = c(.="Mean (SD)", .="Median [Q1, Q3]"))


#### Boxviolin plots #####
bv_g1_MHI <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$internet)))))*as.numeric(factor(rank(df$internet)))),5)),
  x = rankeo,
  y = SUHI,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI",
  title = "MHI per rank of Access to internet service per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T, # cambiar la paleta
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g2_MHI <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$seguro)))))*as.numeric(factor(rank(df$seguro)))),5)),
  x = rankeo,
  y = SUHI,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI",
  title = "MHI per rank of Access of health insurance per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0),
  centrality.label.args = list(size  = 4.2),
  ggsignif.args = list(textsize = 4.2, tip_length = 0.01)
) +
  scale_color_innova("jama") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12))

bv_g3_MHI <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$leer)))))*as.numeric(factor(rank(df$leer)))),5)),
  x = rankeo,
  y = SUHI,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI",
  title = "MHI per rank of Literacy population per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g4_MHI <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$educacion)))))*as.numeric(factor(rank(df$educacion)))),5)),
  x = rankeo,
  y = SUHI,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI",
  title = "MHI per rank of People with higher education level per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0),
  centrality.label.args = list(size  = 4.2),
  ggsignif.args = list(textsize = 4.2, tip_length = 0.01)
) +
  scale_color_innova("jama") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12))

bv_g5_MHI <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$fuerzalab)))))*as.numeric(factor(rank(df$fuerzalab)))),5)),
  x = rankeo,
  y = SUHI,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI",
  title = "MHI per rank of People at least 15 years old and able to work per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g6_MHI <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$remunerado)))))*as.numeric(factor(rank(df$remunerado)))),5)),
  x = rankeo,
  y = SUHI,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI",
  title = "MHI per rank of People who recently received income per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g7_MHI <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$blanco)))))*as.numeric(factor(rank(df$blanco)))),5)),
  x = rankeo,
  y = SUHI,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI",
  title = "MHI per rank of White ethnicity per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0),
  centrality.label.args = list(size  = 4.2),
  ggsignif.args = list(textsize = 4.2, tip_length = 0.01)
) +
  scale_color_innova("jama") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12))

bv_g8_MHI <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$mestizo)))))*as.numeric(factor(rank(df$mestizo)))),5)),
  x = rankeo,
  y = SUHI,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI",
  title = "MHI per rank of Mestizo ethnicity per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g9_MHI <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$afro)))))*as.numeric(factor(rank(df$afro)))),3)),
  x = rankeo,
  y = SUHI,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI",
  title = "MHI per rank of Afroamerican ethnicity per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g10_MHI <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$nativo)))))*as.numeric(factor(rank(df$nativo)))),5)),
  x = rankeo,
  y = SUHI,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI",
  title = "MHI per rank of Native ethnicity per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0),
  centrality.label.args = list(size  = 4.2),
  ggsignif.args = list(textsize = 4.2, tip_length = 0.01)
) +
  scale_color_innova("jama") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12))

bv_g11_MHI <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$Ingresos_mean)))))*as.numeric(factor(rank(df$Ingresos_mean)))),5)),
  x = rankeo,
  y = SUHI,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI",
  title = "MHI per rank of Per capita income per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0),
  centrality.label.args = list(size  = 4.2),
  ggsignif.args = list(textsize = 4.2, tip_length = 0.01)
) +
  scale_color_innova("jama") +
  theme(axis.text.x=element_text(size=12)) +
  theme(axis.text.y=element_text(size=12))


ggarrange(bv_g1_MHI, bv_g2_MHI, bv_g3_MHI, 
          bv_g4_MHI, bv_g5_MHI, bv_g6_MHI,
          bv_g7_MHI, bv_g8_MHI, bv_g9_MHI, 
          bv_g10_MHI, bv_g11_MHI,
          labels = "AUTO",
          ncol = 4, nrow = 3)

ggsave("boxviolin_MHI_general.png", last_plot(), width = 30, heigh = 22, bg = "white",dpi = 300)


bv_MHI_panel4 <- ggarrange(bv_g2_MHI, bv_g4_MHI, 
          bv_g7_MHI, bv_g10_MHI,
          labels = "AUTO",
          ncol = 2, nrow = 2)

ggsave("boxviolin_MHI_4panel.png", last_plot(), width = 23, heigh = 19, bg = "white",dpi = 300)

ggsave("boxviolin_MHI_4panel.pdf", plot = bv_MHI_panel4, width = 23, heigh = 19, bg = "white")


## MHI variation (2021-2017)

bv_g1_delta_2021_2017 <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$internet)))))*as.numeric(factor(rank(df$internet)))),5)),
  x = rankeo,
  y = delta_2021_2017,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI variation (2021-2017)",
  title = "MHI variation (2021-2017) per rank of Access to internet service per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g2_delta_2021_2017 <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$seguro)))))*as.numeric(factor(rank(df$seguro)))),5)),
  x = rankeo,
  y = delta_2021_2017,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI variation (2021-2017)",
  title = "MHI variation (2021-2017) per rank of Access of health insurance per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T, 
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g3_delta_2021_2017 <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$leer)))))*as.numeric(factor(rank(df$leer)))),5)),
  x = rankeo,
  y = delta_2021_2017,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI variation (2021-2017)",
  title = "MHI variation (2021-2017) per rank of Literacy population per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g4_delta_2021_2017 <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$educacion)))))*as.numeric(factor(rank(df$educacion)))),5)),
  x = rankeo,
  y = delta_2021_2017,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI variation (2021-2017)",
  title = "MHI variation (2021-2017) per rank of People with higher education level per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g5_delta_2021_2017 <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$fuerzalab)))))*as.numeric(factor(rank(df$fuerzalab)))),5)),
  x = rankeo,
  y = delta_2021_2017,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI variation (2021-2017)",
  title = "MHI variation (2021-2017) per rank of People at least 15 years old and able to work per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g6_delta_2021_2017 <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$remunerado)))))*as.numeric(factor(rank(df$remunerado)))),5)),
  x = rankeo,
  y = delta_2021_2017,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI variation (2021-2017)",
  title = "MHI variation (2021-2017) per rank of People who recently received income per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g7_delta_2021_2017 <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$blanco)))))*as.numeric(factor(rank(df$blanco)))),5)),
  x = rankeo,
  y = delta_2021_2017,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI variation (2021-2017)",
  title = "MHI variation (2021-2017) per rank of White ethnicity per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g8_delta_2021_2017 <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$mestizo)))))*as.numeric(factor(rank(df$mestizo)))),5)),
  x = rankeo,
  y = delta_2021_2017,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI variation (2021-2017)",
  title = "MHI variation (2021-2017) per rank of Mestizo ethnicity per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g9_delta_2021_2017 <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$afro)))))*as.numeric(factor(rank(df$afro)))),3)),
  x = rankeo,
  y = delta_2021_2017,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI variation (2021-2017)",
  title = "MHI variation (2021-2017) per rank of Afroamerican ethnicity per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T, 
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g10_delta_2021_2017 <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$nativo)))))*as.numeric(factor(rank(df$nativo)))),5)),
  x = rankeo,
  y = delta_2021_2017,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI variation (2021-2017)",
  title = "MHI variation (2021-2017) per rank of Native ethnicity per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T,
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

bv_g11_delta_2021_2017 <- ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$Ingresos_mean)))))*as.numeric(factor(rank(df$Ingresos_mean)))),5)),
  x = rankeo,
  y = delta_2021_2017,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "MHI variation (2021-2017)",
  title = "MHI variation (2021-2017) per rank of Per capita income per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T, 
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
) +
  scale_color_innova("jama")

ggarrange(bv_g1_delta_2021_2017, bv_g2_delta_2021_2017, bv_g3_delta_2021_2017, 
          bv_g4_delta_2021_2017, bv_g5_delta_2021_2017, bv_g6_delta_2021_2017,
          bv_g7_delta_2021_2017, bv_g8_delta_2021_2017, bv_g9_delta_2021_2017, 
          bv_g10_delta_2021_2017, bv_g11_delta_2021_2017,
          labels = "AUTO",
          ncol = 4, nrow = 3)

ggsave("boxviolin_delta_2021_2017_general.png", last_plot(), width = 30, heigh = 22, bg = "white",dpi = 300)




ggstatsplot::grouped_ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$nativo)))))*as.numeric(factor(rank(df$nativo)))),5)),
  x = rankeo,
  y = v5gl02_pm2.5_mean,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "PM2.5",
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # elegir el paquete asociado a la paleta de colores.
  palette = "Darjeeling1", # cambiar la paleta
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0),
  grouping.var = MACRODISTRITO
)

###### PCA ======================================
data <- df %>%
  select(c("IDMANZANA", "casa", "agua", "luz", 
           "internet", "fuerzalab", "remunerado",
           "seguro", "raza", "educacion", "leer", "Ingresos_mean"))

library(textshape)

data <- textshape::column_to_rownames(data, loc = 1) #columnas ID como nombre
colnames(data) <- c("Permanent adress", "Water service", "Elerctric energy service", 
                    "Internet service", ">15 years and able to work",
                    "Recently received income", "Have a health insurance", "White or mestizo ethnicity",
                    "Higher education", "Literacy", "Per capita income")
df1<-as.data.frame(data)


library(stats)
#prcomp() Forma rápida de implementar PCA sobre una matriz de datos.
respca<-prcomp(df1, scale = T)

names(respca)

head(respca$rotation)[, 1:11] #las coordenadas de los datos en el nuevo sistema rotado de coordenadas. 
#Estas coordenadas se corresponden con los scores de los componentes principales.

dim(respca$rotation) #Número de distintos componentes

head(respca$x)[,1:11] #los vectores de los scores.

respca$sdev #las desviaciones estándares de cada CP.

respca$sdev^2  ## Varianza explicada por cada componente

summary(respca)

respca$rotation

#comprobemos la importancia del componente 1
xx<-respca$x
xx<-as.data.frame(xx)
df$PC1<-xx$PC1
df$PC2<-xx$PC2
df$PC3<-xx$PC3
df$PC4<-xx$PC4
df$PC5<-xx$PC5
df$Index <- df$PC1+df$PC2+df$PC3+df$PC4+df$PC5
head(data)
cor(data)

##### PCA 2 ===========================


respca1 <- princomp(df1, cor = TRUE)

#respca1 <- princomp(~ Speed + Power,
#                  data = df, na.action = na.exclude, cor = TRUE)

names(respca1)

respca1$sdev

summary(respca1)

library(FactoMineR)
library(factoextra)
library(FactoInvestigate)

#PCA() #PCA con resultados más detallados. Los valores ausentes se reemplazan por la media de cada columna.
#Pueden incluirse variables categóricas suplementarias. Estandariza automáticamente los datos.

respca2 <- PCA(X = df1, scale.unit = T, ncp = 11, graph = T)

fviz_pca_var(respca2,
             col.var = "contrib",
             repel = TRUE,
             title = "",
             alpha.var = 0.6
) +
  scale_color_innova(discrete = F, palette = "jama") +
  theme_bw()

ggsave("screeplot_index_lima.png", plot = last_plot(), width = 5.75, height = 5.75, dpi = 300)

fviz_pca_var(respca2, col.var="contrib", repel = T)

ggsave("contirbution_index_lima.png", plot = last_plot(), width = 5, height = 5, dpi = 300)

# Esportar base con PCA

library(foreign)
write.csv(df, "Lima_sociodemo_index_mhi.csv")

##### MINAM ========================================

ggstatsplot::ggbetweenstats(
  data = df %>% 
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$Ingresos_mean)))))*as.numeric(factor(rank(df$Ingresos_mean)))),5)),
  x = rankeo,
  y = LST_mean,
  plot.type = "boxviolin", # type of plot tambi?n se puede "box" o "violin"
  notch = TRUE, # box plot cuadrado o ovalado (notch)
  mean.ci = TRUE, # Intervalos de confianza para las medias
  type = "np", #"p" (para parametrica), "np" (no parametrica), "r" (robusta), or "bf" (bayes factor)
  effsize.type = "partial_omega",# hay "biased" (equivalente a la d cohen del t test), "partial_eta" (eta-squared para anova) o "unbiased" (equivalente a "g" Hedge's g para t-test, "partial_omega"(omega-squared para anova)
  k = 2, # cu?ntos decimales?,
  pairwise.comparisons = TRUE, # Muestra las comparaciones post hoc
  p.adjust.method = "bonferroni", # m?todo para utilizar las post hoc. esta: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
  pairwise.display = "s", # "s" solo te muestra las que son significativas, "ns" muestra no significativas, "all" pues todas ;)
  outlier.tagging = FALSE, # mostrar outliers
  outlier.coef = 1.5, # coeficiente para considerarlo outlier siguiendo la Tukey's rule
  xlab = "", 
  ylab = "LST",
  title = "LST per rank of Per capita income per block", # T?tulo del plot
  ggtheme = ggthemes::theme_clean(), # cambiar el fondo del gr?fico
  ggstatsplot.layer = T, # turn off `ggstatsplot` theme layer
  package = "wesanderson", # elegir el paquete asociado a la paleta de colores.
  palette = "Darjeeling1", # cambiar la paleta
  messages = FALSE,
  max.overlaps = 100,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6), 
                    alpha= 0.01, size = 3, stroke = 0)
)

df$Estratos <- "A"

df$Estratos[df$Ingresos_mean <= 863.71] <- "Bajo"
  
df$Estratos[df$Ingresos_mean > 863.71 & df$Ingresos_mean <= 1073] <- "Medio Bajo"

df$Estratos[df$Ingresos_mean > 1073 & df$Ingresos_mean <= 1449.71] <- "Medio"

df$Estratos[df$Ingresos_mean > 1449.71 & df$Ingresos_mean <= 2412.44] <- "Medio Alto"

df$Estratos[df$Ingresos_mean > 2412.44] <- "Alto"

df$Estratos <- factor(df$Estratos, c("Bajo", "Medio Bajo", "Medio", "Medio Alto", "Alto"))

df %>% 
  ggplot(aes(x = Estratos, y = LST_mean, color = Estratos)) + 
  geom_boxplot(lwd = 1.1, fatten = 1) + 
  geom_jitter(alpha = 0.01) +
  theme_update(legend.position="none") +
  scale_color_manual(values=c("#ffbe00", "#fb5607", "#ff006e", "#8338ec", "#3a86ff")) +
  labs(title = 'Temperatura por Estrato socioeconómico', x="Estrato socioeconómico", y="Temperatura [cº]")

ggsave("box_estratos_temperatura.png", last_plot(), width = 6, heigh = 6, bg = "white",dpi = 300)

df %>% 
  ggplot(aes(x = Estratos, y = SUHI, color = Estratos)) + 
  geom_boxplot(lwd = 1.1, fatten = 1) + 
  geom_jitter(alpha = 0.01) +
  theme_update(legend.position="none") +
  scale_color_manual(values=c("#ffbe00", "#fb5607", "#ff006e", "#8338ec", "#3a86ff")) +
  labs(title = 'SUHI por Estrato socioeconómico', x="Estrato socioeconómico", y="SUHI")

ggsave("box_suhi_temperatura.png", last_plot(), width = 6, heigh = 6, bg = "white",dpi = 300)


baseraza <- df %>%
  select(c("blanco", "mestizo", "afro", "nativo", "IDMANZANA"))

dataraza <- baseraza %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("blanco", "mestizo", "afro", "nativo")))])

names(dataraza)[names(dataraza) == 'row_max'] <- "etnia"

df <- merge(x = df, y = dataraza, 
                by.x = "IDMANZANA", by.y = "IDMANZANA", )
suhi_final2 <- df %>% 
  mutate(etnia=case_when(etnia=="nativo"~"Nativo",
                         etnia=="mestizo"~"Mestizo",
                         etnia=="blanco"~"Blanco",
                         etnia=="afro"~"Afroamericano")) %>% 
  mutate(etnia=factor(etnia,levels = c("Blanco","Mestizo","Afroamericano","Nativo")))

ggplot(
  suhi_final2, 
  aes(x = `LST_mean`, y = `etnia`, fill = stat(x), alpha = 0.7)
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, alpha = 0.2, ) +
  scale_fill_viridis_c(name = "Temp. [C]", option = "C") +
  labs(title = 'Temperatura por raza mayoritaria por manzana', x = "Temp. [C]", y = "") +
  theme_ridges(font_size = 15, grid = TRUE)

ggsave("ridge_raza_temperatura.png", last_plot(), width = 7, heigh = 5, bg = "white",dpi = 300)

ggplot(
  suhi_final2, 
  aes(x = `SUHI`, y = `etnia`, fill = stat(x), alpha = 0.7)
) +
  geom_density_ridges_gradient(scale = 3, size = 0.3, alpha = 0.2, ) +
  scale_fill_viridis_c(name = "SUHI", option = "C") +
  labs(title = 'SUHI por raza mayoritaria por manzana', x = "SUHI", y = "") +
  theme_ridges(font_size = 15, grid = TRUE)

ggsave("ridge_raza_suhi.png", last_plot(), width = 7, heigh = 5, bg = "white",dpi = 300)

suhi_final2 %>% 
  ggplot(aes(x = etnia, y = LST_mean, color = etnia)) + 
  geom_boxplot(lwd = 1.1, fatten = 1) + 
  geom_jitter(alpha = 0.008) +
  theme_update(legend.position="none") +
  scale_color_manual(values=c("#fb5607", "#ff006e", "#8338ec", "#3a86ff")) +
  labs(title = 'Temperatura por raza mayoritaria por manzana', x="Raza mayoritaria por manzana", y="Temperatura (cº)")

ggsave("box_raza_temperatura.png", last_plot(), width = 6, heigh = 6, bg = "white",dpi = 300)

suhi_final2 %>% 
  ggplot(aes(x = etnia, y = SUHI, color = etnia)) + 
  geom_boxplot(lwd = 1.1, fatten = 1) + 
  geom_jitter(alpha = 0.008) +
  theme_update(legend.position="none") +
  scale_color_manual(values=c("#fb5607", "#ff006e", "#8338ec", "#3a86ff")) +
  labs(title = 'SUHI por raza mayoritaria por manzana', x="Raza mayoritaria por manzana", y="SUHI")

ggsave("box_raza_suhi.png", last_plot(), width = 6, heigh = 6, bg = "white",dpi = 300)

df$Temperatura <- "A"

df$Temperatura[df$LST_mean <= 25] <- "≤ 25"

df$Temperatura[df$LST_mean > 25] <- "> 25"

df$Temperatura <- factor(df$Temperatura, c("> 25", "≤ 25"))

count <- table(df$Estratos, 
               df$Temperatura)
count
mosaicplot((count), sub = "Estrato socioeconómico", ylab = "Temperatura (cº)", 
           col = c ("#dd1c1a", "#06aed5"), main = "Frecuencia de Estrato socioeconómico expuestas a más de 25 Cº"
)

suhi_final2$Temperatura <- "A"

suhi_final2$Temperatura[suhi_final2$LST_mean <= 25] <- "≤ 25"

suhi_final2$Temperatura[suhi_final2$LST_mean > 25] <- "> 25"

suhi_final2$Temperatura <- factor(suhi_final2$Temperatura, c("> 25", "≤ 25"))

count <- table(suhi_final2$etnia, 
               suhi_final2$Temperatura)
count
mosaicplot(count, main = "Countries Mosaic Plot",
           sub = "Product Colors by Country",
           xlab = "Colors",
           ylab = "Countries",
           las = 1,
           border = "black",
           shade = F)

mosaicplot((count), sub = "Raza mayoritaria por manzana", ylab = "Temperatura (cº)", 
             col = c ("#dd1c1a", "#06aed5"), main = "Frecuencia de Raza mayoritaria por manzana expuestas a más de 25 Cº"
             )

ggsave("mosaic_raza_temp.png", plot = m1, width = 6, heigh = 6, bg = "white",dpi = 300)
m1


table1::table1(~ DISTRITO | etnia , data = df, overall = T)

Lima_tesis <- read_csv("Lima_pm25_index.csv")


Lima_tesis$index_cat <- factor(Lima_tesis$index_cat, c("Bajo", "Medio", "Alto"))

Lima_tesis$pm25_it1 <- factor(Lima_tesis$pm25_it1, c("> 35 µg/m3", "≤ 35 µg/m3"))

count <- table(Lima_tesis$index_cat,
               Lima_tesis$pm25_it1 
               )
count
mosaicplot((count), sub = "Estrato Socioeconómico", ylab = "PM2.5 (µg/m3)", 
           col = c ("#06aed5", "#dd1c1a"), main = "Frecuencia de Estrato socioeconómico expuesto a más de 35 µg/m3 de PM2.5"
)

ggsave("mosaic_estrato_pm25.png", last_plot(), width = 6, heigh = 6, bg = "white",dpi = 300)

Lima_tesis %>% 
  ggplot(aes(x = index_cat, y = v5gl02_edited_pm25_mean, color = index_cat)) + 
  geom_boxplot(lwd = 1.1, fatten = 1) + 
  geom_jitter(alpha = 0.01) +
  theme_update(legend.position="none") +
  scale_color_manual(values=c("#ffbe00", "#ff006e", "#8338ec")) +
  labs(title = 'PM2.5 por Estrato Socioeconómico', x="Estrato socioeconómico", y="PM2.5 (µg/m3)")

ggsave("box_estratos_pm25.png", last_plot(), width = 6, heigh = 5, bg = "white",dpi = 300)

Lima_tesis$v5gl02_edited_pm25_mean

########### QGCOMP #########

library(qgcomp)
library(ggplot2)
library(purrr)
library(tidyverse)

Xnm <- c(
  'casa','agua','luz','internet','seguro','leer',
  'educacion','fuerzalab','remunerado','raza', "Ingresos_mean"
)

system.time(qc.fit <- qgcomp.noboot(df$delta_2021_2017~.,dat=df[,c(Xnm, 'delta_2021_2017')], family=gaussian(), bayes = T))
qc.fit

system.time(qc.fit <- qgcomp.noboot(df$SUHI~.,dat=df[,c(Xnm, 'SUHI')], family=gaussian(), bayes = T))
qc.fit



r1 <- df %>% 
  group_by(MACRODISTRITO) %>% 
  nest() %>% 
  mutate(qcfit=map(.x=data,
                   .f=~qgcomp.noboot(delta_2021_2017~.,dat=.x[,c(Xnm, 'delta_2021_2017')], family=gaussian(), bayes = T))
         ) %>% 
  mutate(negw=map(.x=qcfit,
                  .f=~.x$neg.weights)) %>%
  mutate(dataexport=map(.x=negw,
                        .f=~enframe(.x))) %>% 
  select(MACRODISTRITO, dataexport) %>% 
  unnest()

r1$value <- r1$value*-1

r1 %>% 
  ggplot(aes(x=name,
             y=value,
             fill=MACRODISTRITO)) +
  geom_col(position = "dodge")

r2 <- df %>% 
  group_by(MACRODISTRITO) %>% 
  nest() %>% 
  mutate(qcfit=map(.x=data,
                   .f=~qgcomp.noboot(delta_2021_2017~.,dat=.x[,c(Xnm, 'delta_2021_2017')], family=gaussian(), bayes = T))
  ) %>% 
  mutate(posgw=map(.x=qcfit,
                  .f=~.x$pos.weights)) %>%
  mutate(dataexport=map(.x=posgw,
                        .f=~enframe(.x))) %>% 
  select(MACRODISTRITO, dataexport) %>% 
  unnest()

r2 %>% 
  ggplot(aes(x=name,
             y=value,
             fill=MACRODISTRITO)) +
  geom_col(position = "dodge")

r3 <- rbind(r1, r2)

library(ggsci)

r3$sh <- 0

r3$sh[r3$name=="casa"] <- 4
r3$sh[r3$name=="agua"] <- 3 
r3$sh[r3$name=="luz"] <- 3
r3$sh[r3$name=="internet"] <- 4
r3$sh[r3$name=="fuerzalab"] <- 4
r3$sh[r3$name=="remunerado"] <- 2
r3$sh[r3$name=="seguro"] <- 2
r3$sh[r3$name=="raza"] <- 2
r3$sh[r3$name=="educacion"] <- 5
r3$sh[r3$name=="leer"] <- 3
r3$sh[r3$name=="Ingresos_mean"] <- 5
r3$sh <- as.character(r3$sh)

r3$name[r3$name=="casa"] <- "Permanent adress" 
r3$name[r3$name=="agua"] <- "Water service" 
r3$name[r3$name=="luz"] <- "Elerctric energy service" 
r3$name[r3$name=="internet"] <- "Internet service"
r3$name[r3$name=="fuerzalab"] <- ">15 years and able to work"
r3$name[r3$name=="remunerado"] <- "Recently received income" 
r3$name[r3$name=="seguro"] <- "Have a health insurance"
r3$name[r3$name=="raza"] <- "White or mestizo ethnicity"
r3$name[r3$name=="educacion"] <- "Higher education"
r3$name[r3$name=="leer"] <- "Literacy"
r3$name[r3$name=="Ingresos_mean"] <- "Per capita income"

r3$MACRODISTRITO[r3$MACRODISTRITO=="CENTRO"] <- "Center zone"
r3$MACRODISTRITO[r3$MACRODISTRITO=="NORTE"] <- "North zone"
r3$MACRODISTRITO[r3$MACRODISTRITO=="ESTE"] <- "East zone"
r3$MACRODISTRITO[r3$MACRODISTRITO=="SUR"] <- "South zone"
r3$MACRODISTRITO[r3$MACRODISTRITO=="CALLAO"] <- "Callao zone"

ggplot(r3)+
  geom_linerange(aes(x = MACRODISTRITO, ymin = 0, ymax = value, colour = MACRODISTRITO), 
                 position = position_dodge(width = 1),
                 size=0.9,
                 show.legend = T, ylab = "") +
  geom_point(aes(x = MACRODISTRITO, y = value, color = MACRODISTRITO),
             position = position_dodge(width = 1),
             size=3.3,
             show.legend = T, ylab = "") +
  coord_flip() +
  scale_color_innova("jama") +
  scale_fill_innova("jama") +
  theme_bw() +
  theme(axis.text.y=element_blank()) +
  xlab("Variables included in the Quantile g-computation analysis") +
  labs(colour = "Metropolitan Lima zones") +
  ylab("Scaled effect size on MHI variation (2021 - 2017)") +
  geom_hline(yintercept = 0) +
  guides(scale = "none") +
  facet_grid(rows = "name", switch = "y")

ggsave("llolipop_scaled_qgcomp_MHIvariation.png", last_plot(), width = 10, heigh = 18, bg = "white",dpi = 300)

r4 <- df %>% 
  group_by(MACRODISTRITO) %>% 
  nest() %>% 
  mutate(qcfit=map(.x=data,
                   .f=~qgcomp.noboot(SUHI~.,dat=.x[,c(Xnm, 'SUHI')], family=gaussian(), bayes = T))
  ) %>% 
  mutate(negw=map(.x=qcfit,
                  .f=~.x$neg.weights)) %>%
  mutate(dataexport=map(.x=negw,
                        .f=~enframe(.x))) %>% 
  select(MACRODISTRITO, dataexport) %>% 
  unnest()

r4$value <- r4$value*-1

r4 %>% 
  ggplot(aes(x=name,
             y=value,
             fill=MACRODISTRITO)) +
  geom_col(position = "dodge")

r5 <- df %>% 
  group_by(MACRODISTRITO) %>% 
  nest() %>% 
  mutate(qcfit=map(.x=data,
                   .f=~qgcomp.noboot(SUHI~.,dat=.x[,c(Xnm, 'SUHI')], family=gaussian(), bayes = T))
  ) %>% 
  mutate(posgw=map(.x=qcfit,
                   .f=~.x$pos.weights)) %>%
  mutate(dataexport=map(.x=posgw,
                        .f=~enframe(.x))) %>% 
  select(MACRODISTRITO, dataexport) %>% 
  unnest()

r5 %>% 
  ggplot(aes(x=name,
             y=value,
             fill=MACRODISTRITO)) +
  geom_col(position = "dodge")

r6 <- rbind(r4, r5)

library(ggsci)

r6$sh <- 0

r6$sh[r6$name=="casa"] <- 4
r6$sh[r6$name=="agua"] <- 3 
r6$sh[r6$name=="luz"] <- 3
r6$sh[r6$name=="internet"] <- 4
r6$sh[r6$name=="fuerzalab"] <- 4
r6$sh[r6$name=="remunerado"] <- 2
r6$sh[r6$name=="seguro"] <- 2
r6$sh[r6$name=="raza"] <- 2
r6$sh[r6$name=="educacion"] <- 5
r6$sh[r6$name=="leer"] <- 3
r6$sh[r6$name=="Ingresos_mean"] <- 5
r6$sh <- as.character(r6$sh)

r6$name[r6$name=="casa"] <- "Permanent adress" 
r6$name[r6$name=="agua"] <- "Water service" 
r6$name[r6$name=="luz"] <- "Elerctric energy service" 
r6$name[r6$name=="internet"] <- "Internet service"
r6$name[r6$name=="fuerzalab"] <- ">15 years and able to work"
r6$name[r6$name=="remunerado"] <- "Recently received income" 
r6$name[r6$name=="seguro"] <- "Have a health insurance"
r6$name[r6$name=="raza"] <- "White or mestizo ethnicity"
r6$name[r6$name=="educacion"] <- "Higher education"
r6$name[r6$name=="leer"] <- "Literacy"
r6$name[r6$name=="Ingresos_mean"] <- "Per capita income"

r6$MACRODISTRITO[r6$MACRODISTRITO=="CENTRO"] <- "Center zone"
r6$MACRODISTRITO[r6$MACRODISTRITO=="NORTE"] <- "North zone"
r6$MACRODISTRITO[r6$MACRODISTRITO=="ESTE"] <- "East zone"
r6$MACRODISTRITO[r6$MACRODISTRITO=="SUR"] <- "South zone"
r6$MACRODISTRITO[r6$MACRODISTRITO=="CALLAO"] <- "Callao zone"

ggplot(r6)+
  geom_linerange(aes(x = MACRODISTRITO, ymin = 0, ymax = value, colour = MACRODISTRITO), 
                 position = position_dodge(width = 1),
                 size=0.9,
                 show.legend = T, ylab = "") +
  geom_point(aes(x = MACRODISTRITO, y = value, color = MACRODISTRITO),
             position = position_dodge(width = 1),
             size=3.3,
             show.legend = T, ylab = "") +
  coord_flip() +
  scale_color_innova("jama") +
  scale_fill_innova("jama") +
  theme_bw() +
  theme(axis.text.y=element_blank()) +
  xlab("Variables included in the Quantile g-computation analysis") +
  labs(colour = "Metropolitan Lima zones") +
  ylab("Scaled effect size on MHI estimation") +
  geom_hline(yintercept = 0) +
  guides(scale = "none") +
  facet_grid(rows = "name", switch = "y")

ggsave("llolipop_scaled_qgcomp_MUHI.png", last_plot(), width = 10, heigh = 18, bg = "white",dpi = 300)

#### Ridges plots ====================
baseraza <- df %>%
  select(c("blanco", "mestizo", "afro", "nativo", "IDMANZANA"))

dataraza <- baseraza %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("blanco", "mestizo", "afro", "nativo")))])

names(dataraza)[names(dataraza) == 'row_max'] <- "etnia"

baseingreso <- df %>%
  select(c("Ingresos_mean", "IDMANZANA"))

basecasa <- df %>%
  select(c("casa", "nocasa", "IDMANZANA"))

datacasa <- basecasa %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("casa", "nocasa")))])

names(datacasa)[names(datacasa) == 'row_max'] <- "casacuali"

baseagua <- df %>%
  select(c("agua", "noagua", "IDMANZANA"))

dataagua <- baseagua %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("agua", "noagua")))])

names(dataagua)[names(dataagua) == 'row_max'] <- "aguacuali"

baseluz <- df %>%
  select(c("luz", "noluz", "IDMANZANA"))

dataluz <- baseluz %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("luz", "noluz")))])

names(dataluz)[names(dataluz) == 'row_max'] <- "luzcuali"

baseinternet <- df %>%
  select(c("internet", "nointernet", "IDMANZANA"))

datainternet <- baseinternet %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("internet", "nointernet")))])

names(datainternet)[names(datainternet) == 'row_max'] <- "internetcuali"

baseseguro <- df %>%
  select(c("seguro", "noseguro", "IDMANZANA"))

dataseguro <- baseseguro %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("seguro", "noseguro")))])

names(dataseguro)[names(dataseguro) == 'row_max'] <- "segurocuali"

baseleer <- df %>%
  select(c("leer", "noleer", "IDMANZANA"))

dataleer <- baseleer %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("leer", "noleer")))])

names(dataleer)[names(dataleer) == 'row_max'] <- "leercuali"

baseeducacion <- df %>%
  select(c("educacion", "noeducacion", "IDMANZANA"))

dataeducacion <- baseeducacion %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("educacion", "noeducacion")))])

names(dataeducacion)[names(dataeducacion) == 'row_max'] <- "educacioncuali"

basefuerzalab <- df %>%
  select(c("fuerzalab", "nofuerzalab", "IDMANZANA"))

datafuerzalab <- basefuerzalab %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("fuerzalab", "nofuerzalab")))])

names(datafuerzalab)[names(datafuerzalab) == 'row_max'] <- "fuerzalabcuali"

baseremunerado <- df %>%
  select(c("remunerado", "noremunerado", "IDMANZANA"))

dataremunerado <- baseremunerado %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("remunerado", "noremunerado")))])

names(dataremunerado)[names(dataremunerado) == 'row_max'] <- "remuneradocuali"

basecasa <- df %>%
  select(c("casa", "nocasa", "IDMANZANA"))

datacasa <- basecasa %>% 
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("casa", "nocasa")))])

names(datacasa)[names(datacasa) == 'row_max'] <- "casacuali"

basef2 <- merge(x = dataraza, y = baseingreso, 
                by.x = "IDMANZANA", by.y = "IDMANZANA", )

basef3 <- merge(x = basef2, y = dataagua, 
                by.x = "IDMANZANA", by.y = "IDMANZANA", )

basef4 <- merge(x = basef3, y = datacasa, 
                by.x = "IDMANZANA", by.y = "IDMANZANA", )

basef5 <- merge(x = basef4, y = dataeducacion, 
                by.x = "IDMANZANA", by.y = "IDMANZANA", )

basef6 <- merge(x = basef5, y = datafuerzalab, 
                by.x = "IDMANZANA", by.y = "IDMANZANA", )

basef7 <- merge(x = basef6, y = datainternet, 
                by.x = "IDMANZANA", by.y = "IDMANZANA", )

basef8 <- merge(x = basef7, y = dataleer, 
                by.x = "IDMANZANA", by.y = "IDMANZANA", )

basef9 <- merge(x = basef8, y = dataluz, 
                by.x = "IDMANZANA", by.y = "IDMANZANA", )

basef10 <- merge(x = basef9, y = dataremunerado, 
                 by.x = "IDMANZANA", by.y = "IDMANZANA", )

basef11 <- merge(x = basef10, y = dataseguro, 
                 by.x = "IDMANZANA", by.y = "IDMANZANA", )

DATA <- basef11 %>% 
  select(c("IDMANZANA",
           "etnia", 
           "aguacuali",
           "casacuali",
           "educacioncuali",
           "fuerzalabcuali",
           "internetcuali",
           "leercuali",
           "luzcuali",
           "remuneradocuali",
           "segurocuali"))

df <- merge(x=DATA, y=df,
            by.x="IDMANZANA", by.y="IDMANZANA")


df2 <- df %>% 
  mutate(etnia=case_when(etnia=="nativo"~"Native",
                         etnia=="mestizo"~"Mestizo",
                         etnia=="blanco"~"White",
                         etnia=="afro"~"Afro-american")) %>% 
  mutate(etnia=factor(etnia,levels = c("White","Mestizo","Afro-american","Native")))

e1 <- ggplot(
  df2, 
  aes(x = `SUHI`, y = `etnia`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "MHI") +
  labs(title = 'A. MHI per ethnicity', x = "MHI", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama")

df3 <- df %>% 
  mutate(aguacuali=case_when(aguacuali=="agua"~"Have water service",
                             aguacuali=="noagua"~"No water service")) %>% 
  mutate(aguacuali=factor(aguacuali,levels = c("Have water service","No water service")))

e2 <- ggplot(
  df3, 
  aes(x = `SUHI`, y = `aguacuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "MHI") +
  labs(title = 'C. MHI per water service', x = "MHI", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama")

df4 <- df %>% 
  mutate(casacuali=case_when(casacuali=="casa"~"Have permanent home",
                             casacuali=="nocasa"~"No permanent home")) %>% 
  mutate(casacuali=factor(casacuali,levels = c("Have permanent home","No permanent home")))

e3 <- ggplot(
  df4, 
  aes(x = `SUHI`, y = `casacuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "MHI") +
  theme_ridges(font_size = 15, grid = TRUE) +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'B. MHI per permanent home', x = "MHI", y = "") +
  theme(plot.title.position = "plot")

df5 <- df %>% 
  mutate(educacioncuali=case_when(educacioncuali=="educacion"~"Have higher education",
                                  educacioncuali=="noeducacion"~"No higher education")) %>% 
  mutate(educacioncuali=factor(educacioncuali,levels = c("Have higher education","No higher education")))

e4 <- ggplot(
  df5, 
  aes(x = `SUHI`, y = `educacioncuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "MHI") +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'D. MHI per educaction status', x = "MHI", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot")

df6 <- df %>% 
  mutate(fuerzalabcuali=case_when(fuerzalabcuali=="fuerzalab"~">15 years and able to work",
                                  fuerzalabcuali=="nofuerzalab"~"<15 years or not able to work")) %>% 
  mutate(fuerzalabcuali=factor(fuerzalabcuali,levels = c(">15 years and able to work","<15 years or not able to work")))

e5 <- ggplot(
  df6, 
  aes(x = `SUHI`, y = `fuerzalabcuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "MHI") +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'E. MHI per able to work status', x = "MHI", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot")

df7 <- df %>% 
  mutate(internetcuali=case_when(internetcuali=="internet"~"Have internet service",
                                 internetcuali=="nointernet"~"No internet service")) %>% 
  mutate(internetcuali=factor(internetcuali,levels = c("Have internet service","No internet service")))

e6 <- ggplot(
  df7, 
  aes(x = `SUHI`, y = `internetcuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "MHI") +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'F. MHI per internet service', x = "MHI", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot")

df8 <- df %>% 
  mutate(leercuali=case_when(leercuali=="leer"~"Literacy",
                             leercuali=="noleer"~"Illiterate")) %>% 
  mutate(leercuali=factor(leercuali,levels = c("Literacy","Illiterate")))

e7 <- ggplot(
  df8, 
  aes(x = `SUHI`, y = `leercuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "MHI") +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'G. MHI per literacy status', x = "MHI", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot")

df9 <- df %>% 
  mutate(luzcuali=case_when(luzcuali=="luz"~"Have electric energy service",
                            luzcuali=="noluz"~"No electric energy service")) %>% 
  mutate(luzcuali=factor(luzcuali,levels = c("Have electric energy service","No electric energy service")))

e8 <- ggplot(
  df9, 
  aes(x = `SUHI`, y = `luzcuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "MHI") +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'H. MHI per electric energy service', x = "MHI", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot")

df10 <- df %>% 
  mutate(remuneradocuali=case_when(remuneradocuali=="remunerado"~"Recently received income",
                                   remuneradocuali=="noremunerado"~"Have not recently received income")) %>% 
  mutate(remuneradocuali=factor(remuneradocuali,levels = c("Recently received income","Have not recently received income")))

e9 <- ggplot(
  df10, 
  aes(x = `SUHI`, y = `remuneradocuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "MHI") +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'I. MHI per recent income status', x = "MHI", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot")

df11 <- df %>% 
  mutate(segurocuali=case_when(segurocuali=="seguro"~"Have health insurance",
                               segurocuali=="noseguro"~"No health insurance")) %>% 
  mutate(segurocuali=factor(segurocuali,levels = c("Have health insurance","No health insurance")))

e10 <- ggplot(
  df11, 
  aes(x = `SUHI`, y = `segurocuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "MHI") +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'D. MHI per health insurance status', x = "MHI", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot")

ggarrange(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10,
          ncol = 2, nrow = 5)

ggsave("ridges_SUHI.png", last_plot(), width = 12, heigh = 13, bg = "white",dpi = 300)

ggarrange(e1, e3, e2, e10,
          ncol = 2, nrow = 2)

ggsave("ridges_SUHI_4panels.png", last_plot(), width = 12, heigh = 9, bg = "white",dpi = 300)

ggsave("ridges_SUHI_4panels.pdf", last_plot(), width = 12, heigh = 9, bg = "white")

library("gmodels")
library("car")
library("DescTools")
library("ggplot2")
library("dplyr")

kruskal.test(delta_2021_2017 ~ etnia, data=df)
wilcox.test(delta_2021_2017 ~ aguacuali, data=df)
wilcox.test(delta_2021_2017 ~ casacuali, data=df)
wilcox.test(delta_2021_2017 ~ educacioncuali, data=df)
wilcox.test(delta_2021_2017 ~ fuerzalabcuali, data=df)
wilcox.test(delta_2021_2017 ~ internetcuali, data=df)
wilcox.test(delta_2021_2017 ~ leercuali, data=df)
wilcox.test(delta_2021_2017 ~ luzcuali, data=df)
wilcox.test(delta_2021_2017 ~ remuneradocuali, data=df)
wilcox.test(delta_2021_2017 ~ segurocuali, data=df)

kruskal.test(SUHI ~ etnia, data=df)
wilcox.test(SUHI ~ aguacuali, data=df)
wilcox.test(SUHI ~ casacuali, data=df)
wilcox.test(SUHI ~ educacioncuali, data=df)
wilcox.test(SUHI ~ fuerzalabcuali, data=df)
wilcox.test(SUHI ~ internetcuali, data=df)
wilcox.test(SUHI ~ leercuali, data=df)
wilcox.test(SUHI ~ luzcuali, data=df)
wilcox.test(SUHI ~ remuneradocuali, data=df)
wilcox.test(SUHI ~ segurocuali, data=df)

e1 <- ggplot(
  df2, 
  aes(x = `delta_2021_2017`, y = `etnia`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "Δ MHI") +
  labs(title = 'A. MHI variation (2021-2017) per ethnicity', x = "MHI variation (2021-2017)", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama")

df3 <- df %>% 
  mutate(aguacuali=case_when(aguacuali=="agua"~"Have water service",
                             aguacuali=="noagua"~"No water service")) %>% 
  mutate(aguacuali=factor(aguacuali,levels = c("Have water service","No water service")))

e2 <- ggplot(
  df3, 
  aes(x = `delta_2021_2017`, y = `aguacuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "Δ MHI") +
  labs(title = 'C. MHI variation (2021-2017) per water service', x = "MHI variation (2021-2017)", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama")

df4 <- df %>% 
  mutate(casacuali=case_when(casacuali=="casa"~"Have permanent home",
                             casacuali=="nocasa"~"No permanent home")) %>% 
  mutate(casacuali=factor(casacuali,levels = c("Have permanent home","No permanent home")))

e3 <- ggplot(
  df4, 
  aes(x = `delta_2021_2017`, y = `casacuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "Δ MHI") +
  theme_ridges(font_size = 15, grid = TRUE) +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'B. MHI variation (2021-2017) per permanent home', x = "MHI variation (2021-2017)", y = "") +
  theme(plot.title.position = "plot")

df5 <- df %>% 
  mutate(educacioncuali=case_when(educacioncuali=="educacion"~"Have higher education",
                                  educacioncuali=="noeducacion"~"No higher education")) %>% 
  mutate(educacioncuali=factor(educacioncuali,levels = c("Have higher education","No higher education")))

e4 <- ggplot(
  df5, 
  aes(x = `delta_2021_2017`, y = `educacioncuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "Δ MHI") +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'D. MHI variation (2021-2017) per educaction status', x = "MHI variation (2021-2017)", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot")

df6 <- df %>% 
  mutate(fuerzalabcuali=case_when(fuerzalabcuali=="fuerzalab"~">15 years and able to work",
                                  fuerzalabcuali=="nofuerzalab"~"<15 years or not able to work")) %>% 
  mutate(fuerzalabcuali=factor(fuerzalabcuali,levels = c(">15 years and able to work","<15 years or not able to work")))

e5 <- ggplot(
  df6, 
  aes(x = `delta_2021_2017`, y = `fuerzalabcuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "Δ MHI") +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'E. MHI variation (2021-2017) per able to work status', x = "MHI variation (2021-2017)", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot")

df7 <- df %>% 
  mutate(internetcuali=case_when(internetcuali=="internet"~"Have internet service",
                                 internetcuali=="nointernet"~"No internet service")) %>% 
  mutate(internetcuali=factor(internetcuali,levels = c("Have internet service","No internet service")))

e6 <- ggplot(
  df7, 
  aes(x = `delta_2021_2017`, y = `internetcuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "Δ MHI") +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'F. MHI variation (2021-2017) per internet service', x = "MHI variation (2021-2017)", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot")

df8 <- df %>% 
  mutate(leercuali=case_when(leercuali=="leer"~"Literacy",
                             leercuali=="noleer"~"Illiterate")) %>% 
  mutate(leercuali=factor(leercuali,levels = c("Literacy","Illiterate")))

e7 <- ggplot(
  df8, 
  aes(x = `delta_2021_2017`, y = `leercuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "Δ MHI") +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'G. MHI variation (2021-2017) per literacy status', x = "MHI variation (2021-2017)", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot")

df9 <- df %>% 
  mutate(luzcuali=case_when(luzcuali=="luz"~"Have electric energy service",
                            luzcuali=="noluz"~"No electric energy service")) %>% 
  mutate(luzcuali=factor(luzcuali,levels = c("Have electric energy service","No electric energy service")))

e8 <- ggplot(
  df9, 
  aes(x = `delta_2021_2017`, y = `luzcuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "Δ MHI") +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'H. MHI variation (2021-2017) per electric energy service', x = "MHI variation (2021-2017)", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot")

df10 <- df %>% 
  mutate(remuneradocuali=case_when(remuneradocuali=="remunerado"~"Recently received income",
                                   remuneradocuali=="noremunerado"~"Have not recently received income")) %>% 
  mutate(remuneradocuali=factor(remuneradocuali,levels = c("Recently received income","Have not recently received income")))

e9 <- ggplot(
  df10, 
  aes(x = `delta_2021_2017`, y = `remuneradocuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "Δ MHI") +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'I. MHI variation (2021-2017) per recent income status', x = "MHI variation (2021-2017)", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot")

df11 <- df %>% 
  mutate(segurocuali=case_when(segurocuali=="seguro"~"Have health insurance",
                               segurocuali=="noseguro"~"No health insurance")) %>% 
  mutate(segurocuali=factor(segurocuali,levels = c("Have health insurance","No health insurance")))

e10 <- ggplot(
  df11, 
  aes(x = `delta_2021_2017`, y = `segurocuali`, fill = stat(x))
) +
  geom_density_ridges_gradient(scale = 2, size = 0.3) +
  labs(fill = "Δ MHI") +
  theme(plot.title.position = "plot") +
  scale_fill_innova(discrete = F, palette = "jama") +
  labs(title = 'D. MHI variation (2021-2017) per health insurance status', x = "MHI variation (2021-2017)", y = "") +
  theme_ridges(font_size = 15, grid = TRUE) +
  theme(plot.title.position = "plot")

ggarrange(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10,
          ncol = 2, nrow = 5)

ggsave("ridges_MHIvariation.png", last_plot(), width = 12, heigh = 13, bg = "white",dpi = 300)

ggarrange(e1, e3, e2, e10,
          ncol = 2, nrow = 2)

ggsave("ridges_MHIvariation_4panels.png", last_plot(), width = 12, heigh = 9, bg = "white",dpi = 300)

library("gmodels")
library("car")
library("DescTools")
library("ggplot2")
library("dplyr")

kruskal.test(delta_2021_2017 ~ etnia, data=df)
wilcox.test(delta_2021_2017 ~ aguacuali, data=df)
wilcox.test(delta_2021_2017 ~ casacuali, data=df)
wilcox.test(delta_2021_2017 ~ educacioncuali, data=df)
wilcox.test(delta_2021_2017 ~ fuerzalabcuali, data=df)
wilcox.test(delta_2021_2017 ~ internetcuali, data=df)
wilcox.test(delta_2021_2017 ~ leercuali, data=df)
wilcox.test(delta_2021_2017 ~ luzcuali, data=df)
wilcox.test(delta_2021_2017 ~ remuneradocuali, data=df)
wilcox.test(delta_2021_2017 ~ segurocuali, data=df)

kruskal.test(SUHI ~ etnia, data=df)
wilcox.test(SUHI ~ aguacuali, data=df)
wilcox.test(SUHI ~ casacuali, data=df)
wilcox.test(SUHI ~ educacioncuali, data=df)
wilcox.test(SUHI ~ fuerzalabcuali, data=df)
wilcox.test(SUHI ~ internetcuali, data=df)
wilcox.test(SUHI ~ leercuali, data=df)
wilcox.test(SUHI ~ luzcuali, data=df)
wilcox.test(SUHI ~ remuneradocuali, data=df)
wilcox.test(SUHI ~ segurocuali, data=df)

library(latticeExtra)
require(vioplot)
bv1_mhi <- ggplot(data = df %>% 
                   mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$seguro)))))*as.numeric(factor(rank(df$seguro)))),5))
                 , aes(x=rankeo, y=SUHI, fill = rankeo)) + 
  geom_violin(trim=FALSE, show.legend = F)+
  labs(title="MHI estimation per rank of access of health insurance per block",x="", y = "MHI")+
  geom_boxplot(width=0.1, outlier.alpha = 0, fill = "white") +
  theme_bw() +
  scale_fill_innova("jama") 

bv2_mhi <- ggplot(data = df %>% 
                    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$educacion)))))*as.numeric(factor(rank(df$educacion)))),5))
                  , aes(x=rankeo, y=SUHI, fill = rankeo)) + 
  geom_violin(trim=FALSE, show.legend = F)+
  labs(title="MHI estimation per rank of higher education level per block",x="", y = "MHI")+
  geom_boxplot(width=0.1, outlier.alpha = 0, fill = "white") +
  theme_bw() +
  scale_fill_innova("jama")

bv3_mhi <- ggplot(data = df %>% 
                    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$blanco)))))*as.numeric(factor(rank(df$blanco)))),5))
                  , aes(x=rankeo, y=SUHI, fill = rankeo)) + 
  geom_violin(trim=FALSE, show.legend = F)+
  labs(title="MHI estimation rank of white ethnicity per block",x="", y = "MHI")+
  geom_boxplot(width=0.1, outlier.alpha = 0, fill = "white") +
  theme_bw() +
  scale_fill_innova("jama")

bv4_mhi <- ggplot(data = df %>% 
                    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$nativo)))))*as.numeric(factor(rank(df$nativo)))),5))
                  , aes(x=rankeo, y=SUHI, fill = rankeo)) + 
  geom_violin(trim=FALSE, show.legend = F)+
  labs(title="MHI estimation rank of white ethnicity per block",x="", y = "MHI")+
  geom_boxplot(width=0.1, outlier.alpha = 0, fill = "white") +
  theme_bw() +
  scale_fill_innova("jama")

ggarrange(bv1_mhi,
          bv2_mhi,
          bv3_mhi,
          bv4_mhi,
          labels = "AUTO",
          ncol = 2, nrow = 2)

ggsave("boxviolin_mhi_4panel.pdf", last_plot(), width = 12, heigh = 9, bg = "white")

ggplot(data = df %>% 
         mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$nativo)))))*as.numeric(factor(rank(df$nativo)))),5))
       , aes(x=rankeo, y=SUHI, fill = rankeo)) + 
  geom_violin(trim=FALSE, show.legend = F)+
  labs(title="MHI estimation rank of white ethnicity per block",x="", y = "MHI")+
  geom_boxplot(width=0.1, outlier.alpha = 0, fill = "white") +
  geom_line(colour = "red") +
  theme_bw() +
  scale_fill_innova("jama")
