library(terra)
library(exactextractr)
library(sf)
library(tidyverse)
library(qgcomp)
library(ggridges)
library(ggpubr)
library(innovar)
library(biscale)
library(cowplot)
source("script/R/utils.R")

# Figures 3, 4, 5 and 6 of the article redrawn with the block SUHI of arms (b) and (c) of
# script/R/suhi_mean_supplement.R (same zonal rule). Run from the repository root.
# Code of script/R/box_ridges_qgcomp.R (Figures 3, 4 and 6) and script/R/map_biscale.R
# (Figure 5) with the SUHI column replaced; groupings, model and labels as published.
# innovar: remotes::install_github("healthinnovation/innovar")
# Outputs: ouput/png/mean_supplement/Figure<3 to 6>_<arm>.png (600 dpi),
# ouput/csv/suhi_mean_figure_tests.csv (p-values printed in Figures 3 and 4),
# ouput/csv/suhi_mean_figure5_counts.csv (bars of Figure 5).

pdf(NULL)
dir.create("ouput/png/mean_supplement", showWarnings = FALSE)

blocks <- st_read(
  "raw_data/gpkg/blocks_lima.gpkg"
  ) %>%
  st_transform(32718)

socio <- read_csv("ouput/csv/Lima_sociodemo_index_mhi.csv",
                  col_types = cols(IDMANZANA = "c"), guess_max = 1e5)

blocks <- blocks %>%
  select(IDMANZANA, casa:Ingresos_mean) %>%
  inner_join(socio %>% select(IDMANZANA, MACRODISTRITO), by = "IDMANZANA") %>%
  mutate(id = row_number())
stopifnot(nrow(blocks) == 69618)

rasters <- c(
  b_c2_median = "raw_data/tiff/LST_2017_2021_C2_median.tif",
  c_c2_mean = "raw_data/tiff/LST_2017_2021_C2_mean.tif"
)
r <- rast(rasters)
names(r) <- names(rasters)

centre <- rasterize(vect(blocks), r, field = "id", touches = FALSE)
centre_mean <- zonal(r, centre, fun = "mean", na.rm = TRUE)
centre_n <- zonal(!is.na(r), centre, fun = "sum", na.rm = TRUE)
cover_mean <- exact_extract(r, blocks, "mean", progress = FALSE)

df0 <- blocks %>% st_set_geometry(NULL)

#### Ridges plots (Figure 3) groups ====================
baseraza <- df0 %>%
  select(c("blanco", "mestizo", "afro", "nativo", "IDMANZANA"))

dataraza <- baseraza %>%
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("blanco", "mestizo", "afro", "nativo")))])

names(dataraza)[names(dataraza) == 'row_max'] <- "etnia"

basecasa <- df0 %>%
  select(c("casa", "nocasa", "IDMANZANA"))

datacasa <- basecasa %>%
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("casa", "nocasa")))])

names(datacasa)[names(datacasa) == 'row_max'] <- "casacuali"

baseagua <- df0 %>%
  select(c("agua", "noagua", "IDMANZANA"))

dataagua <- baseagua %>%
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("agua", "noagua")))])

names(dataagua)[names(dataagua) == 'row_max'] <- "aguacuali"

baseseguro <- df0 %>%
  select(c("seguro", "noseguro", "IDMANZANA"))

dataseguro <- baseseguro %>%
  rowwise() %>%
  mutate(row_max = names(.)[which.max(c_across(cols = c("seguro", "noseguro")))])

names(dataseguro)[names(dataseguro) == 'row_max'] <- "segurocuali"

basef2 <- merge(x = dataraza, y = dataagua,
                by.x = "IDMANZANA", by.y = "IDMANZANA", )

basef3 <- merge(x = basef2, y = datacasa,
                by.x = "IDMANZANA", by.y = "IDMANZANA", )

basef4 <- merge(x = basef3, y = dataseguro,
                by.x = "IDMANZANA", by.y = "IDMANZANA", )

DATA <- basef4 %>%
  select(c("IDMANZANA",
           "etnia",
           "aguacuali",
           "casacuali",
           "segurocuali"))

#### Map inputs (Figure 5) ====================
dep <- st_read(
  "raw_data/gpkg/Lima_provincia.gpkg"
  ) %>%
  st_transform(crs = 4326) %>%
  group_by(FUENTE) %>%
  summarise()

spatial <- st_read("ouput/gpkg/mhi_29_11_2022.gpkg") %>%
  st_transform(4326)

Xnm <- c(
  'casa','agua','luz','internet','seguro','leer',
  'educacion','fuerzalab','remunerado','raza', "Ingresos_mean"
)

tests <- list()
counts <- list()
for (a in names(rasters)) {
  n_centre <- centre_n[[a]][match(df0$id, centre_n$id)]
  df <- df0 %>%
    mutate(
      LST = if_else(!is.na(n_centre) & n_centre >= 2,
                    centre_mean[[a]][match(id, centre_mean$id)],
                    cover_mean[[paste0("mean.", a)]]),
      SUHI = LST - min(LST)
    )

  #### Figure 3 ====================
  d3 <- merge(x=DATA, y=df[, c("IDMANZANA", "SUHI")],
              by.x="IDMANZANA", by.y="IDMANZANA")

  tests[[paste(a, 3)]] <- tibble(
    arm = a, figure = 3, panel = c("A", "B", "C", "D"),
    grouping = c("etnia", "casacuali", "aguacuali", "segurocuali"),
    test = c("Kruskal-Wallis", "Mann-Whitney U", "Mann-Whitney U", "Mann-Whitney U"),
    statistic = c(kruskal.test(SUHI ~ etnia, data=d3)$statistic,
                  wilcox.test(SUHI ~ casacuali, data=d3)$statistic,
                  wilcox.test(SUHI ~ aguacuali, data=d3)$statistic,
                  wilcox.test(SUHI ~ segurocuali, data=d3)$statistic),
    p_value = c(kruskal.test(SUHI ~ etnia, data=d3)$p.value,
                wilcox.test(SUHI ~ casacuali, data=d3)$p.value,
                wilcox.test(SUHI ~ aguacuali, data=d3)$p.value,
                wilcox.test(SUHI ~ segurocuali, data=d3)$p.value)
  )
  lab <- ifelse(tests[[paste(a, 3)]]$p_value < 0.001, "bolditalic(p)~bold('< 0.001')",
                paste0("bolditalic(p)~bold('= ",
                       formatC(tests[[paste(a, 3)]]$p_value, format = "f", digits = 3), "')"))

  df2 <- d3 %>%
    mutate(etnia=case_when(etnia=="nativo"~"Native",
                           etnia=="mestizo"~"Mestizo",
                           etnia=="blanco"~"White",
                           etnia=="afro"~"Afro-american")) %>%
    mutate(etnia=factor(etnia,levels = c("White","Mestizo","Afro-american","Native")))

  e1 <- ggplot(
    df2,
    aes(x = `SUHI`, y = `etnia`, fill = after_stat(x))
  ) +
    geom_density_ridges_gradient(scale = 2, linewidth = 0.3) +
    labs(fill = "SUHI (°C)") +
    labs(title = 'A. SUHI per ethnicity', x = "SUHI (°C)", y = "") +
    annotate("text", x = Inf, y = Inf, label = lab[1], parse = TRUE, hjust = 1.02, vjust = 3.2, size = 4.6) +
    theme_ridges(font_size = 15, grid = TRUE) +
    theme(plot.title.position = "plot") +
    scale_fill_innova(discrete = F, palette = "jama")

  df3 <- d3 %>%
    mutate(aguacuali=case_when(aguacuali=="agua"~"Have water service",
                               aguacuali=="noagua"~"No water service")) %>%
    mutate(aguacuali=factor(aguacuali,levels = c("Have water service","No water service")))

  e2 <- ggplot(
    df3,
    aes(x = `SUHI`, y = `aguacuali`, fill = after_stat(x))
  ) +
    geom_density_ridges_gradient(scale = 2, linewidth = 0.3) +
    labs(fill = "SUHI (°C)") +
    labs(title = 'C. SUHI per water service', x = "SUHI (°C)", y = "") +
    annotate("text", x = Inf, y = Inf, label = lab[3], parse = TRUE, hjust = 1.02, vjust = 3.2, size = 4.6) +
    theme_ridges(font_size = 15, grid = TRUE) +
    theme(plot.title.position = "plot") +
    scale_fill_innova(discrete = F, palette = "jama")

  df4 <- d3 %>%
    mutate(casacuali=case_when(casacuali=="casa"~"Have permanent home",
                               casacuali=="nocasa"~"No permanent home")) %>%
    mutate(casacuali=factor(casacuali,levels = c("Have permanent home","No permanent home")))

  e3 <- ggplot(
    df4,
    aes(x = `SUHI`, y = `casacuali`, fill = after_stat(x))
  ) +
    geom_density_ridges_gradient(scale = 2, linewidth = 0.3) +
    labs(fill = "SUHI (°C)") +
    theme_ridges(font_size = 15, grid = TRUE) +
    scale_fill_innova(discrete = F, palette = "jama") +
    labs(title = 'B. SUHI per permanent home', x = "SUHI (°C)", y = "") +
    annotate("text", x = Inf, y = Inf, label = lab[2], parse = TRUE, hjust = 1.02, vjust = 3.2, size = 4.6) +
    theme(plot.title.position = "plot")

  df11 <- d3 %>%
    mutate(segurocuali=case_when(segurocuali=="seguro"~"Have health insurance",
                                 segurocuali=="noseguro"~"No health insurance")) %>%
    mutate(segurocuali=factor(segurocuali,levels = c("Have health insurance","No health insurance")))

  e10 <- ggplot(
    df11,
    aes(x = `SUHI`, y = `segurocuali`, fill = after_stat(x))
  ) +
    geom_density_ridges_gradient(scale = 2, linewidth = 0.3) +
    labs(fill = "SUHI (°C)") +
    theme(plot.title.position = "plot") +
    scale_fill_innova(discrete = F, palette = "jama") +
    labs(title = 'D. SUHI per health insurance status', x = "SUHI (°C)", y = "") +
    annotate("text", x = Inf, y = Inf, label = lab[4], parse = TRUE, hjust = 1.02, vjust = 3.2, size = 4.6) +
    theme_ridges(font_size = 15, grid = TRUE) +
    theme(plot.title.position = "plot")

  fig3 <- ggarrange(e1, e3, e2, e10,
                    ncol = 2, nrow = 2)

  ggsave(paste0("ouput/png/mean_supplement/Figure3_", a, ".png"), fig3,
         width = 12, heigh = 9, bg = "white", dpi = 600)

  #### Figure 4 ====================
  d41 <- df %>%
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$seguro)))))*as.numeric(factor(rank(df$seguro)))),5))
  d42 <- df %>%
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$educacion)))))*as.numeric(factor(rank(df$educacion)))),5))
  d43 <- df %>%
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$blanco)))))*as.numeric(factor(rank(df$blanco)))),5))
  d44 <- df %>%
    mutate(rankeo = cut_number(((100/max(as.numeric(factor(rank(df$nativo)))))*as.numeric(factor(rank(df$nativo)))),5))

  tests[[paste(a, 4)]] <- tibble(
    arm = a, figure = 4, panel = c("A", "B", "C", "D"),
    grouping = c("seguro", "educacion", "blanco", "nativo"),
    test = "Kruskal-Wallis",
    statistic = c(kruskal.test(SUHI ~ rankeo, data=d41)$statistic,
                  kruskal.test(SUHI ~ rankeo, data=d42)$statistic,
                  kruskal.test(SUHI ~ rankeo, data=d43)$statistic,
                  kruskal.test(SUHI ~ rankeo, data=d44)$statistic),
    p_value = c(kruskal.test(SUHI ~ rankeo, data=d41)$p.value,
                kruskal.test(SUHI ~ rankeo, data=d42)$p.value,
                kruskal.test(SUHI ~ rankeo, data=d43)$p.value,
                kruskal.test(SUHI ~ rankeo, data=d44)$p.value)
  )
  lab <- ifelse(tests[[paste(a, 4)]]$p_value < 0.001, "bolditalic(p)~bold('< 0.001')",
                paste0("bolditalic(p)~bold('= ",
                       formatC(tests[[paste(a, 4)]]$p_value, format = "f", digits = 3), "')"))

  bv1_mhi <- ggplot(data = d41, aes(x=rankeo, y=SUHI, fill = rankeo)) +
    geom_violin(trim=FALSE, show.legend = F)+
    labs(title="SUHI estimation per rank of access of health insurance per block",x="", y = "SUHI (°C)")+
    geom_boxplot(width=0.1, outlier.alpha = 0, fill = "white") +
    annotate("text", x = 4.5, y = Inf, label = lab[1], parse = TRUE, vjust = 1.5, size = 4.5) +
    scale_x_discrete(labels = paste0("Q", 1:5, " = ", levels(d41$rankeo))) +
    theme_bw() +
    scale_fill_innova("jama")

  bv2_mhi <- ggplot(data = d42, aes(x=rankeo, y=SUHI, fill = rankeo)) +
    geom_violin(trim=FALSE, show.legend = F)+
    labs(title="SUHI estimation per rank of higher education level per block",x="", y = "SUHI (°C)")+
    geom_boxplot(width=0.1, outlier.alpha = 0, fill = "white") +
    annotate("text", x = 4.5, y = Inf, label = lab[2], parse = TRUE, vjust = 1.5, size = 4.5) +
    scale_x_discrete(labels = paste0("Q", 1:5, " = ", levels(d42$rankeo))) +
    theme_bw() +
    scale_fill_innova("jama")

  bv3_mhi <- ggplot(data = d43, aes(x=rankeo, y=SUHI, fill = rankeo)) +
    geom_violin(trim=FALSE, show.legend = F)+
    labs(title="SUHI estimation rank of white ethnicity per block",x="", y = "SUHI (°C)")+
    geom_boxplot(width=0.1, outlier.alpha = 0, fill = "white") +
    annotate("text", x = 4.5, y = Inf, label = lab[3], parse = TRUE, vjust = 1.5, size = 4.5) +
    scale_x_discrete(labels = paste0("Q", 1:5, " = ", levels(d43$rankeo))) +
    theme_bw() +
    scale_fill_innova("jama")

  bv4_mhi <- ggplot(data = d44, aes(x=rankeo, y=SUHI, fill = rankeo)) +
    geom_violin(trim=FALSE, show.legend = F)+
    labs(title="SUHI estimation rank of native ethnicity per block",x="", y = "SUHI (°C)")+
    geom_boxplot(width=0.1, outlier.alpha = 0, fill = "white") +
    annotate("text", x = 4.5, y = Inf, label = lab[4], parse = TRUE, vjust = 1.5, size = 4.5) +
    scale_x_discrete(labels = paste0("Q", 1:5, " = ", levels(d44$rankeo))) +
    theme_bw() +
    scale_fill_innova("jama")

  fig4 <- ggarrange(bv1_mhi,
                    bv2_mhi,
                    bv3_mhi,
                    bv4_mhi,
                    labels = c("A.", "B.", "C.", "D."),
                    ncol = 2, nrow = 2)

  ggsave(paste0("ouput/png/mean_supplement/Figure4_", a, ".png"), fig4,
         width = 12, heigh = 9, bg = "white", dpi = 600)

  #### Figure 5 ====================
  # MHI column of the deposited block layer replaced by the block SUHI of the arm
  spatial$MHI <- df$SUHI[match(spatial$IDMANZANA, df$IDMANZANA)]

  data <- left_join(spatial, socio %>% select(IDMANZANA,PC1,PC2,PC3,PC4,PC5,Index),
                    by = c("IDMANZANA")) %>%
    drop_na() %>%
    mutate(Index = -Index)
  stopifnot(nrow(data) == 69618)

  data <- bi_class(
    data,
    x = Index,
    y = MHI,
    style = "quantile",
    dim = 3
  )

  newdata <- data %>%
    st_set_geometry(NULL) %>%
    group_by(bi_class) %>%
    summarise(total = n())
  counts[[a]] <- newdata %>% mutate(arm = a, .before = 1)

  ## Bar plot by categories:
  ##> categories: 1-3 | 2-3 | 3-3
  cat1 <- newdata %>%
    filter(bi_class %in% sprintf("%s-3",1:3))

  b1 <- gg_barplot(x = cat1,nrow = 1)

  ##> categories: 1-2 | 2-2 | 3-2
  cat2 <- newdata %>%
    filter(bi_class %in% sprintf("%s-2",1:3))
  b2 <- gg_barplot(x = cat2,nrow = 2)

  ##> categories: 1-1 | 2-1 | 3-1
  cat3 <- newdata %>%
    filter(bi_class %in% sprintf("%s-1",1:3))
  stopifnot(max(cat3$total) <= 14500)
  b3 <- gg_barplot(x = cat3,nrow = 3) +
    ylim(c(0,14500))

  ##> Customization of bar plot
  plot <- plot_grid(b1, b2, b3, ncol = 1)
  barras <- plot +
    draw_plot_label(
      label = "Number of blocks",
      vjust = 0.1,
      x = 0.08,
      y = -0.25,
      angle = 90,
      size = 7
      ) +
    draw_plot_label(
      label = "Higher Index →",
      x = 0.34,
      y = 0.06,
      size = 7
      ) +
    draw_plot_label(
      label = "SUHI (°C) →",
      x = 0.90,
      y = -0.1,
      angle = 90,
      size = 7
      )

  ##> Customization of color palette
  paleta <- bi_pal2(
    pal = "DkViolet",
    dim = 3
    ) +
    theme(
      panel.background = element_blank(),
      axis.text.x = element_blank()
      ) +
    labs(
      x = "Index → ",
      y = "SUHI (°C) →"
    )

  ##> Cooking maps
  basemap <- ggplot() +
    geom_sf(
      data = dep,
      lwd = 0.5,
      fill = "#d9d9d9",
      show.legend = FALSE,
      color = "white"
    ) +
    geom_sf(
      data = data,
      lwd = 0.0,
      aes(fill = bi_class),
      show.legend = FALSE
    ) +
    theme(
      axis.text =  element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank()
      ) +
    bi_scale_fill2(pal = "DkViolet", dim = 3)

  ##> North zone
  data$title <- "North zone"
  p1 <- gg_bimap(
    data = data,
    xlim = c(-77.10173463,-77.04276369),
    ylim = c(-11.85954471,-11.81815091)
    )

  ##> South zone
  data$title <- "South zone"
  p2 <-  gg_bimap(
    data = data,
    xlim = c(-77.03788877,-76.95226743),
    ylim = c(-12.22157088,-12.16004115)
  )

  ##> East zone
  data$title <- "East zone"
  p3 <-  gg_bimap(
    data = data,
    xlim = c(-76.82754759,-76.80022516),
    ylim = c(-12.03109002,-12.01191144)
  )

  ##> West zone
  data$title <- "West zone"
  p4 <-  gg_bimap(
    data = data,
    xlim = c(-77.10442439,-77.05030405),
    ylim = c(-12.09190529,-12.05301300)
  )

  end <- ggdraw() +
    # North - - - - - - - - - - - - - - - - - -
    draw_plot(
      p1 +
        theme(
          legend.justification = "left",
          plot.background = element_blank()
        ),
      0.07, 0.6, 0.95, 0.30,
      scale = 1.4
    ) +
    # South - - - - - - - - - - - - - - - - - -
    draw_plot(
      p2 +
        theme(
          legend.justification = "left",
          plot.background = element_blank()
        ),
      0.38, 0.6, 0.95, 0.30,scale = 1.4
    ) +
    # East - - - - - - - - - - - - - - - - - - -
    draw_plot(
      p3 +
        theme(
          legend.justification = "left",
          plot.background = element_blank()
        ),
      0.38, 0.15, 0.95, 0.30,scale = 1.4
    ) +
    # West - - - - - - - - - - - - - - - - - - -
    draw_plot(
      p4 +
        theme(
          legend.justification = "left",
          plot.background = element_blank()
        ),
      0.07, 0.15, 0.95, 0.30,scale = 1.4
    ) +
    # Map - - - - - - - - - - - - - - - - - - - -
    draw_plot(
      basemap + theme(
        legend.justification = "top",
        plot.background = element_blank()
      ), -0.25, 0.05, 0.95, 0.95,scale = 1.0
    ) +
    # bar plot - - - - - - - - - - - - - - - - - -
    draw_plot(
      barras + theme(
        legend.justification = "bottom",
        plot.background = element_blank()
      ),
      -0.32, -0.085, 0.9, 0.60,
      scale = 0.30
    ) +
    # legend - - - - - - - - - - - - - - - - - - -
    draw_plot(
      paleta + theme(
        legend.justification = "bottom",
        plot.background = element_blank()
      ),
      -0.01, 0.53, 0.7, 0.7,
      scale = 0.30
    )

  ggsave(
    filename = paste0("ouput/png/mean_supplement/Figure5_", a, ".png"),
    plot = end,
    width = 14,
    height = 8,
    bg = "white",
    dpi = 600
    )

  #### Figure 6 ====================
  fits <- df %>%
    group_by(MACRODISTRITO) %>%
    nest() %>%
    mutate(qcfit=map(.x=data,
                     .f=~qgcomp.noboot(SUHI~.,dat=.x[,c(Xnm, 'SUHI')], family=gaussian(), bayes = T))
    )

  r4 <- fits %>%
    mutate(negw=map(.x=qcfit,
                    .f=~.x$neg.weights)) %>%
    mutate(dataexport=map(.x=negw,
                          .f=~enframe(.x))) %>%
    select(MACRODISTRITO, dataexport) %>%
    unnest(dataexport)

  r4$value <- r4$value*-1

  r5 <- fits %>%
    mutate(posgw=map(.x=qcfit,
                     .f=~.x$pos.weights)) %>%
    mutate(dataexport=map(.x=posgw,
                          .f=~enframe(.x))) %>%
    select(MACRODISTRITO, dataexport) %>%
    unnest(dataexport)

  r6 <- rbind(r4, r5)

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

  fig6 <- ggplot(r6)+
    geom_linerange(aes(x = MACRODISTRITO, ymin = 0, ymax = value, colour = MACRODISTRITO),
                   position = position_dodge(width = 1),
                   linewidth=0.9,
                   show.legend = T) +
    geom_point(aes(x = MACRODISTRITO, y = value, color = MACRODISTRITO),
               position = position_dodge(width = 1),
               size=3.3,
               show.legend = T) +
    coord_flip() +
    scale_color_innova("jama") +
    scale_fill_innova("jama") +
    theme_bw() +
    theme(axis.text.y=element_blank()) +
    xlab("Variables included in the Quantile g-computation analysis") +
    labs(colour = "Metropolitan Lima zones") +
    ylab("Scaled effect size on SUHI estimation (°C)") +
    geom_hline(yintercept = 0) +
    facet_grid(rows = "name", switch = "y")

  ggsave(paste0("ouput/png/mean_supplement/Figure6_", a, ".png"), fig6,
         width = 10, heigh = 18, bg = "white", dpi = 600)
}

write_csv(bind_rows(tests), "ouput/csv/suhi_mean_figure_tests.csv")
write_csv(bind_rows(counts), "ouput/csv/suhi_mean_figure5_counts.csv")
bind_rows(tests)
bind_rows(counts)
