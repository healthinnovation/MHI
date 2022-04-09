# 1. Requirements ---------------------------------------------------------
library(biscale)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(sf)
source("utils.R")

# 2. Reading and processing data ------------------------------------------
socio <- read_csv2("final.csv")
dep <- st_read("Lima.gpkg")
spatial <- st_read("SUHI-2017.gpkg")
suhi <- left_join(spatial,socio,by = c("Mz"="ID")) %>% 
  drop_na()

# 3. Bar plot by categories -----------------------------------------------
data <- bi_class(
  data,
  x = Index,
  y = SUHI,
  style = "quantile",
  dim = 3
)

newdata <- data %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "Number of manzanas",
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
    label = "Number SUHI →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
    )

barras

# 4. Biscale map  ---------------------------------------------------------
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
    y = "SUHI →"
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

basemap

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

# 5. Final plot -----------------------------------------------------------

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

##> Export final plot in a png format
ggsave(
  filename = "figure2.png",
  plot = end,
  width = 14,
  height = 8,
  bg = "white"
  )
  
