library(terra)
library(exactextractr)
library(sf)
library(tidyverse)
library(ggstatsplot)
library(ggpubr)
library(table1)

# SUHI on the four Collection 2 composites of raw_data/tiff. Run from the repository root.
# (b) raw_data/tiff/LST_2017_2021_C2_median.tif, all-year, per-pixel median
# (c) raw_data/tiff/LST_2017_2021_C2_mean.tif, all-year, per-pixel mean
# (d) raw_data/tiff/LST_2017_2021_C2_summer_median.tif, summer, per-pixel median
# (e) raw_data/tiff/LST_2017_2021_C2_summer_mean.tif, summer, per-pixel mean
# Block LST with the zonal rule of script/R/suhi_mean_supplement.R on the 69,618 analysis blocks;
# SUHI = block LST minus the lowest block LST of the same composite (Metropolitan Lima).
# Table 1 rows: n, mean (SD), median [Q1, Q3] for Metropolitan Lima and each zone, printed with
# 3 significant digits (table1::signif_pad).
# Rank groups as in figure S3 (script/R/box_ridges_qgcomp.R): dense rank of the share rescaled to
# 0-100, cut_number into 5 groups (3 for afro), formed on all blocks with the shares of
# raw_data/gpkg/blocks_lima.gpkg. Kruskal-Wallis of SUHI across the groups, epsilon squared = H / (n - 1).
# Figures in the format of figures S3 and S4: ggstatsplot::ggbetweenstats, 11 panels A to K.
# Outputs: ouput/csv/suhi_c2_table1.csv, suhi_c2_rank_groups.csv, suhi_c2_kruskal.csv,
# suhi_c2_figure_statistics.csv (statistics printed in the figures);
# ouput/png/c2_composites/<figure>.png (33 x 24 in, 600 dpi).

pdf(NULL)
dir.create("ouput/png/c2_composites", showWarnings = FALSE)

var_labels <- c(
  internet = "Access to internet service",
  seguro = "Access of health insurance",
  leer = "Literacy population",
  educacion = "People with higher education level",
  fuerzalab = "People at least 15 years old and able to work",
  remunerado = "People who recently received income",
  blanco = "White ethnicity",
  mestizo = "Mestizo ethnicity",
  afro = "Afroamerican ethnicity",
  nativo = "Native ethnicity",
  Ingresos_mean = "Per capita income"
)

blocks <- st_read(
  "raw_data/gpkg/blocks_lima.gpkg"
  ) %>%
  st_transform(32718)

df <- read_csv("ouput/csv/Lima_sociodemo_index_mhi.csv",
               col_types = cols(IDMANZANA = "c"), guess_max = 1e5) %>%
  select(IDMANZANA, MACRODISTRITO)

blocks <- blocks %>%
  select(IDMANZANA, all_of(names(var_labels))) %>%
  inner_join(df, by = "IDMANZANA") %>%
  mutate(id = row_number())
stopifnot(nrow(blocks) == 69618)

rasters <- c(
  b_c2_median = "raw_data/tiff/LST_2017_2021_C2_median.tif",
  c_c2_mean = "raw_data/tiff/LST_2017_2021_C2_mean.tif",
  d_c2_summer_median = "raw_data/tiff/LST_2017_2021_C2_summer_median.tif",
  e_c2_summer_mean = "raw_data/tiff/LST_2017_2021_C2_summer_mean.tif"
)
r <- rast(rasters)
names(r) <- names(rasters)

centre <- rasterize(vect(blocks), r, field = "id", touches = FALSE)
centre_mean <- zonal(r, centre, fun = "mean", na.rm = TRUE)
centre_n <- zonal(!is.na(r), centre, fun = "sum", na.rm = TRUE)
cover_mean <- exact_extract(r, blocks, "mean", progress = FALSE)

base <- blocks %>%
  st_set_geometry(NULL) %>%
  mutate(zone = recode(MACRODISTRITO, CALLAO = "Callao", CENTRO = "Center", ESTE = "East",
                       NORTE = "North", SUR = "South"))

groups <- base %>%
  select(id, all_of(names(var_labels))) %>%
  pivot_longer(-id, names_to = "variable", values_to = "share") %>%
  group_by(variable) %>%
  mutate(rank_scaled = (100/max(as.numeric(factor(rank(share)))))*as.numeric(factor(rank(share))),
         rank_group = as.integer(cut_number(rank_scaled, if_else(first(variable) == "afro", 3, 5))),
         rank_group_label = as.character(cut_number(rank_scaled, if_else(first(variable) == "afro", 3, 5)))) %>%
  ungroup()

arms <- list()
for (a in names(rasters)) {
  n_centre <- centre_n[[a]][match(base$id, centre_n$id)]
  arms[[a]] <- base %>%
    mutate(
      LST = if_else(!is.na(n_centre) & n_centre >= 2,
                    centre_mean[[a]][match(id, centre_mean$id)],
                    cover_mean[[paste0("mean.", a)]]),
      SUHI = LST - min(LST)
    )
}

#### Table 1 rows ====================
rows <- list()
for (a in names(arms)) {
  d <- arms[[a]]
  for (z in c("Metropolitan", "Callao", "Center", "East", "North", "South")) {
    dz <- if (z == "Metropolitan") d else d[d$zone == z, ]
    rows[[paste(a, z)]] <- tibble(
      arm = a, zone = z, n = nrow(dz),
      reference_block_id = d$IDMANZANA[which.min(d$LST)], reference_block_lst = min(d$LST),
      mean = mean(dz$SUHI), sd = sd(dz$SUHI), median = median(dz$SUHI),
      q1 = unname(quantile(dz$SUHI, 0.25)), q3 = unname(quantile(dz$SUHI, 0.75)),
      mean_sd = sprintf("%s (%s)", signif_pad(mean, 3), signif_pad(sd, 3)),
      median_q1_q3 = sprintf("%s [%s, %s]", signif_pad(median, 3), signif_pad(q1, 3),
                             signif_pad(q3, 3))
    )
  }
}
table1_rows <- bind_rows(rows)

#### SUHI by rank group and Kruskal-Wallis ====================
group_rows <- list()
kruskal_rows <- list()
for (a in names(arms)) {
  dg <- groups %>%
    inner_join(arms[[a]] %>% select(id, SUHI), by = "id")
  for (v in names(var_labels)) {
    dv <- dg %>% filter(variable == v)
    kw <- kruskal.test(SUHI ~ factor(rank_group), data = dv)
    group_rows[[paste(a, v)]] <- dv %>%
      group_by(rank_group, rank_group_label) %>%
      summarise(n = n(), median = median(SUHI), q1 = unname(quantile(SUHI, 0.25)),
                q3 = unname(quantile(SUHI, 0.75)), .groups = "drop") %>%
      mutate(arm = a, panel = LETTERS[match(v, names(var_labels))], variable = v, .before = 1)
    kruskal_rows[[paste(a, v)]] <- tibble(
      arm = a, panel = LETTERS[match(v, names(var_labels))], variable = v, n = nrow(dv),
      df = unname(kw$parameter), h = unname(kw$statistic), p_value = kw$p.value,
      log10_p_value = pchisq(unname(kw$statistic), unname(kw$parameter), lower.tail = FALSE,
                             log.p = TRUE) / log(10),
      epsilon_squared = unname(kw$statistic) / (nrow(dv) - 1),
      group_sizes = paste(table(dv$rank_group), collapse = " / "),
      median_bottom_group = median(dv$SUHI[dv$rank_group == 1]),
      median_top_group = median(dv$SUHI[dv$rank_group == max(dv$rank_group)]),
      sign_top_minus_bottom = sign(median_top_group - median_bottom_group)
    )
  }
}

#### Figures in the format of figures S3 and S4 ====================
figures <- c(
  b_c2_median = "FigS3fmt_allyear_c2_median",
  c_c2_mean = "FigS3fmt_allyear_c2_mean",
  d_c2_summer_median = "FigS4_summer_c2_median",
  e_c2_summer_mean = "FigS4fmt_summer_c2_mean"
)
# 2023 colours (innovar "jama" ramp, 5 and 3 levels)
pal_rank5 <- c("#0B4E60", "#2D819F", "#5AB7B0", "#F5C690", "#8C3A33")
pal_rank3 <- c("#0B4E60", "#5AB7B0", "#8C3A33")

# The bootstrap interval of epsilon squared and the jitter of the points are random. The seed is
# set before (b) and again before (d), and the figures are drawn in the order (b), (c), (e), (d),
# so that the printed intervals can be reproduced.
figure_stats <- list()
for (a in c("b_c2_median", "c_c2_mean", "e_c2_summer_mean", "d_c2_summer_median")) {
  if (a %in% c("b_c2_median", "d_c2_summer_median")) set.seed(2023)
  period <- if_else(a %in% c("b_c2_median", "c_c2_mean"), "all-year", "in summer")
  dg <- groups %>%
    inner_join(arms[[a]] %>% select(id, SUHI), by = "id")
  panels <- list()
  for (v in names(var_labels)) {
    dv <- dg %>%
      filter(variable == v) %>%
      arrange(rank_group) %>%
      mutate(rank_group_label = factor(rank_group_label, levels = unique(rank_group_label)))
    panels[[v]] <- ggbetweenstats(
      data = dv,
      x = rank_group_label,
      y = SUHI,
      type = "nonparametric",
      pairwise.display = "significant",
      p.adjust.method = "bonferroni",
      digits = 2L,
      xlab = "",
      ylab = paste0("SUHI ", period, " (°C)"),
      title = paste0("SUHI ", period, " per rank of ", var_labels[[v]], " per block"),
      point.args = list(
        position = position_jitterdodge(dodge.width = 0.6),
        alpha = 0.01,
        size = 3,
        stroke = 0
      ),
      boxplot.args = list(width = 0.3, alpha = 0.2, notch = TRUE, na.rm = TRUE),
      ggtheme = theme_minimal(base_size = 12)
    ) +
      scale_color_manual(values = if (v == "afro") pal_rank3 else pal_rank5) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 10),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "none",
        strip.text = element_text(face = "bold")
      )
    figure_stats[[paste(a, v)]] <- extract_stats(panels[[v]])$subtitle_data %>%
      transmute(figure = figures[[a]], arm = a, panel = LETTERS[match(v, names(var_labels))],
                variable = v, statistic, df = df.error, p_value = p.value,
                epsilon_squared = estimate, conf_low = conf.low, conf_high = conf.high,
                n_obs = n.obs)
  }
  fig <- ggarrange(plotlist = panels, labels = "AUTO", ncol = 4, nrow = 3)
  ggsave(paste0("ouput/png/c2_composites/", figures[[a]], ".png"), fig,
         width = 33, height = 24, units = "in", bg = "white", dpi = 600)
}

write_csv(table1_rows, "ouput/csv/suhi_c2_table1.csv")
write_csv(bind_rows(group_rows), "ouput/csv/suhi_c2_rank_groups.csv")
write_csv(bind_rows(kruskal_rows), "ouput/csv/suhi_c2_kruskal.csv")
write_csv(bind_rows(figure_stats) %>% arrange(arm, panel), "ouput/csv/suhi_c2_figure_statistics.csv")
table1_rows %>% select(arm, zone, n, mean_sd, median_q1_q3) %>% print(n = Inf)
bind_rows(kruskal_rows) %>% select(arm, panel, variable, h, epsilon_squared) %>% print(n = Inf)
