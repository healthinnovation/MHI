library(terra)
library(exactextractr)
library(sf)
library(tidyverse)
library(qgcomp)

# All-year analysis under the median and the mean composite. Run from the repository root.
# Block LST of three all-year rasters on the 69,618 analysis blocks:
# (a) raw_data/tiff/LST_2017_2021_mean.tif, deposited raster of the article (per-pixel median)
# (b) raw_data/tiff/LST_2017_2021_C2_median.tif, Collection 2 port, per-pixel median
# (c) raw_data/tiff/LST_2017_2021_C2_mean.tif, Collection 2 port, per-pixel mean
# Zonal rule of QGIS Zonal Statistics: mean of the valid cells whose centre lies in the block
# when there are two or more, otherwise the coverage-weighted mean.
# SUHI = block LST minus the lowest block LST of the same raster (Metropolitan Lima);
# coldest_block_* = lowest block LST within the row's zone.
# published_column = LST2017_2021_mean and SUHI of ouput/csv/Lima_sociodemo_index_mhi.csv.
# Spearman rho of Index with SUHI; Kruskal-Wallis of SUHI across Index quintiles of the zone.
# Model as published (script/R/box_ridges_qgcomp.R): qgcomp.noboot on the eleven exposures,
# gaussian, q = 4, bayes = TRUE, for Metropolitan Lima and each zone (MACRODISTRITO).
# Agreement with (a): port = intercept + slope x deposited; mean_diff = port - deposited.

blocks <- st_read(
  "raw_data/gpkg/blocks_lima.gpkg"
  ) %>%
  st_transform(32718)

df <- read_csv("ouput/csv/Lima_sociodemo_index_mhi.csv",
               col_types = cols(IDMANZANA = "c"), guess_max = 1e5) %>%
  select(IDMANZANA, MACRODISTRITO, Index, LST2017_2021_mean, SUHI)

Xnm <- c(
  'casa','agua','luz','internet','seguro','leer',
  'educacion','fuerzalab','remunerado','raza', "Ingresos_mean"
)

blocks <- blocks %>%
  select(IDMANZANA, all_of(Xnm)) %>%
  inner_join(df, by = "IDMANZANA") %>%
  mutate(id = row_number())
stopifnot(nrow(blocks) == 69618)

rasters <- c(
  a_deposited = "raw_data/tiff/LST_2017_2021_mean.tif",
  b_c2_median = "raw_data/tiff/LST_2017_2021_C2_median.tif",
  c_c2_mean = "raw_data/tiff/LST_2017_2021_C2_mean.tif"
)
r <- rast(rasters)
names(r) <- names(rasters)

centre <- rasterize(vect(blocks), r, field = "id", touches = FALSE)
centre_mean <- zonal(r, centre, fun = "mean", na.rm = TRUE)
centre_n <- zonal(!is.na(r), centre, fun = "sum", na.rm = TRUE)
cover_mean <- exact_extract(r, blocks, "mean", progress = FALSE)
cells <- exact_extract(r, blocks, include_cell = TRUE, include_cols = "id", progress = FALSE) %>%
  bind_rows() %>%
  filter(coverage_fraction > 0)

base <- blocks %>% st_set_geometry(NULL)
arms <- list(published_column = base %>% mutate(LST = LST2017_2021_mean))
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
stopifnot(all(abs(arms$a_deposited$LST - base$LST2017_2021_mean) < 1e-4))

zones <- c(Metropolitan = "ALL", Callao = "CALLAO", Center = "CENTRO", East = "ESTE",
           North = "NORTE", South = "SUR")

rows <- list()
weights <- list()
for (a in names(arms)) {
  d <- arms[[a]]
  for (z in names(zones)) {
    dz <- if (zones[[z]] == "ALL") d else d[d$MACRODISTRITO == zones[[z]], ]
    dz$quintile <- ntile(dz$Index, 5)
    kw <- kruskal.test(SUHI ~ factor(quintile), data = dz)
    sp <- cor.test(dz$Index, dz$SUHI, method = "spearman", exact = FALSE)
    qc.fit <- qgcomp.noboot(SUHI ~ ., dat = as.data.frame(dz[, c(Xnm, "SUHI")]),
                            family = gaussian(), q = 4, bayes = T)
    nd <- if (a %in% names(rasters)) filter(cells, id %in% dz$id, is.na(.data[[a]])) else NULL
    rows[[paste(a, z)]] <- tibble(
      arm = a, zone = z, n = nrow(dz),
      coldest_block_id = dz$IDMANZANA[which.min(dz$LST)],
      coldest_block_lst = min(dz$LST),
      suhi_mean = mean(dz$SUHI), suhi_sd = sd(dz$SUHI),
      suhi_median = median(dz$SUHI),
      suhi_q1 = unname(quantile(dz$SUHI, 0.25)), suhi_q3 = unname(quantile(dz$SUHI, 0.75)),
      spearman_rho = unname(sp$estimate), spearman_p = sp$p.value,
      kw_h = unname(kw$statistic), kw_p = kw$p.value,
      psi = unname(qc.fit$psi), psi_lower = qc.fit$ci[1], psi_upper = qc.fit$ci[2],
      nodata_cells_in_blocks = if (is.null(nd)) NA else n_distinct(nd$cell),
      blocks_touching_nodata = if (is.null(nd)) NA else n_distinct(nd$id),
      block_lst_r_deposited = cor(dz$LST, arms$a_deposited$LST[match(dz$id, arms$a_deposited$id)])
    )
    weights[[paste(a, z)]] <- tibble(
      arm = a, zone = z,
      variable = c(names(qc.fit$pos.weights), names(qc.fit$neg.weights)),
      signed_scaled_effect = c(unname(qc.fit$pos.weights), 0 - unname(qc.fit$neg.weights))
    )
  }
}

v <- values(r)
agreement <- list()
for (a in c("b_c2_median", "c_c2_mean")) {
  for (s in c("valid", "blocks")) {
    i <- if (s == "valid") seq_len(nrow(v)) else sort(unique(cells$cell))
    ok <- !is.na(v[i, "a_deposited"]) & !is.na(v[i, a])
    x <- v[i, "a_deposited"][ok]
    y <- v[i, a][ok]
    agreement[[paste(a, s)]] <- tibble(
      arm = a, set = s, n = length(x), r = cor(x, y),
      slope = cov(x, y) / var(x), intercept = mean(y) - cov(x, y) / var(x) * mean(x),
      mean_diff = mean(y - x)
    )
  }
}
agreement <- bind_rows(agreement) %>%
  pivot_wider(names_from = set, values_from = n:mean_diff, names_glue = "cells_{set}_{.value}",
              names_vary = "slowest") %>%
  mutate(zone = "Metropolitan")

supp <- bind_rows(rows) %>%
  left_join(agreement, by = c("arm", "zone"))
supp_weights <- bind_rows(weights)
supp %>% filter(zone == "Metropolitan") %>% print(width = Inf)

write_csv(supp, "ouput/csv/suhi_mean_supplement.csv")
write_csv(supp_weights, "ouput/csv/suhi_mean_supplement_weights.csv")
