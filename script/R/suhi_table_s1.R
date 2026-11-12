library(terra)
library(exactextractr)
library(sf)
library(tidyverse)
library(table1)

# SUHI and LST per district and for Metropolitan Lima, in the format of supplementary Table S1.
# Run from the repository root.
# suhi_deposited = SUHI and lst_deposited = LST2017_2021_mean of ouput/csv/Lima_sociodemo_index_mhi.csv.
# suhi_c2_mean = block LST of raw_data/tiff/LST_2017_2021_C2_mean.tif (Collection 2 port, all-year,
# per-pixel mean) minus the lowest block LST of that raster (Metropolitan Lima).
# Zonal rule of script/R/suhi_mean_supplement.R (QGIS Zonal Statistics): mean of the valid cells whose
# centre lies in the block when there are two or more, otherwise the coverage-weighted mean.
# n, mean (SD), median [Q1, Q3] for Metropolitan Lima and each district (DISTRITO), printed with
# 3 significant digits (table1::signif_pad).
# Output: ouput/csv/suhi_table_s1.csv.

blocks <- st_read(
  "raw_data/gpkg/blocks_lima.gpkg"
  ) %>%
  st_transform(32718)

df <- read_csv("ouput/csv/Lima_sociodemo_index_mhi.csv",
               col_types = cols(IDMANZANA = "c"), guess_max = 1e5) %>%
  select(IDMANZANA, DISTRITO, lst_deposited = LST2017_2021_mean, suhi_deposited = SUHI)

blocks <- blocks %>%
  select(IDMANZANA) %>%
  inner_join(df, by = "IDMANZANA") %>%
  mutate(id = row_number())
stopifnot(nrow(blocks) == 69618)

rasters <- c(
  c_c2_mean = "raw_data/tiff/LST_2017_2021_C2_mean.tif"
)
r <- rast(rasters)
names(r) <- names(rasters)

centre <- rasterize(vect(blocks), r, field = "id", touches = FALSE)
centre_mean <- zonal(r, centre, fun = "mean", na.rm = TRUE)
centre_n <- zonal(!is.na(r), centre, fun = "sum", na.rm = TRUE)
cover_mean <- exact_extract(r, blocks, "mean", progress = FALSE, force_df = TRUE,
                            full_colnames = TRUE)

base <- blocks %>% st_set_geometry(NULL)
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

quantities <- c("suhi_deposited", "suhi_c2_mean", "lst_deposited")
districts <- c("Metropolitan Lima", sort(unique(base$DISTRITO)))

long <- arms$c_c2_mean %>%
  select(DISTRITO, suhi_deposited, suhi_c2_mean = SUHI, lst_deposited) %>%
  pivot_longer(-DISTRITO, names_to = "quantity", values_to = "value")

table_s1 <- bind_rows(long %>% mutate(DISTRITO = "Metropolitan Lima"), long) %>%
  mutate(district = factor(DISTRITO, levels = districts),
         quantity = factor(quantity, levels = quantities)) %>%
  group_by(district, quantity) %>%
  summarise(n = n(), mean = mean(value), sd = sd(value), median = median(value),
            q1 = unname(quantile(value, 0.25)), q3 = unname(quantile(value, 0.75)),
            .groups = "drop") %>%
  mutate(
    district = as.character(district), quantity = as.character(quantity),
    mean_sd = sprintf("%s (%s)", signif_pad(mean, 3), signif_pad(sd, 3)),
    median_q1_q3 = sprintf("%s [%s, %s]", signif_pad(median, 3), signif_pad(q1, 3),
                           signif_pad(q3, 3))
  )
table_s1 %>% filter(district == "Metropolitan Lima") %>% print(width = Inf)

write_csv(table_s1, "ouput/csv/suhi_table_s1.csv")
