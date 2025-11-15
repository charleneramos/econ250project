library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(here)

uga_adm2 <- st_read(here('data','raw','AidData','6913605476a3925d993413d2','UGA_ADM2.geojson'))
ntl_csv  <- read_csv(here('data','raw','AidData','6913605476a3925d993413d2','6913605476a3925d993413d2_results.csv'))
ntl_csv <- ntl_csv |> select(shapeID, viirs_ntl_annual_v21_avg_masked.2017.mean)

# Inspect to find the join key and NTL column:
names(uga_adm2)
names(ntl_csv)

# Example: both share "ADM2_CODE"
ntl_var   <- "viirs_ntl_annual_v21_avg_masked.2017.mean"   # change to your real NTL column
join_key  <- "shapeID"  # change if needed

uga_ntl <- uga_adm2 |>
  left_join(ntl_csv, by = join_key) |>
  mutate(
    ntl_raw    = .data[[ntl_var]],
    ntl_capped = pmin(ntl_raw, quantile(ntl_raw, 0.99, na.rm = TRUE))
  )

p_ntl <- ggplot() +
  geom_sf(
    data  = uga_ntl,
    aes(fill = ntl_capped),
    color = NA
  ) +
  coord_sf() +
  scale_fill_viridis_c(
    name = "Nighttime lights\n(mean radiance)",
    na.value = "transparent",
    option = "C"
  ) +
  labs(
  ) +
  theme_void(base_family = "Times") +
  theme(
    plot.title   = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 8, hjust = 0),
    legend.title = element_text(size = 9),
    legend.text  = element_text(size = 8)
  )

ggsave(here('output','fig_ntl_uganda.png'),
  p_ntl,
  width  = 6.5,
  height = 4.5,
  dpi    = 300
)
