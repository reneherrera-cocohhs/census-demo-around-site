# package libraries ####
library(here)
library(tidyverse)
library(tidygeocoder)
library(tigris)
library(sf)
library(zipcodeR)
library(janitor)
library(plotly)
library(biscale)
library(cowplot)

# options 
options(tigris_use_cache = TRUE)



# read jail address
jail_address <- read_rds(
  file = "data-raw/jail-address.rds"
)

# zip codes #### 
zip_code_target_list <- c(
  "86001",
  "86004",
  "86005",
  "86011",
  "86015"
)
class(zip_code_target_list)
str(zip_code_target_list)
glimpse(zip_code_target_list)

# use tigris to get list of zip codes 
zip_code_df <- zctas() %>%
  clean_names() 

# inspect
glimpse(zip_code_df)

zip_code_test <- zip_code_df %>%
  filter(str_detect(zcta5ce20, "^86"))

zip_plot <- zip_code_test %>%
  ggplot() +
  geom_sf(fill = "white") +
  geom_sf_text(mapping = aes(label = zcta5ce20)) 

ggplotly(zip_plot)  

# plot_ly(
#   zip_code_test,
#   text = ~str_c(zcta5ce20)
# )

# filter to target 
zip_code_target <- zip_code_df %>%
  # filter(str_detect(zcta5ce20, "^8600"))
  filter(zcta5ce20 %in% zip_code_target_list)

# inspect 
glimpse(zip_code_target)

# flagstaff boundary #### 
flg <- places(
  state = "04"
) %>%
  clean_names()

flg <- flg %>%
  filter(
    name == "Flagstaff"
  )

# inspect 
glimpse(flg)

# clip to specific geographic area 
st_crs(flg) # check  coordinate reference system

# plot flagstaff & zip codes 
ggplot() +
  geom_sf(
    data = flg,
    fill = "white"
  ) +
  geom_sf(
    data = zip_code_target,
    fill = "pink",
    alpha = 1/5
  )

# bounding box 
box <- st_as_sfc(
  st_bbox(flg),
  crs = "4269"
)

# # census blocks #### 
# blocks_df <- blocks(
#   state = "04",
#   county = "005"
# ) %>%
#   clean_names() 
# 
# # inspect 
# glimpse(blocks_df)

# census tracts #### 
tracts_df <- tracts(
  state = "04",
  county = "005"
) %>%
  clean_names() 

# inspect
glimpse(tracts_df)

flg_tract_plot <- ggplot() +
  # geom_sf(
  #   data = flg,
  #   fill = "white",
  #   alpha = 2/5
  # ) +
  geom_sf(
    data = tracts_df,
    fill = "pink",
    alpha = 2/5
  ) +
  geom_sf_text(
    data = tracts_df,
    mapping = aes(label = namelsad)) +
  geom_point(
    data = jail_address,
    mapping = aes(
      x = long,
      y = lat
    ),
    shape = 3,
    size = 4,
    stroke = 2
  )

ggplotly(flg_tract_plot)
  

tracts_target <- map(
  # .x = c("86001", "86004"),
  .x = c("86001", "86004", "86011", "86015"),
  .f = get_tracts
)

tracts_target <- bind_rows(
  tracts_target[[1]],
  tracts_target[[2]]
) %>%
  mutate(GEOID = as.character(GEOID))

tracts_target <- tracts_target$TRACT

tract_list <- c(
  "001201",
  "000100",
  "000200",
  "000800",
  "001103",
  "980200",
  "000900",
  "000301",
  "000302",
  "000700",
  "001000",
  "001104"
)

tracts_df <- tracts_df %>%
  filter(
    tractce %in% tract_list
  ) 

tracts_df %>%
  # filter(
  #   !tractce %in% (c(
  #     "945100",
  #     "001500",
  #     "001302"
  #   ))
  # ) %>%
ggplot() +
  geom_sf() +
  geom_sf_text(mapping = aes(label = tractce))

tracts_flg <- tracts_df %>%
  ggplot() +
  geom_sf() +
  geom_sf_text(mapping = aes(label = tractce))

# intersect to limit to box 
tracts_df_box <- st_intersection(
  x = box,
  y = tracts_df
)

# plot ####
ggplot() +
  geom_sf(
    data = flg,
    fill = "white",
    alpha = 2/5
  ) +
  geom_sf(
    data = zip_code_target,
    fill = "pink",
    alpha = 1/5
  ) +
  geom_sf_label(
    data = zip_code_target,
    mapping = aes(label = zcta5ce20 )
  ) +
  # geom_sf(
  #   data = tracts_df_box,
  #   alpha = 1/5,
  #   fill = "violet",
  #   color = "violet"
  # ) +
  # geom_sf(
  #   data = tracts_df,
  #   alpha = 1/5,
  #   fill = "violet",
  #   color = "violet"
  # ) +
  # geom_sf_label(
  #   data = tracts_df,
  #   mapping = aes(label = tractce)
  # ) +
# geom_sf(
#   data = tracts_flg,
#   alpha = 1/5,
#   fill = "violet",
#   color = "violet"
# ) +
#   geom_sf_text(
#     data = tracts_flg,
#     mapping = aes(label = tractce)
#   ) +
  geom_point(
    data = jail_address,
    mapping = aes(
      x = long,
      y = lat
    ),
    shape = 3,
    size = 4,
    stroke = 2
  )

# 5km radius around a point 
circle <- st_buffer(
  st_sfc(st_point(c(jail_address$long, jail_address$lat)), crs = 4326),
  2500
)

ggplot() +
  geom_sf(
    data = flg,
    fill = "#f1faee",
    alpha = 3/5
  ) +
  geom_sf(
    data = tracts_df,
    alpha = 2/5,
    fill = "#457b9d"
  ) +
  geom_sf(
    data = circle,
    alpha = 2/5,
    fill = "#e63946"
  ) +
  geom_sf_label(
    data = tracts_df,
    mapping = aes(label = namelsad),
    size = 2
  ) +
  geom_point(
    data = jail_address,
    mapping = aes(
      x = long,
      y = lat
    ),
    shape = 3,
    size = 2,
    stroke = 1
  ) +
  theme_void()

ggsave(
  filename = "map-around-site-census-tracts-with-2.5km-radius.pdf",
  device = "pdf",
  path = onedrive_path
)

ggplot() +
  geom_sf(
    data = flg,
    fill = "#f1faee",
    alpha = 3/5
  ) +
  geom_sf(
    data = zip_code_target,
    fill = "#457b9d",
    alpha = 2/5
  ) +
  geom_sf(
    data = circle,
    alpha = 2/5,
    fill = "#e63946"
  ) +
  geom_sf_label(
    data = zip_code_target,
    mapping = aes(label = zcta5ce20 )
  ) +
  geom_point(
    data = jail_address,
    mapping = aes(
      x = long,
      y = lat
    ),
    shape = 3,
    size = 2,
    stroke = 2
  ) +
  theme_void()

# bivar 
bivar_data <- geo_join(
  spatial_data = tracts_df,
  data_frame = bivar_tab,
  by_sp = "geoid",
  by_df = "geoid"
)

bivar_data <- bi_class(
  .data = bivar_data,
  x = calc_est_under5,
  y = calc_est_poverty,
  style = "quantile",
  dim = 2
)

map <- ggplot(
  data = bivar_data
) +
  geom_sf(
    mapping = aes(
      fill = bi_class
    ),
    color = "white",
    size = 0.1,
    show.legend = FALSE
  ) +
  bi_scale_fill(pal = "DkBlue2", dim = 2) +
  labs(
    title = "Poverty & Children Under 5",
    subtitle = "",
    x = "",
    y = ""
  ) +
  geom_point(
    data = jail_address,
    mapping = aes(
      x = long,
      y = lat
    ),
    shape = 3,
    size = 2,
    stroke = 2
  ) +
  coord_sf(expand = TRUE) +
  bi_theme()

legend <- bi_legend(pal = "DkBlue2",
                    dim = 3,
                    xlab = "% Under 5",
                    ylab = "% Below poverty",
                    size = 8)

(finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.65, .65, 0.3, 0.3))

ggsave(
  filename = "map-bivar-poverty-children.pdf",
  device = "pdf",
  path = onedrive_path
)
