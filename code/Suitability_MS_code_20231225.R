library(cropTargetR)
library(sf)
library(terra)
library(tidyverse)
library(cowplot)


#### set working directory for wherever files are saved ####
setwd("C:/Users/monik/Desktop/code")

########## Suitability Analysis #############
#### study area
counties <- data.frame(state = c(      "MI",   "WI",     "IL",    "IL"),
                       counties = c("Ionia", "Sauk", "Fulton", "White"))

aoi_list <- purrr::map2(counties$state, counties$counties, make_county_AOI)



############ Figure 2 ##################
### plot of example AOIs
us <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  sf::st_transform(crs = sf::st_crs(aoi_list[[1]]))

midwest <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  sf::st_transform(crs = sf::st_crs(aoi_list[[1]])) %>%
  dplyr::filter(ID %in% c("illinois", "wisconsin", "michigan"))

us_cropped <- sf::st_crop(us, sf::st_bbox(midwest))

## with basemap using ggmap
crop_spot <- sf::st_bbox(midwest)
crop_spot2 <- crop_spot
crop_spot2[1] <- 450000

us_cropped <- sf::st_crop(us, crop_spot) %>%
  sf::st_transform(crs = 4326)

bbox <- sf::st_as_sfc(sf::st_bbox(us_cropped), crs = st_crs(4326))
point <- sf::st_coordinates(st_centroid(bbox))

basemap <- ggmap::get_map(location = point, crs = 4326, zoom = 6, maptype = "satellite")

## hack to fix spatial reference
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), c("ymin", "xmin", "ymax", "xmax"))
  # Convert the bbox to an sf polygon, transform it to 3857,
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  # Overwrite the bbox of the ggmap object with the transformed coordinates
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

basemap_fixed <- ggmap_bbox(basemap)

## transform all layers to google pseudomercador
us_cropped_gpm <- sf::st_transform(us, crs = 3857)
aoi1_gpm <- sf::st_transform(aoi_list[[1]], crs = 3857)
aoi2_gpm <- sf::st_transform(aoi_list[[2]], crs = 3857)
aoi3_gpm <- sf::st_transform(aoi_list[[3]], crs = 3857)
aoi4_gpm <- sf::st_transform(aoi_list[[4]], crs = 3857)

## add label names for each county
aoi_classes <- class(aoi1_gpm)
aoi_classes <- aoi_classes[aoi_classes != "AOI"]

class(aoi1_gpm) <- aoi_classes
class(aoi2_gpm) <- aoi_classes
class(aoi3_gpm) <- aoi_classes
class(aoi4_gpm) <- aoi_classes

aoi1_gpm <- aoi1_gpm %>% mutate(county.state = paste0(stringr::word(county, 1), ", ", abbr))
aoi2_gpm <- aoi2_gpm %>% mutate(county.state = paste0(stringr::word(county, 1), ", ", abbr))
aoi3_gpm <- aoi3_gpm %>% mutate(county.state = paste0(stringr::word(county, 1), ", ", abbr))
aoi4_gpm <- aoi4_gpm %>% mutate(county.state = paste0(stringr::word(county, 1), ", ", abbr))

## map
study_area <- ggmap::ggmap(basemap_fixed)+
  geom_sf(data = us_cropped_gpm, col = alpha("gray", 0.3), fill = NA, inherit.aes = FALSE)+
  geom_sf(data = aoi1_gpm, fill = "red", alpha = 0.5, inherit.aes = FALSE)+
  geom_sf_label(data = aoi1_gpm, aes(label = county.state), col = "white", fill = NA, label.size = NA, nudge_x = 225000, nudge_y = -60000, inherit.aes = FALSE)+
  geom_sf(data = aoi2_gpm, fill = "red", alpha = 0.5, inherit.aes = FALSE)+
  geom_sf_label(data = aoi2_gpm, aes(label = county.state), col = "white", fill = NA, label.size = NA, nudge_x = 275000, nudge_y = 50000, inherit.aes = FALSE)+
  geom_sf(data = aoi3_gpm, fill = "red", alpha = 0.5, inherit.aes = FALSE)+
  geom_sf_label(data = aoi3_gpm, aes(label = county.state), col = "white", fill = NA, label.size = NA, nudge_x = 280000, nudge_y = -38000, inherit.aes = FALSE)+
  geom_sf(data = aoi4_gpm, fill = "red", alpha = 0.5, inherit.aes = FALSE) +
  geom_sf_label(data = aoi4_gpm, aes(label = county.state), col = "white", fill = NA, label.size = NA, nudge_x = 275000, nudge_y = -28000, inherit.aes = FALSE)+
  coord_sf(datum = NA)+
  theme(axis.title = element_blank(),
        text = element_text(size = 10))

study_area

ggsave(filename = "study_area_map_pre.png",
       plot = study_area, device = "png", path = "./output/manuscript",
       width = 3, height = 3, units = "in", dpi = 600)

### crop western edge in paint

### calculate distance between AOIs
distance1 <- purrr::map(aoi_list, function(x) sf::st_distance(x, aoi_list[[1]]))
distance2 <- purrr::map(aoi_list, function(x) sf::st_distance(x, aoi_list[[2]]))
distance3 <- purrr::map(aoi_list, function(x) sf::st_distance(x, aoi_list[[3]]))
distance4 <- purrr::map(aoi_list, function(x) sf::st_distance(x, aoi_list[[4]]))

distance <- data.frame(aoi1 = unlist(distance1),
                       aoi2 = unlist(distance2),
                       aoi3 = unlist(distance3),
                       aoi4 = unlist(distance4),
                       county = counties$counties)

colnames(distance) = c(counties$counties, "county")

distance <- distance %>%
  tidyr::pivot_longer(cols = 1:4) %>%
  dplyr::filter(value >0) %>%
  dplyr::arrange(value)



######## functions for plotting maps #########
sindex_plot <- function(data, variable, maxpixels = 3E6, plot_aoi = TRUE, aoi_lwd = 0.5, direction = 1){
  legend_name <- variable

  raster.to.plot <- data$spatial[[variable]]

  raster.to.plot2 <- terra::subst(raster.to.plot, NA, -11111)

  legend_name <- variable_lookup(data$rules) %>%
    dplyr::bind_rows(tibble::tibble(pkg_var = c("sindex", "sum"), variable = c("Suitability index", "sum"), layer = c("summary", "summary"))) %>%
    dplyr::filter(pkg_var == !!`variable`) %>%
    dplyr::select(variable) %>%
    dplyr::pull(.) %>%
    stringr::str_to_sentence(.)

  rcl.suit <- data.frame(from = c(-11111),
                         to   = c(-1),
                         becomes = c(NA))

  raster.to.plot.suit <- terra::classify(raster.to.plot2, rcl = rcl.suit, include.lowest = TRUE, others = NULL)
  raster.to.plot.uu <- terra::mask(raster.to.plot2, raster.to.plot.suit, inverse = TRUE)

  plot.suit <- rasterVis::gplot(raster.to.plot.suit, maxpixels = maxpixels)
  plot.uu <- rasterVis::gplot(raster.to.plot.uu, maxpixels = maxpixels)

  uu.fill <- ifelse(plot.uu$data$value > -10000, "#9C9C9C", ifelse(plot.uu$data$value == -10000, "#E4E4E4", ifelse(is.na(plot.uu$data$value), "#FFFFFF", "#FFFFFF")))
  uu.alpha <- ifelse(is.na(plot.uu$data$value), 0, 1)

  plot.out <- plot.suit +
    annotate(
      geom = "raster",
      x = plot.uu$data$x,
      y = plot.uu$data$y,
      fill = uu.fill,
      alpha = uu.alpha,
      na.rm = TRUE
    )+
    geom_raster(aes(fill = value), na.rm = TRUE) +
    geom_point(data = plot.uu$data %>% dplyr::mutate(col = as.factor(value)), aes(x = x, y = y, color = col), alpha = 0) +
    scale_fill_viridis_c(option = "A", na.value = "transparent", direction = direction, limits = c(0, ceiling(max(terra::values(data$spatial[[variable]], na.rm = TRUE))))) +
    scale_color_manual(breaks = c("-1", "-10000", "-11111"),
                       values = c("-1" = "#9C9C9C", "-10000" = "#E4E4E4", "-11111" = "#FFFFFF"),
                       labels = c("Unsuitable", "Undetermined", "Non-agricultural"),
                       guide = guide_legend(override.aes = list(alpha = 1,
                                                                shape = 22,
                                                                size = 7,
                                                                color = "black",
                                                                fill = c("#9C9C9C", "#E4E4E4", "#FFFFFF")),
                                            title = NULL)) +
    guides(fill = guide_colorbar(order = 1))

  plot.out <- plot.out +
    geom_sf(data = data$aoi, inherit.aes = FALSE, fill = "transparent", size = aoi_lwd, color = "black")

  plot.out <- plot.out +
    labs(fill = stringr::str_wrap(legend_name, width = 25)) +
    theme_void()
}

limitations_plot <- function(data, variable, maxpixels = 3E6, plot_aoi = TRUE, aoi_lwd = 0.5, direction = 1){
  legend_name <- variable

  LABELS <- c("Soil physical", "Hydrology", "Soil chemical", "Temperature", "Suitable - No limits", "Undetermined", "Non-agricultural")
  N.categories <- length(data$categories)
  limiting.breaks <- as.character(N.categories:-2)
  limiting.colors <- c(data$category.colors, "#9C9C9C", "#E4E4E4", "#FFFFFF")
  names(limiting.colors) <- limiting.breaks

  raster.to.plot <- data$spatial[[variable]]
  raster.to.plot2 <- terra::subst(raster.to.plot, NA, -2)

  legend_name <- "Limiting variable class"

  limit_colors <- ggplot2::scale_fill_manual(breaks = limiting.breaks,
                                             values = limiting.colors,
                                             labels = LABELS,
                                             na.value = "transparent",
                                             drop = FALSE,
                                             guide = guide_legend(override.aes = list(alpha = 1,
                                                                                      shape = 22,
                                                                                      #size = 8,
                                                                                      color = "black",
                                                                                      fill = limiting.colors)))

  plot.out <- rasterVis::gplot(raster.to.plot2, maxpixels = maxpixels) +
    geom_raster(aes(fill = factor(value, levels = limiting.breaks)), na.rm = TRUE) +
    limit_colors

  plot.out <- plot.out +
    geom_sf(data = data$aoi, inherit.aes = FALSE, fill = "transparent", size = aoi_lwd, color = "black")

  plot.out <- plot.out +
    labs(fill = stringr::str_wrap(legend_name, width = 25)) +
    theme_void()
}

############## Map Suitability #################
#### Chestnut Suitability
chestnut_criteria <- import_rules(paste0("chestnut_suitability"))

chestnut_suitability <- purrr::map(aoi_list, make_suitability_map,
                                   criteria = chestnut_criteria,
                                   species = "chestnut",
                                   export = TRUE,
                                   GDD_region = "CONUS",
                                   GDD_year = 2022,
                                   soil_raster = TRUE,
                                   soil_component_rule = 30,
                                   soil_max_depth = Inf,
                                   soil_range = "fullrange",
                                   mask_cdl = "ag",
                                   cdl_year = 2021,
                                   dl_dir = "./data-raw",
                                   out_dir = "./output")

names(chestnut_suitability) <- purrr::map(aoi_list, function(x) paste0(x$abbr, "_", x$county))

chestnut_suit_plots <- purrr::map(chestnut_suitability, "suitability") %>%
  purrr::map(., sindex_plot, variable = "sindex", maxpixels = 3E6, plot_aoi = TRUE, aoi_lwd = 0.5, direction = 1)

#chestnut_suit_plots

chestnut_limit_plots <- purrr::map(chestnut_suitability, "limitations") %>%
  purrr::map(., purrr::possibly(limitations_plot, otherwise = NA), variable = "most_limiting", plot_aoi = TRUE, aoi_lwd = 0.5, maxpixels = 3E6)

#chestnut_limit_plots

save_plots <- function(plot, location, species, variable){
  path = paste0("./output/", species, "/figures")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  ggplot2::ggsave(filename = paste0(species, "_", variable, "_", location, ".png"),
                  plot = plot, device = "png", path = path,
                  width = 6, height = 6, units = "in", dpi = 600)
}

purrr::walk2(chestnut_suit_plots, names(chestnut_suit_plots), save_plots, species = "chestnut", variable = "sindex")
purrr::walk2(chestnut_limit_plots, names(chestnut_limit_plots), save_plots, species = "chestnut", variable = "limitations")

#### Apple Suitability
apple_criteria <- import_rules(paste0("apple_suitability"))

apple_suitability <- purrr::map(aoi_list, make_suitability_map,
                                criteria = apple_criteria,
                                species = "apple",
                                export = TRUE,
                                GDD_region = "CONUS",
                                GDD_year = 2022,
                                soil_raster = TRUE,
                                soil_component_rule = 30,
                                soil_max_depth = Inf,
                                soil_range = "fullrange",
                                mask_cdl = "ag",
                                cdl_year = 2021,
                                dl_dir = "./data-raw",
                                out_dir = "./output")

names(apple_suitability) <- purrr::map(aoi_list, function(x) paste0(x$abbr, "_", x$county))

apple_suit_plots <- purrr::map(apple_suitability, "suitability") %>%
  purrr::map(., sindex_plot, variable = "sindex", maxpixels = 3E6, plot_aoi = TRUE, aoi_lwd = 0.5, direction = 1)

#apple_suit_plots

apple_limit_plots <- purrr::map(apple_suitability, "limitations") %>%
  purrr::map(., limitations_plot, variable = "most_limiting", plot_aoi = TRUE, aoi_lwd = 0.5, maxpixels = 3E6)

#apple_limit_plots

purrr::walk2(apple_suit_plots, names(apple_suit_plots), save_plots, species = "apple", variable = "sindex")
purrr::walk2(apple_limit_plots, names(apple_limit_plots), save_plots, species = "apple", variable = "limitations")

###### Suitability Mapping Figures

############ Figure 3 ################

#### barplot with suitability score for each county x species x SL combo
### summarize individual suitability layers
chestnut.suitability.layers.summary <- purrr::map2(chestnut_suitability, names(chestnut_suitability), function(x, y) x$suitability$suitability.layers.score %>%
                                                     dplyr::mutate(location = y)) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(species = "chestnut")

apple.suitability.layers.summary <- purrr::map2(apple_suitability, names(apple_suitability), function(x, y) x$suitability$suitability.layers.score %>%
                                                  dplyr::mutate(location = y)) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(species = "apple")

suitability.layers.summary <- dplyr::bind_rows(chestnut.suitability.layers.summary, apple.suitability.layers.summary) %>%
  dplyr::mutate(score = factor(value,
                               levels = c(1, 0, -100, -10000),
                               labels = c("Ideal", "Suitable", "Unsuitable", "Undetermined"))) %>%
  tidyr::pivot_longer(!c(location, species, value, score), values_to = "pixels") %>%
  dplyr::rename(variable = name) %>%
  dplyr::mutate(location = factor(location,
                                  levels = c("MI_Ionia County", "WI_Sauk County", "IL_Fulton County", "IL_White County"),
                                  labels = c("Ionia County", "Sauk County", "Fulton County", "White County")),
                variable = factor(variable,
                                  levels = c("ppt", "min_temp", "gdd", "drainagecl", "ph1to1h2o", "soil_depth", "texture"),
                                  labels = c("annual precip", "min. temp", "gdd", "soil drainage", "soil pH", "soil depth", "soil texture")),
                species = factor(species,
                                 levels = c("apple", "chestnut"),
                                 labels = c("Apple", "Chestnut")))

### export spreadsheet
write.csv(suitability.layers.summary, file = "./output/manuscript/suitability_layers_summary_20230327.csv", row.names = FALSE)

### option to upload data if they already exist
suitability.layers.summary <- readr::read_csv(file = "./output/manuscript/suitability_layers_summary_20230327.csv") %>%
  dplyr::mutate(location2 = ifelse(location == "Ionia County", "Ionia, MI",
                                   ifelse(location == "Sauk County", "Sauk, WI",
                                          ifelse(location == "Fulton County", "Fulton, IL", "White, IL")))) %>%
  dplyr::mutate(score = factor(score,
                               levels = c("Ideal", "Suitable", "Unsuitable", "Undetermined")),
                location2 = factor(location2,
                                   levels = c("Ionia, MI", "Sauk, WI", "Fulton, IL", "White, IL")),
                variable = factor(variable,
                                  levels = c("annual precip", "min. temp", "gdd", "soil drainage", "soil pH", "soil depth", "soil texture")),
                species = factor(species,
                                 levels = c("Apple", "Chestnut")))

### individual layers suitability plot
fill_suitability_scale <- c("#541352", "#2F9AA0", "#9C9C9C", "#E4E4E4")
names(fill_suitability_scale) <- levels(suitability.layers.summary$score)

plot1 <- suitability.layers.summary %>%
  dplyr::filter(score != "Undetermined") %>%
  dplyr::mutate(score = droplevels(score)) %>%
  ggplot2::ggplot(aes(x = variable, y = pixels, fill = score))+
  geom_bar(stat = "identity", position = "fill")+
  facet_grid(species~location2)+
  ylab("Proportion of AOI")+
  xlab("Suitability layer")+
  scale_fill_manual(name = stringr::str_wrap("Suitability layer score", width = 15),
                    values = fill_suitability_scale) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust = 0.5)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.background = element_rect(fill = NA),
        #axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 10))

plot1

ggplot2::ggsave(filename = "suitability_layers_summary_fig2.png",
                plot = plot1, device = "png", path = "./output/manuscript",
                width = 7, height = 4, units = "in", dpi = 600)

############## Figure 5 ##############

#### barplot with sindex score summary for each county x species
### summarize sindex
chestnut.sindex.summary <- purrr::map2(chestnut_suitability, names(chestnut_suitability), function(x, y) x$suitability$sindex.score %>%
                                         dplyr::mutate(location = y)) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(species = "chestnut")

apple.sindex.summary <- purrr::map2(apple_suitability, names(apple_suitability), function(x, y) x$suitability$sindex.score %>%
                                      dplyr::mutate(location = y)) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(species = "apple")

sindex.summary <- dplyr::bind_rows(chestnut.sindex.summary, apple.sindex.summary) %>%
  dplyr::mutate(s.index = factor(cut(value,
                                     breaks = c(-10000, -1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
                                     labels = c("Undetermined", "Unsuitable", "0.0 - 0.1", "0.1 - 0.2", "0.2- 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0"),
                                     include.lowest = TRUE, right = FALSE),
                                 levels = c("Undetermined", "Unsuitable", "0.0 - 0.1", "0.1 - 0.2", "0.2- 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0"))) %>%
  dplyr::group_by(species, location, s.index) %>%
  dplyr::mutate(pixels = sum(count)) %>%
  dplyr::select(-c(layer, value, count)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(species, location) %>%
  dplyr::mutate(prop = pixels/sum(pixels)) %>%
  dplyr::mutate(suitable = factor(ifelse(s.index == "Undetermined", "Undetermined",
                                         ifelse(s.index == "Unsuitable", "Unsuitable", "Suitable")),
                                  levels = c("Undetermined", "Unsuitable", "Suitable")),
                location = factor(location,
                                  levels = c("MI_Ionia County", "WI_Sauk County", "IL_Fulton County", "IL_White County"),
                                  labels = c("Ionia County", "Sauk County", "Fulton County", "White County")),
                species = factor(species,
                                 levels = c("apple", "chestnut"),
                                 labels = c("Apple", "Chestnut")))

### export spreadsheet
write.csv(sindex.summary, file = "./output/manuscript/sindex_summary_20230327.csv", row.names = FALSE)

### option to upload data if they already exist
sindex.summary <- readr::read_csv(file = "./output/manuscript/sindex_summary_20230327.csv") %>%
  dplyr::mutate(location2 = ifelse(location == "Ionia County", "Ionia, MI",
                                   ifelse(location == "Sauk County", "Sauk, WI",
                                          ifelse(location == "Fulton County", "Fulton, IL", "White, IL")))) %>%
  dplyr::mutate(s.index = factor(s.index,
                                 levels = c("Undetermined", "Unsuitable", "0.0 - 0.1", "0.1 - 0.2", "0.2- 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0")),
                suitable = factor(suitable,
                                  levels = c("Undetermined", "Unsuitable", "Suitable")),
                location2 = factor(location2,
                                   levels = c("Ionia, MI", "Sauk, WI", "Fulton, IL", "White, IL")),
                species = factor(species,
                                 levels = c("Apple", "Chestnut")))

### plot sindex summary
fill_viridis_scale <- c("#E4E4E4", "#9C9C9C", rev(viridis::magma(n = 10, direction = -1)))
names(fill_viridis_scale) <- levels(sindex.summary$s.index)

plot2 <- sindex.summary %>%
  dplyr::filter(suitable != "Undetermined") %>%
  dplyr::mutate(suitable = droplevels(suitable)) %>%
  ggplot(aes(x = suitable, y = prop, fill = s.index))+
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE))+
  facet_grid(species~location2)+
  ylab("Proportion of AOI")+
  #theme_classic()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.background = element_rect(fill = NA),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text = element_text(size = 9),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 10))+
  scale_fill_manual(name = stringr::str_wrap("Suitability index", width = 15), values = fill_viridis_scale, limits = c("0.0 - 0.1", "0.1 - 0.2", "0.2- 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0"))+
  guides(fill = guide_legend(reverse=T))

plot2

ggplot2::ggsave(filename = "sindex_summary_fig2.png",
                plot = plot2, device = "png", path = "./output/manuscript",
                width = 7, height = 4, units = "in", dpi = 600)

### export spreadsheet with unsuitable values
chestnut.unsuitable.values.summary <- purrr::map2(chestnut_suitability, names(chestnut_suitability), function(x, y) x$suitability$unsuitable.values %>%
                                                    dplyr::mutate(location = y)) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(species = "chestnut")

apple.unsuitable.values.summary <- purrr::map2(apple_suitability, names(apple_suitability), function(x, y) x$suitability$unsuitable.values %>%
                                                 dplyr::mutate(location = y)) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(species = "apple")

unsuitable.values.summary <- dplyr::bind_rows(chestnut.unsuitable.values.summary, apple.unsuitable.values.summary)

write.csv(unsuitable.values.summary, file = "./output/manuscript/unsuitable_values_summary_20230327.csv", row.names = FALSE)


############## Figure 4 ################
#### suitability maps
### create an image with suitability maps together #####
chestnut_suit_plots2 <- purrr::map(chestnut_suit_plots, function(x) x2 <- x +
                                     theme(legend.position = "none",
                                           plot.margin = unit(c(0, 0, 0, 0), "cm")))
apple_suit_plots2 <- purrr::map(apple_suit_plots, function(x) x2 <- x +
                                  theme(legend.position = "none",
                                        plot.margin = unit(c(0, 0, 0, 0), "cm")))

suit_legend <- cowplot::get_legend(apple_suit_plots2[[1]] +
                                     theme(legend.position = "bottom", legend.direction = "vertical",
                                           legend.text = element_text(size = 10),
                                           legend.title = element_text(size = 10),
                                           legend.key.height = unit(0.3, "cm")))

chestnut_suit <- cowplot::plot_grid(plotlist = chestnut_suit_plots2, ncol = 1)
apple_suit <- cowplot::plot_grid(plotlist = apple_suit_plots2, ncol = 1)

### add county names
apple_suit_labels <- apple_suit +
  draw_label("Ionia, MI", x = 0.1, y = 0.87, size = 10) +
  draw_label("Sauk, WI", x = 0.1, y = 0.61, size = 10) +
  draw_label("Fulton, IL", x = 0.1, y = 0.38, size = 10) +
  draw_label("White, IL", x = 0.1, y = 0.11, size = 10)
apple_suit_labels

### add species names
title_chestnut <- ggdraw()+
  draw_label("Chestnut", size = 10)

title_apple <- ggdraw()+
  draw_label("Apple", size = 10)

chestnut_suit_title <- plot_grid(title_chestnut, chestnut_suit, ncol = 1, rel_heights = c(0.05, 1))
chestnut_suit_title

apple_suit_title <- plot_grid(title_apple, apple_suit_labels, ncol = 1, rel_heights = c(0.05, 1))
apple_suit_title

suit_plots <- cowplot::plot_grid(apple_suit_title, chestnut_suit_title, ncol = 2)
suit_plots

suit_plotsL <- cowplot::plot_grid(suit_plots, suit_legend, ncol = 1, rel_heights = c(1, 0.2))
suit_plotsL

ggplot2::ggsave(filename = "chestnut_apple_suitability_maps2.png",
                plot = suit_plotsL, device = "png", path = "./output/manuscript",
                width = 6, height = 9.5, units = "in", dpi = 600)


################# Figure 6 ###################
### Limitations maps
chestnut_limit_plots2 <- purrr::map(chestnut_limit_plots, function(x) x2 <- x +
                                      theme(legend.position = "none",
                                            plot.margin = unit(c(0, 0, 0, 0), "cm")))
apple_limit_plots2 <- purrr::map(apple_limit_plots, function(x) x2 <- x +
                                   theme(legend.position = "none",
                                         plot.margin = unit(c(0, 0, 0, 0), "cm")))

limit_legend <- cowplot::get_legend(apple_limit_plots2[[1]] +
                                      guides(fill = guide_legend(ncol=2)) +
                                      theme(legend.position = "bottom", legend.direction = "vertical",
                                            legend.text = element_text(size = 10),
                                            legend.title = element_text(size = 10)))

chestnut_limit <- cowplot::plot_grid(plotlist = chestnut_limit_plots2, ncol = 1)
apple_limit <- cowplot::plot_grid(plotlist = apple_limit_plots2, ncol = 1)

### add county names
apple_limit_labels <- apple_limit +
  draw_label("Ionia, MI", x = 0.1, y = 0.87, size = 10) +
  draw_label("Sauk, WI", x = 0.1, y = 0.61, size = 10) +
  draw_label("Fulton, IL", x = 0.1, y = 0.38, size = 10) +
  draw_label("White, IL", x = 0.1, y = 0.11, size = 10)
apple_limit_labels

### add species names
title_chestnut <- ggdraw()+
  draw_label("Chestnut", size = 10)

title_apple <- ggdraw()+
  draw_label("Apple", size = 10)

chestnut_limit_title <- plot_grid(title_chestnut, chestnut_limit, ncol = 1, rel_heights = c(0.05, 1))
chestnut_limit_title

apple_limit_title <- plot_grid(title_apple, apple_limit_labels, ncol = 1, rel_heights = c(0.05, 1))
apple_limit_title

limit_plots <- cowplot::plot_grid(apple_limit_title, chestnut_limit_title, ncol = 2)
limit_plots

limit_plotsL <- cowplot::plot_grid(limit_plots, limit_legend, ncol = 1, rel_heights = c(1, 0.2))
limit_plotsL

ggplot2::ggsave(filename = "chestnut_apple_limitations_maps.png",
                plot = limit_plotsL, device = "png", path = "./output/manuscript",
                width = 6, height = 9.5, units = "in", dpi = 600)


########## Accuracy Assessments ##########
### functions for running automated accuracy assessments
cdl_pixel_clusters <- function(county = NULL, state, species, cdl_year = 2021, neighbors = 4, filter_pixels = NULL, dl_dir = "./data-raw"){
  if(!is.null(county)){
    aoi <- make_county_AOI(state = state, counties = county)

    download_cdl(aoi = aoi, year = cdl_year, dl_dir = dl_dir)
    cdl <- import_cdl(aoi = aoi, year = cdl_year, dl_dir = dl_dir)
  } else {
    download_cdl(aoi = state, year = cdl_year, dl_dir = dl_dir)
    cdl <- import_cdl(aoi = state, year = cdl_year, dl_dir = dl_dir)
  }

  cdl_masked <- cdl_mask(cdl, cdl, keep = species)

  if (terra::freq(cdl_masked$spatial, value = NA)$count == nrow(cdl_masked$spatial)*ncol(cdl_masked$spatial)){
    cdl_patch_freq <- NULL
    cdl_patch_poly <- NULL
  } else {
    cdl_patch <- terra::patches(cdl_masked$spatial, directions = neighbors, allowGaps = FALSE)
    cdl_patch_freq <- tibble::as_tibble(terra::freq(cdl_patch)) %>%
      dplyr::select(-layer) %>%
      dplyr::rename(ID = value) %>%
      dplyr::mutate(state = state) %>%
      dplyr::mutate(county = county) %>%
      dplyr::rename(pixels = count)

    ### turn pixel patches into polygons
    cdl_patch_poly <- terra::as.polygons(cdl_patch)

    cdl_patch_poly <- cdl_patch_poly %>%
      sf::st_as_sf() %>%
      dplyr::rename(ID = patches) %>%
      dplyr::left_join(cdl_patch_freq, by = "ID")

    if (is.numeric(filter_pixels)){
      cdl_patch_poly <- cdl_patch_poly %>%
        dplyr::filter(pixels >= !!filter_pixels)
    }
  }

  return(cdl_patch_poly)
}

state_cdl_pixel_clusters <- function(state, species, cdl_year = 2021, neighbors = 4, filter_pixels = NULL, dl_dir = "./data-raw"){
  all.counties <- fips() %>%
    dplyr::filter(abbr == !!`state`) %>%
    .$fips.county

  state_patches <- purrr::map(all.counties, cdl_pixel_clusters, state = state, species = species, cdl_year = cdl_year, neighbors = neighbors, filter_pixels = filter_pixels, dl_dir = dl_dir)

  state_poly <- state_patches %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(co_ID = paste(county, ID, sep = "-"))
}

cdl_aoi_filters <- function(pts, cdl_poly, radius = 400, pixels = 100){
  cdl_poly_filter <- cdl_poly %>%
    dplyr::filter(pixels >= !!pixels)

  cdl_near_pts <- sf::st_is_within_distance(pts, cdl_poly_filter, radius)

  cdl_near_pts_df <- cdl_near_pts %>%
    purrr::compact(.) %>%
    purrr::map_df(tibble::as_tibble) %>%
    dplyr::distinct() %>%
    dplyr::arrange()

  ### filter out all patches made up of less than 100 pixels
  poly_near_pts <- cdl_poly_filter %>%
    dplyr::slice(cdl_near_pts_df$value) %>%
    dplyr::mutate(acres = pixels*(30*30)*0.000247105) %>%
    dplyr::mutate(hectares = pixels*(30*30)*0.0001)

  ### summarize final selection
  summary <- poly_near_pts %>%
    summarise(sum_pixels = sum(pixels),
              min_pixels = min(pixels),
              max_pixels = max(pixels),
              sum_acres = sum(acres),
              min_acres = min(acres),
              max_acres = max(acres),
              sum_hectares = sum(hectares),
              min_hectares = min(hectares),
              max_hectares = max(hectares))

  cdl_aois <- list(spatial = poly_near_pts,
                   summary = summary)

  return(cdl_aois)
}

#### Apple Accuracy ####
top5 <- c("WA", "NY", "MI", "PA", "CA") ## Top 5 apple producing states, confirmed using NASS Quick stats for 2018-2020

## get CDL apple data
apple_top5 <- purrr::map(top5, state_cdl_pixel_clusters, species = "apple", filter_pixels = 10)
names(apple_top5) <- top5

apple_top5_all <- bind_rows(apple_top5) %>%
  dplyr::mutate(state_ID = paste(state, co_ID, sep = "-"))

## bring in apple orchard data
pm_apples_sp <- sf::st_read("./input/orchards.kml")

## filter CDL data using PM.org and cluster size data
apple_top5_filter <- cdl_aoi_filters(pts = pm_apples_sp, cdl_poly = apple_top5_all, radius = 400, pixels = 100)

sf::st_write(apple_top5_filter$spatial, dsn = "./output/apple_top5_cdl_pm_spatial_20230307.shp")
write.csv(apple_top5_filter$summary, file = "./output/apple_top5_cdl_pm_summary_20230307.csv")

## run suitability analysis
#apple_top5_poly <- sf::st_read(dsn = "./output/apple_top5_cdl_pm_spatial_20230307.shp")

apple_top5_list <- setNames(split(apple_top5_poly, seq(nrow(apple_top5_poly))), apple_top5_poly$state_ID)
names(apple_top5_list)

# NOTE: This takes about an hour to run
start <- proc.time()[3]
apple_assess_raster_spatial <- purrr::map2(apple_top5_list, names(apple_top5_list), purrr::possibly(assess_criteria, otherwise = NA),
                                           species = "apple",
                                           soil_range = "fullrange",
                                           component_rule = 30,
                                           soil_raster = TRUE,
                                           spatial = TRUE,
                                           cdl_year = 2021,
                                           GDD_region  = "CONUS",
                                           GDD_year = 2022,
                                           mask = NULL)
elapsed <- proc.time()[3] - start
print(paste("Completed in", round(elapsed / 60, 1), "minutes"))

## summarize results
apple_assess_raster_spatial_score <- purrr::map(apple_assess_raster_spatial, "raw.score") %>%
  dplyr::bind_rows()

apple_assess_raster_spatial_summary <- purrr::map(apple_assess_raster_spatial, "summary") %>%
  dplyr::bind_rows()

apple_assess_raster_spatial_soil.unsuitable <- purrr::map(apple_assess_raster_spatial, "soil.unsuitable") %>%
  dplyr::bind_rows()

## export summaries
write_csv(apple_assess_raster_spatial_score, "./output/apple/accuracy/apple_top5_CDL_assess_raster_spatial_rawscore.csv")
write_csv(apple_assess_raster_spatial_summary, "./output/apple/accuracy/apple_top5_CDL_assess_raster_spatial_summary.csv")
write_csv(apple_assess_raster_spatial_soil.unsuitable, "./output/apple/accuracy/apple_top5_CDL_assess_raster_spatial_unsuitable.csv")

#### Chestnut Accuracy ####
## read in known site data
chestnuts <- sf::st_read("./input/chestnuts.kml")

chestnut_list <- setNames(split(chestnuts, seq(nrow(chestnuts))), chestnuts$Name)

## run suitability analysis
start <- proc.time()[3]
chestnut_assess_raster_spatial <- purrr::map2(chestnut_list, names(chestnut_list), purrr::possibly(assess_criteria, otherwise = NA), species = "chestnut", soil_range = "fullrange", component_rule = 30, soil_raster = TRUE, spatial = TRUE, cdl_year = 2021, GDD_region  = "CONUS", GDD_year = 2022, mask = NULL)
elapsed <- proc.time()[3] - start
print(paste("Completed in", round(elapsed / 60, 1), "minutes"))

## summarize results
chestnut_assess_raster_spatial_score <- purrr::map(chestnut_assess_raster_spatial, "raw.score") %>%
  dplyr::bind_rows()

chestnut_assess_raster_spatial_summary <- purrr::map(chestnut_assess_raster_spatial, "summary") %>%
  dplyr::bind_rows()

chestnut_assess_raster_spatial_soil.unsuitable <- purrr::map(chestnut_assess_raster_spatial, "soil.unsuitable") %>%
  dplyr::bind_rows()

## export summaries
write_csv(chestnut_assess_raster_spatial_score, "./output/chestnut/accuracy/chestnut_assess_raster_spatial_rawscore.csv")
write_csv(chestnut_assess_raster_spatial_summary, "./output/chestnut/accuracy/chestnut_assess_raster_spatial_summary.csv")
write_csv(chestnut_assess_raster_spatial_soil.unsuitable, "./output/chestnut/accuracy/chestnut_assess_raster_spatial_unsuitable.csv")


################# Figure 7 ######################
#### Plot Results ####
apple_assess_raster_spatial_score <- read_csv("./output/apple/accuracy/apple_top5_CDL_assess_raster_spatial_rawscore.csv")

apple_summary <- apple_assess_raster_spatial_score %>%
  dplyr::mutate(s.index = factor(cut(sindex,
                                     breaks = c(-10000, -1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
                                     labels = c("Undetermined", "Unsuitable", "0.0 - 0.1", "0.1 - 0.2", "0.2- 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0"),
                                     include.lowest = TRUE, right = FALSE),
                                 levels = c("Undetermined", "Unsuitable", "0.0 - 0.1", "0.1 - 0.2", "0.2- 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0"))) %>%
  dplyr::count(s.index) %>%
  dplyr::mutate(prop = n/sum(n)) %>%
  dplyr::mutate(species = "Apple") %>%
  dplyr::mutate(suitable = factor(ifelse(s.index == "Undetermined", "Undetermined",
                                         ifelse(s.index == "Unsuitable", "Unsuitable", "Suitable")),
                                  levels = c("Undetermined", "Unsuitable", "Suitable")))

### remove WA data
apple_score_minusWA <- apple_assess_raster_spatial_score %>%
  dplyr::mutate(state = stringr::str_extract(location, "\\b[A-z]{2}")) %>%
  dplyr::filter(state != "WA")

apple_pixel_summary <- apple_score_minusWA %>%
  dplyr::group_by(location) %>%
  dplyr::summarise(pixels = n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(hectares = sum(pixels)*(30*30)*0.0001,
                pixels = sum(pixels))

apple_summary <- apple_score_minusWA %>%
  dplyr::mutate(s.index = factor(cut(sindex,
                                     breaks = c(-10000, -1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
                                     labels = c("Undetermined", "Unsuitable", "0.0 - 0.1", "0.1 - 0.2", "0.2- 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0"),
                                     include.lowest = TRUE, right = FALSE),
                                 levels = c("Undetermined", "Unsuitable", "0.0 - 0.1", "0.1 - 0.2", "0.2- 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0"))) %>%
  dplyr::count(s.index) %>%
  dplyr::mutate(prop = n/sum(n)) %>%
  dplyr::mutate(species = "Apple") %>%
  dplyr::mutate(suitable = factor(ifelse(s.index == "Undetermined", "Undetermined",
                                         ifelse(s.index == "Unsuitable", "Unsuitable", "Suitable")),
                                  levels = c("Undetermined", "Unsuitable", "Suitable")))


chestnut_assess_raster_spatial_score <- read_csv("./output/chestnut/accuracy/chestnut_assess_raster_spatial_rawscore.csv")

chestnut_summary <- chestnut_assess_raster_spatial_score %>%
  dplyr::mutate(s.index = factor(cut(sindex,
                                     breaks = c(-10000, -1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
                                     labels = c("Undetermined", "Unsuitable", "0.0 - 0.1", "0.1 - 0.2", "0.2- 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0"),
                                     include.lowest = TRUE, right = FALSE),
                                 levels = c("Undetermined", "Unsuitable", "0.0 - 0.1", "0.1 - 0.2", "0.2- 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0"))) %>%
  dplyr::count(s.index) %>%
  dplyr::mutate(prop = n/sum(n)) %>%
  dplyr::mutate(species = "Chestnut") %>%
  dplyr::mutate(suitable = factor(ifelse(s.index == "Undetermined", "Undetermined",
                                         ifelse(s.index == "Unsuitable", "Unsuitable", "Suitable")),
                                  levels = c("Undetermined", "Unsuitable", "Suitable")))

chestnut_pixel_summary <- chestnut_assess_raster_spatial_score %>%
  dplyr::group_by(location) %>%
  dplyr::summarise(pixels = n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(hectares = sum(pixels)*(30*30)*0.0001,
                pixels = sum(pixels))


combine_summary <- dplyr::bind_rows(apple_summary, chestnut_summary)

### accuracy plot
fill_viridis_scale <- c("#E4E4E4", "#9C9C9C", rev(viridis::magma(n = 10, direction = -1)))
names(fill_viridis_scale) <- levels(combine_summary$s.index)

accuracy_plot <- combine_summary %>%
  dplyr::filter(suitable != "Undetermined") %>%
  dplyr::mutate(suitable = droplevels(suitable)) %>%
  ggplot(aes(x = suitable, y = prop, fill = s.index))+
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE))+
  facet_grid(~species)+
  ylab("Proportion of known site area")+
  #theme_classic()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        panel.background = element_rect(fill = NA))+
  theme(axis.title.x=element_blank())+
  scale_fill_manual(name = stringr::str_wrap("Suitability index", width = 15), values = fill_viridis_scale, limits = c("0.0 - 0.1", "0.1 - 0.2", "0.2- 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6", "0.6 - 0.7", "0.7 - 0.8", "0.8 - 0.9", "0.9 - 1.0"))+
  guides(fill = guide_legend(reverse=T))

accuracy_plot

ggplot2::ggsave(filename = "accuracy_plot2.png",
                plot = accuracy_plot, device = "png", path = "./output/manuscript",
                width = 5, height = 3, units = "in", dpi = 600)


