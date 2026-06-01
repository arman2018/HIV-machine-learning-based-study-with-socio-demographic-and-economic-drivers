library(sf)
library(dplyr)
library(readxl)
library(ggplot2)
library(stringr)
library(patchwork)

df <- read_excel("C:/Users/hp/OneDrive/Desktop/HIV/Case range Map.xlsx")

df <- df %>%
  mutate(
    Year = as.integer(Year),
    Division = str_trim(as.character(Division)),
    Infected = as.numeric(Infected)
  )

bd_map <- st_read("C:/Users/hp/OneDrive/Desktop/Multi- Diseases/Airborne/Shapefile/gadm41_BGD_1.shp")

bd_map <- bd_map %>% 
  st_transform(4326) %>%
  mutate(Division = str_trim(as.character(NAME_1)))

map_data <- bd_map %>%
  left_join(df, by = "Division")

make_map <- function(yr, label, hide_x = FALSE, hide_y = FALSE) {
  
  p <- ggplot(data = filter(map_data, Year == yr)) +
    geom_sf(aes(fill = Infected), color = "white", linewidth = 0.3) +
    geom_sf_text(aes(label = Division), size = 3.2, fontface = "bold", color = "black", check_overlap = TRUE) +
    scale_fill_gradient(
      low = "#FFF7CC",
      high = "#B30000",
      na.value = "grey90",
      name = "Case\nrange"
    ) +
    coord_sf(
      crs = st_crs(4326),
      xlim = c(88, 93),
      ylim = c(20.5, 26.8),
      expand = FALSE
    ) +
    labs(
      title = paste0(label, ": ", yr),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal(base_size = 19) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.title = element_text(face = "bold", size = 18), 
      legend.text = element_text(face = "bold", size = 15),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
  
  if (hide_x) {
    p <- p + theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    )
  }
  
  if (hide_y) {
    p <- p + theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank()
    )
  }
  
  return(p)
}


p2020 <- make_map(2020, "A", hide_x = TRUE, hide_y = FALSE)

p2021 <- make_map(2021, "B", hide_x = TRUE, hide_y = TRUE)

p2022 <- make_map(2022, "C", hide_x = TRUE, hide_y = TRUE)

p2023 <- make_map(2023, "D", hide_x = FALSE, hide_y = FALSE)

p2024 <- make_map(2024, "E", hide_x = FALSE, hide_y = TRUE)

p2025 <- make_map(2025, "F", hide_x = FALSE, hide_y = TRUE)


final_map <- wrap_plots(p2020, p2021, p2022, p2023, p2024, p2025, ncol = 3)

final_map

ggsave(
  "HIV_choropleth_map1.png",
  plot = final_map,
  width = 18,
  height = 12,
  dpi = 300
)