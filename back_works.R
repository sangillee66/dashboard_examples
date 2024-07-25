
# 지방소멸위험지수 ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(tmap)
library(ggspatial)
library(readxl)

# 데이터

data_sigungu <- read_rds("data_sigungu.rds")
data_sido <- read_rds("data_sido.rds")

sido_shp <- st_read("D:/My R/Korean Administrative Areas/행정구역 셰이프 파일/3 Generalization/2021_4Q/SIDO_2021_4Q_GEN_0050.shp", options = "ENCODING=CP949")
sigungu_shp <- st_read("D:/My R/Korean Administrative Areas/행정구역 셰이프 파일/3 Generalization/2021_4Q/SIGUNGU1_2021_4Q_GEN_0050.shp", options = "ENCODING=CP949")
sido_line_shp <- st_read("D:/My R/Korean Administrative Areas/행정구역 셰이프 파일/3 Generalization/2021_4Q/SIDO_Polyline_2021_4Q_GEN_0050.shp", options = "ENCODING=CP949")
bbox_seohae <- st_read("D:/My R/Korean Administrative Areas/행정구역 셰이프 파일/3 Generalization/2021_4Q/BBOX_Seohae3_2021_4Q_GEN_0050.shp", options = "ENCODING=CP949")
bbox_ulleung <- st_read("D:/My R/Korean Administrative Areas/행정구역 셰이프 파일/3 Generalization/2021_4Q/BBOX_Ulleung_2021_4Q_GEN_0050.shp", options = "ENCODING=CP949")

# 그래프

data_sido_new <- data_sido |> 
  mutate(
    index_class = case_when(
      index < 0.2 ~ "1",
      index >= 0.2 & index < 0.5 ~ "2",
      index >= 0.5 & index < 1.0 ~ "3",
      index >= 1.0 & index < 1.5 ~ "4",
      index >= 1.5 ~ "5"
    ),
    index_class = fct(index_class, levels = as.character(1:5))
  )

class_color <- c("1" = "#d7191c", "2" = "#fdae61",
                 "3" = "#ffffbf", "4" = "#a6d96a", 
                 "5" = "#1a9641")

P <- data_sido_new |> 
  ggplot(aes(x = index, y = fct_reorder(C1_NM, index))) +
  geom_col(aes(fill = index_class), show.legend = TRUE) +
  geom_text(aes(label = format(round(index, digits = 3), nsmall = 3)), hjust = -0.3, size = 5) +
  scale_x_continuous(limits = c(0, 1.5)) +
  scale_fill_manual(name = "지수값", 
                    labels = c("< 0.2", "0.2 ~ 0.5", "0.5 ~ 1.0", 
                               "1.0 ~ 1.5", ">= 1.5"), 
                    values = class_color, drop = FALSE) +
  labs(title = "인구소멸위험지수, 2022년", 
       x = "인구소멸위험지수", 
       y = "") +
  theme(
    title = element_text(size = 18), 
    axis.title = element_text(size = 17),
    axis.text = element_text(size = 15),
    legend.position = c(0.8, 0.4), 
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )
P   

ggsave(P, file = "pop_extinct.jpg", width = 14, height = 14 * 0.618, dpi = 600)


# 지도 1

sigungu_data <- sigungu_shp |> 
  left_join(
    data_sigungu, join_by(SGG1_CD == C1)
  )

data_ <- sigungu_data |> 
  mutate(
    index = format(index, digits = 4, nsmall = 4),
    my_tooltip = str_c("Name: ", SGG1_FNM, "\n Index: ", index)
  )

class_color <- c("1" = "#d7191c", "2" = "#fdae61",
                 "3" = "#ffffbf", "4" = "#a6d96a", 
                 "5" = "#1a9641")
sigungu_data <- sigungu_data |> 
  mutate(
    index = as.numeric(index)
  )

my_tmap <- tm_shape(sigungu_data) + 
  tm_polygons(
    col = "index",
    palette = class_color, 
    breaks = c(0, 0.2, 0.5, 1.0, 1.5, Inf), 
    labels = c("< 0.2", "0.2~0.5", "0.5~1.0", "1.0~1.5", ">= 1.5"),
    title = "지수"
  ) +
  tm_shape(sido_line_shp) + tm_lines(col = "black", lwd = 2) +
  tm_shape(bbox_seohae) + tm_lines(col = "black") +
  tm_shape(bbox_ulleung) + tm_lines(col = "black") + 
  tm_scale_bar(breaks = seq(0, 200, 50), text.size = 0.4, color.dark = "gray60", position = c(0.53, 0.01)) +
  tm_legend(
    legend.title.size = 1.5, 
    legend.text.size = 0.8, 
    legend.width = 0.7, 
    legend.position = c(0.75, 0.06)
  ) +
  tm_layout(
    title = "인구소멸위험지수, 2022", title.size = 1.8, title.position = c(0.02, 0.96), 
    inner.margins = c(0.05, 0.025, 0.07, 0.05)
  )
my_tmap

tmap_save(my_tmap, filename = "pop_extinct_map.jpg", height = 10, dpi = 600)

# 지도 2

sigungu_data <- sigungu_data |> 
  mutate(
    index_class = case_when(
      index < 0.2 ~ "1",
      index >= 0.2 & index < 0.5 ~ "2",
      index >= 0.5 & index < 1.0 ~ "3",
      index >= 1.0 & index < 1.5 ~ "4",
      index >= 1.5 ~ "5"
    ),
    index_class = fct(index_class, levels = as.character(1:5))
  )

class_color <- c("1" = "#d7191c", "2" = "#fdae61",
                 "3" = "#ffffbf", "4" = "#a6d96a", 
                 "5" = "#1a9641")
my_map <- ggplot() +
  geom_sf(data = sigungu_data, aes(fill = index_class), show.legend = TRUE) +
  geom_sf(data = sido_line_shp, lwd = 0.75) +
  geom_sf(data = bbox_seohae) +
  geom_sf(data = bbox_ulleung) +
  scale_fill_manual(name = "지수", 
                    labels = c("< 0.2", "0.2 ~ 0.5", "0.5 ~ 1.0", 
                               "1.0 ~ 1.5", ">= 1.5"), 
                    values = class_color, drop = FALSE) + 
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.82, 0.15), 
    legend.background = element_rect(fill = "white")
  ) +
  annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.5, size = 7,   
           label = "인구소멸위험지수, 2022") +
  annotation_scale(location = "br", bar_cols = c("gray40", "white"), width_hint = 0.4, pad_y = unit(0.2, "cm"))
my_map

ggsave(my_map, file = "pop_extinct_map_2.jpg", height = 10, dpi = 600)


# WPP 데이터 수정 --------------------------------------------------------------

wpp_2022 <- read_rds("wpp_2022.rds")

wpp_2022_new <- wpp_2022 |> 
  mutate(
    across(
      c(pop_jan_total, pop_jul_total, pop_jul_male, pop_jul_female, 
        natural_change, pop_change, births, deaths_total, 
        deaths_male, deaths_female, net_migrants), \(x) x * 1000
    )
  )

wpp_2022_new_2024 <- wpp_2022_new |> 
  filter(
    year == 2024
  )
  

write_rds(wpp_2022_new, "wpp_2022_new.rds")
write_rds(wpp_2022_new_2024, "wpp_2022_new_2024")


# 지역 코드 결합 ----------------------------------------------------------------

world_region_code <- read_excel("World_Region_Code.xlsx", sheet = 1, col_names = TRUE)
wpp_2022_new_region <- wpp_2022_new |> 
  left_join(
    world_region_code, join_by(location_code == `M49 Code`)
  )

write_rds(wpp_2022_new_region, "wpp_2022_new_region.rsd")


# 국가만 ---------------------------------------------------------------------

wpp_2022_new_region_only_country <- wpp_2022_new_region |> 
  filter(
    type == "Country/Area"
  )

write_rds(wpp_2022_new_region_only_country, "wpp_2022_new_region_only_country.rds")



# 시군구 인구소멸지수 지도(quarto) -----------------------------------------------------------

## Row

```{r}
#| output: false
library(tmap)

sido_shp <- st_read("sido_line.shp", options = "ENCODING=CP949")
sigungu_shp <- st_read("sigungu.shp", options = "ENCODING=CP949")
data_sigungu <- read_rds("data_sigungu.rds")

```

```{r}
#| title: 우리나라 시군구 단위 인구소멸위험지수(2022년)
sigungu_data <- sigungu_shp |> 
  left_join(
    data_sigungu, join_by(SGG1_CD == C1)
  )
sigungu_data <- sigungu_data |> 
  mutate(
    index_class = case_when(
      index < 0.2 ~ "1",
      index >= 0.2 & index < 0.5 ~ "2",
      index >= 0.5 & index < 1.0 ~ "3",
      index >= 1.0 & index < 1.5 ~ "4",
      index >= 1.5 ~ "5"
    ),
    index_class = fct(index_class, levels = as.character(1:5))
  )

class_color <- c("1" = "#d7191c", "2" = "#fdae61",
                 "3" = "#ffffbf", "4" = "#a6d96a", 
                 "5" = "#1a9641")
class_color <- c("1" = "#d7191c", "2" = "#fdae61",
                 "3" = "#ffffbf", "4" = "#a6d96a", 
                 "5" = "#1a9641")
sigungu_data <- sigungu_data |> 
  mutate(
    index = as.numeric(index)
  )
tmap_mode(mode = "view")


my_tmap <- tm_shape(sigungu_data) + 
  tm_polygons(
    col = "index",
    palette = class_color, 
    breaks = c(0, 0.2, 0.5, 1.0, 1.5, Inf), 
    labels = c("< 0.2", "0.2~0.5", "0.5~1.0", "1.0~1.5", ">= 1.5"),
    title = "Classes", 
    popup.vars=c("지역소멸위험지수: " = "index"), 
    popup.format = list(index = list(digits = 3)), 
    id = "SGG1_FNM", 
    alpha = 0.6, 
    border.alpha = 0.5
  ) +
  tm_shape(sido_shp) + tm_lines(lwd = 2)
my_tmap

```

