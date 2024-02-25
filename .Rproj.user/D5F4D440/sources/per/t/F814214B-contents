# Visualización de las temperaturas registradas en Palma durante los inviernos de 2022, 2023 y 2024
# añadiendo  también los meses de noviembre y diciembre para dar más contexto al cambio de temperaturas.
# Los datos han sido extraidos Agencia Estatal de Meteorología (aemet.es) el 25/02/2024 

# Este script emplea las fuentes de las familias Roboto y Lato, que pueden no estar accesibles en algunos sitemas
# se pueden descargar aquí
# https://fonts.google.com/specimen/Roboto
# https://fonts.google.com/specimen/Lato

# LIBRERIAS ---------------------------------------------------------------


library(tidyverse)
library(jsonlite)
library(tidyjson)
library(lubridate)
library(Cairo)
library(ggthemes)
library(ggtext)
library(extrafont)



# CONSTANTES --------------------------------------------------------------

color_fondo <- "#f2eae4"
color_titulo <- "#171717"
color_caption <- "#595959"
color_2022 <- "#b4b0ac"
color_2023 <- "#958b83"
color_2024 <- "#8a5b38"
color_subtitulos <- "#636363"
color_valores_eje <- "#707070"

fuente_titulo <- "Roboto Black"
fuente_subtitulos <- "Roboto Medium"
fuente_caption <- "Lato"

# CARGA DE DATOS ----------------------------------------------------------


temp <- jsonlite::read_json(here::here("raw_data", "datos_temp_aerop_palma24.json"))



# DATA WRANGLING ----------------------------------------------------------

# paso de lista a data.frame
dat <- temp |> spread_all()

dat <- dat |> 
  filter(month(fecha) >= 11 | month(fecha) <= 2)


dat <- dat |> 
  mutate(fecha = ymd(fecha),
         year = year(fecha), 
         
         # a fin de que las series estén alineadas en la misma fecha se transforman los años en los datos originales
         # que no pertenecen al último invierno
         
         fecha_new = if_else(month(fecha) >= 9 , 
                             fecha + years(year(today()) - year(fecha) - 1), 
                             fecha + years(year(today()) - year(fecha))), 
         
         # Las agrupaciones de las series en la visualización se hacen en "seasons"
         # una "season" queda acotada entre el primero de noviembere de un año y el último día de febrero del año siguiente
         season = if_else(month(fecha) >= 9 & month(fecha) <= 12, year + 1, year), 
         tmed = as.numeric(str_replace(tmed, ",", ".")))


# VISUALIZACIÓN -----------------------------------------------------------


dat |> 
  ggplot(aes(x = fecha_new, tmed)) + 
  
  # colores de fondo
  geom_rect(xmin = ymd(20231101), xmax = ymd(20231201), ymin = -1, ymax = 30, fill = "#e5ddd6", alpha = 0.6) +
  geom_rect(xmin = ymd(20231201), xmax = ymd(20240101), ymin = -1, ymax = 30, fill = "#e9e2dc", alpha = 0.5) +
  geom_rect(xmin = ymd(20240101), xmax = ymd(20240201), ymin = -1, ymax = 30, fill = "#e5ddd6", alpha = 0.6) + 
  geom_rect(xmin = ymd(20240201), xmax = ymd(20240229), ymin = -1, ymax = 30, fill = "#e9e2dc", alpha = 0.5) + 
  
  # series
  geom_line(aes(group = season, 
                color = as.factor(season)), 
            size = 0.2, lty = "dashed", 
            show.legend = F) + 
  
  geom_smooth(aes(color = as.factor(season), 
                  group = season),
              size = 0.6, se = F,
              show.legend = F,
              span = 0.5) + 
  
  # solsticio invierno
  geom_vline(xintercept = ymd(20231221), 
             color = "#8eb4b8", 
             size = 0.7, 
             lty = "dotted") + 
  
  geom_text(x = ymd(20231222), y = 21, 
            label = "Solsticio \nde invierno",
            color = "#607d80", 
            hjust = "left", 
            family = fuente_caption, 
            size = 3.5) + 
  
  # meses
  geom_text(data = data.frame(x = seq(ymd(20231115), ymd(20240215), by = "month"),
                              y = rep(4, 4),
                              mes = c("Noviembre", "Diciembre", "Enero", "Febrero")),
            
            aes(x = x, y = y,
                label = mes), 
            family = "Roboto Medium", 
            color = color_valores_eje, 
            size = 4) +
  
  
  labs(title = "Invierno en Palma en **<span style= 'color:#b4b0ac'> 2022 </span>**, **<span style= 'color:#958b83'> 2023 </span>** y **<span style = 'color:#8a5b38'> 2024 </span>**", 
       subtitle = "Series suavizadas (líneas sólidas) de las **temperaturas medias diaras** (líneas discontinuas)", 
       y = "Grados celsius", 
       caption = "Suavizado realizado mediante regresiones locales con un ancho de banda de 0.5
       \nFuente: Agencia Estatal de Meteorología. Estación B278 - Palma de Mallorca, Aeropuerto. Extracción del 25/02/2024
       \nVisualización: Martín Pons | @MartinPonsM") +
  
  scale_color_manual(values = c(color_2022, color_2023, color_2024)) +
  
  theme_tufte() + 
  theme(panel.background = element_rect(fill = color_fondo, color = color_fondo), 
        plot.background = element_rect(fill = color_fondo, color = color_fondo), 
        plot.title = element_textbox(family = fuente_titulo, size = 16, color = color_titulo, hjust = 0, margin = margin(l = 34)),
        plot.subtitle = element_markdown(family = fuente_subtitulos, size = 13, color = color_subtitulos, hjust = 0, margin = margin(l = 34, t = 6, b = 5)),
        plot.caption = element_textbox(family = fuente_caption, color = color_caption, size = 9, lineheight = 0.5, hjust = 0, margin = margin(l = 34)),
        axis.line = element_blank(), 
        axis.ticks = element_blank(), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_text(family = fuente_subtitulos, size = 13, color = color_subtitulos), 
        axis.text.y = element_text(family = fuente_subtitulos, size = 12, color = color_valores_eje, margin = margin(l = 5, r = -25))) 



ggsave(here::here("temperatura_invierno_palma.png"),
       device = png, 
       type = "cairo",
       dpi = 300, 
       width = 28.3, 
       height = 17.5, 
       units = "cm")