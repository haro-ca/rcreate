# Librarías ====
library(tidyverse)
library(lubridate)
library(ggtext)

# Lectura ====
imss_raw <- read_csv('asegurados_por_mes.csv') %>% 
  janitor::clean_names()

# Limpieza ====
try(Sys.setlocale("LC_ALL","es_ES"))
imss <- imss_raw %>% 
  extract(asegurados_asociados_a_un_empleo_empleos_asegurados, 
          into = 'codigo_de_empleo',
          regex = '(\\d+)', 
          remove = F) %>% 
  filter(codigo_de_empleo == 1268) %>% 
  mutate(fecha = parse_date(x2, format = '%Y/%b'), 
         cambio_porc = ((nacional / lead(nacional)) - 1) * 100) %>% 
  filter(year(fecha) >= 2000) %>% 
  arrange(desc(fecha)) %>% 
  select(fecha, asegurados = nacional, cambio_porc)

# Chart  ====
# Parámetros gráficos ----
rojo_fuerte <- '#BD343E'
rojo <- '#FFA9A0'
gris <- '#cecece'

# Frames intermedios ----
ribbon_frame <- imss %>% 
  group_by(mes = month(fecha)) %>% 
  mutate(mediana = median(cambio_porc[year(fecha) < 2020], na.rm = T)) %>% 
  filter(year(fecha) == 2020)

frame_de_mediana <- imss %>%
  filter(year(fecha) != 2020) %>% 
  group_by(mes = month(fecha)) %>% 
  summarise(mediana = median(cambio_porc, na.rm = T)) 

# Gráfica ----
imss %>% 
  filter(year(fecha) < 2020) %>% 
  ggplot() +
  geom_ribbon(aes(ymin = cambio_porc + 0.1, ymax = mediana - 0.1, 
                  x = month(fecha)), fill = rojo,
              data = ribbon_frame) + 
  geom_line(aes(x = month(fecha), y = cambio_porc, group = year(fecha)), color = gris) +
  geom_line(aes(x = mes, y = mediana), data = frame_de_mediana) +
  geom_line(aes(x = month(fecha), y = cambio_porc, group = year(fecha)), color = rojo_fuerte, 
            data = imss %>% filter(year(fecha) == 2020)) +
  annotate('curve', x = 5.5, y = -1.75, xend = 4.1, yend = -2, curvature = -.3, 
           arrow = arrow(length = unit(0.1, "cm"))) +
  annotate('text', x = 7, y = -1.4,
           label = str_wrap('De enero a abril de este año\nse han perdido 564,701 empleos', 
                            width = 21), 
           family = 'Andale Mono', 
           color = rojo_fuerte,
           size = 3) +
  scale_x_continuous(breaks = c(1, 4, 8, 12), 
                     labels = c('Enero', 'Abril', 'Agosto', 'Diciembre')) +
  scale_y_continuous(breaks = c(-2.5, -1.5, 0, 1, 2), 
                     sec.axis = sec_axis(~ ., 
                                         breaks = c(-2.5, -1.5, 0, 1, 2), 
                                         labels = c('-2.5%', '-1.5%', '0%', '1%', '2%')), 
                     expand = expansion(c(0.1, 0.1))) +
  labs(title = "Cambio en el % de asegurados del IMSS", 
       subtitle = "<b style='color:#BD343E'>2020</b> vs. <b style='color:#909090'>2000 - 2019</b> y su mediana",
       x = '', y = '') +
  ggthemes::theme_clean() +
  theme(text = element_text(family = "Andale Mono"),
        plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 1.1),
        legend.position = c(0.3, 0.95), 
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y.right  = element_line(size = 0.75), 
        axis.ticks.y.right = element_line(size = 0.75),
        axis.ticks.length.y.right = unit(0.20, 'cm'),
        axis.text.y.right = element_text(),
        plot.margin = margin(2, 1, 1, 1, "cm")) +
  ggsave('imss_lines.png', width = 6, height = 3.8)


