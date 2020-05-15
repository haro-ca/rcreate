# Librerías =====
library(tidyverse)
library(lubridate)
library(ggtext)

# Lectura ====
banxico_raw <- read_csv('banxico.csv', locale = locale(encoding = 'latin1'),) 

# Limpieza ==== 
banxico <- banxico_raw %>% 
  janitor::clean_names() %>% 
  select(fecha = titulo, tasa_objetivo) %>% 
  mutate(fecha = parse_date(fecha, format = '%d/%m/%y'), 
         tasa_objetivo = parse_number(na_if(tasa_objetivo, 'N/E'))) %>% 
  filter(!is.na(tasa_objetivo)) %>% 
  arrange(desc(fecha)) %>% 
  mutate(cambio_pb = (tasa_objetivo - lead(tasa_objetivo)) * 100, 
         direccion = if_else(cambio_pb > 0, 'positivo', 'negativo')) %>% 
  filter(cambio_pb != 0) 
 
# Chart ====
# Parámetros gráficos ----
rojo_fuerte <- '#BD343E'
rojo <- '#FFA9A0'
verde_fuerte <- '#32746D'
verde <- '#9EC5AB'
gris <- '#cecece'

banxico %>% 
  filter(cambio_pb < 0) %>% 
  arrange(cambio_pb, fecha) %>% 
  mutate(orden = rank(cambio_pb, ties.method = 'first'))

# Gráfica =====
banxico %>% 
  ggplot() +
  geom_hline(yintercept = seq(2008, 2020, by = 1),
             linetype = 'dotted',
             alpha = 0.5) +
  geom_point(aes(y = year(fecha), x = month(fecha),
                 color = direccion, fill = direccion, 
                 size = abs(cambio_pb)), 
             shape = 21) +
  scale_x_continuous(breaks = c(1, 3, 5, 9, 12), 
                     labels = c('Enero', 'Marzo', 'Mayo', 'Septiembre', 'Diciembre')) +
  scale_y_continuous(breaks = seq(2008, 2020, by = 1), 
                     expand = expansion(c(0.1, 0.25))) + 
  scale_color_manual(values = c(rojo_fuerte, verde_fuerte), guide = 'none') +
  scale_fill_manual(values = c(rojo, verde), guide = 'none') +
  scale_size_continuous(breaks = c(25, 50, 75), 
                        labels = c('25pb', '50pb', '75pb')) +
  annotate('curve', x = 5.4, y = 2022, xend = 5.05, yend = 2020.7, curvature = .3, 
           arrow = arrow(length = unit(0.1, "cm"))) +
  annotate('text', x = 8.6, y = 2021.5,
           label = '3er descuento de 50pb en el año,\n7mo en la historia.', 
           family = 'Andale Mono', 
           size = 3) +
  labs(title = 'Tasa objetivo del Banco de México', 
       subtitle = "<b style='color:#9EC5AB'>Aumentos</b> y <b style='color:#BD343E'>disminuciones</b> de",
       x = '', y = '', 
      size = '',
      caption = '@haro_ca_\n14/Mayo/2020\nFuente: Banxico') +
  theme_classic() +
  theme(text = element_text(family = "Andale Mono"),
        plot.title = element_markdown(lineheight = 1.1),
        plot.subtitle = element_markdown(lineheight = 1.1),
        legend.position =  c(0.675, 1.02),
        axis.line.y = element_blank(),
        axis.line.x = element_line(), 
        axis.ticks.y = element_blank(), 
        plot.margin = margin(2, 1, 1, 1, "cm")) +
  guides(size = guide_legend(direction = 'horizontal', label.position = 'bottom')) +
  ggsave('mx_inflation_bubbles.png', width = 5.74, height = 4.76)










