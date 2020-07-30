# Librerías ====
library(tidyverse)

# Lectura ====
inflacion <- readxl::read_excel('inflacion.xlsx') %>% 
     janitor::clean_names()

# Limpieza ====
inf_clean <- inflacion %>% 
     pivot_longer(-periodo, names_to = 'tipo', values_to = 'valor')

# Gráfica ====
naranja <- '#F0961B'
verde <- '#90CB55'
azul <- '#81C4EE' 
rojo <- '#CC1217'

theme_set(ggthemes::theme_clean() +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(face = 'bold', size = 20),
                  axis.text = element_text(size = 20),
                  axis.title = element_text(size = 20),
                  panel.grid.major.y = element_blank(),
                  text = element_text(family = 'Courier New'),
                  legend.text = element_text(family = 'Courier New', face = 'bold'),
                  legend.background = element_rect(color = 'transparent'),
                  legend.position = c(0.1, 0.87),
                  plot.background = element_rect(color = 'transparent'),
                  rect = element_rect(fill = 'transparent', color = 'white')))


inf_clean %>% 
     filter(tipo != 'inpc') %>% 
     mutate(tipo = factor(tipo, levels = c('mercancias', 'servicios', 'no_subyacente'))) %>% 
     ggplot() +
     geom_col(aes(x = periodo, y = valor, fill = tipo)) +
     geom_line(aes(x = periodo, y = valor, fill = tipo), 
               color = rojo,
               data = inf_clean %>% filter(tipo == 'inpc'), 
               size = 2) +
     geom_text(aes(x = periodo, y = valor, color = tipo, label = valor), 
               data = inf_clean %>% 
                       filter(tipo != 'inpc') %>% 
                       mutate(tipo = factor(tipo, levels = c('mercancias', 'servicios', 'no_subyacente'))) %>% 
                       filter(periodo == last(periodo)), 
               position = position_stack(0.5), 
               vjust = 2.4, 
               angle = 90, 
               size = 4, 
               family = 'Courier New', 
               fontface = 'bold') +
     geom_text(aes(x = periodo, y = valor, label = valor), 
               data = inf_clean %>% 
                       filter(tipo == 'inpc') %>% 
                       filter(periodo == last(periodo)), 
               angle = 90, 
               size = 4, 
               vjust = 2.4, 
               color = rojo, 
               family = 'Courier New', 
               fontface = 'bold') +  
     scale_color_manual(values = c(azul, verde, naranja)) +
     scale_fill_manual(values = c(rojo, azul, naranja, verde),
                       labels = c('INPC', 'Mercancías', 'Servicios', 'No Subyacente'),
                       name = '') +
     scale_y_continuous(breaks = seq(-1, 8, by = 1), 
                        limits = c(-1, 8)) +
     scale_x_datetime(expand = expansion(c(0, 0.02))) +
     labs(x = '', y = '') +
     guides(color = FALSE) +
     ggsave('rcreated.png',  width = 13, heigh = 9)

