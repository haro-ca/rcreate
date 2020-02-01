---
title: "BBC"
output: 
    html_document:
        keep_md: yes
        highlight: kate
---


```r
# Las secciones de código utilizan el siguiente layout
# ├─ Sección ===
# │ ├─ Sub 1 ---
# │ │ ├─ Sub 2 ---
# └ Fin de la sección

# ├ Librerías ====
options(tidiverse.quiet = TRUE)
library(tidyverse)

# ├─ Simulación de los datos ====
# │ ├─ Muestreo ----
set.seed(2124)
house = tibble(dem = rnorm(n = 500/2, mean = 2)/100, rep = rnorm(n = 500/2, mean = -2)/100)

# │ ├─ Truncado ----
house = house %>% filter(dem < 30, dem > -25, 
                         rep < 15, rep > -30, )

# │ ├─ Variables añadidas ----
# │ │ ├─ Won ----
house = house %>% 
    gather() %>% 
    add_column(won = if_else(rbinom(n = 500, size = 1, prob = 0.65) == 1, 
                             'won', 'lost'))

# │ │ ├─ Key won ----
# Esta es la que se usa para distinguir el color claro
house = house %>% 
    unite('key_won', c('key', 'won'), remove = FALSE) %>% 
    mutate(key_won = factor(key_won, levels = c('dem_won', 'dem_lost', 'rep_won', 'rep_lost')))

# │ │ ├─ Number ----
# Esta se ocupa para que las barras estén divididas en 'cajas'
house = house %>% 
    arrange(desc(key_won, won, value)) %>% 
    add_column(number = c(1:250, 1:250))

# └ ├─ Modificación de variables ----
# Se cambia para que el facet tengas estas leyendas
house = house %>% 
    mutate(key = case_when(
        key == 'dem' ~ 'Democrat candidates',
        TRUE ~ 'Republican candidates'
        )
    )

# ├ Gráfica ====
# │ ├─ Parámetros gráficos ----
blue = '#045a8d'
light_blue = '#a6bddb'
red = 'red'
light_red = '#ef6548'

# │ ├─ Inicialización ----
house %>% 
    ggplot() +
# │ ├─ Facet ----
    facet_wrap(vars(key), nrow = 2) +
# │ ├─ Geoms ----
    geom_histogram(aes(x = value, group = number, fill = key_won), 
                   color = 'white', size = 0.25) +
    geom_hline(yintercept = 0, color = 'black', size = 2) +
    geom_vline(aes(xintercept = 0), linetype = 'dashed', size = 0.5) +
# │ ├─ Scales and labels ----
    scale_fill_manual(values = c(blue, light_blue, red, light_red), name = '', 
                      labels = c('Won seat', 'Did not win', '', '')) +
    scale_x_continuous(labels = scales::percent) +
    labs(
      title = 'Blue wave', 
      y = '', 
      x = ''
    ) +
# │ ├─ Theme ----
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = c(0, 0.87),
          strip.text = element_text(hjust = 0, face = 'bold', size = 25), 
          legend.justification = c(0, -2),
          legend.text = element_text(face = 'bold'), 
          plot.margin = margin(2, 1, 1, 1, "cm"),
          legend.spacing.y = unit(0.01, 'cm'), 
          text = element_text(face = 'bold', size = 25), 
          plot.title = element_text(vjust = 20, hjust = 0.005)) +
# └ ├─ Guides ----
    guides(fill = guide_legend(title.position = "top",
                               title.hjust = 0,
                               label.position = "right", 
                               label.vjust = 4,
                               reverse = TRUE, 
                               nrow = 2, 
                               keyheight = 0.25, 
                               keywidth = 2,
                               byrow = T, 
                               rev = FALSE)) + 
  ggsave(here::here('rcreated_viz.jpeg'), height = 13, width = 15)
```

![](BBC_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

