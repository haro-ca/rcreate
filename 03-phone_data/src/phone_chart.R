# Libraries ====
library(tidyverse)
library(extrafont)

# Import ====
calls <- read_csv('data/Eleanor_phone_records.csv') %>% 
  janitor::clean_names()
texts <- read_csv('data/Eleanor_text_records.csv') %>% 
  janitor::clean_names()


# Cleaning ====
calls_counted <- calls %>% 
  mutate(minutes = parse_number(minutes)) %>% 
  count(date, direction, wt = minutes) 
  ggplot(aes(x = date, y = n, fill = direction)) +
  geom_col()

texts_counted <- texts %>% 
  count(date, direction) %>% 
  mutate(n = -n, 
         direction = if_else(direction == 'Incoming', 'inc_text', 'out_text'))

# Chart ====
# Graphical parameters ----
strong_pink <- '#F87175'
light_pink <- '#FF9295'
strong_blue <- '#75BDD3'
light_blue <- '#98E1E8' 

# Chart ====
bind_rows(calls_counted, texts_counted) %>% 
  mutate(n = n/60) %>% 
  ggplot(aes(x = date, y = n, fill = direction)) +
  geom_col(width = 1) +
  geom_hline(yintercept = 0, size = 0.75) +
  geom_vline(aes(xintercept = max(date) + lubridate::days(1))) +
  annotate('segment', 
           x = seq.Date(from = as.Date('2019-12-31'), 
                            to = as.Date('2020-04-30'), 
                            by = 'month'), 
           xend = seq.Date(from = as.Date('2019-12-31'), 
                           to = as.Date('2020-04-30'), 
                           by = 'month'), 
           y = 0, 
           yend = -0.1, 
           size = 0.75) +
  annotate('text', 
           x = seq.Date(from = as.Date('2019-12-31'), 
                                to = as.Date('2020-04-30'), 
                                by = 'month'), 
           y = c(0, 0, 0, 0), 
           label = c('JAN', 'FEB', 'MAR', 'APR'), 
           vjust = 1.2, hjust = -0.2, 
           family = "Andale Mono", size = 4.5) +
  scale_fill_manual(values = c(light_blue, light_pink, strong_blue, strong_pink), 
                    labels = c('', 'Outgoing', '', 'Incoming')) +
  scale_x_date(date_breaks = '1 month', 
               expand = expansion(c(0, 0))) +
  scale_y_continuous(breaks = c(-100/60, -50/60, 0, 1, 2, 3), 
                     sec.axis = sec_axis(~ ., 
                                         breaks = c(-100/60, -50/60, 0, 1, 2, 3), 
                                         labels = c('100 texts', 
                                                    '50       ', 
                                                    '0        ', 
                                                    '1        ', 
                                                    '2        ', 
                                                    '3 hours')), 
                     limits = c(-100/60, 3), 
                     expand = expansion(c(0, 0))) +
  labs(fill = 'Phone calls  \n\n\nTexts', x = '', y = '') +
  theme_clean() +
  theme(text = element_text(family = "Andale Mono"),
        legend.position = c(0.3, 0.87), 
        axis.text = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_blank(), 
        axis.line.y.right  = element_line(size = 0.75), 
        axis.ticks.y.right = element_line(size = 0.75),
        axis.ticks.length.y.right = unit(0.20, 'cm'),
        axis.text.y.right = element_text(),
        plot.margin = margin(2, 1, 1, 1, "cm")) +
  guides(fill = guide_legend(title.position = "left",
                               title.hjust = 0,
                               label.position = "top", 
                               reverse = TRUE, 
                               nrow = 2, 
                               keyheight = 0.75, 
                               keywidth = 2,
                               byrow = F)) +
  ggsave('phone_chart.png', width = 6, height = 4)
