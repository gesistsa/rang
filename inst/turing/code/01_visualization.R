## This file is modified from
## https://github.com/allisonhorst/palmerpenguins/blob/main/README.Rmd
## License CC0: https://github.com/allisonhorst/palmerpenguins/blob/main/LICENSE.md

library(ggplot2)
library(readr)
library(here)

penguins <- read_csv(here("data_clean", "penguins.csv"))

flipper_bill <- ggplot(data = penguins,
                       aes(x = flipper_length_mm,
                           y = bill_length_mm)) +
    geom_point(aes(color = species,
                   shape = species),
               size = 3,
               alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, aes(color = species)) +
    theme_minimal() +
    scale_color_manual(values = c("darkorange","purple","cyan4")) +
    labs(title = "Flipper and bill length",
         subtitle = "Dimensions for Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
         x = "Flipper length (mm)",
         y = "Bill length (mm)",
         color = "Penguin species",
         shape = "Penguin species") +
    theme(legend.position = c(0.85, 0.15),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0, face= "italic"),
          plot.caption.position = "plot")

ggsave(here("figures", "flipper_bil.png"), flipper_bill)
