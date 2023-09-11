library(tidyverse)
library(palmerpenguins)
library(ggplot2)

View(penguins)
plot(x = penguins$bill_depth_mm, y = penguins$bill_length_mm,
     main = "Bill depth and length",
     sub = "Dimensions for Adelie, Chinstrap, and Gentoo")
ggplot(
  data = penguins,
  mapping = aes(x = bill_depth_mm,
                y = bill_length_mm)) + 
  geom_point(mapping = aes(colour = species)) + 
  # Labelling the axis
  labs(
    title = "Bill depth and length",
    subtitle = paste(
      "Dimensions for Adelie, Chinstrap,",
      "and Gentoo Penguins"
    ),
    x = "Bill depth (mm)",
    y = "Bill length (mm)",
    colour = "Species",
    caption = "Source: palmerpenguins package"
  )
)


