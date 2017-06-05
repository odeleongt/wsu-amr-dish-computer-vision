# Synthesize arbitrary sample dish images

# Load used libraries
library(package = "imager")
library(package = "tidyverse")

# Set well parameters
diameter <- 3
rows <- letters[1:8]
cols <- 1:12

# Reproducible
set.seed(2017-06-03)

# Generate random plate layout
plate <- data_frame(
    row = rep(rows, each = length(cols)),
    col = rep(cols, times = length(rows)),
    value = runif(n = length(rows) * length(cols), min = 0, max = diameter)
  )


# Plot generated plate
dish <- plate %>%
  ggplot(aes(x = col, y = row)) +
  geom_point(aes(size = I(value)), color = "#bd6b6bff") +
  coord_equal() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#5b4c4cff"),
    plot.margin = margin(0, 0, 0, 0)
  )

# Save dish
ggsave(
  plot = dish, file = "data/dishes/synthetic/dish.png",
  width = 3.699, height = 2.419
)


# Read dish for modification
dish <- load.image("data/dishes/synthetic/dish.png")

dish %>%
  autocrop() %>%
  isoblur(sigma = 5, neumann = FALSE) %>%
  save.image(file = "data/dishes/synthetic/dish.jpg")


# End of script
