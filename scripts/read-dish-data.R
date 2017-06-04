
# Load used packages
library(package = "imager")
library(package = "tidyverse")

# Load image
dish <- load.image(file = "data/dishes/dish-sample.jpg")
plot(dish)

# Extract areas with highest luminance
lum_threshold <- 0.7
wells <- dish > lum_threshold
plot(wells)


# Find wells (i.e. isolated blobs)
labelled_dish <- wells %>%
  label %>%
  as.data.frame() %>%
  as_tibble %>%
  rename(region = value) %>%
  filter(region > 0)

# Find centers
well_centers <- labelled_dish %>%
  group_by(region) %>%
  summarize(
    x = mean(x),
    y = mean(y)
  )


# Look at identified centers
plot(wells)
well_centers %>%
  # Exclude controls
  filter(
    !value %in% range(value)
  ) %>%
  with(points(x, y, col = "white", pch = 20))



# Expected well numbers per dimension
x_wells <- 12
y_wells <- 8

# Plate limits
lims <- well_centers %>%
  # Exclude controls
  filter(
    !value %in% range(value)
  ) %>%
  summarize(
    min_x = min(x),
    min_y = min(y),
    max_x = max(x),
    max_y = max(y)
  )

# Expected spatial distribution
x_spread <- ( lims$max_x - lims$min_x ) / ( x_wells - 1)
y_spread <- ( lims$max_y - lims$min_y ) / ( y_wells - 1)

# Expected divisions between rows and cols
x_cols <- seq(
  from = lims$min_x - x_spread/2,
  to = lims$max_x + x_spread/2,
  length.out = x_wells + 1
)
y_rows <- seq(
  from = lims$min_y - y_spread/2,
  to = lims$max_y + y_spread/2,
  length.out = y_wells + 1
)

# Label wells
labelled_wells <- well_centers %>%
  # Exclude controls
  filter(
    !value %in% range(value)
  ) %>%
  mutate(
    # Asign positions
    y_lab = cut(
      y, breaks = y_rows,
      labels = letters[seq(1, y_wells, length.out = y_wells)]
    ),
    x_lab = cut(
      x, breaks = x_cols,
      labels = seq(1, x_wells, length.out = x_wells),
      ordered_result = TRUE
    )
  )

# See plate structure
labelled_wells %>%
  select(-x, -y) %>%
  spread(key = x_lab, value = region)


# End of script
