
# Load used packages
library(package = "RNiftyReg")
library(package = "imager")
library(package = "tidyverse")

# Rotation
rotation_angle <- 10

# Load images
reference_dish <- load.image(file = "data/dishes/dish-sample.jpg")
rotated_dish <- imrotate(
  im = reference_dish,
  angle = rotation_angle
)


# Plot dishes
par(mfrow = c(1, 2))
plot(reference_dish)
plot(rotated_dish)
par(mfrow = c(1, 1))

# Aligne rotated dish to reference
registration <- niftyreg.linear(
  source = grayscale(rotated_dish  > 0.7),
  target = grayscale(reference_dish  > 0.7)
)

# get angle from affine transformation
# https://math.stackexchange.com/a/13165
dish_affine <- unclass(registration$forwardTransforms[[1]])
estimated_angle <- atan2(dish_affine[2,1], dish_affine[2,2]) * 180/pi


# Show fixed rotation
par(mfrow = c(1, 2))
rotated_dish %>%
  magrittr::is_greater_than(0.7) %>%
  plot()
rotated_dish %>%
  imrotate(-estimated_angle) %>%
  magrittr::is_greater_than(0.7) %>%
  plot
par(mfrow = c(1, 1))

# End of script
