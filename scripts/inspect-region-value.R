dish %>%
  as.data.frame() %>%
  as_tibble %>%
  rename(pixel_value = value) %>%
  left_join(select(labelled_dish, -cc)) %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  summarize(
    pixel_count = n(),
    pixel_mean = mean(pixel_value),
    pixel_sum = sum(pixel_value)
  ) %>%
  left_join(labelled_wells) %>%
  select(x_lab, y_lab, pixel_sum) %>%
  spread(x_lab, pixel_sum)
