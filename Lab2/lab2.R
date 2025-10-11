library(dplyr)

starwars %>% nrow()

starwars %>% ncol()

starwars %>% glimpse()

starwars %>% distinct(species)

starwars %>%
  filter(!is.na(height)) %>%
  arrange(desc(height)) %>%
  slice(1)

starwars %>% filter(height < 170)

starwars %>%
  mutate(
    height_m = height / 100,
    BMI = mass / (height_m^2)
  ) %>%
  select(name, mass, height, BMI)

starwars %>%
  filter(!is.na(mass), !is.na(height), height > 0) %>%
  mutate(stretchiness = mass / height) %>%
  arrange(desc(stretchiness)) %>%
  head(10)

starwars %>%
  count(eye_color, sort = TRUE) %>%
  slice(1)

starwars %>%
  filter(!is.na(species), !is.na(name)) %>%
  mutate(name_length = nchar(name)) %>%
  group_by(species) %>%
  summarise(avg_name_length = mean(name_length))
