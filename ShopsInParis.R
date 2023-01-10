library(tidyverse)

# Import
library(jsonlite)
shop = fromJSON("C:/Users/33665/Documents/commerces.json", simplifyVector = FALSE)
# We delete a shop for which we don't have all the informations
shop = shop[-83443]

#  1. Display the first shop of the list
shop[[1]]

#  2. Display shop `86181` (cf field `ordre`)
shop %>%
  keep(~ .x$fields$ordre == 86181)

#  3. Display shops located at `"137 AV VERSAILLES"` 
# (field `adresse_complete`)
shop %>%
  keep(~ .x$fields$adresse_complete == "137 AV VERSAILLES") 

#  4. Display the number of shops at "avenue de Versailles" 
# (the way of wording must be équal to `"VERSAILLES"`)
shop %>%
  keep(~ .x$fields$libelle_voie == "VERSAILLES") %>%
  length()

#  5. Create a `tibble` from the shops list, with as columns :
# - `ordre`, `codact`, `libact`, 
# - `adresse_complete`, `num`, `type_voie`, `libelle_voie`
# - `iris`, `ilot`, `quartier`, `arro`
# - longitude and latitude (`xy` - *careful* with the values `NULL`) 
tab = shop %>%
  map("fields") %>%
  map_dfr(function(e) {
    if (!is.null(e$xy)) {
      lat = e$xy[[1]]
      lon = e$xy[[2]]
    } else {
      lat = lon = NA
    }
    attach(e, warn.conflicts = FALSE)
    t = tibble(ordre, codact, libact, 
               adresse_complete, num, type_voie, libelle_voie, 
               iris, ilot, qua, arro, lon, lat)
    detach(e)
    return(t)
  })
(tab)

#  6. What are the ten types of shops the most represent ? 
# (show its numerically and graphically)
df6 = tab %>%
  group_by(libact) %>%
  summarise(nb = n()) %>%
  arrange(desc(nb)) %>%
  slice(1:10)
df6

ggplot(df6, aes(nb, fct_reorder(libact, nb, .desc = FALSE), col = nb)) +
  geom_point(size = 2) +
  scale_color_continuous(low = "gray50", high = "gray10") +
  theme_minimal() +
  xlim(0, max(df6$nb)) +
  theme(axis.title = element_blank(), legend.position = "none")

#  7. How much shops and types of shops
# per district ? (same, table + graphic)
df7 = tab %>%
  group_by(arro) %>%
  summarise(
    nb_shops = n(),
    nb_types = n_distinct(codact)
  ) %>%
  mutate(arro = str_sub(arro, 4, 5))
df7

ggplot(df7, aes(arro, nb_shops, fill = nb_shops)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "gray50", high = "gray10") +
  theme_minimal() +
  labs(caption = "Number of shops per district") +
  theme(axis.title = element_blank(), legend.position = "none")
ggplot(df7, aes(arro, nb_types, fill = nb_types)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "gray50", high = "gray10") +
  theme_minimal() +
  labs(caption = "Types of shops number per district") +
  theme(axis.title = element_blank(), legend.position = "none")

#  8. Draw a map of Paris with a point for each shop
# free to use any background map
library(ggmap)

paris = c(left = min(tab$lon, na.rm = TRUE) - .01, 
          bottom = min(tab$lat, na.rm = TRUE) - .01, 
          right = max(tab$lon, na.rm = TRUE) + .02, 
          top = max(tab$lat, na.rm = TRUE) + .01)

map = get_stamenmap(paris, zoom = 11, maptype = "toner-lite")

ggmap(map) +
  theme_void() +
  geom_point(data = tab, aes(lon, lat))
ggmap(map) +
  theme_void() +
  stat_density_2d(data = tab, aes(lon, lat, fill = stat(level)),
                  geom = "polygon") +
  scale_fill_gradient(low = "gray50", high = "gray10")

#  9. Represent the ten types of shops the most represent 
# graphically
ggmap(map) +
  theme_void() +
  geom_point(data = tab %>% semi_join(df6), 
             aes(lon, lat, col = libact)) +
  facet_wrap("libact") +
  theme(legend.position = "none")
ggmap(map) +
  theme_void() +
  stat_density_2d(data = tab %>% semi_join(df6), 
                  aes(lon, lat, fill = stat(level)),
                  geom = "polygon") +
  scale_fill_gradient(low = "gray50", high = "gray10") +
  facet_wrap("libact") +
  theme(legend.position = "none")

# 10. Use districts limitis available 
# [here](https://opendata.paris.fr/explore/dataset/arrondissements/map/?location=12,48.85889,2.34692) 
# to represent, number of shops per district
library(geojsonio)
district = geojson_read("C:/Users/33665/Documents/arrondissements.geojson", what = "sp")
district_f = fortify(district, region = "c_ar")
district_fj = district_f %>%
  inner_join(df7 %>% mutate(id = as.character(as.numeric(arro))))

ggmap(map) +
  geom_polygon(data = district_fj, 
               aes(long, lat, group = group, fill = nb_shops), 
               color = "white", alpha = .8) +
  theme_void() +
  scale_fill_gradient(low = "gray50", high = "gray10")

# 11. Graphic representation, for each of the ten most represent type of shops
# how they are dispatched in the different districts
df11 = tab %>%
  semi_join(df6) %>%
  count(libact, arro) %>%
  group_by(libact) %>%
  mutate(prop = n / sum(n))%>%
  mutate(id = as.character(as.numeric(str_sub(arro, 4, 5))))
df11_fj = district_f %>%
  inner_join(df11)

ggmap(map) +
  geom_polygon(data = df11_fj, 
               aes(long, lat, group = group, fill = prop), 
               color = "white", alpha = .8) +
  theme_void() +
  scale_fill_gradient(low = "gray50", high = "gray10") +
  facet_wrap("libact")

