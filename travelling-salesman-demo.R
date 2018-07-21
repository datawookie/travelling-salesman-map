# =====================================================================================================================
# =                                                                                                                   =
# =                                                                                                                   =
# = Author: Andrew B. Collier <andrew@exegetic.biz> | @datawookie                                                     =
# =====================================================================================================================

# CONFIGURATION -------------------------------------------------------------------------------------------------------

ADDRESSES = c(
  "115 St Andrew's Drive, Durban North, KwaZulu-Natal, South Africa",
  "1 Evans Road, Glenwood, Berea, KwaZulu-Natal, South Africa",
  "7 Radar Drive, Durban North, KwaZulu-Natal, South Africa",
  "25 Gainsborough Drive, Durban North, KwaZulu-Natal, South Africa",
  "77 Armstrong Avenue, Umhlanga, KwaZulu-Natal, South Africa",
  "255 Musgrave Road, Berea, KwaZulu-Natal, South Africa",
  "11 Cassia Road, Reservoir Hills, Durban, KwaZulu-Natal, South Africa",
  "98 Shepstone Road, Berkshire Downs, New Germany, KwaZulu-Natal, South Africa",
  "12 Finchley Road, Berea West, Westville, KwaZulu-Natal, South Africa"
)

# LIBRARIES -----------------------------------------------------------------------------------------------------------

library(dplyr)
library(purrr)
library(ggmap)
library(gmapsdistance)
library(TSP)

# GEOCODE -------------------------------------------------------------------------------------------------------------

geocode_df <- function(address) {
  reply <- geocode(address, output = "all")$results[[1]]
  
  tibble(
    address = reply$formatted_address,
    lon = reply$geometry$location$lng,
    lat = reply$geometry$location$lat
  )
}

addresses <- ADDRESSES %>%
  map_dfr(geocode_df) %>%
  mutate(latlon = sprintf("%f+%f", lat, lon)) %>%
  mutate(label = LETTERS[1:nrow(.)]) %>%
  select(label, everything())

# DISTANCES -----------------------------------------------------------------------------------------------------------

# Get the distances using the Google Maps API. However, for production purposes this would be done with a local OSRM
# instance.
#
distances <- gmapsdistance(origin = addresses$latlon,
                          destination = addresses$latlon,
                          combinations = "all",
                          mode = "driving")$Distance[, -1]

# Scale to km.
#
distances <- as.matrix(distances) / 1000
#
colnames(distances) <- addresses$label
rownames(distances) <- addresses$label

# Convert to distance matrix.
#
distances <- as.dist(distances)

# TRAVELLING SALESMAN -------------------------------------------------------------------------------------------------

tsp <- TSP(distances)

methods <- c(
  "nearest_insertion",
  "farthest_insertion",
  "cheapest_insertion",
  "arbitrary_insertion",
  "nn",
  "repetitive_nn",
  "two_opt"
)

tours <- methods %>% map(function(method) {
  solve_TSP(tsp, method)
})

tour <- solve_TSP(tsp)
#
# Order of locations in tour.
#
tour_order <- as.integer(tour)
#
# Sort addresses.
#
addresses <- addresses[tour_order,]

# BUILD ROUTE ---------------------------------------------------------------------------------------------------------

route <- lapply(seq(nrow(addresses) - 1), function(n) {
  route(addresses$latlon[n], addresses$latlon[n+1], structure = "route") %>%
    mutate(section = n)
})

route <- route %>% bind_rows()

# ---------------------------------------------------------------------------------------------------------------------

map <- get_map(location = c(lon = 30.970622, lat = -29.810094), zoom = 12, maptype = "roadmap")

ggmap(map, extent = "device") +
  geom_path(data = route, aes(x = lon, y = lat),  colour = "blue", size = 1, alpha = 0.5) +
  geom_point(data = addresses, aes(x = lon, y = lat), size = 3, alpha = 0.75) +
  labs(x = "", y = "")
