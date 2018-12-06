library(dplyr)


## Part 1

crds = readr::read_csv("day_06/simple.txt", col_names = FALSE) %>% setNames(c("x","y"))
crds = readr::read_csv("day_06/input.txt",  col_names = FALSE) %>% setNames(c("x","y"))

crds = crds %>% 
  as_tibble()

xmn = min(crds$x)
xmx = max(crds$x)
ymn = min(crds$y)
ymx = max(crds$y)

manh_dist_shortest = function(x,y, crds) {
  dist = abs(crds$x - x) + abs(crds$y - y)
  i = which( min(dist) == dist  ) 
  if (length(i) > 1)
    NA
  else
    i
}

grid = expand.grid(x = seq(xmn, xmx), y = seq(ymn,ymx)) %>% 
  as_tibble() %>%
  mutate(
    edge = x == min(x) | x == max(x) | y == min(y) | y == max(y),
    index = purrr::map2_int(x, y, manh_dist_shortest, crds = crds)
  ) 

grid %>% select(-edge) %>% raster::rasterFromXYZ() %>% plot()

res = grid %>%
  na.omit() %>%
  count(edge, index)

filter(res, edge==FALSE) %>%
  anti_join(filter(res, edge==TRUE), by="index") %>%
  arrange(desc(n))


## Part 2

manh_dist_within = function(x, y, crds, max_total_dist=10000) {
  dist = abs(crds$x - x) + abs(crds$y - y)
  sum(dist) < max_total_dist
}

grid = expand.grid(x = seq(xmn, xmx), y = seq(ymn,ymx)) %>% 
  as_tibble() %>%
  mutate(
    within = purrr::map2_int(x, y, manh_dist_within, crds = crds)
  ) 

grid %>% raster::rasterFromXYZ() %>% plot()

sum(grid$within)

