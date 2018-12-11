library(raster)
library(dplyr)


serial_number = 6392

## Part 1

d = expand.grid(x = 1:300, y = 1:300) %>%
  mutate(rack_id = x + 10) %>%
  mutate(
    power = rack_id * y,
    power = power + serial_number,
    power = power * rack_id,
    power = floor((power %% 1000) / 100) - 5
  ) %>%
  select(-rack_id) 
  
r = d %>% 
  as.matrix() %>%
  raster::rasterFromXYZ()



which.max(total[]) %>% xyFromCell(total, .) %>% {.-1}


## Part 2

for(i in seq(3,19,by=2)) {
  total = r %>% raster::focal(w = matrix(1, i, i), sum)
  cat(i, ":", max(total[], na.rm=TRUE), ", ", which.max(total[]) %>% xyFromCell(total, .),"\n")
}

c(239, 274) - 6