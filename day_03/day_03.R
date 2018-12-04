library(raster)
library(dplyr)
library(stringr)



d = readLines("day_03/input.txt") %>%
  #c("#1 @ 1,3: 4x4","#2 @ 3,1: 4x4","#3 @ 5,5: 2x2") %>%
  str_match("#\\d+ @ (\\d+),(\\d+): (\\d+)x(\\d+)") %>%
  .[,-1] %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  setNames(c("x", "y", "w", "h")) %>%
  mutate_all(as.numeric) %>%
  mutate(i = seq_len(n()))

n = 1000
r = raster(ncol=n, nrow=n, xmn=0, xmx=n, ymn=0, ymx=n)
r[] = 0


rasts = purrr::pmap(
    d,
    function(x,y,w,h, i) {
       d = r
       d[cellsFromExtent(r,extent(x, x+w, y, y+h))] = 1
       d
    }
  ) 

overlaps = purrr::reduce(rasts, `+`)

plot(overlaps)

sum(overlaps[] > 1)

for(i in seq_along(rasts)) {
  cells = (rasts[[i]][] != 0 )
  print(i)
  if (all(overlaps[cells] == 1)) {
    break
  }
}


