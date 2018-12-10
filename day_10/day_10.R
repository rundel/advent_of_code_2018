library(dplyr)

process = function(file) {
  readLines(file) %>%
    stringr::str_match("position=<([0-9- ]+),([0-9- ]+)> velocity=<([0-9- ]+),([0-9- ]+)>") %>%
    as.data.frame(stringsAsFactors=FALSE) %>%
    select(-1) %>%
    mutate_all(stringr::str_trim) %>%
    mutate_all(as.integer) %>%
    setNames(c("x","y","v_x","v_y"))
}

d = process("day_10/input.txt") 
d_simple = process("day_10/simple.txt") 

project = function(d, t=0) {
  d %>% transmute(
    x = x + v_x * t,
    y = y + v_y * t
  ) %>%
    as.matrix()
}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


l = purrr::map(1:1000, project, d=d)
dists = purrr::map_int(l, ~ dist(.x) %>% Mode())
