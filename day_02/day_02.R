library(dplyr)
library(stringr)

d = readr::read_table("day_02/input.txt", col_names = FALSE) %>%
  rename(id = X1)

d %>% 
  mutate(
    id = str_split(id, ""),
    n2 = purrr::map(id, ~ table(.x) %>% purrr::keep(~ .x == 2, .default=NULL) %>% length() ),
    n3 = purrr::map(id, ~ table(.x) %>% purrr::keep(~ .x == 3, .default=NULL) %>% length() )
   ) %>% 
  summarize(
    checksum = sum(n2 >= 1) * sum(n3 >= 1)
  )

library(stringdist)

input = readLines("day_02/input.txt")
z = stringdistmatrix(input)
ind = which(as.matrix(z) == 1, arr.ind = TRUE)

ind[1,]

str1 = input[ind[1]] %>% str_split("") %>% .[[1]]
str2 = input[ind[2]] %>% str_split("") %>% .[[1]]

str1[str1 == str2] %>% paste0(collapse="")


  
