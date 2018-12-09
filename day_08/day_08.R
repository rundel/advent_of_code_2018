library(dplyr)

d = readLines("day_08/input.txt") %>%
  stringr::str_split(" ") %>%
  unlist() %>%
  as.integer()

## Part 1

parse = function(d, level=0) {
  n_child = d[1]
  n_meta  = d[2]
  d = d[-(1:2)]
  

  l = list()
  
  for(i in seq_len(n_child)) {
    res = parse(d, level+1)
    l[[i]] = res$l
    d = res$d
  }
  
  l$meta = d[seq_len(n_meta)]
  d = d[-seq_len(n_meta)]
  
  list(
    l = l,
    d = d
  )
}

collect_meta = function(l) {
  n_child = length(l)
  
  vals = l$meta
  for(i in seq_len(n_child-1)) {
    vals = c(vals, collect_meta(l[[i]]))
  }
  
  vals
}

parse(d) %>% .$l %>% collect_meta() %>% sum()



## Part 2

score = function(l) {
  pts = 0
  if (length(l) == 1) {
    pts = sum(l$meta)
  } else {
    inds = seq_len(length(l)-1)
    for(i in l$meta) {
      if (i %in% inds)
        pts = pts + score(l[[i]])
    }
  }
  
  pts
}

parse(d) %>% .$l %>% score()