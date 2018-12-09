library(dplyr)

# Part 1

d = "day_07/input.txt" %>%
  readLines() %>%
  stringr::str_match("Step ([A-Z]) .* step ([A-Z]) can begin\\.") %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  setNames(c("text","start","end")) %>%
  select(-text)

possible = c(d$start, d$end) %>% unique() %>% sort()

find_path = function(path = c()) {
  d_cur = filter(d, !start %in% path)
  
  available_steps = setdiff(possible, path) %>% setdiff(d_cur$end)
  
  if (length(available_steps) == 0)
    return(path)
  
  path = c(path, sort(available_steps)[1])
  
  return(find_path(path))
}

find_path() %>% paste(collapse="")


# Part 2

`%||%` = function (a, b) {
  if (!(is.null(a) | length(a) == 0)) a else b
}

file = "day_07/simple.txt"
n_worker = 2
delay = 1

file = "day_07/input.txt"
n_worker = 5
delay = 61

d = input %>%
  readLines() %>%
  stringr::str_match("Step ([A-Z]) .* step ([A-Z]) can begin\\.") %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  setNames(c("text","start","end")) %>%
  select(-text)

possible = c(d$start, d$end) %>% unique() %>% sort()



workers = purrr::map(seq_len(n_worker), ~c())
path = c()
processing = c()
queue = setdiff(possible, path) %>% setdiff(d$end)


t = 1
repeat {
  finished = FALSE
  for(i in seq_len(n_worker)) {
    if (length(workers[[i]]) < t) {
      if (length(queue) == 0) {
        workers[[i]] = c(workers[[i]], NA)
      } else {
        step = queue[1]
        queue = queue[-1]
        n = utf8ToInt(step) - utf8ToInt("A") + delay
        workers[[i]] = c(workers[[i]], rep(step, n))
        processing = c(processing, step)
      }
    }
    
    if (length(workers[[i]]) == t) {
      if (!is.na(workers[[i]][t])) {
        # Finished working
        path = c(path, workers[[i]][t])
        processing = processing[processing != workers[[i]][t]]
        finished = TRUE
      }
    }
  }
  
  if (finished) {
    d_cur = filter(d, !start %in% path)
    available_steps = setdiff(possible, c(path, processing)) %>% setdiff(d_cur$end)
    queue = available_steps %>% sort()
  }
  
  t = t+1
  
  if (purrr::map_chr(workers, last) %>% is.na() %>% all())
    break;
}
workers %>% setNames(paste0("worker_", seq_len(n_worker))) %>% as.data.frame(stringsAsFactors=FALSE)

length(workers[[1]])-1
