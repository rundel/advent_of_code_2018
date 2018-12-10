library(dplyr)
  
process = function(file = "day_09/input.txt") {
  readLines(file) %>%
  stringr::str_match("(\\d+) players; last marble is worth (\\d+) points(?:: high score is (\\d+))?") %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  select(-1) %>%
  setNames(c("players","last_marble", "best_score")) %>%
  mutate_all(as.integer)
}

d = process("day_09/input.txt")
d_test = process("day_09/simple.txt")



run_game = function(players = 9, last_marble = 32, verbose=FALSE) {
  
  circle = c(0,1)
  cur = 1
  
  scores = rep(0, players)
  
  pb = dplyr::progress_estimated(last_marble, min_time = 5)
  
  for(i in seq(2, last_marble)) {
    pts = 0
    if (i %% 23 == 0) {  
      idx = (cur - 7 - 1) %% length(circle) + 1
      pts = circle[idx] + i
      scores[(i-1) %% players + 1] = scores[(i-1) %% players + 1] + pts
      circle = circle[-idx]
      cur = idx
      if (idx > length(circle))
        cur = 1
    } else {
      i1 = (cur + 1 - 1) %% length(circle) + 1
      i2 = (cur + 2 - 1) %% length(circle) + 1
      
      start = min(i1,i2)
      end   = max(i1,i2) 
      if (end - start > 1) {
        circle = c(circle, i)
        cur = length(circle)
      } else {
        circle = c(circle[1:start], i, circle[end:length(circle)])
        cur = end
      }
    }
    
    if (verbose) {
      cat(i, "[", (i-1) %% players + 1, "] ", sep="")
      cat(circle, " : ", cur)
      if (pts != 0)
        cat("# Scored ", pts)
      cat('\n')
    }
    
    pb$tick()$print()
  }
  
  c(which.max(scores), max(scores))
}

## Test

profvis::profvis({
  run_game(d_test$players[1], d_test$last_marble[1])
})


## Part 1
profvis::profvis({
  run_game(d$players, d$last_marble)
})

## Part 2

d %>% 
  select(-3) %>%
  mutate(last_marble = last_marble*100) %>%
  purrr::pmap(run_game, verbose=FALSE)
