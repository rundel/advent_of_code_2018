library(dplyr)

fix_rules = function(rules) {
  all = expand.grid(a = c('#','.'), b = c('#','.'), c = c('#','.'), d = c('#','.'), e = c('#','.')) %>%
    transmute(
      start = paste0(a,b,c,d,e),
      end = "."
    )
  
  needed = anti_join(all, rules, by="start")
  
  bind_rows(rules, needed)
}

process = function(file) {
  l = readLines(file)
  
  state = l[1] %>% stringr::str_remove("initial state: ")
  rules = l[-(1:2)] %>% 
    stringr::str_split(" => ") %>% 
    do.call(rbind, .) %>% 
    as.data.frame(stringsAsFactors=FALSE) %>%
    setNames(c("start","end")) %>%
    fix_rules()
  
  
  list(
    state = state,
    rules = rules$end %>% setNames(rules$start) 
  )
}

pad = function(state) {
  state %>%
    stringr::str_replace("^\\.{0,4}#", ".....#") %>%
    stringr::str_replace("#\\.{0,4}$", "#.....")
}

run = function(state, rules, origin=0, n=20) {
  
  pb = progress_estimated(n, min_time=5)
  j = 1
  while(j < n) {
    old_state = state
    state = pad(state)
    
    origin = origin - stringr::str_locate(state, old_state)[1,1] + 1
    
    n_empty_start = stringr::str_locate(state,"#")[1,1] - 1
    if (n_empty_start > 5) {
      state = stringr::str_replace(state, "^\\.{5,}#", ".....#")
      origin = origin + n_empty_start - 5
    }
    
    
    res = stringr::str_split(state,"")[[1]]
    
    for(i in 3:(length(res)-2)) {
      res[i] = rules[stringr::str_sub(state, i-2, i+2)]
    }
    
    state = paste(res, collapse = "")
    #if (j %% 100 == 0)
      cat(j,": ", state,  " - origin=", origin, "\n")
  
    if (old_state == state)
      break
    
    j = j + 1
    pb$tick()$print()
  }
  
  origin = origin + n - j
  
  #indexes = seq_len(nchar(state)) -1 + origin
  
  plants = stringr::str_split(state, "")[[1]] == "#"
  format(sum( which(plants) - 1 ) + origin*sum(plants), digits=16)
}

d = process("day_12/simple.txt")
d = process("day_12/input.txt")

run(d$state, d$rules)


## Part 2

run(d$state, d$rules, n = 50000000000)
