library(dplyr)
library(stringr)

## Task 1

#x = readLines("day_05/simple.txt") %>% str_split("") %>% .[[1]]
x = readLines("day_05/input.txt") %>% str_split("") %>% .[[1]]

collapse = function(x) {
  repeat {
    y = lead(x)
    
    i = which( (x == tolower(y) & toupper(x) == y) |
                 (y == tolower(x) & toupper(y) == x) )
    
    sub = which(diff(i) == 1) + 1
    if (length(i) > 1 & length(sub) >=1 )
      i = i[-sub]
    
    if (length(i) == 0)
      break
    
    x = x[-c(i,i+1)]
    
    #print(paste(x,collapse=""))
    #print( length(x) )
  }
  
  length(x)
}

collapse(x)


## Task 2

#x = readLines("day_05/simple.txt") %>% str_split("") %>% .[[1]]
x = readLines("day_05/input.txt") %>% str_split("") %>% .[[1]]

collapse_let = function(x, let = 'a') {
  collapse(x[!x %in% c(tolower(let), toupper(let))])
}

lets = unique(tolower(x)) %>% 
  sort() 

purrr::map_int(lets,
    ~collapse_let(x, .x)
  ) %>%
  setNames(lets)

