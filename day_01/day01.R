library(magrittr)

input = readLines("day_01/input.txt") %>% as.numeric()

sum(input)

start = 0
res = c()
i = 1
while(!any(duplicated(res))) {
  cs = input %>% c(start,.) %>% cumsum()
  start = cs[length(cs)]
  res = c(res, cs[-length(cs)])
  i = i + 1
  print(i)
  
  if (i > 200)
    break
}

duplicated(res) %>% which() %>% .[1] %>% res[.]


