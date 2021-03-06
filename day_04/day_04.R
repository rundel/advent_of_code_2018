library(dplyr)
library(stringr)

d = readr::read_lines("day_04/input.txt") %>%
#readr::read_lines("day_04/simple.txt") %>%
  str_match("\\[(.*)\\](?: Guard #(\\d+))? (.*)") %>%
  as.data.frame(stringsAsFactors=FALSE) %>%
  setNames(c("data", "time","id","event")) %>%
  as_tibble() %>%
  select(-data) %>%
  mutate(time = lubridate::ymd_hm(time)) %>%
  arrange(time) %>%
  tidyr::fill(id) %>%
  mutate(
    event = case_when(
      event %in% c("begins shift", "wakes up") ~ "wake",
      event == "falls asleep" ~ "sleep"
    )
  ) %>%
  mutate(
    start = time,
    end = lead(time),
    time_len = difftime(end, start),
    start_min = lubridate::minute(start)
  ) %>%
  select(-time) %>%
  na.omit() %>%
  mutate(
    mins = purrr::map2(
      time_len, start_min, 
      function(len, start) {
        (start + (seq_len(len) - 1)) %% 60 %>%
          as.character() %>%
          factor(levels = as.character(0:59)) %>%
          table()
      }
    )
  ) %>%
  select(id, event, mins) %>%
  filter(event == "sleep") %>%
  group_by(id) %>%
  summarize(
    mins = list(purrr::reduce(mins, `+`))
  )


# Part 1

mutate(
  d,
  n_mins = purrr::map_int(mins, sum),
  common_min = purrr::map_chr(
    mins, 
    ~ names(.x)[which.max(.x)]  
  ),
) %>%
  arrange(desc(n_mins)) %>%
  mutate(
    answer = as.integer(id) * as.integer(common_min)
  )

# Part 2

mutate(
  d,
  max = purrr::map_int(mins, max),
  which_max = purrr::map_chr(
    mins, 
    ~ names(.x)[which.max(.x)]  
  ),
) %>%
  arrange(desc(max)) %>%
  mutate(
    answer = as.integer(id) * as.integer(which_max)
  )

  
  
