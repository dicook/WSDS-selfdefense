# can we use the first three letters of a pollster/publisher as an identifier of where to
# cut off the repetition?
# 

library(stringr)
polls <- polls %>% mutate(
  three = substr(Poll, 1, 3)
)

locations <- with(polls, str_locate_all(Poll, three))
polls$start <- locations %>% 
  purrr::map(.f = function(x) if(nrow(x) == 1) NA else x[2,1]) %>% unlist()

polls %>% mutate(
  pollster = ifelse(is.na(start), Poll, substr(Poll, 1, start-1))
) %>% group_by(Poll, pollster) %>% tally()

# Yes!!!

polls <- polls %>% mutate(
  pollster = ifelse(is.na(start), Poll, substr(Poll, 1, start-1))
)