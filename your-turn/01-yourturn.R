library(rvest)

url <- "http://www.realclearpolitics.com/epolls/latest_polls/state/"
doc <- xml2::read_html(url)
tables <- html_table(doc)

length(tables)
# that's a lot of tables!

# and unfortunately the organization is a bit messy:

tables[[1]] # date for polls on tables[[2]]
tables[[2]]

tables[[3]] # date for polls on tables[[4]]
tables[[4]]

# let's try to clean up tables[[4]]
raw <- tables[[6]]

# Checklist:
# - separate Race/Topic into 
#    (a) Race 
#    (b) Candidates
# - separate Results along comma - 

library(tidyr)

step1 <- raw %>%
  separate(`Race/Topic   (Click to Sort)`, into=c("Race", "Candidates"), sep=":") %>% 
  separate(Results, into=c("_cand1", "_cand2", "_cand3", "_cand4"), sep=", ")

# Candidates' names are re-ordered according to the outcome. Argh!!
# - reformat the data: 
#    (a) introduce an ID for each poll
#    (b) make the four candidate variables _cand1, _cand2, ... a single variable

library(dplyr)

step2 <- step1 %>% mutate(
  id = 1:nrow(step1)
) %>% gather(key="Position", value = "Candidate/Result", starts_with("_cand"))

glimpse(step2)

# - separate Candidate/Result into two variables
# - make result a numeric variable
# - filter out NAs

step3 <- step2 %>% 
  separate(col= `Candidate/Result`, into = c("Candidate", "Result"), sep = " ") %>%
  mutate(
    Result = as.numeric(Result)
  ) %>% filter(!is.na(Result))

# focus on just the comparison between Clinton and Trump:
# - spread Candidate & Result back into Clinton/Trump

step4 <- step3 %>% filter(Candidate %in% c("Clinton", "Trump")) %>% 
  select(-Position) %>%
  spread(Candidate, Result)

library(ggplot2)
step4 %>% mutate(
  Race = reorder(Race, Clinton-Trump)
) %>%
  ggplot(aes(x = Race, y = Clinton - Trump, colour= Poll)) +
  geom_point() + coord_flip()

# Next steps:
# - wrap steps 1 through 4 into a function 
# - try the function out on all the other tables
# - get the dates into the data (use tables[[n-1]] info on tables[[n]])
# - write out the data into a nice format
# - put the data on github
# - update it for the next XX days