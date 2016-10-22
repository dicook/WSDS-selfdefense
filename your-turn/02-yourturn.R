# get the author names
namess <- data.frame(db, stringsAsFactors = F)$Author
# looks very messy, much more so than the maintainer
head(namess)

# get rid of punctuation
namess <- gsub("[[:punct:]]", " ", namess)
# remove leading space before first name
namess <- str_trim(namess)
# remove the line breaks 
namess <- gsub("\\n", "", namess)
# remove the aut, cre
namess <- gsub("aut", "", namess)
namess <- gsub("cre", "", namess)
namess <- str_trim(namess)
namess <- data.frame(name = namess, stringsAsFactors = F)
# separate the names by white space 
namess %>% 
  separate(name, into = paste("name", 1:488), sep = "\\s+") -> name_cols 
# put the package back in for grouping 
name_cols$package <- data.frame(db)$Package
# put the year in for grouping
name_cols$year <- as.integer(substr(data.frame(db)$Published, 1, 4))
# take only the odd numbered columns. 
# First Last names, assume two names per author, get only the first name (and the ID columns for gathering)
names_cols_first <- name_cols[,c(which(gtools::odd(1:ncol(name_cols))),490)]
# create package, year, name data frame
names_cols_first %>% 
  gather(key = var, value = name, -c(package,year)) %>% 
  filter(!is.na(name)) %>% 
  mutate(name = tolower(name)) -> first_names_pkg
# use genderized names from before 
first_names_pkg_gender <- left_join(first_names_pkg,
                                    genderized_names, by=c("name"="firstname"))
head(first_names_pkg_gender)
# 
# Be careful here: there is a limit of 1000 queries a day
# givennames <- findGivenNames(unique(first_names_pkg_gender$name))


first_names_pkg_gender %>% 
  group_by(gender, year) %>% 
  dplyr::summarize(count = n()) -> author_summ
ggplot(first_names_pkg_gender %>% filter(!is.na(gender)), 
       aes(x = year, fill = gender)) +
  geom_bar(position = 'fill') + 
  scale_fill_manual("Gender", values=c("female"="#fc8d59", "male"="#2166ac"),
                    na.value="white")