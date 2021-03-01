library(tidyverse)
library(readxl)
library(yaml)
library(ahp)

dataf <- "data/survey.xlsx"

data <- read_excel(dataf)

data <- data %>% select(matches("^(prio|imp)_")) %>% mutate_all(as.numeric)

data <- data %>% 
  mutate(respondent = str_c("respondent_", row_number())) %>% 
  gather(key, val, -respondent) %>% 
  extract(key, c("measure", "criteria1", "criteria2"), regex="(.*)_(.*)_x_(.*)") %>% 
  spread(measure, val) %>% 
  mutate(pref = imp^prio) %>%
  select(-prio, -imp)

criteria <- union(data$criteria1, data$criteria2)
respondents <- unique(data$respondent)

data <- 
  data %>% 
  nest(-respondent) %>% 
  mutate(prefs = map(data, ~list(pairwise = pmap(., ~list(...)))))

goal.preferences <- set_names(data$prefs, data$respondent)

cases <- 
  rep(list(c(0, 1)), length(criteria)) %>% 
  set_names(criteria) %>% 
  cross_df() 

caseids <- 
  str_c("case", str_pad(seq_along(pull(cases)), ceiling(log10(2^length(criteria))), pad = "0"))

cases %>% mutate(id = caseids) %>% write_csv("out/cases.csv")

alternatives <- 
  cases %>% 
  pmap(~list(...)) %>% 
  set_names(caseids)

goal.children <- 
  cases %>% 
  map(
    ~list(
      preferences =
        rerun(
          length(respondents),
          list(score = map2(., caseids, ~set_names(list(.x), .y)))) %>% 
        set_names(respondents),
      children = alternatives))

ahptree <- 
  list(
    Version = 2.0,
    Goal = 
      list(
        name = "Criteria ranking",
        preferences = goal.preferences,
        children = goal.children))

ahptree %>% write_yaml("out/tree.ahp")
