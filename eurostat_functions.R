
time_to_date <- function(data){
  data %>%
    mutate(date = paste0(time, "-01-01") %>% as.Date) %>%
    select(-time) %>%
    select(date, everything())
}


time_to_enddate <- function(data){
  data %>%
    mutate(date = paste0(time, "-12-31") %>% as.Date) %>%
    select(-time) %>%
    select(date, everything())
}



year_to_date <- function(data){
  data %>%
    mutate(date = paste0(time, "-01-01") %>% as.Date) %>%
    select(-time) %>%
    select(date, everything())
}



year_to_enddate <- function(data){
  data %>%
    mutate(date = paste0(time, "-12-31") %>% as.Date) %>%
    select(-time) %>%
    select(date, everything())
}


quarter_to_date <- function(data){
  data %>%
    mutate(year = time %>% substr(1, 4),
           qtr = time %>% substr(6, 6) %>% as.numeric,
           month = (qtr - 1)*3 + 1,
           month = month %>% str_pad(., 2, pad = "0"),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -qtr, -month, -time) %>%
    select(date, everything())
}


day_to_date <- function(data){
  data %>%
    mutate(year = time %>% substr(1, 4),
           month = time %>% substr(6, 7),
           day = time %>% substr(9, 10),
           date = paste0(year, "-", month, "-", day) %>% as.Date) %>%
    select(-year, -month, -day, -time) %>%
    select(date, everything())
}

month_to_date <- function(data){
  data %>%
    mutate(year = time %>% substr(1, 4),
           month = time %>% substr(6, 7),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -month, -time) %>%
    select(date, everything())
}

semester_to_date <- function(data){
  data %>%
    mutate(year = time %>% substr(1, 4),
           semester = time %>% substr(6, 6),
           month = ifelse(semester == "1", "01", "07"),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -month, -semester, -time) %>%
    select(date, everything())
}
