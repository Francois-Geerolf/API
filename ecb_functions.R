

wave_to_date <- function(data){
  data %>%
    mutate(year = wave %>% substr(4, 7),
           month = wave %>% substr(1, 2),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -month, -wave) %>%
    select(date, everything())
}

time_to_date <- function(data){
  data %>%
    mutate(date = paste0(obsTime, "-01-01") %>% as.Date) %>%
    select(-obsTime) %>%
    select(date, everything())
}


year_to_date <- function(data){
  data %>%
    mutate(date = paste0(obsTime, "-01-01") %>% as.Date) %>%
    select(-obsTime) %>%
    select(date, everything())
}


quarter_to_date <- function(data){
  data %>%
    mutate(year = obsTime %>% substr(1, 4),
           qtr = obsTime %>% substr(7, 7) %>% as.numeric,
           month = (qtr - 1)*3 + 1,
           month = month %>% str_pad(., 2, pad = "0"),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -qtr, -month, -obsTime) %>%
    select(date, everything())
}

month_to_date <- function(data){
  data %>%
    mutate(date = paste0(obsTime, "-01-01") %>% as.Date) %>%
    select(-obsTime) %>%
    select(date, everything())
}


day_to_date <- function(data){
  data %>%
    mutate(date = obsTime %>% as.Date) %>%
    select(-obsTime) %>%
    select(date, everything())
}
