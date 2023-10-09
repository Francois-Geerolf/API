

obsTime_FREQUENCY_to_date <- function(obsTime, FREQUENCY){
  if (FREQUENCY == "A"){
    date = paste0(obsTime, "-01-01") %>% as.Date
  } else if (FREQUENCY == "Q"){
    year = obsTime %>% substr(1, 4)
    qtr = obsTime %>% substr(7, 7) %>% as.numeric
    month = (qtr - 1)*3 + 1
    month = month %>% str_pad(., 2, pad = "0")
    date = paste0(year, "-", month, "-01") %>% as.Date
  } else if (FREQUENCY == "M"){
    date = paste0(obsTime, "-01-01") %>% as.Date
  }
  return(date)
}

frequency_to_date <- function(data){
  data %>%
    rowwise() %>%
    mutate(date = obsTime_FREQUENCY_to_date(obsTime, FREQUENCY)) %>%
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


quarter_to_enddate <- function(data){
  data %>%
    mutate(year = obsTime %>% substr(1, 4),
           qtr = obsTime %>% substr(7, 7) %>% as.numeric,
           month = (qtr)*3,
           month = month %>% str_pad(., 2, pad = "0"),
           date = as.Date(paste0(year, "-", month, "-01")),
           date = date + months(1) - days(1)) %>%
    select(-year, -qtr, -month, -obsTime) %>%
    select(date, everything())
}



quarter_to_date2 <- function(data){
  data %>%
    mutate(year = yearqtr %>% substr(1, 4),
           qtr = yearqtr %>% substr(6, 6) %>% as.numeric,
           month = (qtr - 1)*3 + 1,
           month = month %>% str_pad(., 2, pad = "0"),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -qtr, -month, -yearqtr) %>%
    select(date, everything())
}


month_to_date <- function(data){
  data %>%
    mutate(date = paste0(obsTime, "-01") %>% as.Date) %>%
    select(-obsTime) %>%
    select(date, everything())
}


year_to_date <- function(data){
  data %>%
    mutate(date = paste0(obsTime, "-01-01") %>% as.Date) %>%
    select(-obsTime) %>%
    select(date, everything())
}


year_to_enddate <- function(data){
  data %>%
    mutate(date = paste0(obsTime, "-12-31") %>% as.Date) %>%
    select(-obsTime) %>%
    select(date, everything())
}

yearqtr_to_date <- function(data){
  data %>%
    mutate(year = yearqtr %>% floor,
           month = (yearqtr - year)*12 + 1,
           month = str_pad(month, 2, pad = 0),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -month, -yearqtr) %>%
    select(date, everything())
}

yearqtr_to_enddate <- function(data){
  data %>%
    mutate(year = yearqtr %>% floor,
           month = (yearqtr - year)*12 + 1,
           month = str_pad(month, 2, pad = 0),
           date = as.Date(paste0(year, "-", month, "-01")) + months(1) - day(1)) %>%
    select(-year, -month, -yearqtr) %>%
    select(date, everything())
}



yearqtr_to_date2 <- function(data){
  data %>%
    mutate(year = yearqtr %>% floor,
           month = (yearqtr - year)*12 + 1,
           month = str_pad(month, 2, pad = 0),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -month) %>%
    select(date, everything())
}

date_to_yearqtr <- function(data){
  data %>%
    mutate(year = date %>% year,
           month = date %>% month,
           yearqtr = year + (month-1)/12) %>%
    select(-year, -month, -date) %>%
    select(yearqtr, everything())
}


enddate_to_yearqtr <- function(data){
  data %>%
    mutate(year = date %>% year,
           month = date %>% month,
           yearqtr = year + (month-3)/12) %>%
    select(-year, -month, -date) %>%
    select(yearqtr, everything())
}


enddate_to_yearqtr2 <- function(data){
  data %>%
    mutate(year = date %>% year,
           month = date %>% month,
           yearqtr = year + (month)/12) %>%
    select(-year, -month, -date) %>%
    select(yearqtr, everything())
}

