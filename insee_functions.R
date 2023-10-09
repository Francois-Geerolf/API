

TIME_PERIOD_to_date <- function(TIME_PERIOD, FREQ = NA){
  if (is.na(FREQ)){
    if (grepl("Q", TIME_PERIOD)){
      FREQ <- "T"
    } else if (nchar(TIME_PERIOD) == 4){
      FREQ <- "A"
    } else if (nchar(TIME_PERIOD) == 7){
      FREQ <- "M"
    } else if (nchar(TIME_PERIOD) == 10){
      FREQ <- "D"
    } else stop("Unknown time period")
  }
  if (FREQ == "T"){
    year = TIME_PERIOD %>% substr(1, 4)
    qtr = TIME_PERIOD %>% substr(7, 7) %>% as.numeric
    month = (qtr - 1)*3 + 1
    month = month %>% str_pad(., 2, pad = "0")
    date = paste0(year, "-", month, "-01") %>% as.Date
    return(date)
  } else if (FREQ == "A"){
    date = paste0(TIME_PERIOD, "-01-01") %>% as.Date
    return(date)
  } else if (FREQ == "M"){
    date = paste0(TIME_PERIOD, "-01-01") %>% as.Date
    return(date)
  } else if (FREQ == "D"){
    date = TIME_PERIOD %>% as.Date
  } else stop("Unknown time period")
}



year_to_date <- function(data){
  data %>%
    mutate(date = paste0(TIME_PERIOD, "-01-01") %>% as.Date) %>%
    select(-TIME_PERIOD) %>%
    select(date, everything())
}


year_to_date2 <- function(data){
  data %>%
    mutate(date = paste0(year, "-01-01") %>% as.Date) %>%
    select(-year) %>%
    select(date, everything())
}


yearend_to_date <- function(data){
  data %>%
    mutate(date = paste0(TIME_PERIOD, "-12-31") %>% as.Date) %>%
    select(-TIME_PERIOD) %>%
    select(date, everything())
}

year_to_enddate <- function(data){
  data %>%
    mutate(date = paste0(TIME_PERIOD, "-12-31") %>% as.Date) %>%
    select(-TIME_PERIOD) %>%
    select(date, everything())
}


quarter_to_date <- function(data){
  data %>%
    mutate(year = TIME_PERIOD %>% substr(1, 4),
           qtr = TIME_PERIOD %>% substr(7, 7) %>% as.numeric,
           month = (qtr - 1)*3 + 1,
           month = month %>% str_pad(., 2, pad = "0"),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -qtr, -month, - TIME_PERIOD) %>%
    select(date, everything())
}


quarter_to_date2 <- function(data){
  data %>%
    mutate(year = TIME_PERIOD %>% substr(1, 4),
           qtr = TIME_PERIOD %>% substr(6, 6) %>% as.numeric,
           month = (qtr - 1)*3 + 1,
           month = month %>% str_pad(., 2, pad = "0"),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -qtr, -month, - TIME_PERIOD) %>%
    select(date, everything())
}

quarterend_to_date <- function(data){
  data %>%
    mutate(year = TIME_PERIOD %>% substr(1, 4),
           qtr = TIME_PERIOD %>% substr(7, 7) %>% as.numeric,
           month = qtr*3,
           month = month %>% str_pad(., 2, pad = "0"),
           date = (paste0(year, "-", month, "-01") %>% as.Date) + months(1) - days(1)) %>%
    select(-year, -qtr, -month, - TIME_PERIOD) %>%
    select(date, everything())
}

month_to_date <- function(data){
  data %>%
    mutate(date = paste0(TIME_PERIOD, "-01") %>% as.Date) %>%
    select(-TIME_PERIOD) %>%
    select(date, everything())
}
