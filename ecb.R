# install.packages("rsdmx")
library(rsdmx)
library(tidyverse)
library(scales)
source("oecd_functions.R")

# oecd_codelist --------

oecd_datasets <- "https://stats.oecd.org/RestSDMX/sdmx.ashx/GetKeyFamily/all" %>%
  readSDMX() %>%
  as_tibble

oecd_datastructure <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/QNA" %>%
  readSDMX() %>%
  slot("concepts") %>%
  as.data.frame()

oecd_codelist <- "http://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/QNA" %>%
  readSDMX() %>%
  slot("codelists") %>%
  as.data.frame(codelistId = "CL_QNA_SUBJECT")

# Example ---------

"http://stats.oecd.org/restsdmx/sdmx.ashx/GetData/QNA/EA20+USA.B1_GE.VOBARSA.Q?startTime=2000" %>%
  readSDMX() %>%
  as_tibble %>%
  quarter_to_date %>%
  select_if(~ n_distinct(.) > 1) %>%
  group_by(LOCATION) %>%
  mutate(obsValue = 100 * obsValue / obsValue[date == as.Date("2007-01-01")]) %>%
  ggplot(.) + theme_minimal() + xlab("") + ylab("Real GDP (2007 = 100)") +
  geom_line(aes(x = date, y = obsValue, color = LOCATION)) +
  scale_color_manual(values = c("#003399", "#BF0A30")) +
  scale_x_date(breaks = seq(1960, 2026, 2) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  scale_y_log10(breaks = seq(70, 200, 2))


