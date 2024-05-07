library("tidyverse")
library("rsdmx")
library("zoo")
library("scales")

nber_recessions <- read_csv("
Peak,Trough
2001-03-01,2001-11-01
2007-12-01,2009-06-01
2020-02-01,2020-04-01
")

cepr_recessions <- read_csv("
Peak,Trough
2019-10-01,2020-04-01
2011-07-01,2013-01-01
2008-01-01,2009-04-01
")

QNA_EXPENDITURE_CAPITA <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA_EXPENDITURE_CAPITA,1.0/Q..USA+EA20........LR.." %>%
  readSDMX() %>%
  as_tibble

QNA_EXPENDITURE_CAPITA_var <- "https://sdmx.oecd.org/public/rest/dataflow/OECD.SDD.NAD/DSD_NAMAIN1@DF_QNA_EXPENDITURE_CAPITA/1.0?references=all" %>%
  readSDMX()

## In English -------------

metadata_load <- function(code, CL_code, data = QNA_EXPENDITURE_CAPITA_var){
  assign(code, as.data.frame(data@codelists, codelistId = CL_code) %>%
           select(id, label.en) %>%
           setNames(c(code, str_to_title(code))),
         envir = .GlobalEnv)
}

metadata_load("REF_AREA", "CL_AREA")

QNA_EXPENDITURE_CAPITA %>%
  mutate(date = obsTime %>% as.yearqtr(format = "%Y-Q%q") %>% as.Date()) %>%
  filter(year(date) >= 1999) %>%
  left_join(REF_AREA, by = "REF_AREA") %>%
  group_by(Ref_area) %>%
  arrange(date) %>%
  mutate(obsValue = 100 * obsValue / obsValue[1]) %>%
  ggplot(.) + theme_minimal() + xlab("") + ylab("GDP Per Capita (1999-Q1 = 100)") +
  geom_line(aes(x = date, y = obsValue, color = Ref_area)) + 
  scale_color_manual(values = c("#003399", "#B22234")) +
  geom_rect(data = nber_recessions %>%
              filter(Peak > as.Date("1999-01-01")), 
            aes(xmin = Peak, xmax = Trough, ymin = 0, ymax = +Inf), 
            fill = "#003399", alpha = 0.1)  +
  geom_rect(data = cepr_recessions %>%
              filter(Peak > as.Date("1999-01-01")), 
            aes(xmin = Peak, xmax = Trough, ymin = 0, ymax = +Inf), 
            fill = "#B22234", alpha = 0.1)  +
  scale_x_date(breaks = c(seq(1999, 2100, 5), seq(1997, 2100, 5)) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  theme(legend.position = c(0.26, 0.8),
        legend.title = element_blank()) +
  scale_y_log10(breaks = seq(50, 200, 5))

## En Français -------------

metadata_load_fr <- function(code, CL_code, data = QNA_EXPENDITURE_CAPITA_var){
  assign(code, as.data.frame(data@codelists, codelistId = CL_code) %>%
           select(id, label.fr) %>%
           setNames(c(code, str_to_title(code))),
         envir = .GlobalEnv)
}

metadata_load_fr("REF_AREA", "CL_AREA")

QNA_EXPENDITURE_CAPITA %>%
  mutate(date = obsTime %>% as.yearqtr(format = "%Y-Q%q") %>% as.Date()) %>%
  filter(year(date) >= 1999) %>%
  left_join(REF_AREA, by = "REF_AREA") %>%
  group_by(Ref_area) %>%
  arrange(date) %>%
  mutate(obsValue = 100 * obsValue / obsValue[1]) %>%
  ggplot(.) + theme_minimal() + xlab("") + ylab("PIB par habitant (1999T1 = 100)") +
  geom_line(aes(x = date, y = obsValue, color = Ref_area)) + 
  scale_color_manual(values = c("#B22234", "#003399")) +
  geom_rect(data = nber_recessions %>%
              filter(Peak > as.Date("1999-01-01")), 
            aes(xmin = Peak, xmax = Trough, ymin = 0, ymax = +Inf), 
            fill = "#B22234", alpha = 0.1)  +
  geom_rect(data = cepr_recessions %>%
              filter(Peak > as.Date("1999-01-01")), 
            aes(xmin = Peak, xmax = Trough, ymin = 0, ymax = +Inf), 
            fill = "#003399", alpha = 0.1)  +
  scale_x_date(breaks = c(seq(1999, 2100, 5), seq(1997, 2100, 5)) %>% paste0("-01-01") %>% as.Date,
               labels = date_format("%Y")) +
  theme(legend.position = c(0.26, 0.8),
        legend.title = element_blank()) +
  scale_y_log10(breaks = seq(50, 200, 5))



