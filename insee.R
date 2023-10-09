# install.packages("rsdmx")
library(rsdmx)
library(tidyverse)
library(scales)
source("insee_functions.R")

# insee_datasets --------

insee_datasets <- "https://bdm.insee.fr/series/sdmx/dataflow" %>%
  readSDMX %>%
  as_tibble

save(insee_datasets, file = "insee_datasets.RData")

View(insee_datasets)

# insee_datastructure_2 ------

insee_datastructure <- "https://www.bdm.insee.fr/series/sdmx/datastructure/FR1/IPLA-IPLNA-2015" %>%
  readSDMX()

insee_datastructure <- "https://www.bdm.insee.fr/series/sdmx/datastructure/FR1/IPLA-IPLNA-2015" %>%
  readSDMX() %>%
  slot("datastructures") %>%
  pluck(1) %>%
  slot("Components") %>%
  as_tibble

save(insee_datastructure, file = "insee_datastructure.RData")

View(insee_datastructure)

# insee_dataset ---------

insee_dataset <- "https://bdm.insee.fr/series/sdmx/data/IPLA-IPLNA-2015" %>%
  readSDMX %>%
  as_tibble %>%
  mutate(OBS_VALUE = as.numeric(OBS_VALUE))

save(insee_dataset, file = "insee_dataset.RData")

insee_dataset %>%
  group_by(INDICATEUR) %>%
  summarise(Nobs = n())

# insee_codelist ----------

insee_codelist <- paste0("https://www.bdm.insee.fr/series/sdmx/codelist/FR1/CL_INDICATEUR") %>%
  readSDMX() %>%
  as_tibble %>%
  select(INDICATEUR = id, Indicateur = label.fr)


insee_dataset %>%
  left_join(insee_codelist, by = "INDICATEUR") %>%
  group_by(INDICATEUR, Indicateur) %>%
  summarise(Nobs = n())


# En utilisant les IDBANKs --------

"010567006+010567010+010567012+010567056" %>%
  paste0("https://www.bdm.insee.fr/series/sdmx/data/SERIES_BDM/", .) %>%
  readSDMX %>%
  as_tibble %>%
  quarter_to_date %>%
  mutate(OBS_VALUE = as.numeric(OBS_VALUE)) %>%
  group_by(REF_AREA) %>%
  mutate(OBS_VALUE = 100*OBS_VALUE / OBS_VALUE[date == as.Date("1998-01-01")]) %>%
  mutate(TITLE_FR = gsub("- Appartements", "\nAppartements", TITLE_FR)) %>%
  ggplot + geom_line(aes(x = date, y = OBS_VALUE, color = TITLE_FR)) +
  theme_minimal()  +
  scale_x_date(breaks = as.Date(paste0(seq(1960, 2050, 2), "-01-01")),
               labels = date_format("%Y")) +
  theme(legend.position = c(0.25, 0.85),
        legend.title = element_blank()) +
  xlab("") + ylab("Indice des prix des logements anciens (1998 = 100)") +
  scale_y_log10(breaks = seq(0, 7000, 50))
