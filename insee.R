# install.packages("rsdmx")
library(rsdmx)
library(tidyverse)

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


