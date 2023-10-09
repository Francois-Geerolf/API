# install.packages("rsdmx")
library(rsdmx)
library(tidyverse)

# insee_datasets --------

insee_datasets <- "https://bdm.insee.fr/series/sdmx/dataflow" %>%
  readSDMX %>%
  as_tibble

save(insee_datasets, file = "insee_datasets.RData")

View(insee_datasets)

# insee_datastructure ------

insee_datastructure <- "https://www.bdm.insee.fr/series/sdmx/datastructure/FR1/IPLA-IPLNA-2015" %>%
  readSDMX()

insee_datastructure_2 <- "https://www.bdm.insee.fr/series/sdmx/datastructure/FR1/IPLA-IPLNA-2015" %>%
  readSDMX() %>%
  slot("datastructures") %>%
  pluck(1) %>%
  slot("Components") %>%
  as_tibble

save(insee_datastructure_2, file = "insee_datastructure_2.RData")

View(insee_datastructure_2)
