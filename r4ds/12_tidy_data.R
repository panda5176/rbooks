library(tidyverse)

tidy4a <- pivot_longer(table4a,
                       c("1999", "2000"),
                       names_to = "year",
                       values_to = "cases")
tidy4b <- pivot_longer(table4b,
                       c("1999", "2000"),
                       names_to = "year",
                       values_to = "population")
left_join(tidy4a, tidy4b)

pivot_wider(table2, names_from = type, values_from = count)

separate(
  table3,
  rate,
  into = c("cases", "population"),
  sep = "/",
  convert = TRUE
) %>%
  separate(year, into = c("century", "year"), sep = 2) %>%
  unite(year, century, year, sep = "")
