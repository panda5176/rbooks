library(tidyverse)

parse_logical(c("TRUE", "FALSE", "NA"))
parse_integer(c("1", "2", "3"))

parse_double("1,23", locale = locale(decimal_mark = ","))
parse_number("$123.456.789", locale = locale(grouping_mark = "."))
parse_number("It cost $123.45")

parse_datetime("2010-10-01T2010")
parse_datetime("20101001")
parse_date("2010-10-01")
parse_date("01/02/15", "%m/%d/%y")
parse_time("20:10:01")

read_csv(readr_example("challenge.csv"),
         col_types = cols(x = col_double(),
                          y = col_date()))
