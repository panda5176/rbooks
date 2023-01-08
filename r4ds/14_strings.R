library(tidyverse)

str_length("R for data science")

str_c("x", "y")
str_c("x", "y", sep = ", ")
str_c(c("x", "y"), collapse = ", ")
str_c("prefix-", c("a", "b"), "-suffix")
str_split("apple, banana, pear", ", ")

str_sub(c("Apple", "Banana"), 2, 4)
str_sub(c("Apple", "Banana"), -4, -2)
str_trim(" a ")

str_view(c("apple", "banana", "pear"), "an")
str_view(c("apple", "banana", "pear"), ".a.")
str_detect(c("apple", "banana", "pear"), "e")
str_count(c("apple", "banana", "pear"), "a")

str_replace(c("apple", "banana", "pear"), "[aeiou]", "-")
str_replace_all(c("apple", "banana", "pear"), "[aeiou]", "-")
str_replace_all(c("apple", "banana", "pear"), c("a" = "1", "e" = "2"))
