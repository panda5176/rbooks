library(tidyverse)

tibble(x = 1:5, y = 1, z = x ^ 2 + y)
tribble(~ x, ~ y, ~ z,
        "a", 2, 3.6,
        "b", 1, 8.5)

as_tibble(iris)
as.data.frame(as_tibble(iris))

enframe(1:3)
enframe(c(a = 5, b = 7))
deframe(enframe(c(a = 5, b = 7)))
