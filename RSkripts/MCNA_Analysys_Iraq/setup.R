# PACKAGE INSTALLATION
# run this once on your system (or to update)
# do not source this file


install.packages("assertthat")
install.packages("crayon")
install.packages("data.table")
install.packages("devtools")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("htmltools")
install.packages("knitr")
install.packages("magrittr")
install.packages("purrr")
install.packages("questionr")
install.packages("reshape")
install.packages("reshape2")
install.packages("rmarkdown")
install.packages("stringi")
install.packages("stringr")
install.packages("survey")
install.packages("testthat")
install.packages("tibble")
install.packages("tidyr")
install.packages("utils")

devtools::install_github(
  "mabafaba/koboquest",
  build_opts = c())

devtools::install_github(
  "mabafaba/kobostandards",
  build_opts = c()
)


devtools::install_github(
  "mabafaba/xlsformfill",
  build_opts = c()
)




devtools::install_github(
  "mabafaba/composr",
  build_opts = c()
)



devtools::install_github(
  "mabafaba/hypegrammaR", 
  ref = "master", 
  build_vignettes = TRUE
)

devtools::install_github(
  "mabafaba/hypegrammaR", 
  ref = "use_srvyr", 
  build_vignettes = TRUE)









