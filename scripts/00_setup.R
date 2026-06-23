packages <- c(
  "tidyverse",
  "lubridate",
  "janitor",
  "plotly",
  "corrplot",
  "rmarkdown",
  "flexdashboard"
)

install.packages(packages)

lapply(packages, library, character.only = TRUE)
