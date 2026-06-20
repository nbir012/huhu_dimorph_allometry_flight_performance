library(tidyverse)
library(rptR)

dat <- read_csv(here::here("data", "allometry", "repeatabilityLong.csv"))

traits <- c(
  "lengthElytron",
  "lengthWing",
  "lengthFemur",
  "lengthTibia",
  "lengthF9",
  "lengthAntenna"
)

rpt_tbl <- map_dfr(traits, function(tr) {
  r <- rpt(
    reformulate("(1 | id)", response = tr),
    grname = "id",
    data = dat,
    datatype = "Gaussian",
    nboot = 1000,
    npermut = 0
  )
  tibble(
    trait = tr,
    R = r$R[["id"]],
    CI_low = r$CI_emp[1],
    CI_high = r$CI_emp[2]
  )
})

rpt_tbl |> arrange(R)
