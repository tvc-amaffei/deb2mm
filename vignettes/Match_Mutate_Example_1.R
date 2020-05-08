## -----------------------------------------------------------------------------
library(deb2mm)
library(dplyr)
library(stringr)

## -----------------------------------------------------------------------------
joes_function <- function(string){
  lookup_tbl <- tribble(
    ~name, ~val,
    "AMAFF", "Andrew Maffei",
    "JFUTR", "Joe Futrelle")
    ans <- lookup_tbl %>%
      filter (name == string) %>%
      select (val) %>%
      toString()
  return(ans)
}

## -----------------------------------------------------------------------------
observed_data_tbl  <- tibble::tribble(
 ~date, ~expcode, ~desc, ~ref, ~amt,
 "2019-01-01", "5170", "AMAFF-SANFRAN-21320","","329.00",
 "2019-01-02", "5210", "PO# 79342 to Staples", "AMAFF", "92.64",
 "2019-01-03", "5170", "Car Rental Refund","","120.32")
observed_data_tbl

## -----------------------------------------------------------------------------
rules_tbl <- tibble::tribble(
 ~date, ~expcode, ~desc, ~ref, ~amt, ~category, ~object, ~place, ~po,
 "","(?<expcode>5170)","","","","Travel:Domestic:Unknown","","Woods Hole", "",
 "","(?<expcode>5170)","(?<object>[^-]+)-[^-]+-(?<po>\\d{5})","","","Travel:Domestic:{object}","{joes_function(object)}","","{po}",
 "","(?P<expcode>5210)","","","","Equipment:Unknown","","","",
 "","(?P<expcode>5210)","PO# (?<po>\\d{5}) to Staples,(?<ref>.*)","","Equipment:Staples:{ref}","Staples","","{po}",""
 )
rules_tbl

## -----------------------------------------------------------------------------
result_tbl <- mm(observed_data_tbl, rules_tbl)
result_tbl

