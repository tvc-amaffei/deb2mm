#' Mutate on observed data table based on a table of match/replace rules
#'
#' @param input_table table of observations to be matched
#' @param match_table table of match/replace rules
#' @return tibble
#' @examples
#'
#' observed_data_tbl <- tibble::tribble(
#' ~date, ~expcode, ~desc, ~ref, ~amt,
#' "2019-01-01", "5170", "AMAFF-SANFRAN-21320","","329.00",
#' "2019-01-02", "5210", "PO# 79342 to Staples", "AMAFF", "92.64",
#' "2019-01-03", "5170", "Car Rental Refund","","120.32")
#'
#' rules_tbl <- tibble::tribble(
#' ~date, ~expcode, ~desc, ~ref, ~amt, ~category, ~object, ~place, ~po,
#' ".*", "5170", ".*", ".*", ".*", "~'Travel:Domestic:Unknown'",  "", "~'Woods Hole'", "",
#' ".*", "5170", "^([A-Z]+)-([A-Z]+)-([0-9]{5})$", "^(.*)$", ".*", '~paste0("Travel:Domestic:", gr(desc,1))', "~gr(desc,1)", "", "~gr(desc,3)",
#' ".*", "5210", ".*", ".*", ".*", "~'Equipment:Unknown'",  "", "", "",
#' ".*", "5210", "^PO# ([0-9]{5}) to Staples$", ".*", ".*", '~paste0("Equipment:Staples:", gr(ref,0))', "~'Staples'", "", "~gr(desc,1)")
#'
#' match_mutate(observed_data_tbl, rules_tbl)
#' @importFrom tibble tribble
#' @export
match_mutate <- function ( input_table, match_table) {
  return()
}
