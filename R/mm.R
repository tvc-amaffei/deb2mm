#' Mutate input tibble based on match rules found in a match-mutate rules tibble
#'
#' @param observations_tbl table of observations to be matched
#' @param rules_tbl table of match/replace rules
#' @return tibble
#' @examples
#' # This function is based on a python script written by Andrew Maffei and
#' # Joe Futrelle while they worked at WHOI
#' joes_function <- function(string){
#'   library(stringr)
#'   lookup_tbl <- tibble::tribble(
#'     ~name, ~val,
#'     "AMAFF", "Andrew Maffei",
#'     "JFUTR", "Joe Futrelle")
#'   ans <- lookup_tbl %>%
#'     dplyr::filter (name == string) %>%
#'     dplyr::select (val) %>%
#'     toString()
#'   return(ans)
#' }
#'
#' transactions  <- tibble::tribble(
#'   ~date, ~expcode, ~desc, ~ref, ~amt,
#'   "2019-01-01", "5170", "AMAFF-SANFRAN-21320","","329.00",
#'   "2019-01-02", "5210", "PO# 79342 to Staples", "AMAFF", "92.64",
#'   "2019-01-03", "5170", "Car Rental Refund","","120.32")
#'
#' rules <- tibble::tribble(
#'   ~date, ~expcode, ~desc, ~ref, ~amt, ~category, ~object, ~xactloc, ~po, ~destination, ~person,
#'   "","(?<expcode>5170)","","","","Travel:Domestic:Unknown","Unknown","Unknown", "Unknown","Unknown","Unknown",
#'   "","(?<expcode>5170)","(?<person>[^-]+)-(?<dest>[^-]+)-(?<ponum>\\d{5})","","","Travel:Domestic:{person}","TravelVendor","WWW","{ponum}", "{dest}","{joes_function({person})}",
#'   "","(?<expcode>5210)","Unknown","Unknown","Unknown","Equipment:Unknown","Unknown","Unknown","Unknown","Unknown","Unknown",
#'   "","(?<expcode>5210)","PO# (?<ponum>\\d{5}) to Staples","(?<person>.*)","","Supplies:Staples:Rocketbooks","Staples","Falmouth","{ponum}","Unknown","{joes_function({person})}"
#' )
#' mm(transactions, rules)
#'
#' @importFrom stringr str_glue_data %>%
#' @importFrom rematch2 re_match
#' @importFrom dplyr bind_rows bind_cols select
#' @export
mm <- function (observations_tbl, rules_tbl) {
  # Determine sets of column names
  obs_columns <- colnames(observations_tbl)
  match_columns <- colnames(rules_tbl)
  addon_columns <-
    setdiff(colnames(rules_tbl), colnames(observations_tbl))
  output_columns <-
    union(colnames(rules_tbl), colnames(observations_tbl))

  result_tbl <- NULL

  # For each row in observations_tbl, scan through all rows of match table
  # This can not be a vector op because order of match table rows is important
  for (irow in 1:nrow(observations_tbl)) {
    # for each observation row
    obsvec <- observations_tbl[irow,]      #   grab the observation vector
    resultvec <- NULL                      #   initialize result vector
    for (mrow in 1:nrow(rules_tbl)) {
      #   for each row/vector in match tibble
      # look at each mm rule
      # loop through obscol names in match vector
      rulesvec <- rules_tbl[mrow,]
      namedcap  <- NULL
      row_matched <- TRUE
      for (obscol in obs_columns) {
        # loop through columns found observation vector
        if (obscol %in% match_columns) {
          # column also exists in the match vector
          regex <- rulesvec[obscol]       # grab regex string for rule
        } else
          regex <- ""                  # else set regex to null string
        if (regex == "") { next }
        regex <-
          paste0("^", regex, "$") # polish up the regex to match full string
        m <-
          re_match(text = obsvec[obscol], pattern = regex) # check for match & grab captured values
        if (!is.na(m$.match)) {  # observation vector matches regex
          if (is.null(namedcap)) {
            namedcap  <- select(m,-c(.text, .match)) # grab values of named captures in regex
          } else {
            namedcap  <- bind_cols(namedcap , select(m,-c(.text, .match)))
          }
        } else {
          # did not match
          row_matched <- FALSE
        }
      }
      if (!row_matched) { next } # top of loop since all the matches in rulesvec are not true

      # if a row matched this rule continue to add new columns to result table
      namedcap  <-
        bind_cols(obsvec, namedcap) # add derived named captures to orig col values
      namedcap  <- as.list(namedcap)
      for (addcol in addon_columns) {
        if (rulesvec[addcol] == "") { # not looking for match in this column, go back
          next
        }
        # substitute and evalute R expression contained in column of rules vector
        resultvec[addcol] <-
          str_glue_data(namedcap , toString(rulesvec[addcol]))
      }
    }
    result_tbl <- bind_rows(result_tbl, resultvec) # add the row to the results table
  }
  bind_cols(observations_tbl, result_tbl) # bind the columns in orig observations and results tables
}
