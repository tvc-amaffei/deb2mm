#' Mutate input tibble based on match rules found in a match-mutate tibble
#'
#' @param observations_tbl table of observations to be matched
#' @param match_tbl table of match/replace rules
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
#' observed_data_tbl  <- tibble::tribble(
#'   ~date, ~expcode, ~desc, ~ref, ~amt,
#'   "2019-01-01", "5170", "AMAFF-SANFRAN-21320","","329.00",
#'   "2019-01-02", "5210", "PO# 79342 to Staples", "AMAFF", "92.64",
#'   "2019-01-03", "5170", "Car Rental Refund","","120.32")
#'
#' rules_tbl <- tibble::tribble(
#'   ~date, ~expcode, ~desc, ~ref, ~amt, ~category, ~object, ~place, ~po, ~destination, ~traveler,
#'   "","(?<expcode>5170)","","","","Travel:Domestic:Unknown","","Woods Hole", "","","",
#'   "","(?<expcode>5170)","(?<trav>[^-]+)-(<dest>[^-]+)-(?<po>\\d{5})","","","Travel:Domestic:{trav}","TravelVendor","","{po}", "{dest}","{joes_function(trav)}",
#'   "","(?P<expcode>5210)","","","","Equipment:Unknown","","","","","",
#'   "","(?P<expcode>5210)","PO# (?<po>\\d{5}) to Staples","(?<ref>.*)","","Equipment:Staples:{ref}","Staples","","{po}","",""
#' )
#' mm(observed_data_tbl, rules_tbl)
#'
#' @importFrom stringr str_glue_data %>%
#' @importFrom rematch2 re_match
#' @importFrom dplyr bind_rows bind_cols select
#' @export
mm <- function (observations_tbl, match_tbl) {
  # Determine sets of column names
  obs_columns <- colnames(observations_tbl)
  match_columns <- colnames(match_tbl)
  addon_columns <-
    setdiff(colnames(match_tbl), colnames(observations_tbl))
  output_columns <-
    union(colnames(match_tbl), colnames(observations_tbl))

  result_tbl <- NULL

  # For each ron in observations_tbl, scan all rows of the match table
  # This can not be a vector op because order of rows is important
  for (irow in 1:nrow(observations_tbl)) {
    # for each observation row
    obsvec <-
      observations_tbl[irow,]     #   grab the observation vector
    resultvec <-
      NULL                      #   initialize result vector
    for (mrow in 1:nrow(match_tbl)) {
      #   for each row/vector in match tibble
      # look at each mm rule
      # loop through obscol names in match vector
      matchvec <- match_tbl[mrow,]
      namedcap  <- NULL
      row_matched <- TRUE
      for (obscol in obs_columns) {
        # loop through cols found observation vector
        if (obscol %in% match_columns) {
          # if col also exists in match vector
          regex <- matchvec[obscol]       # grab it
        } else
          regex <-
            ""                  # else set it to null string
        if (regex == "") {
          next
        }
        regex <-
          paste0("^", regex, "$") # polish up the regex to match full string
        m <-
          re_match(text = obsvec[obscol], pattern = regex) # check for match
        if (!is.na(m$.match)) {
          # observation vector meets match criteria
          if (is.null(namedcap)) {
            # grab values of named captures in namedcap
            namedcap  <- select(m,-c(.text, .match))
          } else {
            namedcap  <- bind_cols(namedcap , select(m,-c(.text, .match)))
          }
        } else {
          # did not match
          row_matched <- FALSE
        }
      }
      if (!row_matched) {
        # if all the matches in matchvec are not true
        next
      }
      # row matched this rule so continue to add new columns
      namedcap  <-
        bind_cols(obsvec, namedcap) # add derived named captures to orig col values
      namedcap  <- as.list(namedcap)
      for (addcol in addon_columns) {
        if (matchvec[addcol] == "") { # not looking for match in this column, go back
          next
        }
        # substitute and evalute R expression contained in cell
        resultvec[addcol] <-
          str_glue_data(namedcap , toString(matchvec[addcol]))
      }
    }
    result_tbl <- bind_rows(result_tbl, resultvec)
  }
  bind_cols(observations_tbl, result_tbl)
}
