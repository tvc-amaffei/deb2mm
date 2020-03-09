#' Mutate input tibble based on match rules found in a match-mutate tibble
#'
#' @param input_tbl table of observations to be matched
#' @param mm_table table of match/replace rules
#' @return tibble
#' @examples
#' joes_function <- function(string){
#'    lookup_tbl <- tribble(
#'        ~name, ~val,
#'        "AMAFF", "Andrew Maffei",
#'        "JFUTR", "Joe Futrelle")
#'        ans <- lookup_tbl %>%
#'            filter (name == string) %>%
#'            select (val) %>%
#'            toString()
#'    return(ans)
#'   }
#'
#' observed_data_tbl <- tibble::tribble(
#' ~date, ~expcode, ~desc, ~ref, ~amt,
#' "2019-01-01", "5170", "AMAFF-SANFRAN-21320","","329.00",
#' "2019-01-02", "5210", "PO# 79342 to Staples", "AMAFF", "92.64",
#' "2019-01-03", "5170", "Car Rental Refund","","120.32")
#'
#' rules_tbl <- tibble::tribble(
#' ~date, ~expcode, ~desc, ~ref, ~amt, ~category, ~object, ~place, ~po,
#' "","(?<expcode>5170)","","","","Travel:Domestic:Unknown","","Woods Hole", "",
#' "","(?<expcode>5170)","(?<object>[^-]+)-[^-]+-(?<po>\\d{5})","","","Travel:Domestic:{object}","{joes_function(object)}","","{po}",
#' "","(?<expcode>5210)","","","","Equipment:Unknown","","","",
#' "","(?<expcode>5210)","PO# (?<po>\\d{5}) to Staples,(?P<ref>.*)","","Equipment:Staples:{ref}","Staples","","{po}",""
#' )
#'
#' rules_tbl <- tibble::tribble(
#' ~date, ~expcode, ~desc, ~ref, ~amt, ~category, ~object, ~place, ~po,
#' ".*", "(?<expcode>5170)", ".*", ".*", ".*", "~'Travel:Domestic:Unknown'",  "", "~'Woods Hole'", "",
#' ".*", "(?<expcode>5170)", "^([A-Z]+)-([A-Z]+)-([0-9]{5})$", "^(.*)$", ".*", '~paste0("Travel:Domestic:", gr(desc,1))', "~gr(desc,1)", "", "~gr(desc,3)",
#' ".*", "(?<expcode>5210)", ".*", ".*", ".*", "~'Equipment:Unknown'",  "", "", "",
#' ".*", "(?<expcode>5210)", "^PO# (?<po>[0-9]{5}) to Staples$", ".*", ".*", '~paste0("Equipment:Staples:", gr(ref,0))', "~'Staples'", "", "~gr(desc,1)"
#' )
#'
#' result_tbl <- mm(observed_data_tbl, rules_tbl)
#' @importFrom tibble tribble
#' @importFrom stringr str_glue_data
#' @importFrom rematch2 re_match
#' @importFrom dplyr bind_rows bind_cols select
#' @export
mm <- function (input_tbl, mm_table) {
  # Determine sets of column names

  input_columns <- colnames(input_tbl)
  addon_columns <- setdiff(colnames(mm_table), colnames(input_tbl))
  output_columns <- union(colnames(mm_table), colnames(input_tbl))

  result_rows <- NULL

  # For each ron in input_tbl, scan all rows of the match table
  for (irow in 1:nrow(input_tbl)) {
    # for each input_tbl row
    # loop through rows in input_tbl
    i_row <- input_tbl[irow, ]
    result_row <- NULL
    for (mrow in 1:nrow(mm_table)) {
      # look at each mm rule
      # loop through col names in match mutate table
      mm_row <- mm_table[mrow, ]
      namedc <- NULL
      row_matched <- TRUE
      for (col in input_columns) {
        # loop through columns, regex match and accumulate named captures
        regex <- mm_row[col]
        if (regex == "") {
          next
        }
        regex <- paste0("^", regex, "$") # match against whole string
        m <- re_match(text = i_row[col], pattern = regex)
        if (!is.na(m$.match)) {
          #matched
          # accumulate named captures
          if (is.null(namedc)){
              namedc <- select(m, -c(.text, .match))}
          else {
              namedc <- bind_cols(namedc, select(m, -c(.text, .match)))
              }
        } else {
          # did not match
          row_matched <- FALSE
        }
      }
      if (!row_matched) {
        # whole row must match
        next
      }
      # row matched this rule so continue to add new columns
      namedc <- bind_cols(i_row, namedc) # add derived named captures to orig col values
      namedc <- as.list(namedc)
      for (col in addon_columns) {
        if (mm_row[col] == "") {
          # there's no default value, don't fill it in
          next
        }
        # substitute and evalute R expression contained in cell
        result_row[col] <- str_glue_data(namedc,toString(mm_row[col]))
      }
    }
    result_rows <- bind_rows(result_rows,result_row)
  }
  output_rows <- bind_cols(input_tbl,result_rows)
  return(output_rows)
}
