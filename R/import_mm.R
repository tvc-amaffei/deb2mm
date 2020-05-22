#' import DEB2 match mutate model from a google spreadsheet
#' @description match mutate model
#' @param gsheet name of sheet using spreadsheet/worksheet syntax
#' in the users googledrive
#' @return a pov object of class \code{de_pov}.
#' @examples
#' \dontrun{
#' # We create an
#' # empty pov object (and add in nodes and edges later,
#' # with other functions)
#' # TODO: Change this to deb2_mm_example later
#' mmm <- import_mm(gsheet = "amaffei_mm_whoifsr_03/whoifsr_itdcsv_2019")
#' mmm
#' }
#' @importFrom googledrive drive_find
#' @importFrom googlesheets4 read_sheet as_sheets_id
#' @importFrom readr cols
#' @importFrom stringr str_split
#' @importFrom dplyr select
#' @importFrom tidyr replace_na
#' @export
import_mm <- function(gsheet = NULL) {

  #Grab doc name and sheet name
  names <- str_split(gsheet,"/")
  spreadsheet <- names[[1]][1]
  worksheet <- names[[1]][2]

  # Grab the doc
  pov_dribble <- drive_find(pattern = "amaffei_mm_whoifsr_03",
                            type = drive_mime_type("spreadsheet"))
  gs_id <- as_sheets_id(pov_dribble)

  #gap <- gs_title(spreadsheet)

  # All columns are considered character at start

  colspec = cols(.default = col_character())

  # Grab the worksheet
  mmm <-
    mmm <- read_sheet(gs_id,sheet=worksheet, col_types = "c" ) %>%
    select(-c(mm_ord,mm_wt))  %>%
    replace(is.na(.),"")
}
