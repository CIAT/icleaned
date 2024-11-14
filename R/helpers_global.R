#' Download and read Excel file from a given share point URL.
#'
#' @param url A string. The URL of the Excel file to download.
#' @param sheet A string or number. The name or index of the sheet to read.
#' @return A data frame containing the data from the specified sheet. Default
#' to 1.
#'
#' This function downloads an Excel file from the provided URL, reads the 
#' specified sheet, and returns the data as a data frame.
#' The temporary file created for downloading will be deleted automatically 
#' after use.
read_sharepoint <- function(url, sheet = 1) {
  # Create a temporary file
  temp_file <- tempfile(fileext = ".xlsx")
  # Ensure the temp file is deleted after use, even if an error occurs
  on.exit(unlink(temp_file))
  # Download the Excel file
  GET(url, write_disk(temp_file, overwrite = TRUE))
  # Read the specified sheet from the Excel file
  dt_sharepoint <- read_excel(temp_file, sheet = sheet)
  # Return the data
  return(dt_sharepoint)
}