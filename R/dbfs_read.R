#'
#' Read data from DBFS
#'
#' Return the contents of a file. If the file does not exist, this call
#' throws an exception with RESOURCE_DOES_NOT_EXIST.
#'
#' The API endpoint for reading files from DBFS is '2.0/dbfs/read'.
#'   For all details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param path Absolute path to a file on DBFS
#' @param workspace A string representing the web workspace of your Databricks
#' instance. E.g., "https://eastus2.azuredatabricks.net" or
#' "https://demo.cloud.databricks.com".
#' @param token A valid authentication token generated via User Settings in
#' Databricks or via the Databricks REST API 2.0.  If none is provided,
#' netrc will be used
#' @param verbose If TRUE, will print the API response to the console.  Defaults to
#' FALSE.
#' @param ... Additional options to be passed
#' @return The API response and the base6-encoded contents of the file read.
#' @examples
#' # No need to include /dbfs/
#' file_path <- "/tmp/iris.json"
#'
#' res <- dbfs_read(path = file_path, workspace = workspace)
#'
#' data <- res$data
#'
#' # Decode from base64
#' tidy_data <- jsonlite::fromJSON(rawToChar(jsonlite::base64_dec(data)))
#' @export
dbfs_read <- function(path,
                     workspace,
                     token = NULL,
                     verbose = T,
                     ...) {

  # Make request, using netrc by default
  if (is.null(token)) {

    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::GET(url = paste0(workspace, "/api/2.0/dbfs/read?path=", path))
    })
  }

  else {

    # Authenticate with token
    headers <- c(
      Authorization = paste("Bearer", token)
    )

    # Using token for authentication instead of netrc
    res <- httr::GET(url = paste0(workspace, "/api/2.0/dbfs/read?path=", path),
                      httr::add_headers(.headers = headers))

  }

  # Handling successful API request
  if (res$status_code[1] == 200) {

    if (verbose == T) {

      message(paste0(
        "Status: ", res$status_code[1],
        "\nFile \"", path, "\" read from DBFS successfully."
      ))
    }
  }

  # Handling unsuccessful request
  else {

    if (verbose == T) {

      message(paste0(
        "Status: ", res$status_code[1],
        "\nThe request was not successful:\n\n", suppressMessages(jsonlite::prettify(res))
      ))
    }
  }

  # Return response
  reslist <- list(
    response = res,
    data = jsonlite::fromJSON(rawToChar(res$content))$data
  )
}
