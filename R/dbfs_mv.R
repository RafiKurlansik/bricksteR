#'
#' Move a file or directory on DBFS
#'
#' Move a file from one location to another location *within* DBFS. This will
#' not move files from your local system to DBFS.  See `dbfs_put()` and
#' `dbfs_read()` to work with your local filesystem.
#'
#' If the source file does not exist, this call throws an exception with
#' RESOURCE_DOES_NOT_EXIST. If there already exists a file in the
#' destination path, this call throws an exception with
#' RESOURCE_ALREADY_EXISTS. If the given source path is a directory,
#' this call always recursively moves all files.
#'
#' The API endpoint for moving files or directories on DBFS is '2.0/dbfs/move'.
#'   For all details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param from A string representing the source path in DBFS
#' @param to A string representing the destination path in DBFS
#' @param workspace A string representing the web workspace of your Databricks
#' instance. E.g., "https://eastus2.azuredatabricks.net" or
#' "https://demo.cloud.databricks.com".
#' @param token A valid authentication token generated via User Settings in
#' Databricks or via the Databricks REST API 2.0.  If none is provided,
#' netrc will be used
#' @param verbose If TRUE, will print the API response to the console.  Defaults to
#' FALSE.
#' @param ... Additional options to be passed
#' @return The API response
#' @examples
#' # No need to include /dbfs/
#' source_path <- "/data/my_model.rds"
#' destination_path <- "/models/my_model.rds"
#'
#' dbfs_mv(from = source_path, to = destination_path, workspace = workspace)
#' @export
dbfs_mv <- function(from,
                    to,
                    workspace,
                    token = NULL,
                    verbose = T,
                    ...) {

  payload <- paste0('{"source_path": "', from, '",
                    "destination_path": "', to,'"}')

  # Make request, using netrc by default
  if (is.null(token)) {

    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::POST(url = paste0(workspace, "/api/2.0/dbfs/move"),
                 httr::content_type_json(),
                 body = payload)})
  }

  else {

    # Authenticate with token
    headers <- c(
      Authorization = paste("Bearer", token)
    )

    # Using token for authentication instead of netrc
    res <- httr::POST(url = paste0(workspace, "/api/2.0/dbfs/move"),
                      httr::add_headers(.headers = headers),
                      httr::content_type_json(),
                      body = payload)

  }

  # Handling successful API request
  if (res$status_code[1] == 200) {

    if (verbose == T) {

      message(paste0(
        "Status: ", res$status_code[1],
        "\nMoved \"", from, "\" to \"", to, "\" successfully."
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
  res
}
