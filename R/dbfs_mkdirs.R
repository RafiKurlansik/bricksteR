#'
#' Create a path on DBFS
#'
#' Create the given directory and necessary parent directories if they do not exist.
#' If there exists a file (not a directory) at any prefix of the input path, this
#' call throws an exception with RESOURCE_ALREADY_EXISTS. If this operation fails
#' it may have succeeded in creating some of the necessary parent directories.
#'
#' The API endpoint for creating a path on DBFS is '2.0/dbfs/mkdirs'.
#'   For all details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param path A string representing the path to create in DBFS
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
#' path <- "/rk/data/new_dir"
#'
#' dbfs_mkdirs(path = path, workspace = workspace)
#' @export
dbfs_mkdirs <- function(path,
                    workspace,
                    token = NULL,
                    verbose = T,
                    ...) {

  payload <- paste0('{"path": "', path, '"}')

  # Make request, using netrc by default
  if (is.null(token)) {

    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::POST(url = paste0(workspace, "/api/2.0/dbfs/mkdirs"),
                 httr::content_type_json(),
                 body = payload)})
  }

  else {

    # Authenticate with token
    headers <- c(
      Authorization = paste("Bearer", token)
    )

    # Using token for authentication instead of netrc
    res <- httr::POST(url = paste0(workspace, "/api/2.0/dbfs/mkdirs"),
                     httr::add_headers(.headers = headers),
                     httr::content_type_json(),
                     body = payload)

  }

  # Handling successful API request
  if (res$status_code[1] == 200) {

    if (verbose == T) {

      message(paste0(
        "Status: ", res$status_code[1],
        "\nCreated \"", path, "\":"
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
