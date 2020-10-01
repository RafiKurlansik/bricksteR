#'
#' Delete a file or directory on DBFS
#'
#' Delete the file or directory (optionally recursively delete all files in
#' the directory). This call throws an exception with IO_ERROR if the path is
#' a non-empty directory and recursive is set to false or on other similar errors.
#' When you delete a large number of files, the delete operation is done in increments.
#' The call returns a response after approximately 45s with an error message
#' (503 Service Unavailable) asking you to re-invoke the delete operation until the
#' directory structure is fully deleted.
#'
#' The API endpoint for creating a path on DBFS is '2.0/dbfs/delete'.
#'   For all details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param path A string representing the path to delete in DBFS
#' @param recursive Should the deletes be recursive or not?  Defaults to 'false'
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
#' dbfs_rm(path = path, workspace = workspace)
#' @export
dbfs_rm <- function(path,
                    recursive  = 'false',
                    workspace,
                    token = NULL,
                    verbose = T,
                        ...) {

  payload <- paste0('{"path": "', path, '",
                    "recursive": "', recursive,'"}')

  # Make request, using netrc by default
  if (is.null(token)) {

    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::POST(url = paste0(workspace, "/api/2.0/dbfs/delete"),
                 httr::content_type_json(),
                 body = payload)})
  }

  else {

    # Authenticate with token
    headers <- c(
      Authorization = paste("Bearer", token)
    )

    # Using token for authentication instead of netrc
    res <- httr::POST(url = paste0(workspace, "/api/2.0/dbfs/delete"),
                      httr::add_headers(.headers = headers),
                      httr::content_type_json(),
                      body = payload)

  }

  # Handling successful API request
  if (res$status_code[1] == 200) {

    if (verbose == T) {

      message(paste0(
        "Status: ", res$status_code[1],
        "\nDeleted \"", path, "\" successfully."
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
