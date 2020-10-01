#'
#' List the contents of a path on DBFS
#'
#' List the contents of a directory, or the details of a file.  If the directory
#' does not exist a RESOURCE_DOES_NOT_EXIST exception will be thrown.
#'
#' When calling list on a large directory, the list operation will time out after
#'  approximately 60s. We strongly recommend using list only on directories containing
#'  less than 10K files and discourage using the DBFS REST API for operations that
#'  list more than 10k files. Instead, we recommend that you perform such operations
#'  in the context of a cluster, using File system utilities, which provides the same
#'  functionality without timing out.
#'
#' The API endpoint for listing a path on DBFS is '2.0/dbfs/list'.
#'   For all details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param path A string representing the path to list in DBFS
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
#' path <- "/rk/data"
#'
#' dbfs_ls(path = path, workspace = workspace)
#' @export
dbfs_ls <- function(path,
                 workspace,
                 token = NULL,
                 verbose = T,
                 ...) {

  # Make request, using netrc by default
  if (is.null(token)) {

    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::GET(url = paste0(workspace, "/api/2.0/dbfs/list?path=", path))
      })
  }

  else {

    # Authenticate with token
    headers <- c(
      Authorization = paste("Bearer", token)
    )

    # Using token for authentication instead of netrc
    res <- httr::GET(url = paste0(workspace, "/api/2.0/dbfs/list?path=", path),
                      httr::add_headers(.headers = headers)
                     )
    }

  # Handling successful API request
  if (res$status_code[1] == 200) {

    if (verbose == T) {

      message(paste0(
        "Status: ", res$status_code[1],
        "\nListing \"", path, "\":"
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
  reslist <- list(response = res,
                  data = as.data.frame(jsonlite::fromJSON(rawToChar(res$content))))

  reslist
}
