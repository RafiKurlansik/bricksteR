#'
#' Upload a file to DBFS
#'
#' Upload a file through the use of multipart form post. It is mainly
#' used for streaming uploads, but can also be used as a convenient
#' single call for data upload.
#'
#' The API endpoint for putting files on DBFS is '2.0/dbfs/put'.
#'   For all details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param local_file Path to the file for upload
#' @param destination_path A string representing the destination path in DBFS
#' @param overwrite Boolean ('true', 'false') that specifies whether to overwrite existing files.
#' Defaults to 'false'
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
#' file <- "my_model.rds"
#' destination_path <- "/models/my_model.rds"
#'
#' dbfs_put(file = file, destination_path = destination_path, workspace = workspace)
#' @export
dbfs_put <- function(local_file,
                    destination_path,
                    overwrite = 'false',
                    workspace,
                    token = NULL,
                    verbose = T,
                    ...) {

  params <- list(
    path = destination_path,
    contents = httr::upload_file(local_file),
    overwrite = overwrite
  )

  # Make request, using netrc by default
  if (is.null(token)) {

    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::POST(url = paste0(workspace, "/api/2.0/dbfs/put"),
                 httr::add_headers(`Content-Type`="multipart/form-data"),
                 body = params)
      })
  }

  else {

    # Authenticate with token
    headers <- c(
      Authorization = paste("Bearer", token),
      `Content-Type`="multipart/form-data"
    )

    # Using token for authentication instead of netrc
    res <- httr::POST(url = paste0(workspace, "/api/2.0/dbfs/put"),
                      httr::add_headers(.headers = headers),
                      body = params)

  }

  # Handling successful API request
  if (res$status_code[1] == 200) {

    if (verbose == T) {

      message(paste0(
        "Status: ", res$status_code[1],
        "\nFile \"", local_file, "\" uploaded to \"", destination_path, "\" successfully."
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
