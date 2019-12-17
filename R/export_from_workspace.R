#' Export a Notebook or Directory from a Databricks Workspace
#'
#' Export a notebook or contents of an entire directory. If path does not
#' exist, this call returns an error RESOURCE_DOES_NOT_EXIST. You can export
#'  a directory only in DBC format. If the exported data exceeds the size
#'  limit, this call returns an error MAX_NOTEBOOK_SIZE_EXCEEDED. This API
#'  does not support exporting a library.
#' The API endpoint for importing files to the workspace is
#' '2.0/workspace/import'.  For all details on API calls please see the
#' official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param workspace_path A string representing the path to notebook or folder
#' in the Databricks workspace.
#' @param format A string.  This specifies the format of the exported file.
#' By default, this is SOURCE. However it may be one of: SOURCE, HTML,
#' JUPYTER, DBC. The value is case sensitive.
#' @param direct_download A string.  Flag to enable direct download. If it is
#' true, the response will be the exported file itself. Otherwise, the
#' response contains content as base64 encoded string. Defaults to 'false'.
#' @param filename Optional string representing the path to save the file locally.
#' @param workspace A string representing the web workspace of your Databricks
#' instance. E.g., "https://eastus2.azuredatabricks.net" or
#' "https://company.cloud.databricks.com".
#' @param token A valid authentication token generated via User Settings in
#' Databricks or via the Databricks REST API 2.0.  If none is provided,
#' netrc will be used.
#' @param verbose If true, will pretty print the success or failure of the request.  Defaults to TRUE.
#' @return The API response.  If \emph{filename} is provided, will write the
#' file to disk.
#'
#' @examples
#' # Export a notebook as HTML
#' export_from_workspace(workspace_path = "/Shared/R_Notebook",
#'                       format = "HTML",
#'                       filename = "/Desktop/r_notebook.html",
#'                       direct_download = 'true',
#'                       workspace = workspace,
#'                       token = token)
#'
#' # Export a directory as DBC
#' export_from_workspace(workspace_path = "/Shared/",
#'                       format = "DBC",
#'                       filename = "/Desktop/shared.dbc",
#'                       direct_download = 'true',
#'                       workspace = workspace,
#'                       token = token)
export_from_workspace <- function(workspace_path, format,
                                  direct_download = 'false', filename = NULL,
                                  workspace, token = NULL, verbose = T) {

  if (is.null(token)) {

    ## Using netrc by default
    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::GET(url = paste0(workspace,
                              "/api/2.0/workspace/export?format=",
                              format,
                              "&path=",
                              utils::URLencode(workspace_path),
                              "&direct_download=",
                              direct_download),
                 httr::content_type_json())
      })
  }

  else {

    ## Authenticate with token
    headers <- c(
      Authorization = paste("Bearer", token)
    )

    ## Make request
    res <- httr::GET(url = paste0(workspace,
                                  "/api/2.0/workspace/export?format=",
                                  format,
                                  "&path=",
                                  utils::URLencode(path),
                                  "&direct_download=",
                                  direct_download),
                     httr::content_type_json())
  }


  if (verbose == T) {
    ## Successful request message
    if (res$status_code[1] == 200) {

      message(paste0(
        "Status: ", res$status_code[1],
        "\n\nWorkspace object: ", notebook_path,
        " was exported successfully. "
      ))

    }

    ## Unsuccessful request message
    else {
      return(message(paste0(
        "Status: ", res$status_code[1],
        "\nThe request was not successful:\n\n", jsonlite::prettify(res)
      )))
    }
  }

  if (!is.null(file)) {
    fileConn <- file(filename)
    writeLines(rawToChar(res$content), fileConn)
    close(fileConn)

    message(paste0("File downloaded here: ", filename))
  }

  ## Return API response
  res
}

