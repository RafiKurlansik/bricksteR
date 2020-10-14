#'
#' List all clusters in a Databricks workspace.
#'
#' Fetches all of the details for clusters in the authenticated workspace.
#'
#' The API endpoint for running a job is '2.0/clusters/list'.  For all
#' details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param workspace A string representing the web workspace of your Databricks
#' instance. E.g., "https://eastus2.azuredatabricks.net" or
#' "https://demo.cloud.databricks.com".
#' @param token A valid authentication token generated via User Settings in
#' Databricks or via the Databricks REST API 2.0.  If none is provided,
#' netrc will be used.
#' @param verbose If true, will pretty print the success or failure of the
#' request.  Defaults to TRUE.
#' @return A list with two elements:
#' \itemize{
#'     \item \emph{response} - The full API response, including status code.
#'     \item \emph{response_df} - The JSON result transformed into a dataframe.
#' }
#' @examples
#' clusters <- clusters_list(workspace = "https://eastus2.azuredatabricks.net",
#'                   token = "dapiafoi32984320890d9asaf1")
#'
#' ## Get active clusters
#' df <- clusters$df
#' active <- df[df$state == "RUNNING", c("cluster_id", "state")]

clusters_list <- function(workspace, token = NULL, verbose = T) {

  ## Make request, using netrc by default
  if (is.null(token)) {

    use_netrc <- httr::config(netrc = 1)
    clusters <- httr::with_config(use_netrc, {
      httr::GET(url = paste0(workspace, "/api/2.0/clusters/list/"))
    })
  }

  else {

    ## Bearer Authentication
    headers <- c(
      Authorization = paste("Bearer", token)
    )

    clusters <- httr::GET(url = paste0(workspace, "/api/2.0/clusters/list/"),
                      httr::add_headers(.headers = headers))
  }

  ## Objects to return
  clusters_df <- jsonlite::fromJSON(rawToChar(clusters$content), flatten = T)$clusters

  ## Handling successful API response
  if (clusters$status_code[1] == 200) {

    if (verbose == T) {
      message(paste0(
        "Status: ", clusters$status_code[1],
        "\nNumber of clusters: ", nrow(jsonlite::fromJSON(rawToChar(clusters$content))$clusters)

      ))
    }
  }

  ## Else status not 200
  else {

    if (verbose == T) {
      message(paste0(
        "Status: ", clusters$status_code[1],
        "\nThe request was not successful:\n\n", jsonlite::prettify(clusters)
      ))
    }

  }

  ## Return complete response, list, and dataframe of results
  reslist <- list(response = clusters,
                  response_df = clusters_df)

  reslist
}
