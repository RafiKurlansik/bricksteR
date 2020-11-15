#'
#' Get the status of libraries on Databricks clusters
#'
#' Get a list of libraries and their statuses for Databricks clusters.
#'
#' By default will return library statuses for *all* clusters.  If a `cluster_id`
#' is supplied, will return the status for that cluster alone. You can
#' locate the cluster ID in the URL of the cluster configuration page.
#' For example:
#'
#' https://mycompany.cloud.databricks.com/#/setting/clusters/xxxx-xxxxx-xxxxxx/
#'
#' Where xxxx-xxxxx-xxxxxx is the cluster ID.
#'
#' The API endpoints for getting library statuses are
#' '2.0/libraries/all-cluster-statuses' and '2.0/libraries/cluster-status'.
#'   For all details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param cluster_id A string containing the unique id for an online
#' Databricks cluster
#' @param workspace A string representing the web workspace of your Databricks
#' instance. E.g., "https://eastus2.azuredatabricks.net" or
#' "https://demo.cloud.databricks.com".
#' @param token A valid authentication token generated via User Settings in
#' Databricks or via the Databricks REST API 2.0.  If none is provided,
#' netrc will be used
#' @param verbose If TRUE, will print the API response to the console.  Defaults to
#' FALSE.
#' @param ... Additional options to be passed
#' @return A list with three elements:
#' \itemize{
#'     \item \emph{response} - The full API response, including status code.
#'     \item \emph{response_list} - The JSON result transformed into a list.
#'     \item \emph{response_df} - The JSON result transformed into a dataframe.
#' }
#' If the details of jobs are not needed, use the \emph{response_tidy}
#' dataframe.
#' @examples
#' # For all clusters
#' library_statuses <- get_library_statuses(workspace = workspace)
#'
#' # For a single cluster
#' cluster_id <- "0818-155203-cheese22"
#' library_statuses <- get_library_statuses(cluster_id = cluster_id, workspace = workspace)
#' @export
get_library_statuses <- function(cluster_id = NULL,
                                 workspace,
                                 token = NULL,
                                 verbose = T,
                                 ...) {

    if(is.null(cluster_id)) {

    # Make request for all statuses, using netrc by default
    if (is.null(token)) {

      use_netrc <- httr::config(netrc = 1)
      res <- httr::with_config(use_netrc, {
        httr::GET(url = paste0(workspace, "/api/2.0/libraries/all-cluster-statuses"))
      })
    }

    else {

      # Authenticate with token
      headers <- c(
        Authorization = paste("Bearer", token)
      )

      # Using token for authentication instead of netrc
      res <- httr::GET(url = paste0(workspace, "/api/2.0/libraries/all-cluster-statuses"),
                       httr::add_headers(.headers = headers)
      )
    }
  }

  else {

    # Make request for all statuses, using netrc by default
    if (is.null(token)) {

      use_netrc <- httr::config(netrc = 1)
      res <- httr::with_config(use_netrc, {
        httr::GET(url = paste0(workspace,
                               "/api/2.0/libraries/cluster-status?cluster_id=",
                               cluster_id))
      })
    }

    else {

      # Authenticate with token
      headers <- c(
        Authorization = paste("Bearer", token)
      )

      # Using token for authentication instead of netrc
      res <- httr::GET(url = paste0(workspace,
                                    "/api/2.0/libraries/cluster-status?cluster_id=",
                                    cluster_id),
                       httr::add_headers(.headers = headers)
      )
    }
  }



  # Handling successful API request
  if (res$status_code[1] == 200) {

    if (verbose == T) {

      message(paste0(
        "Status: ", res$status_code[1],
        "\nLibrary statuses retrieved."
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
                    response_json = suppressMessages(jsonlite::prettify(res)),
                    response_list = jsonlite::fromJSON(rawToChar(res$content)),
                    response_df = as.data.frame(jsonlite::fromJSON(rawToChar(res$content)))

    )

    reslist
}
