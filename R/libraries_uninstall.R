#'
#' Uninstall Packages (libraries) on a Databricks Cluster
#'
#' Set libraries to be uninstalled on a cluster. The libraries aren’t
#' uninstalled until the cluster is restarted. Uninstalling libraries
#' that are not installed on the cluster has no impact but is not an error.
#' You can locate the cluster ID in the URL of the cluster configuration page.
#' For example:
#'
#' https://mycompany.cloud.databricks.com/#/setting/clusters/xxxx-xxxxx-xxxxxx/
#'
#' Where xxxx-xxxxx-xxxxxx is the cluster ID.
#'
#' The API endpoint for uninstalling libraries is
#' '2.0/libraries/uninstall'.
#'   For all details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param cluster_id A string containing the unique id for an online
#' Databricks cluster.
#' @param package A string with the name of the package to uninstall.
#' @param workspace A string representing the web workspace of your Databricks
#' instance. E.g., "https://eastus2.azuredatabricks.net" or
#' "https://demo.cloud.databricks.com".
#' @param token A valid authentication token generated via User Settings in
#' Databricks or via the Databricks REST API 2.0.  If none is provided,
#' netrc will be used.
#' @param verbose If TRUE, will print the API response to the console.  Defaults to
#' FALSE.
#' @return The API response.
#'
#' @examples
#' # Cluster to install on
#' workspace <- "https://mydb.cloud.databricks.com"
#' cluster_id <- "0818-155203-cheese22"
#'
#' # Uninstall package
#' libraries_uninstall(package = "broom", cluster_id, workspace)
#'
#' # Check installation status
#' get_library_statuses(cluster_id, workspace)
#' @export
libraries_uninstall <- function(cluster_id,
                              package,
                              workspace,
                              token = NULL,
                              verbose = T,
                              ...) {

  payload <- paste0(
    '{"cluster_id": "', cluster_id, '",
         "libraries": [{
                        "cran": {
                                  "package": "', package,'"
                                }
                      }]
       }'
  )

  # Make request, using netrc by default
  if (is.null(token)) {

    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::POST(url = paste0(workspace, "/api/2.0/libraries/uninstall"),
                 httr::content_type_json(),
                 body = payload)})
  }

  else {

    # Authenticate with token
    headers <- c(
      Authorization = paste("Bearer", token)
    )

    # Using token for authentication instead of netrc
    res <- httr::POST(url = paste0(workspace, "/api/2.0/libraries/uninstall"),
                      httr::add_headers(.headers = headers),
                      httr::content_type_json(),
                      body = payload)
  }


  # Handling successful API request
  if (res$status_code[1] == 200) {

    if (verbose == T) {

      message(paste0(
        "Status: ", res$status_code[1],
        "\nRequest successful.  Packages will be uninstalled upon cluster restart.  \n
        Use restart_cluster() to finish uninstalling."
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
