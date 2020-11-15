#'
#' Install Packages (libraries) on a Databricks Cluster
#'
#' Packages installed this way will be added to the cluster configuration
#' going forward.  When you restart the cluster they will show up in the UI.
#' Cluster cannot be in a terminated state to use this function.  You can
#' locate the cluster ID in the URL of the cluster configuration page.
#' For example:
#'
#' https://mycompany.cloud.databricks.com/#/setting/clusters/xxxx-xxxxx-xxxxxx/
#'
#' Where xxxx-xxxxx-xxxxxx is the cluster ID.
#'
#' The API endpoint for installing libraries is
#' '2.0/libraries/install'.
#'   For all details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param cluster_id A string containing the unique id for an online
#' Databricks cluster.
#' @param package A string with the name of the package to install.
#' @param repo A string representing the repo hosting the package.
#' Defaults to RStudio Public Package Manager
#' (https://packagemanager.rstudio.com/all/__linux__/xenial/latest) for R
#' packages.
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
#' # Install package
#' libraries_install(package = "broom", cluster_id, workspace)
#'
#' # Check installation status
#' get_library_statuses(cluster_id, workspace)
#' @export
libraries_install <- function(cluster_id,
                              package,
                              repo = "https://packagemanager.rstudio.com/all/__linux__/xenial/latest",
                              workspace,
                              token = NULL,
                              verbose = T,
                              ...) {

    payload <- paste0(
      '{"cluster_id": "', cluster_id, '",
         "libraries": [{
                        "cran": {
                                  "package": "', package,'",
                                  "repo": "', repo, '"
                                }
                      }]
       }'
      )

    # Make request, using netrc by default
    if (is.null(token)) {

      use_netrc <- httr::config(netrc = 1)
      res <- httr::with_config(use_netrc, {
        httr::POST(url = paste0(workspace, "/api/2.0/libraries/install"),
                   httr::content_type_json(),
                   body = payload)})
    }

    else {

      # Authenticate with token
      headers <- c(
        Authorization = paste("Bearer", token)
      )

      # Using token for authentication instead of netrc
      res <- httr::POST(url = paste0(workspace, "/api/2.0/libraries/install"),
                        httr::add_headers(.headers = headers),
                        httr::content_type_json(),
                        body = payload)
    }


  # Handling successful API request
  if (res$status_code[1] == 200) {

    if (verbose == T) {

      message(paste0(
        "Status: ", res$status_code[1],
        "\nLibraries installing.  Use get_library_statuses() to check status."
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
