#'
#' Start a Databricks cluster.
#'
#' Will start an interactive Databricks cluster.  You can locate the cluster ID in the URL of the cluster configuration
#' page.  For example:
#' 
#' https://mycompany.cloud.databricks.com/#/setting/clusters/xxxx-xxxxx-xxxxxx/
#' 
#' Where xxxx-xxxxx-xxxxxx is the cluster ID.
#'
#' The API endpoint for terminating a cluster is '2.0/clusters/start'.
#'   For all details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#'#' @param workspace A string representing the web workspace of your Databricks
#' instance. E.g., "https://eastus2.azuredatabricks.net" or
#' "https://demo.cloud.databricks.com".
#' @param cluster_id A string containing the unique id for an online Databricks cluster
#' @param verbose If TRUE, will print the API response to the console.  Defaults to
#' FALSE.
#' @param ... Additional options to be passed to \code{data.table::fread} which is used to
#' parse the API response.
#' @return The API response
#' @examples
#' cluster_id <- "0818-155203-cheese22"
#' 
#' start_cluster(workspace = workspace, cluster_id = cluster_id)
#' @export
start_cluster <- function(cluster_id,
                              workspace,
                              token = NULL,
                              verbose = T,
                              ...) {
  
  payload <- paste0('{"cluster_id": "', cluster_id, '"}')
  
  # Make request, using netrc by default
  if (is.null(token)) {
    
    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::POST(url = paste0(workspace, "/api/2.0/clusters/start"),
                 httr::content_type_json(),
                 body = payload)})
  }
  
  else {
    
    # Authenticate with token
    headers <- c(
      Authorization = paste("Bearer", token)
    )
    
    # Using token for authentication instead of netrc
    res <- httr::POST(url = paste0(workspace, "/api/2.0/clusters/start"),
                      httr::add_headers(.headers = headers),
                      httr::content_type_json(),
                      body = payload)
  }
  
  # Handling successful API request
  if (res$status_code[1] == 200) {
    
    if (verbose == T) {
      
      message(paste0(
        "Status: ", res$status_code[1],
        "\nCluster \"", cluster_id, "\" started."
      ))
    }
  }
  
  # Handling unsuccessful request
  else {
    
    if (verbose == T) {
      
      message(paste0(
        "Status: ", res$status_code[1],
        "\nThe request was not successful:\n\n", jsonlite::prettify(res)
      ))
    }
  }
  
  # Return response
  res
}
