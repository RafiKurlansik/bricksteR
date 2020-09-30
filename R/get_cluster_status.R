#'
#' Retrieve the information for a cluster.
#'
#' Will retrieve detailed information on a Databricks cluster.  You can locate the cluster ID in
#'  the URL of the cluster configuration page.  For example:
#' 
#' https://mycompany.cloud.databricks.com/#/setting/clusters/xxxx-xxxxx-xxxxxx/
#' 
#' Where xxxx-xxxxx-xxxxxx is the cluster ID.
#'
#' The API endpoint for terminating a cluster is '2.0/clusters/get'.
#'   For all details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param workspace A string representing the web workspace of your Databricks
#' instance. E.g., "https://eastus2.azuredatabricks.net" or
#' "https://demo.cloud.databricks.com".
#' @param cluster_id A string containing the unique id for an online Databricks cluster
#' @param verbose If TRUE, will print the API response to the console.  Defaults to
#' FALSE.
#' @param ... Additional options to be passed to \code{data.table::fread} which is used to
#' parse the API response.
##' @return A list with four elements:
#' \itemize{
#'     \item \emph{response} - The full API response, including status code.
#'     \item \emph{response_json} - The JSON result.
#'     \item \emph{response_list} - The JSON result transformed into a list.
#'     \item \emph{response_df} - The JSON result transformed into a dataframe.
#' }
#' @examples
#' workspace <- "https://myworkspace.cloud.databricks.com"
#' cluster_id <- "0818-155203-cheese22"
#' 
#' get_cluster_status(workspace = workspace, cluster_id = cluster_id)
#' @export
get_cluster_status <- function(cluster_id,
                          workspace,
                          token = NULL,
                          verbose = T,
                          ...) {

  # Make request, using netrc by default
  if (is.null(token)) {
    
    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::GET(url = paste0(workspace,
                             "/api/2.0/clusters/get/?cluster_id=", cluster_id),
                encoding = "UTF-8")})
  }
  
  else {
    
    # Authenticate with token
    headers <- c(
      Authorization = paste("Bearer", token)
    )
    
    # Using token for authentication instead of netrc
    res <- httr::GET(url = paste0(workspace,
                                  "/api/2.0/clusters/get/?cluster_id=", cluster_id),
                     httr::add_headers(.headers = headers), 
                     encoding = "UTF-8")
  }
  
  # Handling successful API request
  if (res$status_code[1] == 200) {
    
    if (verbose == T) {
      
      message(paste0(
        "Status: ", res$status_code[1],
        "\nCluster \"", cluster_id, "\" status retrieved."
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
  
  # Clean up datetimes in API response
  response_df = as.data.frame(jsonlite::fromJSON(rawToChar(res$content)))
  response_df$driver.start_timestamp <- as.POSIXct(response_df$driver.start_timestamp/1000,
                                                   origin = "1970-01-01")
  response_df$executors.start_timestamp <- as.POSIXct(response_df$executors.start_timestamp/1000,
                                                   origin = "1970-01-01")
  response_df$start_time <- as.POSIXct(response_df$start_time/1000,
                                       origin = "1970-01-01")
  response_df$last_state_loss_time <- as.POSIXct(response_df$last_state_loss_time/1000,
                                                 origin = "1970-01-01")
  response_df$last_activity_time <- as.POSIXct(response_df$last_activity_time/1000,
                                               origin = "1970-01-01")
  reslist <-   ## Return list of values
    reslist <- list(response = res,
                    response_json = suppressMessages(jsonlite::prettify(res)),
                    response_list = jsonlite::fromJSON(rawToChar(res$content)),
                    response_df = response_df
    )
}
