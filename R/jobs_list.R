#'
#' List all jobs in a Databricks workspace.
#'
#' Fetches all of the details for jobs in the authenticated workspace.
#' See also \code{runs_list} to learn more about a particular job run.
#'
#' The API endpoint for running a job is '2.0/jobs/list'.  For all
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
#' @return A list with four elements:
#' \itemize{
#'     \item \emph{response} - The full API response, including status code.
#'     \item \emph{response_list} - The JSON result transformed into a list.
#'     \item \emph{response_df} - The JSON result transformed into a dataframe.
#'     \item \emph{response_tidy} - A simple dataframe with the job ID, name,
#'     time of creation, and who created it.
#' }
#' If the details of jobs are not needed, use the \emph{response_tidy}
#' dataframe.
#' @examples
#' jobs <- jobs_list(workspace = "https://eastus2.azuredatabricks.net",
#'                   token = "dapiafoi32984320890d9asaf1")
#'
#' ## Get data in different formats
#' jobs_json <- jobs$response_json
#' jobs_list <- jobs$response_list
#' jobs_df <- jobs$response_df
#'

jobs_list <- function(workspace, token = NULL, verbose = T) {

  ## Make request, using netrc by default
  if (is.null(token)) {
    
    use_netrc <- httr::config(netrc = 1)
    jobs <- httr::with_config(use_netrc, {
      httr::GET(url = paste0(workspace, "/api/2.0/jobs/list/"))
      })
  }
  
  else {
    
    ## Bearer Authentication
    headers <- c(
      Authorization = paste("Bearer", token)
    )
    
    jobs <- httr::GET(url = paste0(workspace, "/api/2.0/jobs/list/"),
                      httr::add_headers(.headers = headers))
  }

  ## Objects to return, including tidy jobs list
  jobs_list <- jsonlite::fromJSON(rawToChar(jobs$content))
  jobs_df <- jsonlite::fromJSON(rawToChar(jobs$content), flatten = T)$jobs
  jobs_tidy <- jobs_df[, c("job_id", "settings.name",
                           "created_time", "creator_user_name")]
  jobs_tidy$created_time <- as.POSIXlt(jobs_tidy$created_time/1000,
                                       origin = '1970-01-01')

  ## Handling successful API response
  if (jobs$status_code[1] == 200) {

    if (verbose == T) {
      message(paste0(
        "Status: ", jobs$status_code[1],
        "\nNumber of jobs: ", length(jsonlite::fromJSON(
          rawToChar(jobs$content))$jobs$job_id)

      ))
    }
  }

  ## Else status not 200
  else {

    if (verbose == T) {
      message(paste0(
        "Status: ", jobs$status_code[1],
        "\nThe request was not successful:\n\n", jsonlite::prettify(jobs)
      ))
    }

    jobs_list <- NA
    jobs_df <- NA
    jobs_tidy <- NA
  }

  ## Return complete response, list, and dataframe of results
  reslist <- list(response = jobs,
                  response_list = jobs_list,
                  response_df = jobs_df,
                  response_tidy = jobs_tidy)

  reslist
}
