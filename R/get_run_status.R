#'
#' Get the status of a job run on Databricks
#'
#' Fetches the status of a job run on Databricks, whether it has completed or
#' not.  Includes details about start time, cluster spec, task, and more.
#'
#' The API endpoint for running a job is '2.0/jobs/runs/get'.  For all
#' details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param run_id A number representing a valid run id generating by launching
#'  a job on Databricks.
#' @param workspace A string representing the web workspace of your Databricks
#' instance. E.g., "https://eastus2.azuredatabricks.net" or
#' "https://demo.cloud.databricks.com".
#' @param token A valid authentication token generated via User Settings in
#' Databricks or via the Databricks REST API 2.0.  If none is provided,
#' netrc will be used.
#' @param verbose If true, will pretty print the success or failure of the
#' request and add a run_id variable to the R environment.  Defaults to TRUE.
#' @return A list with four elements:
#' \itemize{
#'     \item \emph{response} - The full API response, including status code.
#'     \item \emph{response_json} - The JSON result.
#'     \item \emph{response_list} - The JSON result transformed into a list.
#'     \item \emph{response_df} - The JSON result transformed into a dataframe.
#' }
#' @examples
#' res <- get_run_status(run_id = 2932371,
#'                       workspace = "https://eastus2.azuredatabricks.net",
#'                       token = "dapiafoi32984320890d9asaf1")
#'
#' ## Get data in different formats
#' resjson <- res$response_json
#' reslist <- res$response_list
#' res_df <- res$response_df
#'
get_run_status <- function(run_id, workspace, token = NULL, verbose = TRUE) {

  ## Make request with netrc by default
  if (is.null(token)) {

    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::GET(url = paste0(workspace, "/api/2.0/jobs/runs/get?run_id=", run_id))})
  }

  else {

    ## Use Bearer Authentication
    headers <- c(
      Authorization = paste("Bearer", token)
    )

    ## Make request
    res <- httr::GET(url = paste0(workspace, "/api/2.0/jobs/runs/get?run_id=", run_id),
                     httr::add_headers(.headers = headers))
  }

  if (verbose == T) {
    ## Successful request message
    if (res$status_code[1] == 200) {

      message(paste0(
        "Status: ",
        res$status_code[1],
        "\nRun ID: ",
        jsonlite::fromJSON(rawToChar(res$content))$run_id,
        "\nNumber in Job: ",
        jsonlite::fromJSON(rawToChar(res$content))$number_in_job,
        "\n\nYou can check the status of this run at: \n ",
        jsonlite::fromJSON(rawToChar(res$content))$run_page_url
      ))

    }

    ## Unsuccessful request message
    else {
      message(paste0(
        "Status: ",
        res$status_code[1],
        "\nThe request was not successful:\n\n",
        jsonlite::prettify(res)
      ))
    }
  }


  ## Return list of values
  reslist <- list(response = res,
                  response_json = jsonlite::prettify(res),
                  response_list = jsonlite::fromJSON(rawToChar(res$content)),
                  response_df = as.data.frame(jsonlite::fromJSON(rawToChar(res$content))))

  reslist
}
