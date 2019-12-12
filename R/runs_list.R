#'
#' List runs for a job from most recently to least.
#'
#' Fetches all of the completed and active runs for a given job in the last 60 days.
#' Runs expire after 60 days, so see \code{runs_export} to save run results before expiry.
#'
#' The API endpoint for running a job is '2.0/jobs/runs/list'.  For all
#' details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param job_id A number representing a valid run id generating by launching
#'  a job on Databricks.
#' @param name Optional.  A string representing the name of the job.  If multiple
#' jobs share the same name, you'll need to rename the jobs or provide
#' the unique job ID.
#' @param workspace A string representing the web workspace of your Databricks
#' instance. E.g., "https://eastus2.azuredatabricks.net" or
#' "https://demo.cloud.databricks.com".
#' @param token A valid authentication token generated via User Settings in
#' Databricks or via the Databricks REST API 2.0.  If none is provided,
#' netrc will be used.
#' @param offset The number of the first run to return, relative to the most
#' recent run.  Defaults to 0 (the most recent run).
#' @param limit The number of runs to return.  Should be greater than 0 and
#' less than 150.  If 0 is used, the service will use the maximum limit.
#' @param active_only A string.  If 'true', only active runs will be
#' included in the results.  Cannot be true while completed_only is true.
#' @param completed_only A string.  If 'true', only completed runs will be
#' included in the results.  Cannot be true while active_only is true.
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
#' ## Get runs by Job ID
#' res <- runs_list(job_id = 206,
#'                       workspace = "https://eastus2.azuredatabricks.net",
#'                       token = "dapiafoi32984320890d9asaf1")
#'
#' ## Get runs by Job name
#' res <- runs_list(name = "My Other Unique Job Name", workspace = workspace, token = token)
#'
#' ## Get data in different formats
#' resjson <- res$response_json
#' reslist <- res$response_list
#' res_df <- res$response_df
#'
runs_list <- function(job_id = NULL, name = NULL, workspace, token = NULL,
                      offset = 0, limit = 20, active_only = "false",
                      completed_only = "false", verbose = TRUE) {

  ## If name provided, call jobs_list to find the job ID
  if (!is.null(name)) {

    jobs_tidy <- jobs_list(workspace = workspace, token = token, verbose = F)$response_tidy
    matches <- jobs_tidy[grepl(pattern = paste0("^", name,"$"), jobs_tidy$settings.name), ]

    ## If there is more than one job with the same name
    if (length(matches$settings.name) > 1){
      message(paste0("Found multiple jobs with name \"", name, "\":\n"))
      message(paste0(
        capture.output(
          jobs_tidy[grepl(pattern = paste0("^", name,"$"), jobs_tidy$settings.name), ]),
        collapse = "\n"))
      message(paste0("\n\nPlease use a job ID or give the job a unique name.\n"))
      stop("Too many jobs with that name.")
    }

    ## If no matches found
    else if (length(matches$settings.name) < 1) {
      message(paste0("No job with name \"", name, "\" found.\n Please try a different name."))
      stop("Couldn't find a job with that name.")
    }

    ## If exact match fetch the job id for the run config
    job_id <- matches$job_id

    message(paste0("Job \"", name, "\" found with ", matches$job_id, "."))
  }

  ## Make request with netrc by default
  if (is.null(token)) {

    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::GET(url = paste0(workspace,
                             "/api/2.0/jobs/runs/list/?job_id=", job_id,
                             "&active_only=", active_only,
                             "&completed_only=", completed_only,
                             "&offset=", offset,
                             "&limit=", limit))})
  }

  else {

    ## Authenticate with token
    headers <- c(
      Authorization = paste("Bearer", token)
    )

    ## Make request
    res <- httr::GET(url = paste0(workspace,
                                  "/api/2.0/jobs/runs/list/?job_id=", job_id,
                                  "&active_only=", active_only,
                                  "&completed_only=", completed_only,
                                  "&offset=", offset,
                                  "&limit=", limit),
                     httr::add_headers(.headers = headers))
  }

  if (verbose == T) {
    ## Successful request message
    if (res$status_code[1] == 200) {

      message(paste0(
        "Status: ",
        res$status_code[1],
        "\nJob ID: ",
        unique(jsonlite::fromJSON(rawToChar(res$content))$runs$job_id),
        "\nNumber of runs: ",
        length(jsonlite::fromJSON(rawToChar(res$content))$runs$run_id),
        "\n\nAre there more runs to list? \n ",
        jsonlite::fromJSON(rawToChar(res$content))$has_more
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
