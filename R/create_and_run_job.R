#' Create and run a job on Databricks
#'
#' Single function to both create a job and immediately run it.  Each call
#' will register a new job in the Databricks Jobs UI, so for subsequent runs
#' it is best to submit the job_id to \code{run_job}.
#'
#' Uses  '2.0/jobs/create', '2.0/jobs/run-now' and '2.0/runs/get' API endpoints. For all
#' details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param name A string representing the name of the job.  It is encouraged
#' to choose a unique name for each job.
#' @param notebook_path A string representing the path to a Databricks notebook in the
#' workspace.
#' @param file The path to a local .R or .Rmd file.  Will be imported to the
#' workspace at the \emph{notebook_path}.
#' @param job_config A JSON formatted string specifying the details of the job, i.e., the
#'  name, cluster spec, and so on.
#' @param workspace A string representing the web workspace of your Databricks
#' instance. E.g., "https://eastus2.azuredatabricks.net" or
#' "https://demo.cloud.databricks.com".
#' @param token A valid authentication token generated via User Settings in
#' Databricks or via the Databricks REST API 2.0.  If none is provided,
#' netrc will be used.
#' @param ... additional arguments to be passed, i.e., overwrite = 'false' when
#' importing a file to run as a job.
#' @return A list with three elements containing the values of \code{create_job},
#'  \code{run_job}, and \code{get_run_status}:
#' \itemize{
#'     \item \emph{job} - The full API response, including status code.
#'     \item \emph{run} - The JSON result.
#'     \item \emph{run_status} - The JSON result transformed into a list.
#' }
#' @examples
#' results <- create_and_run_job(name = "Forecasting Report",
#'                               notebook_path = "/Shared/forecasting_report_notebook".
#'                               workspace = "https://eastus2.azuredatabricks.net",
#'                               token = "dapi2930482309ddavascscq")
#'
#' # Get each response
#' job <- results$job
#' run <- results$run
#' run_status <- results$run_status
#'
#'
create_and_run_job <- function(name = "R Job", file = NULL, notebook_path,
                               job_config = "default", workspace,
                               token = NULL, ...) {

  # If file provided, import it
  if (!is.null(file)) {

    import_response <- import_to_workspace(file = file,
                                           notebook_path = notebook_path,
                                           overwrite = ...,
                                           workspace = workspace,
                                           token = token,
                                           verbose = F)
  }

  # If  import fails, exit
  if (import_response$status_code[1] != 200) {
    return(message(paste0(
      "Unable to import file.  Please check the response code:\n\n",
      jsonlite::prettify(import_response)
    )))
  }

  # Check for JSON file with job config
  if (file.exists(job_config)) {

    job_config <- toJSON(fromJSON(job_config), auto_unbox = T)

  }

  # Create the job
  job <- create_job(name = name,
                    file = file,
                    notebook_path = notebook_path,
                    job_config = job_config,
                    workspace = workspace,
                    token = token,
                    verbose = F,
                    overwrite = ...)

  # Run using job_id from create_job()
  run <- run_job(job_id = job$job_id[[1]],
                 workspace = workspace,
                 token = token,
                 verbose = F)

  # Get status using run_id from run_job()
  run_status <- get_run_status(run_id = run$run_id,
                               workspace = workspace,
                               token = token,
                               verbose = F)

  # Handling create job request
  if (job$response$status_code == 200) {

    message(paste0(
      "Job \"", name, "\" successfully created...  \nThe Job ID is: ",
      job$job_id[[1]]
    ))
  }

  else {
    return(message(paste0(
      "Status: ", job$response$status_code[1],
      "\nThe request was not successful:\n\n", jsonlite::prettify(job)
    )))
  }

  # Handling run job request
  if (run$run_response$status_code == 200) {
    message(paste0(
      "\nRun successfully launched...  \nThe run_id is: ", run$run_id
    ))
  }

  else {
    return(message(paste0(
      "Status: ", run$status_code[1],
      "\nUnable to launch run:\n\n", jsonlite::prettify(run$run_response)
    )))
  }

  # Handling run status request
  if (run_status$response$status_code == 200) {
    message(paste0(
      "\nYou can check the status of this run at: \n ",
      run_status$response_list$run_page_url
    ))
  }

  else {
    return(message(paste0(
      "Status: ", run_status$response$status_code[1],
      "\n Unable to get status of run: \n\n",
      jsonlite::prettify(run_status$response)
    )))
  }

  # Return the output of all API calls in a list
  reslist <- list(job = job,
                  run = run,
                  run_status = run_status)

  reslist
}
