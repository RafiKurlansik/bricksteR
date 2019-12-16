#' Delete a job on Databricks
#'
#' Delete the job and send an email to the addresses specified in
#' JobSettings.email_notifications. No action occurs if the job
#' has already been removed. After the job is removed, neither
#'  its details or its run history is visible via the Jobs UI or API.
#'
#' The job is guaranteed to be removed upon completion of this request.
#'  However, runs that were active before the receipt of this request
#'  may still be active. They will be terminated asynchronously.
#' The API endpoint for deleting a job is '2.0/jobs/delete'.  For all details
#' on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param job_id A number representing the unique identifier of a job on Databricks.
#' @param name Optional.  A string representing the name of the job.  If multiple
#' jobs share the same name, you'll need to rename the jobs or provide
#' the unique job ID.
#' @param workspace A string representing the web workspace of your Databricks
#' instance. E.g., "https://eastus2.azuredatabricks.net" or
#' "https://demo.cloud.databricks.com".
#' @param token A valid authentication token generated via User Settings in
#' Databricks or via the Databricks REST API 2.0.  If none is provided, netrc will be used.
#' @param verbose If true, will pretty print the success or failure of the
#' request and add a `job_id` variable to the R environment.  Defaults to TRUE.
#' @return The API response.
#' @examples
#' delete_job(job_id = 206,
#' workspace = "https://dbc-z64b06b4-d212.cloud.databricks.com",
#' token = "dapi310240980a9dgqwebdsfadsf21")
#'
delete_job <- function(job_id = NULL, name = NULL, workspace, token = NULL, verbose = T) {

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
      return(message(
        paste0("\n\nPlease use a job ID or give the job a unique name.\n",
      "Too many jobs with that name.")
      ))
    }

    ## If no matches found
    else if (length(matches$settings.name) < 1) {
      message(paste0("No job with name \"", name, "\" found.\n Please try a different name."))
      return("Couldn't find a job with that name.")
    }

    ## If exact match fetch the job id
    job_id <- paste0('{ "job_id": ', matches$job_id, ' }')

    message(paste0("Job \"", name, "\" found with ", job_id, "."))
  }

  ## If no name provided, use job_id param
  else{
    job_id <- paste0('{ "job_id": ', job_id, ' }')
  }

  ## Make request with netrc by default
  if (is.null(token)) {

    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::POST(url = paste0(workspace, "/api/2.0/jobs/delete"),
                 httr::content_type_json(),
                 body = job_id)})
  }

  else {
    ## Authenticate with token
    headers <- c(
      Authorization = paste("Bearer", token)
    )

    ## Make request
    res <- httr::POST(url = paste0(workspace, "/api/2.0/jobs/delete"),
                      httr::add_headers(.headers = headers),
                      httr::content_type_json(),
                      body = job_id)
  }

  ## Handle API response statuses
  if (verbose == T) {
    message(paste0(
      "Status: ", res$status_code[1],
      "\n", job_id, " has been deleted."))
  }

  else {
    message(paste0(
      "Status: ", res$status_code[1],
      "\nThe request was not successful:\n\n", jsonlite::prettify(res)
    ))
  }
}
