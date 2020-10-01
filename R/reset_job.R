

#' Reset the configuration of a Job on Databricks
#'
#' This function will reset the configuration of an existing job
#' on Databricks.
#'
#' The API endpoint for creating a job is '2.0/jobs/create'.  For all details
#' on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param job_id A number representing the unique identifier of a job on Databricks.
#' @param name A string representing the name of the job.  It is encouraged
#' to choose a unique name for each job.
#' @param new_config A JSON formatted string or file specifying the details of
#'  the job, i.e., the name, cluster spec, and so on.
#' @param workspace A string representing the web workspace of your Databricks
#' instance. E.g., "https://eastus2.azuredatabricks.net" or
#' "https://demo.cloud.databricks.com".
#' @param token A valid authentication token generated via User Settings in
#' Databricks or via the Databricks REST API 2.0.  If none is provided,
#' netrc will be used.
#' @param verbose If true, will pretty print the success or failure of the
#' request.  Defaults to TRUE.
#' @return The complete API response.
#' @examples
#' # JSON string or file path can be passed
#' new_config <- "/configs/nightly_model_training.json"
#'
#' reset_job(job_id = 30198, new_config, workspace)
#'
#' # Reset by name
#' reset_job(name = "My R Job", new_config, workspace, token = "dapi1908sfapf2")

reset_job <- function(job_id = NULL,
                      name = NULL,
                      new_config,
                      workspace,
                      token = NULL,
                      verbose = T) {

    # If name provided, call jobs_list to find the job ID
  if (!is.null(name)) {

    jobs_tidy <- jobs_list(workspace = workspace,
                           token = token,
                           verbose = F)$response_tidy

    matches <- jobs_tidy[grepl(pattern = paste0("^", name,"$"), jobs_tidy$settings.name), ]

    # If there is more than one job with the same name
    if (length(matches$settings.name) > 1){

      message(paste0("Found multiple jobs with name \"", name, "\":\n"))
      message(paste0(
        capture.output(
          jobs_tidy[grepl(pattern = paste0("^", name,"$"), jobs_tidy$settings.name), ]),
        collapse = "\n"))
      return(message(paste0("\n\nPlease use a job ID or give the job a unique name.\n")))
    }

    # If no matches found
    else if (length(matches$settings.name) < 1) {
      message(paste0("No job with name \"", name,
                     "\" found.\n Please try a different name."))
      return("Couldn't find a job with that name.")
    }

    # If matched, set job_id and name for consistency between file and webapp
    config_list <- jsonlite::fromJSON(new_config)
    config_list$job_id <- matches$job_id
    config_list$new_settings$name <- name

    message(paste0("Job \"", name, "\" found with ", matches$job_id, "."))
  }

  else {

    # Set the job_id for the config
    config_list <- jsonlite::fromJSON(new_config)
    config_list$job_id <- job_id

    new_config <- jsonlite::toJSON(config_list, auto_unbox = T)
  }



  # Make request with netrc by default
  if (is.null(token)) {

    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::POST(url = paste0(workspace, "/api/2.0/jobs/reset"),
                 httr::content_type_json(),
                 body = new_config)})
  }

  else {

    # Authenticate with token
    headers <- c(
      Authorization = paste("Bearer", token)
    )

    # Make request
    res <- httr::POST(url = paste0(workspace, "/api/2.0/jobs/reset"),
                      httr::add_headers(.headers = headers),
                      httr::content_type_json(),
                      body = new_config)
  }

  # Handling successful API response
  if (res$status_code[1] == 200) {

    if (verbose == T) {
      message(paste0(
        "Status: ",
        res$status_code[1],
        "\nJob Name:", name,
        "\nJob ID: ", job_id,
        "\nJob settings updated.  Use jobs_list() for more information."
      ))
    }
  }
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







