#' Create a new Job on Databricks
#'
#' This function will create a new job on Databricks, but will not run it.  To
#'  run a job, see \code{\link{run_job}} or \code{\link{runs_submit}}.
#'
#' The API endpoint for creating a job is '2.0/jobs/create'.  For all details
#' on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param name A string representing the name of the job.  It is encouraged
#' to choose a unique name for each job.
#' @param notebook_path A string representing the path to a Databricks notebook in the
#' workspace.
#' @param file The path to a local .R or .Rmd file.  Will be imported to the
#' workspace at the \emph{notebook_path}.
#' @param job_config A JSON formatted string or file specifying the details of the job, i.e., the
#'  name, cluster spec, and so on.
#' @param workspace A string representing the web workspace of your Databricks
#' instance. E.g., "https://eastus2.azuredatabricks.net" or
#' "https://demo.cloud.databricks.com".
#' @param token A valid authentication token generated via User Settings in
#' Databricks or via the Databricks REST API 2.0.  If none is provided, netrc will be used.
#' @param verbose If true, will pretty print the success or failure of the
#' request and add a `job_id` variable to the R environment.  Defaults to TRUE.
#' @param ... additional arguments to be passed, i.e., overwrite = 'false' when
#' importing a file to run as a job.
#' @return A list with two elements - the complete API response and the job ID.
#' @examples
#' # Default JSON used
#' create_job(path = "/Shared/R/brickster_tutorial", # A notebook in the workspace
#'  workspace = "https://dbc-z64b06b4-d212.cloud.databricks.com", # The workspace of your Databricks instance
#'   token = "dapi30912309sdfdsa9iu09") # The valid auth token
#'
#' # Passing custom JSON
#' job_config <- '{"name": "New R Job",
#'                    "new_cluster": {
#'                        "spark_version": "7.3.x-scala2.12",
#'                        "node_type_id": "i3.xlarge",
#'                        "aws_attributes": {
#'                            "availability": "ON_DEMAND"
#'                        },
#'                        "num_workers": 2,
#'                        "email_notifications": {
#'                            "on_start": [],
#'                            "on_success": [],
#'                            "on_failure": []
#'                        },
#'                        "notebook_task": {
#'                            "notebook_path": "/Shared/R/brickster_tutorial"
#'                        }
#'                    }
#'                  }'
#'
#' # Specifying the path now unnecessary
#' create_job(job_config,
#' workspace = "https://dbc-z64b06b4-d212.cloud.databricks.com",
#' token = "dapi310240980a9dgqwebdsfadsf21")
create_job <- function(name = "R Job",
                       file = NULL,
                       notebook_path,
                       job_config = "default",
                       workspace,
                       token = NULL,
                       verbose = T,
                       ...) {

    # Import R file to workspace if needed
  if (!is.null(file)) {
    import_response <- import_to_workspace(file = file,
                                           notebook_path = notebook_path,
                                           overwrite = ...,
                                           workspace = workspace,
                                           token = token,
                                           verbose = F)
    # If import fails, exit
    if (import_response$status_code[1] != 200) {

      return(message(paste0(
        "Unable to import file.  Please check the response code:\n\n",
        jsonlite::prettify(import_response)
      )))
    }
  }

  # Check for job config in JSON file
  if (file.exists(job_config)) {

    job_config <- toJSON(fromJSON(job_config), auto_unbox = T)

  }

  # Default small cluster spec
  if (job_config == "default") {
    job_config <- paste0('{
                         "name": "', name, '",
                         "new_cluster": {
                         "spark_version": "7.3.x-scala2.12",
                         "node_type_id": "i3.xlarge",
                         "num_workers": 2
                         },
                         "email_notifications": {
                         "on_start": [],
                         "on_success": [],
                         "on_failure": []
                         },
                         "notebook_task": {
                         "notebook_path": "', notebook_path, '"
                         }
  }')
  }

  # Make request, using netrc by default
  if (is.null(token)) {

    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::POST(url = paste0(workspace, "/api/2.0/jobs/create"),
                 httr::content_type_json(),
                 body = job_config)})
  }

  else {

    # Authenticate with token
    headers <- c(
      Authorization = paste("Bearer", token)
    )

    # Using token for authentication instead of netrc
    res <- httr::POST(url = paste0(workspace, "/api/2.0/jobs/create"),
                      httr::add_headers(.headers = headers),
                      httr::content_type_json(),
                      body = job_config)
  }

  # Handling successful API request
  if (res$status_code[1] == 200) {

    job_id <- jsonlite::fromJSON(rawToChar(res$content))[[1]]

    if (verbose == T) {

      message(paste0(
        "Status: ", res$status_code[1],
        "\nJob \"", name, "\" created.",
        "\nJob ID: ", job_id
      ))
    }
  }

  # Handling unsuccesful request
  else {

    job_id <- NA

    if (verbose == T) {

      message(paste0(
        "Status: ", res$status_code[1],
        "\nThe request was not successful:\n\n", jsonlite::prettify(res)
      ))
    }
  }

  # Return response
  reslist <- list(response = res,
                  job_id = job_id)
}
