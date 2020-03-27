#'
#' Create an execution context for running commands on Databricks.
#'
#' This function provides the context for executing remote commands on an existing
#' Databricks cluster via REST API.  The output is intended to be passed to
#' \code{databricks_execute}.
#'
#' The API endpoint for creating the execution context is is '1.2/contexts/create'.
#'   For all details on API calls please see the official documentation at
#' \url{https://docs.databricks.com/dev-tools/api/latest/}.
#'
#' @param workspace A string representing the web workspace of your Databricks
#' instance. E.g., "https://eastus2.azuredatabricks.net" or
#' "https://demo.cloud.databricks.com".
#' @param cluster_id A string containing the ID of an active Databricks cluster.
#' @param language The language to execute commands in.  The default is 'r', but
#'  can also be set to 'python', 'scala' and 'sql'.
#' @param token A valid authentication token generated via User Settings in
#' Databricks or via the Databricks REST API 2.0.  If none is provided,
#' netrc will be used.
#' @param verbose If true, will pretty print the success or failure of the
#' request.  Defaults to FALSE.
#' @return A list with five elements:
#' \itemize{
#'     \item \emph{language} - The language to execute commands in.
#'     \item \emph{cluster_id} - ID of the active Databricks cluster.
#'     \item \emph{context_id} - ID of the execution context on the cluster.
#'     \item \emph{workspace} - The URL of the Databricks workspace.
#'     \item \emph{token} - The token used to authenticate.
#' }
#' @examples
#' context <- create_execution_context(workspace = "https://eastus2.azuredatabricks.net",
#'                   token = "dapiafoi32984320890d9asaf1",
#'                   language = "r",
#'                   cluster_id = "1017-337483-jars232")
#'
#' ## Use the context to execute a command on Databricks
#' command <- "iris[1, ]"
#' databricks_execute(command, context)
#'

create_execution_context <- function(workspace, cluster_id, language = 'r', token = NULL, verbose = F) {
  payload <- paste0('{
                  "language": "', language, '",
                  "clusterId": "', cluster_id, '"
                  }')

  ## Make request, using netrc by default
  if (is.null(token)) {

    use_netrc <- httr::config(netrc = 1)
    res <- httr::with_config(use_netrc, {
      httr::POST(url = paste0(workspace, "/api/1.2/contexts/create"),
                 httr::content_type_json(),
                 body = payload)
    })
  }

  else {

    ## Bearer Authentication
    headers <- c(
      Authorization = paste("Bearer", token)
    )

    res <- httr::POST(url = paste0(workspace, "/api/1.2/contexts/create"),
                      httr::add_headers(.headers = headers),
                      httr::content_type_json(),
                      body = payload)
  }

  ## Extract execution ID from response
  context_id <- jsonlite::fromJSON(rawToChar(res$content))$id

  if (verbose == T) {
    ## Successful request message
    if (res$status_code[1] == 200) {

      message(paste0(
        "Status: ", res$status_code[1],
        "\n\nExecution Context ID ", context_id,
        " created. "
      ))

    }

    ## Unsuccessful request message
    else {
      return(message(paste0(
        "Status: ", res$status_code[1],
        "\nThe request was not successful:\n\n", jsonlite::prettify(res)
      )))
    }
  }

  ## Return the values necessary for command execution endpoint
  context <- list(language = language,
                  cluster_id = cluster_id,
                  context_id = context_id,
                  workspace = workspace,
                  token = token)

  context
}
