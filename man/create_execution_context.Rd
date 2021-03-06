% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_execution_context.R
\name{create_execution_context}
\alias{create_execution_context}
\title{Create an execution context for running commands on Databricks.}
\usage{
create_execution_context(
  workspace,
  cluster_id,
  language = "r",
  token = NULL,
  verbose = F
)
}
\arguments{
\item{workspace}{A string representing the web workspace of your Databricks
instance. E.g., "https://eastus2.azuredatabricks.net" or
"https://demo.cloud.databricks.com".}

\item{cluster_id}{A string containing the ID of an active Databricks cluster.}

\item{language}{The language to execute commands in.  The default is 'r', but
can also be set to 'python', 'scala' and 'sql'.}

\item{token}{A valid authentication token generated via User Settings in
Databricks or via the Databricks REST API 2.0.  If none is provided,
netrc will be used.}

\item{verbose}{If true, will pretty print the success or failure of the
request.  Defaults to FALSE.}
}
\value{
A list with five elements:
\itemize{
    \item \emph{language} - The language to execute commands in.
    \item \emph{cluster_id} - ID of the active Databricks cluster.
    \item \emph{context_id} - ID of the execution context on the cluster.
    \item \emph{workspace} - The URL of the Databricks workspace.
    \item \emph{token} - The token used to authenticate.
}
}
\description{
This function provides the context for executing remote commands on an existing
Databricks cluster via REST API.  The output is intended to be passed to
\code{databricks_execute}.
}
\details{
The API endpoint for creating the execution context is is '1.2/contexts/create'.
  For all details on API calls please see the official documentation at
\url{https://docs.databricks.com/dev-tools/api/latest/}.
}
\examples{
# Using netrc
context <- create_execution_context(workspace = "https://eastus2.azuredatabricks.net",
                  language = "r",
                  cluster_id = "1017-337483-jars232")

## Use the context to execute a command on Databricks
command <- "iris[1, ]"
databricks_execute(command, context)

}
