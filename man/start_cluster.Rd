% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/start_cluster.R
\name{start_cluster}
\alias{start_cluster}
\title{Start a Databricks cluster.}
\usage{
start_cluster(cluster_id, workspace, token = NULL, verbose = T, ...)
}
\arguments{
\item{cluster_id}{A string containing the unique id for an online Databricks cluster}

\item{workspace}{A string representing the web workspace of your Databricks
instance. E.g., "https://eastus2.azuredatabricks.net" or
"https://demo.cloud.databricks.com".}

\item{token}{A valid authentication token generated via User Settings in
Databricks or via the Databricks REST API 2.0.  If none is provided,
netrc will be used}

\item{verbose}{If TRUE, will print the API response to the console.  Defaults to
FALSE.}

\item{...}{Additional options to be passed to \code{data.table::fread} which is used to
parse the API response.}
}
\value{
The API response
}
\description{
Will start an interactive Databricks cluster.  You can locate the cluster ID in the URL of the cluster configuration
page.  For example:
}
\details{
https://mycompany.cloud.databricks.com/#/setting/clusters/xxxx-xxxxx-xxxxxx/

Where xxxx-xxxxx-xxxxxx is the cluster ID.

The API endpoint for terminating a cluster is '2.0/clusters/start'.
  For all details on API calls please see the official documentation at
\url{https://docs.databricks.com/dev-tools/api/latest/}.
}
\examples{
cluster_id <- "0818-155203-cheese22"

start_cluster(workspace = workspace, cluster_id = cluster_id)
}
