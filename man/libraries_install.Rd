% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/libraries_install.R
\name{libraries_install}
\alias{libraries_install}
\title{Install Packages (libraries) on a Databricks Cluster}
\usage{
libraries_install(
  cluster_id,
  package,
  repo = "https://packagemanager.rstudio.com/all/__linux__/xenial/latest",
  workspace,
  token = NULL,
  verbose = T,
  ...
)
}
\arguments{
\item{cluster_id}{A string containing the unique id for an online
Databricks cluster.}

\item{package}{A string with the name of the package to install.}

\item{repo}{A string representing the repo hosting the package.
Defaults to RStudio Public Package Manager
(https://packagemanager.rstudio.com/all/__linux__/xenial/latest) for R
packages.}

\item{workspace}{A string representing the web workspace of your Databricks
instance. E.g., "https://eastus2.azuredatabricks.net" or
"https://demo.cloud.databricks.com".}

\item{token}{A valid authentication token generated via User Settings in
Databricks or via the Databricks REST API 2.0.  If none is provided,
netrc will be used.}

\item{verbose}{If TRUE, will print the API response to the console.  Defaults to
FALSE.}
}
\value{
The API response.
}
\description{
Packages installed this way will be added to the cluster configuration
going forward.  When you restart the cluster they will show up in the UI.
Cluster cannot be in a terminated state to use this function.  You can
locate the cluster ID in the URL of the cluster configuration page.
For example:
}
\details{
https://mycompany.cloud.databricks.com/#/setting/clusters/xxxx-xxxxx-xxxxxx/

Where xxxx-xxxxx-xxxxxx is the cluster ID.

The API endpoint for installing libraries is
'2.0/libraries/install'.
  For all details on API calls please see the official documentation at
\url{https://docs.databricks.com/dev-tools/api/latest/}.
}
\examples{
# Cluster to install on
workspace <- "https://mydb.cloud.databricks.com"
cluster_id <- "0818-155203-cheese22"

# Install package
libraries_install(package = "broom", cluster_id, workspace)

# Check installation status
get_library_statuses(cluster_id, workspace)
}
