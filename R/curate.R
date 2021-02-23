#'
#' Curate a shared or personal library of packages
#'
#' Helper function to manage package installation across libraries
#'
#' `curate()` lets you easily install packages into different libraries.  It will
#' manage the copying of packages to a path on DBFS, which will persist them from
#' session to session on Databricks.
#'
#' @param pkg String, name of package to install or a valid URL if using GitHub/Gitlab
#' @param repo Where to pull the package from.  Default is RStudio Package Manager
#' (https://packagemanager.rstudio.com/all/__linux__/xenial/latest).  If NULL, specify
#' the path to a source file in `pkgs`.
#' @param version The desired version of the package to install.  Defaults to latest.
#' @param dest_lib String, the path to the library where packages will be installed.  Default
#' is the first path on `.libPaths()`.  It's recommended to use `set_library()` prior to
#' `curate()`
#' @param git_provider String, one of "github" or "gitlab".  Default is NULL.
#' @param ... Additional arguments to be passed to `install.packages`,
#' `remotes::install_version`, `remotes::install_github`, and `remotes::install_gitlab`.
#' Use this to pass authentication variables to git providers.
#'
#' @return The user library path
#' @examples
#' # Setting user library first
#' set_library("/dbfs/rk/my_packages")
#' curate(pkg = "broom")
#'
#' # Install version, setting user library
#' curate(pkg = "broom", version = "0.4.2", lib = set_library("/dbfs/rk/my_old_packages"))
#'
#' # Install GitHub
#' curate(repo = "RafiKurlansik/bricksteR")
#'
#' # Install Gitlab
#' curate(repo = "jimhester/covr")
#' @export
curate <- function(pkg,
                   repos = "https://packagemanager.rstudio.com/all/__linux__/xenial/latest",
                   version = NULL,
                   dest_lib = .libPaths()[1],
                   git_provider = NULL,
                   ...) {

  # Set up temp directory for installation
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  set_library(tmp_dir, Rversion = F)

  if(is.null(version) && is.null(git_provider)){

    # Normal installation
    install.packages(pkgs = pkg, repos = repos, lib = tmp_dir, ...)

    # Remove tmp_dir from .libPaths()
    .libPaths(c(.libPaths()[-1]))

    system(paste0("cp -r ", tmp_dir, "/* ", dest_lib))
    cat(c("Package: ", pkg, " installed in ", dest_lib))
  }

  # Check for install from version control
  if(!is.null(git_provider)) {
    git_provider <- tolower(git_provider)

    if(!(git_provider %in% c("gitlab", "github"))) {
      stop("Check your git_provider parameter, only \"gitlab\" or \"github\" are supported")
    }

    # Install GitHub
    if(git_provider == "github") {

      remotes::install_github(repo = pkg, lib = tmp_dir, dependencies = T, ...)

      # Remove tmp_dir from .libPaths()
      .libPaths(c(.libPaths()[-1]))

      system(paste0("cp -r ", tmp_dir, "/* ", dest_lib))
      cat(c("\n\nRemote GitHub package installed from ", pkg, " in ", dest_lib))

    } else {

      # Install Gitlab
      remotes::install_gitlab(repo = pkg, lib = tmp_dir, dependencies = T, ...)

      # Remove tmp_dir from .libPaths()
      .libPaths(c(.libPaths()[-1]))

      system(paste0("cp -r ", tmpDir, "/* ", dest_lib))
      cat(c("\n\nRemote Gitlab package installed from  ", pkg, " in ", dest_lib))

    }
  }

  # Check for installing a version
  if(!is.null(version)) {
    remotes::install_version(package = pkg,
                             version = version,
                             repos = "https://cloud.r-project.org",
                             lib = tmp_dir,
                             ...)

    # Remove tmp_dir from .libPaths()
    .libPaths(c(.libPaths()[-1]))

    # Copy package from tmp_dir to first on .libPaths()
    system(paste0("cp -r ", tmp_dir, "/* ", dest_lib))
    cat(c("Version ", version, " of ", pkg, " installed in ", dest_lib))

  }

  # Clean up temp directory
  system(paste0("rm -r ", tmp_dir))

}



