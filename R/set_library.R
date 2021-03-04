#'
#' Set the User Library Path
#'
#' Set the path for where R will install packages.
#'
#' To persist packages on Databricks use a path that begins with `/dbfs/`.
#' This is designed to be used in conjunction with `bricksteR::curate()`.
#' For shared clusters where users prefer package isolation, each user should
#' create their own path for package installation.  If building a central
#' shared repo of packages, set the path to a common directory.
#'
#' @param lib_path A string representing the path to install packages in DBFS
#' @param r_version boolean.  Should the current version of R be added as a directory in
#' the path?  Defaults to TRUE.
#'
#' @return The user library path
#' @examples
#'
#' path <- "/dbfs/rk/my_packages"
#'
#' set_library(lib_path = path)
#' @export
set_library <- function(lib_path = "/dbfs/Rlib", r_version = F){

  # If using default, add random path to avoid package collisions
  if (lib_path == '/dbfs/Rlib') {
    if(r_version == T) {
      user_lib_path <- file.path(lib_path,
                                 round(rnorm(1, sd = 10000)),
                                 getRversion())
    } else {
      user_lib_path <- file.path(lib_path,
                                 round(rnorm(1, sd = 10000)))
    }
  }

  if (r_version == T){
    user_lib_path <- file.path(lib_path, getRversion())
  } else {
    user_lib_path <- file.path(lib_path)
  }

  if (file.exists(user_lib_path) == F) {
    dir.create(user_lib_path, recursive = TRUE)
  }

  search_path <- .libPaths()
  if (user_lib_path %in% search_path) {
    search_path <- setdiff(search_path, user_lib_path)
  }

  # adding user specified library path into search paths
  search_path <- c(user_lib_path, search_path)
  .libPaths(search_path)

  cat(c("Library: ", user_lib_path, " added. \n\nUse .libPaths() to see all libraries."))
}

#' @rdname set_library
#'
#' @param user_lib_path A string representing the path to be removed from .libPaths()
#' @return The removed library path
#' @examples
#'
#' remove_library(path)
#' @export
remove_library <- function(lib_path){

  search_path <- .libPaths()
  if (lib_path %in% search_path) {
    search_path <- setdiff(search_path, lib_path)
    .libPaths(search_path)
  }
  cat(c("Library: ", lib_path, " removed. \n\nUse .libPaths() to see all  libraries."))
}
