##' Download the example data set from madams1/nbadata
##'  (\url{https://github.com/madams1/nbadata/})
##' @title Download example data set
##'
##' @param version Version number.  The default will load the most
##'   recent version on your computer or the most recent version known
##'   to the package if you have never downloaded the data before.
##'   With \code{nbadata_del}, specifying \code{version=NULL} will
##'   delete \emph{all} data sets.
##'
##' @param path Path to store the data at.  If not given,
##'   \code{datastorr} will use \code{rappdirs} to find the best place
##'   to put persistent application data on your system.  You can
##'   delete the persistent data at any time by running
##'   \code{nbadata_del(NULL)} (or \code{nbadata_del(NULL, path)} if you
##'   use a different path).
##'
##' @export
nbadata <- function(version=NULL, path=NULL) {
    datastorr::github_release_get(nbadata_info(path), version)
}

##' @export
##' @rdname nbadata
##'
##' @param local Logical indicating if local or github versions should
##'   be polled.  With any luck, \code{local=FALSE} is a superset of
##'   \code{local=TRUE}.  For \code{nbadata_version_current}, if
##'   \code{TRUE}, but there are no local versions, then we do check
##'   for the most recent github version.
##'
nbadata_versions <- function(local=TRUE, path=NULL) {
    datastorr::github_release_versions(nbadata_info(path), local)
}

##' @export
##' @rdname nbadata
nbadata_version_current <- function(local=TRUE, path=NULL) {
    datastorr::github_release_version_current(nbadata_info(path), local)
}

##' @export
##' @rdname nbadata
nbadata_del <- function(version, path=NULL) {
    datastorr::github_release_del(nbadata_info(path), version)
}

## Core data:
nbadata_info <- function(path) {
    datastorr::github_release_info("madams1/nbadata",
                                   filename="nbadata.zip",
                                   read=utils::unzip,
                                   path=path)
}

##' Maintainer-only function for releasing data.  This will look at
##' the version in the DESCRIPTION file and make a data release if the
##' GitHub repository contains the same version as we have locally.
##' Requires the \code{GITHUB_TOKEN} environment variable to be set.
##'
##' @title Make a data release.
##' @param ... Parameters passed through to \code{\link{github_release_create}}
##' @param path Path to the data (see \code{\link{nbadata}}).
##' @export
nbadata_release <- function(..., path=NULL) {
    datastorr::github_release_create(nbadata_info(path), ...)
}


# make use of unexported formatting function from utils
file_size_format <- utils:::format.object_size


#' Load all nbadata into workspace
#'
#' @param path A character string for the path to load nbadata files from.
#'   Defaults to current working directory.
#'
#' @return Called for its side effects. Returns invisible NULL
#' @export
#'
nbadata_load_all <- function(path = "./") {

    dataset_names <- c(
        "nba_teams",
        "nba_players",
        "nba_team_games",
        "nba_player_shots",
        "nba_play_by_play"
    )

    load_file <- function(fn) {
        load(paste0(path, fn, ".rda"), envir = .GlobalEnv)
    }

    for (i in seq_along(dataset_names)) {
        file_name <- paste0("./", dataset_names[i], ".rda")
        file_size <- file_size_format(file.size(file_name), "auto")

        message(
            "[", i, "/", length(dataset_names), "] Loading ",
            file_name, " (", file_size, ")"
        )
        load_file(dataset_names[i])
    }
    return(invisible(NULL))
}
