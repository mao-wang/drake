#' @title List all the `storr` cache namespaces used by drake.
#' @export
#' @seealso [make()]
#' @return A character vector of `storr` namespaces used for drake.
#' @description Ordinary users do not need to worry about this function.
#' It is just another window into `drake`'s internals.
#' @param default name of the default `storr` namespace
#' @examples
#' cache_namespaces()
cache_namespaces <- function(
  default = storr::storr_environment()$default_namespace
) {
  out <- c(
    target_namespaces(default = default),
    "change",   # value returned by the "change" trigger
    "config",   # elements of the config list
    "memoize",  # for the memoization in preprocessing
    "progress", # build progress: in progress, finished, failed, etc.
    "session"   # session info
  )
  sort(out)
}

#' @title For drake caches,
#'   list the `storr` namespaces that are cleaned
#'   during a call to [clean()].
#' @description All these
#' namespaces store target-level data, but not all
#' target-level namespaces are cleaned during
#' [clean()].
#' @export
#' @seealso [cache_namespaces()], [clean()]
#' @return A character vector of `storr` namespaces
#'   that are cleaned during [clean()].
#' @param default Name of the default `storr` namespace.
#' @examples
#' cleaned_namespaces()
cleaned_namespaces <- function(
  default = storr::storr_environment()$default_namespace
) {
  out <- c(
    default,   # the target values themselves
    "kernels", # reproducibly-tracked representation of targets. watched for changes # nolint
    "meta"     # watched metadata such as hashes and time stamps
  )
  sort(out)
}

#' @title For drake caches,
#'   list the `storr` cache namespaces
#'   that store target-level information.
#' @export
#' @seealso [make()]
#' @return A character vector of `storr` namespaces that store
#'   target-level information.
#' @description Ordinary users do not need to worry about this function.
#' It is just another window into `drake`'s internals.
#' @param default name of the default `storr` namespace
#' @examples
#' target_namespaces()
target_namespaces <- function(
  default = storr::storr_environment()$default_namespace
) {
  out <- c(
    cleaned_namespaces(default = default),
    "progress"
  )
  sort(out)
}

list_multiple_namespaces <- function(cache, namespaces, jobs = 1) {
  out <- lightly_parallelize(
    X = namespaces,
    FUN = function(namespace) {
      cache$list(namespace = namespace)
    },
    jobs = jobs
  )
  Reduce(out, f = base::union)
}
