#' Prepend https to a hostname
#'
#' @param hostname host name (with or without protocol prefix)
#'
#' @returns host name with https:// prefix
#' @noRd
#' @examples
#' hostname_to_url("example.com")
#' hostname_to_url("http://old.example.com")
#'
hostname_to_url <- function(hostname) {
  ifelse(
         startsWith(hostname, "https://"),
         hostname,
         ifelse(
                startsWith(hostname, "http://"),
                hostname,
                paste0("https://", hostname)))
}


#' Builds the Nextcloud base URL for a nextcloud server from host and user names
#'
#' @param hostname host name
#' @param username user name
#' @param path_prefix optional path to webdav directory
#'
#' @returns base url to use with \code{\link{wd_connect}}
#' @export
#
#' @examples
#' ncl_baseurl("example.com","johndoe")
#' ncl_baseurl("example.com","johndoe","sub/dir/")
#'
ncl_baseurl <- function(hostname, username, path_prefix = "") {
  httr2::url_parse(hostname_to_url(hostname)) |>
    httr2::url_modify(path = path_prefix) |>
    httr2::url_modify_relative("remote.php/dav/files/") |>
    httr2::url_modify_relative(username) |>
    httr2::url_build()
}



#' Builds the Nextcloud base URL for a public share from host and user names
#'
#' @param hostname host name
#' @param username user name
#' @param path_prefix ptional path to webdav directory
#'
#' @returns share url to use with \code{\link{wd_connect}}
#' @export
#'
#' @examples
#' ncl_shareurl("example.com", "johndoe")
#'
ncl_shareurl <- function(hostname, username, path_prefix = "") {
  httr2::url_parse(hostname_to_url(hostname)) |>
    httr2::url_modify(path = path_prefix) |>
    httr2::url_modify_relative("public.php/dav/files/") |>
    httr2::url_modify_relative(username) |>
    httr2::url_build()
}

#' Creates the Nextcloud base URL for a share from a public share url
#'
#' @param url link to public share
#'
#' @returns share url to use with \code{\link{wd_connect}}
#' @export
#'
#' @examples
#' ncl_shareurl_from_publicurl("https://example.com/s/87d7edad/")
#'
ncl_shareurl_from_publicurl <- function(url) {
  surl <- httr2::url_parse(url)
  user <- ""
  path_prefix <- ""
  spl <- strsplit(surl$path, "/s/", fixed = TRUE)[[1]]
  if (length(spl) > 1) {
    path_prefix <- spl[1]
    user <- spl[2]
  }
  hn <- paste0(surl$scheme, "://", surl$hostname)
  ncl_shareurl(hn, user, path_prefix)
}

#' Extracts the user name from a Nextcloud Base URL
#'
#' @param url base or share url
#'
#' @returns user name
#' @export
#'
#' @examples
#' ncl_username_from_url("https://example.com/remote.php/dav/files/johndoe")
#'
ncl_username_from_url <- function(url) {
  surl <- httr2::url_parse(url)
  user <- ""
  spl <- strsplit(surl$path, "/dav/files/", fixed = TRUE)[[1]]
  if (length(spl) > 1) {
    user <- spl[2]
  }
  user
}
