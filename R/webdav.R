#' rdav: Interchange Files With 'WebDAV' Servers
#'
#' Provides functions to interchange files with WebDAV servers
#'
#' - download a file or a directory (recursively) from a WebDAV server
#' - upload a file or a directory (recursively) to a WebDAV server
#' - copy, move, delete files or directories on a WebDAV server
#' - list directories on the WebDAV server
#'
#' Notice: when uploading or downloading files, they are overwritten without any
#' warnings.
#'
#' @author {Gunther Krauss}
#'
#'
#' @examples
#' \dontrun{
#' # establish a connection, you will be asked for a password
#' r <- wd_connect("https://example.com/remote.php/webdav/","exampleuser")
#'
#' # show files / directoriess in main directory
#' wd_dir(r)
#'
#' # lists 'subdir', returns a dataframe
#' wd_dir(r, "subdir", as_df = TRUE)
#'
#' # create directory 'mydirectory' on the server
#' wd_mkdir(r,"mydirectory")
#'
#' # upload the local file testfile.R to the subdirectory 'mydirectory'
#' wd_upload(r, "testfile.R", "mydirectory/testfile.R")
#'
#' # download content of 'mydirectory' from the server and
#' # store it in 'd:/data/fromserver' on your computer
#' wd_download(r, "mydirectory", "d:/data/fromserver")
#'
#' }
#'
#' @md
#' @docType package
#' @name rdav
"_PACKAGE"


ns <- c(
  d = "DAV:",
  s = "http://sabredav.org/ns",
  oc = "http://owncloud.org/ns"
)


#' Establishes a connection to a WebDAV server
#'
#' Creates and authenticate a request handle to the WebDAV server
#'
#' Notice: it's not recommended to write the password as plain text. Either omit
#' the parameter (then you will be asked to enter a password interactively)
#' or use for example the system credential store via keyring package.
#'
#' @param url url of the WebDAV directory
#' @param username username
#' @param password password - if not given, you will be asked for it
#'
#' @return a httr2 request to the WebDAV server location
#' @export
#' @examples
#' \dontrun{
#' # establish a connection, you will be asked for a password
#'
#' r <- wd_connect("https://example.com/remote.php/webdav/","exampleuser")
#'
#'
#' # establish a connection, use keyring package to retrieve the password
#'
#' keyring::key_set("mydav", "exampleuser") # call only once
#'
#' r <- wd_connect("https://example.com/remote.php/webdav/",
#'                 "exampleuser"
#'                 keyring::key_get("mydav", "exampleuser"))
#' }


wd_connect <- function(url, username, password = NULL) {
  req <- httr2::request(url) |>
    httr2::req_auth_basic(username, password)
  resp <- req |>
    httr2::req_method("HEAD") |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_perform()
  if (httr2::resp_is_error(resp)) {
    stop(httr2::resp_status_desc(resp))
  }
  req
}


#' Copies a file or directory on the WebDAV server
#'
#' @param req request handle obtained from \code{\link{wd_connect}}
#' @param source path of the source on the server
#' @param target path of the target on the server
#' @param overwrite overwrites files when TRUE (default)
#'
#' @return TRUE on success, FALSE on failure (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' wd_copy(r, "testfile.R", "testfile_old.R")
#'
#' }

wd_copy <- function(req, source, target, overwrite = TRUE) {
  resp <- req |>
    httr2::req_url_path_append(utils::URLencode(source)) |>
    httr2::req_method("COPY") |>
    httr2::req_headers(
      Overwrite = ifelse(overwrite, "T", "F"),
      Destination = paste0(req$url, utils::URLencode(target))
    )  |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_perform()
  if (httr2::resp_is_error(resp)) {
    warning("File could not be copied. Maybe the file already exists. ",
            httr2::resp_status_desc(resp))
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}

#' Moves a file or directory on the server
#'
#' @param req request handle obtained from \code{\link{wd_connect}}
#' @param source path of the source on the server
#' @param target path of the target on the server
#' @param overwrite overwrites files  when TRUE (default)
#'
#' @return TRUE on success, FALSE on failure (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' wd_move(r, "testfile.R", "testfile_old.R")
#'
#' }
wd_move <- function(req, source, target, overwrite = TRUE) {
  resp <- req |>
    httr2::req_url_path_append(utils::URLencode(source)) |>
    httr2::req_method("MOVE") |>
    httr2::req_headers(
      Overwrite = ifelse(overwrite, "T", "F"),
      Destination = paste0(req$url, utils::URLencode(target))
    )  |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_perform()
  if (httr2::resp_is_error(resp)) {
    warning("File could not be moved. Maybe target file already exists. ",
            httr2::resp_status_desc(resp))
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}


#' Deletes a file or directory (collection) on WebDAV server
#'
#' @param req request handle obtained from \code{\link{wd_connect}}
#' @param file path to file or directory to delete on the server
#'
#' @return TRUE on success, FALSE on failure (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' wd_delete(r, "testfile.R")
#'
#' }
wd_delete <- function(req, file) {
  resp <- req |>
    httr2::req_method("DELETE") |>
    httr2::req_url_path_append(utils::URLencode(file)) |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_perform()
  if (httr2::resp_is_error(resp)) {
    warning(httr2::resp_status_desc(resp))
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}

#' Creates a directory (collection) on WebDAV server
#'
#' When creating a subdirectoy, all parent directories have to exist on the
#' server.
#'
#' @param req request handle obtained from \code{\link{wd_connect}}
#' @param directory directory path on server
#'
#' @return TRUE on success, FALSE on failure (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # creates 'newdir' inside the subdirectory 'existing/directory'
#' wd_mkdir(r, "existing/directory/newdir")
#'
#' }
wd_mkdir <- function(req, directory) {
  if(!wd_isdir(req, directory, TRUE)) {
    resp <- req |>
      httr2::req_method("MKCOL") |>
      httr2::req_url_path_append(utils::URLencode(directory)) |>
      httr2::req_error(is_error = \(x) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_is_error(resp)) {
      warning(httr2::resp_status_desc(resp))
      invisible(FALSE)
    } else {
      invisible(TRUE)
    }
  }
  else {
    invisible(TRUE)
  }

}

#' Lists the content of a WebDAV directory
#'
#' @param req request handle obtained from \code{\link{wd_connect}}
#' @param directory directory path
#' @param full_names if TRUE, the directory path is prepended to the file names
#'   to give a relative file path (relevant only if as_df is FALSE)
#' @param as_df if TRUE outputs a data.frame with file information
#'
#' @return a vector of filenames or a dataframe (when as_df is TRUE) with
#'   detailed file information (filename, path, isdir, size, lastmodified)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # lists names of files and directories in the main directory
#' wd_dir(r)
#'
#' # lists names of files and directories in the subdirectory "mydirectory"
#' wd_dir(r, "mydirectory")
#'
#' # lists names of files and directories with the relative path
#' wd_dir(r, "mydirectory", full_names=TRUE)
#'
#' # returns a data.frame with the columns filename, size and isdir (whether
#' # it's a directory or file
#' wd_dir(r, "mydirectory", as_df=TRUE)
#'
#' }
wd_dir <- function(req, directory = "", full_names = FALSE, as_df = FALSE) {

  resp <- req |>
    httr2::req_url_path_append(utils::URLencode(directory)) |>
    httr2::req_method("PROPFIND") |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_headers(
      Depth = "1"
    )  |>
    httr2::req_perform()
  if (httr2::resp_is_error(resp)) {
    warning(httr2::resp_status_desc(resp))
    invisible(NA)
  } else {
    dr <- resp |>
      httr2::resp_body_xml() |>
      xml2::as_xml_document()

    rs <- xml2::xml_find_all(dr, "//d:response", ns)

    href <- sapply(rs,
      \(x) {
        xml2::xml_find_first(x, "d:href", ns) |>
          xml2::xml_text()
      }
    )
    dpath <- httr2::url_parse(req$url)$path
    if(is.null(dpath)) {
      dpath <- "/"
    }
    path <- substring(utils::URLdecode(href), nchar(dpath) + 1)


    file <- basename(path)

    if (as_df) {
      ps <- xml2::xml_find_all(dr, "//d:response/d:propstat/d:prop", ns)

      isdir <- sapply(ps,
        \(x) {
          xml2::xml_find_first(x, "d:resourcetype/d:collection", ns) |>
            length() > 0
        }
      )
      lastmodified <- sapply(ps,
        \(x) {
          xml2::xml_find_first(x, "d:getlastmodified", ns) |>
            xml2::xml_text()
        }
      )
      lastmodified <- substr(lastmodified, 6, nchar(lastmodified)) |>
        as.POSIXct(format = "%e %b %Y %H:%M:%S GMT", tz = "GMT")
      contenttype <- sapply(ps,
        \(x) {
          xml2::xml_find_first(x, "d:getcontenttype", ns) |>
            xml2::xml_text()
        }
      )
      size <- sapply(ps,
        \(x) {
          xml2::xml_find_first(x, "d:getcontentlength", ns) |>
            xml2::xml_text()
        }
      )

      df <- data.frame(
        file,
        path,
        isdir,
        size,
        lastmodified,
        contenttype,
        href
      )
      if (isdir[1]) {
        df[-1, ]
      } else {
        df
      }
    } else {
      if (full_names) {
        path[-1]
      } else {
        file[-1]
      }
    }
  }
}

#' Checks if the resource on WebDAV is a directory
#'
#' @param req request handle obtained from \code{\link{wd_connect}}
#' @param directory path to directory
#' @param silent if FALSE a warning is given if the directory does not exists
#'
#' @return TRUE if it is a directory, FALSE else
#' @export
#'
#' @examples
#' \dontrun{
#'
#' wd_isdir(r, "testfile.R") # FALSE
#' wd_isdir(r, "mydirectory")   # TRUE
#'
#' }
wd_isdir <- function(req, directory, silent = FALSE) {

  resp <- req |>
    httr2::req_url_path_append(utils::URLencode(directory)) |>
    httr2::req_method("PROPFIND") |>
    httr2::req_headers(
      Depth = "0",
      ContentType = "application/xml"
    )  |>
    httr2::req_body_raw('<?xml version="1.0" encoding="utf-8" ?>
      <d:propfind xmlns:d=\"DAV:\">
       <d:prop><d:resourcetype/></d:prop>
      </d:propfind>') |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_perform()

  if (httr2::resp_is_error(resp)) {
    if (!silent) warning(httr2::resp_status_desc(resp))
    invisible(FALSE)
  } else {
    dr <- resp |>
      httr2::resp_body_xml() |>
      xml2::as_xml_document() |>
      xml2::xml_find_first(
        "//d:response/d:propstat/d:prop/d:resourcetype/d:collection",
        ns
      ) |>
      length()
    dr[1] > 0

  }
}

#' Uploads a file or directory to WebDAV
#'
#' Directories are uploaded recursively.
#' If the source is a file and the target a directory, then the file is uploaded
#' into the directory.
#' If the target is omitted, then the file or directory name
#' (\code{\link[base]{basename}}) will be used.
#'
#' @param req request handle obtained from \code{\link{wd_connect}}
#' @param source path to local file or directory
#' @param target path to remote file or directory, if omitted the file or
#'   directory name will be used, if source is a file and target a directory
#'   then the file will be put into the target directory
#'
#' @return vector of uploaded files (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' wd_upload(r, "d:/data/weather", "weatherfiles")
#' wd_upload(r, "d:/data/abc.txt", "test/xyz.txt")
#' wd_upload(r, "d:/data/abc.txt", "test") # uploaded file will be test/abc.txt
#'
#' }
wd_upload <- function(req, source, target = "") {
  if (target == "") {
    target <- basename(source)
  }
  if (file.exists(source) && !dir.exists(source) && wd_isdir(req, target, silent = TRUE)) {
    target <- paste0(target, "/", basename(source))
  }
  if (dir.exists(source)) {
    u <- character(0)
    wd_mkdir(req, target)
    fl <- list.files(source, no.. = TRUE)
    for (f in fl) {
      u <- c(u, wd_upload(req, paste0(source, "/", f), paste0(target, "/", f)))
    }
    invisible(u)
  } else {
    resp <- req |>
      httr2::req_method("PUT") |>
      httr2::req_url_path_append(utils::URLencode(target)) |>
      httr2::req_body_file(source) |>
      httr2::req_error(is_error = \(x) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_is_error(resp)) {
      warning(httr2::resp_status_desc(resp))
      invisible(character(0))
    } else {
      invisible(target)
    }
  }
}

#' Fetches a file or directory (recursively) from the WebDAV server
#'
#' Directories are downloaded recursively.
#' If the source is a file and the target a directory, then the file is
#' downloaded to the target directory.
#' If the target is omitted, then the file or directory name
#' (\code{\link[base]{basename}}) will be used.
#'
#' @param req request handle obtained from \code{\link{wd_connect}}
#' @param source path to source file or directory on server
#' @param target path to local target file or directory, if omitted the file or
#'   directory name will be used, if source is a file and target a directory
#'   then the file will be put into the target directory
#'
#' @return vector of downloaded files (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' wd_download(r, "weatherfiles", "d:/data/weather")
#' wd_download(r, "test/xyz.txt", "d:/data/abc.txt")
#'
#' }
wd_download <-  function(req, source, target = "") {
  if (target == "") {
    target <- basename(source)
  }
  isdir <- wd_isdir(req, source, silent = TRUE)
  if (!isdir && dir.exists(target)) {
    target <- paste0(target, "/", basename(source))
  }
  if (isdir) {
    dl <- character(0)
    fl <- wd_dir(req, source)
    dir.create(target)
    for (f in fl) {
      dl <- c(dl,
              wd_download(req, paste0(source, "/", f), paste0(target, "/", f)))
    }
    invisible(dl)
  } else {
    resp <- req |>
      httr2::req_method("GET") |>
      httr2::req_url_path_append(utils::URLencode(source)) |>
      httr2::req_error(is_error = \(x) FALSE) |>
      httr2::req_perform(target)
    if (httr2::resp_is_error(resp)) {
      warning(httr2::resp_status_desc(resp))
      invisible(character(0))
    } else {
      invisible(target)
    }
  }
}
