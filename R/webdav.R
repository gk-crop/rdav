#' Establishes a connection to a WebDAV server
#'
#' Creates and authenticate a request handle to the webserver
#'
#' @param url url of the webdav folder
#' @param user username
#'
#' @return a httr2 request
#' @export
#'
wd_connect <- function(url, user) {
  httr2::request(url) |>
    httr2::req_auth_basic(user)
}


#' Copies a file or folder on the WebDAV server
#'
#' @param req request handle
#' @param source path of the source on server
#' @param target target path on the server
#'
#' @return invisibly true on success or false on failure
#' @export
#'
#'
wd_copy <- function(req, source, target) {
  resp <- req |>
    httr2::req_url_path_append(source) |>
    httr2::req_method('COPY') |>
    httr2::req_headers(
      Destination = paste0(req$url,target)
    )  |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_perform()
  if(httr2::resp_is_error(resp)) {
    warning(httr2::resp_status_desc(resp))
    invisible(FALSE)
  }
  else {
    invisible(TRUE)
  }
}

#' Moves a file or folder on the server
#'
#' @param req request handle
#' @param source path of the source on server
#' @param target target path on the server
#'
#' @return invisibly true on success or false on failure
#' @export
#'
#'
wd_move <- function(req, source, target) {
  resp <- req |>
    httr2::req_url_path_append(source) |>
    httr2::req_method('MOVE') |>
    httr2::req_headers(
      Destination = paste0(req$url,target)
    )  |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_perform()
  if(httr2::resp_is_error(resp)) {
    warning(httr2::resp_status_desc(resp))
    invisible(FALSE)
  }
  else {
    invisible(TRUE)
  }
}


#' Deletes a file or folder (collection) on WebDAV server
#'
#' @param req request handle
#' @param folder folder
#'
#' @return invisibly true on success or false on failure
#' @export
#'
#'
wd_delete <- function(req, folder) {
  resp <- req |>
    httr2::req_method("DELETE") |>
    httr2::req_url_path_append(folder) |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_perform()
  if(httr2::resp_is_error(resp)) {
    warning(httr2::resp_status_desc(resp))
    invisible(FALSE)
  }
  else {
    invisible(TRUE)
  }
}

#' Creates a directory (collection) on WebDAV server
#'
#' @param req request handle
#' @param folder folder path on server
#'
#' @return @return invisibly true on success or false on failure
#' @export
#'
#'
wd_mkdir <- function(req, folder) {
  resp <- req |>
    httr2::req_method("MKCOL") |>
    httr2::req_url_path_append(folder) |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_perform()
  if(httr2::resp_is_error(resp)) {
    warning(httr2::resp_status_desc(resp))
    invisible(FALSE)
  }
  else {
    invisible(TRUE)
  }
}

#' Lists the content of a WebDAV directory
#'
#' @param req request handle
#' @param folder folder path
#' @param full_names whether filenames should be returned with path
#' @param as_df outputs a data.frame with file information
#'
#' @return a vector of filenames or a dataframe
#' @importFrom rlang .data
#' @export
#'
#'
wd_dir <- function(req, folder="", full_names=FALSE, as_df = FALSE ) {

  resp <- req |>
    httr2::req_url_path_append(folder)|>
    httr2::req_method('PROPFIND') |>
    httr2::req_headers(
      Depth = "1",
    )  |>
    httr2::req_perform()
  if(httr2::resp_is_error(resp)) {
    warning(httr2::resp_status_desc(resp))
    invisible(NA)
  }
  else {
    dr <- resp |>
      httr2::resp_body_xml() |>
      xml2::as_xml_document()

    df <- xml2::as_list(dr) |>
      tibble::as_tibble() |>
      tidyr::unnest_wider(.data$`multistatus`, transform=list(href=unlist)) |>
      tidyr::unnest_wider(.data$`propstat`, transform=list(status=unlist)) |>
      tidyr::unnest_wider(.data$`prop`, simplify=TRUE,
                          transform=list(resourcetype=length, getlastmodified=unlist,
                                         getcontentlength=unlist, getcontenttype=unlist,
                                         getetag=unlist, `quota-used-bytes`=unlist)) |>
      dplyr:: mutate(path=gsub(httr2::url_parse(req$url)$path,"",.data$href),file=basename(.data$path))

    if(as_df) {
      if('getcontentlength' %in% names(df)) {
        df <- df |> dplyr::select(.data$file, .data$path, isdir = .data$resourcetype, size=.data$getcontentlength,.data$href)
      }
      else {
        df <- df |> dplyr::select(.data$file, .data$path, isdir = .data$resourcetype, .data$href)
      }
      df[-1,]
    }
    else {
      if(full_names) {
        df$path[-1]
      }
      else {
        df$file[-1]
      }
    }
  }
}

#' Checks if the resource on WebDAV is a folder
#'
#' @param req request handle
#' @param folder path to folder
#'
#' @return TRUE if it is a folder, FALSE else
#' @export
#' @importFrom rlang .data
#'
wd_isdir <- function(req, folder) {

  resp <- req |>
    httr2::req_url_path_append(folder) |>
    httr2::req_method('PROPFIND') |>
    httr2::req_headers(
      Depth = "0",
    )  |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_perform()

  if(httr2::resp_is_error(resp)) {
    warning(httr2::resp_status_desc(resp))
    invisible(FALSE)
  }
  else {
    dr <- resp |>
      httr2::resp_body_xml() |>
      xml2::as_xml_document()

    df <- xml2::as_list(dr) |>
      tibble::as_tibble() |>
      tidyr::unnest_wider(.data$`multistatus`, transform=list(href=unlist)) |>
      tidyr::unnest_wider(.data$`propstat`, transform=list(status=unlist)) |>
      tidyr::unnest_wider(.data$`prop`, simplify=TRUE,
                          transform=list(resourcetype=length)) |>
      dplyr::mutate(isdir = (.data$resourcetype == 1))

    df$isdir[1]
  }


}

#' Send a file or folder to WebDAV
#'
#' @param req request handle
#' @param source local file or folder
#' @param target remote file or foldername
#'
#' @return vector of uploaded files
#' @export
#'
#'
wd_upload <- function(req, source, target="") {
  if(target=="") {
    target = basename(source)
  }
  if(dir.exists(source)) {
    ul <- character(0)
    wd_mkdir(req, target)
    fl <- list.files(source, no.. = TRUE)
    for(f in fl) {
      ul <- c(ul,wd_upload(req, paste0(source,'/',f),paste0(target,'/',f)))
    }
    invisible(ul)
  }
  else {
    resp <- req |>
      httr2::req_method("PUT") |>
      httr2::req_url_path_append(target) |>
      httr2::req_body_file(source) |>
      httr2::req_error(is_error = \(x) FALSE)|>
      httr2::req_perform()
    if(httr2::resp_is_error(resp)) {
      warning(httr2::resp_status_desc(resp))
      invisible(character(0))
    }
    else {
      invisible(target)
    }
  }
}

#' Fetches a file or folder (recursively) from the WebDAV server
#'
#' @param req request handle
#' @param source source file or folder on server
#' @param target local target folder
#'
#' @return vector of downloaded files
#' @export
#'
#'
wd_download <-  function(req, source, target="") {
  if(target=="") {
    target = basename(source)
  }
  if(wd_isdir(req, source)) {
    dl <- character(0)
    fl <- wd_dir(req, source)
    dir.create(target)
    for(f in fl) {
      dl <- c(dl, wd_download(req, paste0(source,'/',f),paste0(target,'/',f)))
    }
    invisible(dl)
  }
  else {
    resp <- req |>
      httr2::req_method("GET") |>
      httr2::req_url_path_append(source) |>
      httr2::req_error(is_error = \(x) FALSE) |>
      httr2::req_perform(target)
    if(httr2::resp_is_error(resp)) {
      warning(httr2::resp_status_desc(resp))
      invisible(character(0))
    }
    else {
      invisible(target)
    }
  }
}







