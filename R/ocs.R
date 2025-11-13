
#' Warning texts
#'
#' @param method API method
#' @param code HTTP error code
#' @param description HTTP error description
#' @param item item
#'
#' @returns warning string
#' @noRd
#'
ocs_warnings <- function(method, code, description = "", item = "") {
  wl <- list(
    "share" = list(
      "404" = paste("Share does not exist:", item)
    ),
    "shares" = list(
      "404" = paste("File or folder does not exist:", item),
      "400" = paste("Is not a directory:", item)
    ),
    "delete_share" = list(
      "404" = paste("Share does not exist:", item)
    ),
    "create_share" = list(
      "400" = paste("Share type is unknown or password is to weak."),
      "403" = paste("Public upload was disabled by the admin"),
      "404" = paste("File / folder could not be shared:", item),
      "500" = paste("Something went wrong. Maybe the password is to weak.")
    )
  )
  code <- as.character(code)

  if (!is.null(wl[[method]]) &&
        !is.null(wl[[method]][[code]]) &&
        wl[[method]][[code]] != "") {
    warning(wl[[method]][[code]], call. = FALSE)
  } else {
    if (description != "") {
      warning(description, call. = FALSE)
    }
  }
}


#' Creates API url from Nextcloud base url
#'
#' @param url Nextcloud base url
#' @param api one of "share" (default), "sharee", "status"
#'
#' @returns url to the api endpoint
#' @noRd
#'
#' @examples
#' ocs_api_url("https://example.com/sub/dir/remote.php/dav/files/user/","share")

ocs_api_url <- function(url, api = "share") {

  apis <- c(
    share = "ocs/v2.php/apps/files_sharing/api/v1",
    sharee = "ocs/v1.php/apps/files_sharing/api/v1",
    status = "ocs/v2.php/apps/user_status/api/v1/user_status",
    recommendations = "ocs/v2.php/apps/recommendations/api/v1/",
    userpreferences = "ocs/v2.php/apps/provisioning_api/api/v1/config/users/"
  )

  surl <- httr2::url_parse(url)
  spl <- strsplit(surl$path, "/dav/files/")[[1]]
  api_url <- ""
  if (length(spl) > 0) {
    path <- gsub("public.php",
                 "",
                 gsub("remote.php", "", spl[1], fixed = TRUE),
                 fixed = TRUE)
    nurl <- paste0(surl$scheme, "://", surl$hostname, path)
    api_url <- httr2::url_parse(nurl) |>
      httr2::url_modify_relative(apis[api])
  }
  httr2::url_build(api_url)
}


#' Creates an API request from a WebDAV request
#'
#' @param req WebDAV request as returned by \code{\link{wd_connect}}
#' @param api one of "share" (default), "sharee", "status"
#'
#' @returns API request to use with the ocs_... functions
#' @noRd
#' @examples
#' \dontrun{
#' r <- wd_connect("https://example.com/remote.php/dav/files/johndoe")
#' ocs_r <- ocs_request(r)
#' }

ocs_request <- function(req, api = "share") {
  url <- ocs_api_url(req$url, api)
  req |>
    httr2::req_url(url) |>
    httr2::req_headers("OCS-APIRequest" = "true")
}


#' Parses XML returnd by the share API to dataframe
#'
#' @param resp response from an API request
#' @param as_df if TRUE (default) a data.frame is returned, else a list of IDs
#' @param columns column names that should be included into the result
#' (NULL means all available columns)
#' @param tag XML-Tag for entries as XSLT path
#'
#' @returns data.frame or named list of IDs
#' @noRd
#'
parse_share_xml <- function(resp,
                            as_df = TRUE,
                            columns = NULL,
                            tag = "//data/element") {
  sr <- resp |>
    httr2::resp_body_xml() |>
    xml2::as_xml_document() |>
    xml2::xml_find_all(tag)

  if (length(sr) > 0) {
    cols <- xml2::xml_children(sr[[1]]) |> xml2::xml_name()
    cols <- setdiff(cols, c("id", "path"))
    if (!is.null(columns)) {
      cols <- intersect(columns, cols)
    }
    if (as_df) {
      id <- sapply(sr,
                   \(x) xml2::xml_find_first(x, "id") |> xml2::xml_text())
      path <- sapply(sr,
                     \(x) xml2::xml_find_first(x, "path") |> xml2::xml_text())
      data <- data.frame(id, path)
      for (n in cols) {
        data[[n]] <-
          sapply(sr, \(x) xml2::xml_find_first(x, n) |> xml2::xml_text())
        if (n == "permissions") {
          data[["permissions_chr"]] <- permissions_to_chr(data[[n]])
        }
      }
      data
    } else {
      id <- sapply(sr, \(x) xml2::xml_find_first(x, "id") |> xml2::xml_text())
      path <- sapply(sr,
                     \(x) xml2::xml_find_first(x, "path") |> xml2::xml_text())
      names(id) <- path
      id
    }
  }
}

#' Parses XML returnd by the sharee API to dataframe
#'
#' @param resp response from an API request
#' @param as_df if TRUE (default) a data.frame is returned, else a list of IDs
#' @param tag XML-Tag for entries as XSLT path
#'
#' @returns data.frame or named list of IDs
#' @noRd
#'
parse_user_xml <- function(resp, as_df = TRUE, tag = "//data/element") {
  sr <- xml2::xml_find_all(xml2::as_xml_document(httr2::resp_body_xml(resp)),
                           tag)
  if (length(sr) > 0) {
    cols <- xml2::xml_name(xml2::xml_children(sr[[1]]))
    if (as_df) {
      data <- data.frame(
        uuid = sapply(
          sr,
          function(x) xml2::xml_text(xml2::xml_find_first(x,  "uuid"))
        )
      )
      for (n in cols) {
        if (n == "value") {
          vs <- xml2::xml_find_all(sr, "value")
          vcols <- xml2::xml_name(xml2::xml_children(vs))
          for (vn in vcols) {
            data[[vn]] <- xml2::xml_text(xml2::xml_find_first(vs,  vn))
          }
        } else {
          data[[n]] <- sapply(
            sr, function(x) xml2::xml_text(xml2::xml_find_first(x,  n))
          )
        }
      }
      data
    } else {
      id <- sapply(
        sr,
        function(x) xml2::xml_text(xml2::xml_find_first(x, "value/shareWith"))
      )
      name <- sapply(
        sr,
        function(x) xml2::xml_text(xml2::xml_find_first(x, "name"))
      )
      names(id) <- name
      id
    }
  }
}

#' Packs integer or character vector of integers to a share permission
#'
#' @param permissions vector of integers or characters ("read", "update",
#' "create", "delete", "share")
#' @returns integer between 1 and 31, representing share permissions
#' @noRd
#'
permissions_to_int <- function(permissions) {
  pid <- c(r = 1, u = 2, c = 4, d = 8, s = 16, w = 2)
  if (is.numeric(permissions)) {
    min(max(0, sum(unique(permissions))), 31)
  } else if (is.character(permissions)) {
    cid <- substr(trimws(unlist(strsplit(permissions, ","))), 1, 1)
    sum(unique(pid[cid]))
  } else {
    1
  }
}

#' Converts share permission number to readable format
#'
#' @param permissions integer representing the share permissions
#'
#' @returns character vector containing permissions ("read", "update",
#' "create", "delete", "share")
#' @noRd
#'
permissions_to_chr <- function(permissions) {
  pc <- c("read", "update", "create", "delete", "share")
  sapply(permissions,
         \(p) paste0(pc[as.logical(intToBits(p))], collapse = ","))
}


#' Returns information for shares
#'
#' \code{ocs_shares_extended} returns extended information for shares.
#' \code{ocs_shares} returns the shares of a file or folder,
#' \code{ocs_child_shares} the shares of the files
#' and subfolders of the given path.
#'
#' @param req WebDAV request as returned by \code{\link{wd_connect}}
#' @param path folder or file path
#' @param as_df if TRUE (default) a data.frame is returned, else a list of IDs
#' @param columns column names that should be included into the result
#' (default `NULL` includes all)
#'
#' @param subfiles list shares of subfolders
#' @param reshares include shares from others
#'
#' @returns data.frame or named vector of IDs
#' @export
#' @md
#' @examples
#' \dontrun{
#' r <- wd_connect("https://example.com/remote.php/dav/files/johndoe")
#' ocs_shares_extended(r, "myfolder/shares")
#' ocs_shares(r, "myfolder/shares")
#' ocs_child_shares(r, "myfolder")
#' }
ocs_shares_extended <- function(req, path = "",
                                as_df = TRUE,
                                columns = NULL,
                                subfiles = TRUE,
                                reshares = FALSE) {

  reshares <- ifelse(reshares, "true", "false")
  subfiles <- ifelse(subfiles &&
                       path != "" &&
                       wd_isdir(req, path, silent = TRUE), "true", "false")

  r <- ocs_request(req, api = "share") |>
    httr2::req_method("GET") |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_url_path_append("/shares")
  if (path != "") {
    r <- r |>
      httr2::req_url_query(path = path, reshares = reshares,
                           subfiles = subfiles)
  }

  resp <- httr2::req_perform(r)

  if (httr2::resp_is_error(resp)) {
    ocs_warnings("shares", httr2::resp_status(resp),
                 httr2::resp_status_desc(resp), path)
    invisible(NULL)
  } else {
    parse_share_xml(resp, as_df, columns)
  }
}

#' @rdname ocs_shares_extended
#' @export
#'
ocs_child_shares <- function(req, path = "", as_df = TRUE,
                             columns = c("share_type", "item_type",
                                         "permissions",
                                         "label", "uid_owner",
                                         "share_with_displayname")) {
  ocs_shares_extended(req, path, as_df = as_df,
                      columns = columns, subfiles = TRUE)
}

#' @rdname ocs_shares_extended
#' @export
#'
ocs_shares <- function(req, path = "/", as_df = TRUE,
                       columns = c("share_type", "item_type", "permissions",
                                   "label", "uid_owner",
                                   "share_with_displayname")) {
  ocs_shares_extended(req, path, as_df = as_df,
                      columns = columns, subfiles = FALSE)
}

#' Get infor for a specific share
#'
#' @inheritParams ocs_shares_extended
#' @param id share id
#' @returns one row data.frame with share properties
#' @export
#' @examples
#' \dontrun{
#' r <- wd_connect("https://example.com/remote.php/dav/files/johndoe")
#' ocs_share_info(r, 158742)
#' }
ocs_share_info <- function(req, id, columns = NULL) {

  resp <- ocs_request(req, api = "share") |>
    httr2::req_method("GET") |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_url_path_append(paste0("/shares/", id)) |>
    httr2::req_perform()

  if (httr2::resp_is_error(resp)) {
    warning(httr2::resp_status_desc(resp))
    invisible(NULL)
  } else {
    parse_share_xml(resp, as_df = TRUE, columns = columns)
  }
}

#' Deletes a share
#'
#' @inheritParams ocs_share_info
#' @returns invisible boolean TRUE on success and FALSE on failure
#' @export
#' @examples
#' \dontrun{
#' r <- wd_connect("https://example.com/remote.php/dav/files/johndoe")
#' ocs_delete_share(r, 12342)
#' }
#'

ocs_delete_share <- function(req, id) {
  resp <- ocs_request(req, api = "share") |>
    httr2::req_method("DELETE") |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_url_path_append(paste0("/shares/", id)) |>
    httr2::req_perform()

  if (httr2::resp_is_error(resp)) {
    ocs_warnings("delete_share",
                 httr2::resp_status(resp), httr2::resp_status_desc(resp), id)
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}

#' Notifies the user of a mail share
#'
#' @inheritParams ocs_share_info
#' @param password password of the share if it is password protected
#' @returns invisible TRUE on success or FALSE on failure
#' @examples
#' \dontrun{
#' r <- wd_connect("https://example.com/remote.php/dav/files/johndoe")
#'
#' # add a password to a mail share and notify the user
#' ocs_modify_share(r, 12342, password = "super_secret")
#' ocs_send_mail(r, 12342, password = "super_secret")
#' }
ocs_send_mail <- function(req, id, password = NULL) {
  r <- ocs_request(req, api = "share") |>
    httr2::req_method("POST") |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_url_path_append(paste0("/shares/", id, "/send-email"))
  pw <- ifelse(is.null(password), "", password)
  if (nchar(pw) > 0) {
    r <- httr2::req_body_form(r, password = pw)
  }

  resp <- r |> httr2::req_perform()
  if (httr2::resp_is_error(resp)) {
    warning(httr2::resp_status_desc(resp))
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}


#' Creates a share
#'
#' Creates different share types:
#'
#' - `ocs_create_share` - generic method that takes share type as argument
#' - `ocs_create_share_user` - creates a share for a nextcloud user
#' - `ocs_create_share_group` - creates a share for a nextcloud group
#' - `ocs_create_share_link` - create a public share link
#' - `ocs_create_share_mail` - creates an e-mail share
#' - `ocs_create_share_federated` - creates a federated share
#'
#' Notice: if protecting a public link or e-mail share with a password, make
#' sure that the password meets the services' password policy.
#'
#' Share permissions can be given as vector of integers or characters,
#' e.g. `c(1, 2, 8)` or `c("read", "update", "delete")`, or as their sum or
#' concatenation, e.g. `11` or `"read,update,delete"`.
#'
#' @inheritParams ocs_shares_extended
#' @param share_type integer 0:user, 1:group, 3:link, 4:e-mail, 6:federated
#' @param share_with depending on share type: user id, group id,
#' e-mail address or federated cloud id (only for `ocs_create_share`)
#' @param password optional password for link and e-mail shares
#' @param permissions integer or char vector
#' (1:read, 2:update, 4:create, 8:delete, 16:share), default is 1:read
#' @param public_upload TRUE if public upload should be enabled
#' @param expire_date expiration date as string in the format YYYY-MM-DD
#' @param label label for the share
#' @param note note for the share
#' @param send_mail if TRUE the user is notified via e-mail
#' @param attributes optional attributes
#'
#' @returns information for the newly created share as data.frame
#' @md
#' @export
#' @examples
#' \dontrun{
#' r <- wd_connect("https://example.com/remote.php/dav/files/johndoe")
#' ocs_create_share(r, "myfolder/share", 4)
#' ocs_create_share_link(r, "myfolder/share")
#' ocs_create_share_mail(r, "myfolder/share", "jack@example.com")
#' ocs_create_share_user(r, "myfolder/share", "jackdoe", c("read", "update"))
#'
#' }
#'
ocs_create_share <- function(req, path, share_type,
                             share_with = NULL,
                             password = NULL,
                             permissions = 1,
                             public_upload = FALSE,
                             expire_date = NULL,
                             label = "",
                             note = "",
                             send_mail = FALSE,
                             attributes = NULL) {

  if (share_type < 2 && (is.null(share_with) || share_with == "")) {
    stop("For user or group shares the parameter 'shareWith' is required")
  }

  if (share_type %in% c(3, 4) && !is.null(password) && nchar(password) < 10) {
    stop("Your password is too short.")
  }


  sw <- ifelse(is.null(share_with), "", share_with)
  pw <- ifelse(is.null(password), "", password)
  pu <- ifelse(public_upload, "true", "false")
  sm <- ifelse(send_mail, "true", "false")
  at <- ifelse(is.null(attributes), "", attributes)
  ed <- ""
  if (!is.null(expire_date)) {
    ed <- format(as.Date(expire_date, format = "%Y-%m-%d"), format = "%Y-%m-%d")
    ed <- ifelse(is.na(ed), "", ed)
  }

  r <- ocs_request(req, api = "share") |>
    httr2::req_method("POST") |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_url_path_append("/shares")  |>
    httr2::req_body_form(path = path, shareType = share_type,
                         shareWith = sw,
                         permissions = permissions_to_int(permissions),
                         publicUpload = pu, expireDate = ed, label = label,
                         note = note, sendMail = sm)

  if (share_type %in% c(3, 4) && nchar(pw) > 9) {
    r <- r |>
      httr2::req_body_form(password = pw)
  }

  if (nchar(at) > 1) {
    r <- r |>
      httr2::req_body_form(attributes = attributes)
  }

  resp <- r |>
    httr2::req_perform()

  if (httr2::resp_is_error(resp)) {
    ocs_warnings("create_share",
                 httr2::resp_status(resp), httr2::resp_status_desc(resp), path)
    invisible(NULL)
  } else {
    df <- parse_share_xml(resp, as_df = TRUE, tag = "//data")
    df
  }

}

#' @rdname ocs_create_share
#' @export
ocs_create_share_link <- function(req, path, password = NULL,
  permissions = 1, public_upload = FALSE,
  expire_date = NULL, note = "", label = ""
) {
  ocs_create_share(req, path, 3,
    password = password, permissions = permissions,
    public_upload = public_upload, expire_date = expire_date,
    note = note, label = label
  )
}

#' @rdname ocs_create_share
#' @param email e-mail address  (only for \code{ocs_create_share_mail})
#' @export
ocs_create_share_mail <- function(req, path, email, password = NULL,
                                  permissions = 1, public_upload = FALSE,
                                  expire_date = NULL,
                                  note = "", label = "", send_mail = TRUE) {



  ocs_create_share(req, path, 4, share_with = email, password = password,
                   permissions = permissions, public_upload = public_upload,
                   expire_date = expire_date, note = note, label = label,
                   send_mail = send_mail)
}

#' @rdname ocs_create_share
#' @param user id of the user (only for \code{ocs_create_share_user})
#' @export
ocs_create_share_user <- function(req, path, user,
                                  permissions = 1, public_upload = FALSE,
                                  expire_date = NULL, note = "", label = "",
                                  send_mail = TRUE) {
  ocs_create_share(req, path, 0, share_with = user, password = NULL,
                   permissions = permissions, public_upload = public_upload,
                   expire_date = expire_date, note = note, label = label,
                   send_mail = send_mail)
}

#' @rdname ocs_create_share
#' @param group id of the group (only for \code{ocs_create_share_group})
#' @export
ocs_create_share_group <- function(req, path, group, permissions = 1,
                                   public_upload = FALSE, expire_date = NULL,
                                   note = "", label = "", send_mail = TRUE) {
  ocs_create_share(req, path, 1, share_with = group, password = NULL,
                   permissions = permissions,
                   public_upload = public_upload, expire_date = expire_date,
                   note = note, label = label,
                   send_mail = send_mail)
}


#' @rdname ocs_create_share
#' @param cloud_id cloud id (only for \code{ocs_create_share_federated})
#' @export
ocs_create_share_federated <- function(req, path, cloud_id,
                                       permissions = 1, public_upload = FALSE,
                                       expire_date = NULL,
                                       note = "", label = "",
                                       send_mail = TRUE) {
  ocs_create_share(req, path, 6, share_with = cloud_id, password = NULL,
                   permissions = permissions,
                   public_upload = public_upload, expire_date = expire_date,
                   note = note, label = label,
                   send_mail = send_mail)
}


#' Modifies properties of a share
#'
#' If a parameter omitted or is `NULL`, then the coresponding property is not
#' modified.
#'
#' Share permissions can be given as vector of integers or characters,
#' e.g. `c(1, 2, 8)` or `c("read", "update", "delete")`, or as their sum or
#' concatenation, e.g. `11` or `"read,update,delete"`.
#'
#' @inheritParams ocs_create_share
#' @param id share id
#' @returns data.frame with the share properties
#' @export
#' @md
#' @examples
#' \dontrun{
#' r <- wd_connect("https://example.com/remote.php/dav/files/johndoe")
#' ocs_modify_share(r, 12345, permissions = 31, expire_date = "2025-11-01")
#' }
#'
ocs_modify_share <- function(req, id,
                             password = NULL,
                             permissions = NULL,
                             public_upload = NULL,
                             expire_date = NULL,
                             label = NULL,
                             note = NULL,
                             send_mail = NULL,
                             attributes = NULL) {


  if (!is.null(password) && nchar(password) < 10) {
    stop("Your password is too short.")
  }

  pw <- ifelse(is.null(password), "", password)
  pu <- ifelse(public_upload, "true", "false")
  sm <- ifelse(send_mail, "true", "false")
  at <- ifelse(is.null(attributes), "", attributes)
  ed <- ""
  if (!is.null(expire_date)) {
    ed <- format(as.Date(expire_date, format = "%Y-%m-%d"), format = "%Y-%m-%d")
    ed <- ifelse(is.na(ed), "", ed)
  }

  r <- ocs_request(req, api = "share") |>
    httr2::req_method("PUT") |>
    httr2::req_error(is_error = \(x) FALSE) |>
    httr2::req_url_path_append(paste0("/shares/", id))

  if (!is.null(password)) {
    resp <- r |>
      httr2::req_body_form(password = pw) |>
      httr2::req_perform()

    if (httr2::resp_is_error(resp)) {
      warning("Could not change password")
    }
  }

  if (!is.null(permissions)) {
    resp <- r |>
      httr2::req_body_form(permissions = permissions_to_int(permissions)) |>
      httr2::req_perform()

    if (httr2::resp_is_error(resp)) {
      warning("Could not change permissions")
    }
  }

  if (!is.null(expire_date)) {
    resp <- r |>
      httr2::req_body_form(expireDate = ed) |>
      httr2::req_perform()

    if (httr2::resp_is_error(resp)) {
      warning("Could not change expireDate")
    }
  }

  if (!is.null(public_upload)) {
    resp <- r |>
      httr2::req_body_form(publicUpload = pu) |>
      httr2::req_perform()

    if (httr2::resp_is_error(resp)) {
      warning("Could not change publicUpload")
    }
  }

  if (!is.null(label)) {
    resp <- r |>
      httr2::req_body_form(label = label) |>
      httr2::req_perform()

    if (httr2::resp_is_error(resp)) {
      warning("Could not change label")
    }
  }

  if (!is.null(note)) {
    resp <- r |>
      httr2::req_body_form(note = note) |>
      httr2::req_perform()

    if (httr2::resp_is_error(resp)) {
      warning("Could not change note")
    }
  }

  if (!is.null(send_mail)) {
    resp <- r |>
      httr2::req_body_form(sendMail = sm) |>
      httr2::req_perform()

    if (httr2::resp_is_error(resp)) {
      warning("Could not change sendMail")
    }
  }

  if (!is.null(attributes)) {
    resp <- r |>
      httr2::req_body_form(attributes = at) |>
      httr2::req_perform()
    if (httr2::resp_is_error(resp)) {
      warning("Could not change attributes")
    }
  }

  ocs_share_info(req, id, NULL)
}


#' Search for users one could share data
#'
#' @param req WebDAV request as returned by \code{\link{wd_connect}}
#' @param search search string (e.g. user name, e-mail, group name)
#' @param lookup if TRUE (and supported) search on Nextcloud lookup server
#' @param as_df if TRUE (default) a data.frame is returned, else a list of IDs
#'
#' @returns data.frame with user informations or named vector of user ids.
#' @export
#'
#' @examples
#' \dontrun{
#' r <- wd_connect("https://example.com/remote.php/dav/files/johndoe")
#' ocs_find_users(r, "Doe, Jack")
#' }
ocs_find_users <- function(req, search, lookup = FALSE, as_df = TRUE) {
  lu <- ifelse(lookup, "true", "false")
  ocs_request(req, "sharee") |>
    httr2::req_method("GET") |>
    httr2::req_url_path_append("sharees") |>
    httr2::req_url_query(search = search, itemType = "file", lookup = lu) |>
    httr2::req_perform() |>
    parse_user_xml(tag = "//data//element", as_df = as_df)
}
