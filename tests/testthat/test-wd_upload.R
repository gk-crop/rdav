mock_upload <- function(req) {
  if (req$method == "MKCOL") {
    httr2::response(body = 200)
  } else if (req$method == "PUT") {
    if (req$url == "https://cloud.example.com/missing") {
      httr2::response(status_code = 404)
    } else {
      httr2::response(status_code = 200)
    }
  } else if (req$method == "PROPFIND") {
    if (req$url == "https://cloud.example.com/test") {
      httr2::response(status_code = 207,
                      header = list("Content-Type" = "application/xml"),
                      body = '<?xml version="1.0"?>
<d:multistatus xmlns:d="DAV:" 
  xmlns:s="http://sabredav.org/ns" 
  xmlns:oc="http://owncloud.org/ns">
  <d:response>
    <d:href>/test/</d:href>
    <d:propstat>
      <d:prop><d:resourcetype><d:collection/></d:resourcetype></d:prop>
      <d:status>HTTP/1.1 200 OK</d:status>
    </d:propstat>
  </d:response>
</d:multistatus>')
    } else if (req$url == "https://cloud.example.com/file.txt") {
      httr2::response(status_code = 207,
                      header = list("Content-Type" = "application/xml"),
                      body = '<?xml version="1.0"?>
<d:multistatus xmlns:d="DAV:" 
  xmlns:s="http://sabredav.org/ns" 
  xmlns:oc="http://owncloud.org/ns">
  <d:response>
    <d:href>/file.txt</d:href>
    <d:propstat>
      <d:prop><d:resourcetype/></d:prop>
      <d:status>HTTP/1.1 200 OK</d:status>
    </d:propstat>
  </d:response>
</d:multistatus>')
    } else {
      httr2::response(status_code = 404)
    }
  }else {
    httr2::response(status_code = 405)
  }
}

test_that("upload works", {
  r <- httr2::request("https://cloud.example.com/")
  local <- paste0(tempdir(), "/ul")
  dir.create(local)
  dir.create(paste0(local, "/test"))
  file.create(paste0(local, "/test/abc.txt"))
  expect_equal(
    httr2::with_mocked_responses(mock_upload, wd_upload(r, local, "dir")),
    c("dir/test/abc.txt")
  )
  unlink(local, recursive = TRUE)
})

test_that("upload file works", {
  r <- httr2::request("https://cloud.example.com/")
  local <- paste0(tempdir(), "/ul")
  dir.create(local)
  file.create(paste0(local, "/file.txt"))
  expect_equal(
    httr2::with_mocked_responses(
      mock_upload, wd_upload(r, paste0(local, "/file.txt"), "test")
    ),
    c("test/file.txt")
  )
  unlink(local, recursive = TRUE)
})

test_that("upload without target works", {
  r <- httr2::request("https://cloud.example.com/")
  local <- paste0(tempdir(), "/ul")
  dir.create(local)
  dir.create(paste0(local, "/test"))
  file.create(paste0(local, "/test/abc.txt"))
  expect_equal(
    httr2::with_mocked_responses(mock_upload, wd_upload(r, local)),
    c("ul/test/abc.txt")
  )
  unlink(local, recursive = TRUE)
})


test_that("upload missing warning works", {
  r <- httr2::request("https://cloud.example.com/")
  local <- paste0(tempdir(), "/ul")
  dir.create(local)
  file.create(paste0(local, "/file.txt"))
  expect_warning(
    httr2::with_mocked_responses(
      mock_upload, wd_upload(r, paste0(local, "/file.txt"), "missing")
    ),
    "Not Found"
  )
  unlink(local, recursive = TRUE)
})

test_that("upload missing works", {
  r <- httr2::request("https://cloud.example.com/")
  local <- paste0(tempdir(), "/ul")
  dir.create(local)
  file.create(paste0(local, "/file.txt"))
  expect_equal(
    httr2::with_mocked_responses(
      mock_upload,
      suppressWarnings(
        wd_upload(r, paste0(local, "/file.txt"), "missing")
      )
    ),
    character(0)
  )
  unlink(local, recursive = TRUE)
})
