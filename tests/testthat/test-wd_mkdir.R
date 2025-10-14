mock_mkdir <- function(req) {
  if (req$method == "PROPFIND") {
    if (req$url == "https://cloud.example.com/new") {
      httr2::response(status_code = 207,
                      headers = list("Content-Type" = "application/xml"),
                      body = charToRaw('<?xml version="1.0"?>
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
</d:multistatus>'))
    }
    else {
      httr2::response(status_code = 207,
                      headers = list("Content-Type" = "application/xml"),
                      body = charToRaw('<?xml version="1.0"?>
<d:multistatus xmlns:d="DAV:"
  xmlns:s="http://sabredav.org/ns"
  xmlns:oc="http://owncloud.org/ns">
  <d:response>
    <d:href>/test/</d:href>
    <d:propstat>
      <d:prop><d:resourcetype></d:resourcetype></d:prop>
      <d:status>HTTP/1.1 200 OK</d:status>
    </d:propstat>
  </d:response>
</d:multistatus>'))

    }

  }
  else if (req$method != "MKCOL") {
    httr2::response(body = 405)
  } else {
    if (req$url == "https://cloud.example.com/new") {
      httr2::response(status_code = 200)
    } else {
      httr2::response(status_code = 409)
    }
  }
}


test_that("mkdir works", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(mock_mkdir, wd_mkdir(r, "new")),
    TRUE
  )
})

test_that("mkdir warning", {
  r <- httr2::request("https://cloud.example.com")
  expect_warning(
    httr2::with_mocked_responses(mock_mkdir, wd_mkdir(r, "old/new")),
    "Conflict"
  )
})

test_that("mkdir return false", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(
      mock_mkdir, suppressWarnings(wd_mkdir(r, "old/new"))
    ),
    FALSE
  )
})
