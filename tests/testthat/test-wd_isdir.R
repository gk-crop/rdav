mock_isdir <- function(req) {
  if(req$method != "PROPFIND") {
    httr2::response(body=405)
  } else {
    if(req$url == "https://cloud.example.com/test") {
      httr2::response(status_code=207,
                      header = list("Content-Type"="application/xml"),
                      body = '<d:multistatus xmlns:d="DAV:" xmlns:s="http://sabredav.org/ns" xmlns:oc="http://owncloud.org/ns"><d:response><d:href>/test/</d:href><d:propstat><d:prop><d:resourcetype><d:collection/></d:resourcetype></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response></d:multistatus>')
    } else if(req$url == "https://cloud.example.com/file.txt") {
      httr2::response(status_code=207,
                      header = list("Content-Type"="application/xml"),
                      body = '<d:multistatus xmlns:d="DAV:" xmlns:s="http://sabredav.org/ns" xmlns:oc="http://owncloud.org/ns"><d:response><d:href>/file.txt</d:href><d:propstat><d:prop><d:resourcetype/></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response></d:multistatus>')
    } else {
      httr2::response(status_code=404)
    }
  }
}


test_that("is dir works", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(mock_isdir, wd_isdir(r,"test")),
    TRUE
  )
})

test_that("is not dir works", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(mock_isdir, wd_isdir(r,"file.txt")),
    FALSE
  )
})

test_that("dir  warning", {
  r <- httr2::request("https://cloud.example.com")
  expect_warning(
    httr2::with_mocked_responses(mock_isdir, wd_isdir(r,"testx")),
    'Not Found'
  )
})
