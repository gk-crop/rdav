mock_dir <- function(req) {
  
  if(req$method != "PROPFIND") {
    httr2::response(body=405)
  } else {
    if(req$url == "https://cloud.example.com/test") {
      httr2::response(status_code=207,
                      header = list("Content-Type"="application/xml"),
                      body = '<?xml version="1.0"?>
<d:multistatus xmlns:d="DAV:" xmlns:s="http://sabredav.org/ns" xmlns:oc="http://owncloud.org/ns"><d:response><d:href>/test/</d:href><d:propstat><d:prop><d:getlastmodified>Wed, 28 Feb 2024 21:30:45 GMT</d:getlastmodified><d:resourcetype><d:collection/></d:resourcetype><d:quota-used-bytes>41</d:quota-used-bytes><d:quota-available-bytes>527309756657</d:quota-available-bytes><d:getetag>&quot;65dfa6056af07&quot;</d:getetag></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response><d:response><d:href>/test/a.csv</d:href><d:propstat><d:prop><d:getlastmodified>Wed, 28 Feb 2024 21:30:45 GMT</d:getlastmodified><d:getcontentlength>26</d:getcontentlength><d:resourcetype/><d:getetag>&quot;433f89eafeada7a93501f5b2d0c05651&quot;</d:getetag><d:getcontenttype>text/csv</d:getcontenttype></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response><d:response><d:href>/test/file.txt</d:href><d:propstat><d:prop><d:getlastmodified>Wed, 28 Feb 2024 21:28:32 GMT</d:getlastmodified><d:getcontentlength>15</d:getcontentlength><d:resourcetype/><d:getetag>&quot;ff938d81afc36821bb417c98a8e4f2eb&quot;</d:getetag><d:getcontenttype>text/plain</d:getcontenttype></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response><d:response><d:href>/test/tmp/</d:href><d:propstat><d:prop><d:getlastmodified>Wed, 28 Feb 2024 21:29:42 GMT</d:getlastmodified><d:resourcetype><d:collection/></d:resourcetype><d:quota-used-bytes>0</d:quota-used-bytes><d:quota-available-bytes>527309756657</d:quota-available-bytes><d:getetag>&quot;65dfa5c6d4cbe&quot;</d:getetag></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response></d:multistatus>')
    } else if(req$url == "https://cloud.example.com/file.txt") {
      httr2::response(status_code=207,
                      header = list("Content-Type"="application/xml"),
                      body = '<?xml version="1.0"?>
<d:multistatus xmlns:d="DAV:" xmlns:s="http://sabredav.org/ns" xmlns:oc="http://owncloud.org/ns"><d:response><d:href>/file.txt</d:href><d:propstat><d:prop><d:getlastmodified>Wed, 28 Feb 2024 21:17:51 GMT</d:getlastmodified><d:getcontentlength>15</d:getcontentlength><d:resourcetype/><d:getetag>&quot;763a11253670561a3f03defbbb2f5921&quot;</d:getetag><d:getcontenttype>text/plain</d:getcontenttype></d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat></d:response></d:multistatus>')
    } else {
      httr2::response(status_code=404)
    }
  }
}


test_that("dir works", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(mock_dir, wd_dir(r,"test")),
    c("a.csv","file.txt","tmp")
  )
})

test_that("dir file works", {
  r <- httr2::request("https://cloud.example.com/")
  expect_equal(
    httr2::with_mocked_responses(mock_dir, wd_dir(r,"file.txt")),
    character(0)
  )
})

test_that("dir full path works", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(mock_dir, wd_dir(r,"test", full_names = TRUE)),
    c("test/a.csv","test/file.txt","test/tmp/")
  )
})

test_that("dir as_df works", {
  r <- httr2::request("https://cloud.example.com")
  df <- httr2::with_mocked_responses(mock_dir, wd_dir(r,"test", as_df = TRUE))
  
  expect_equal(nrow(df), 3)
  expect_equal(df$contenttype, c("text/csv", "text/plain", NA))
})

test_that("dir file as_df works", {
  r <- httr2::request("https://cloud.example.com")
  df <- httr2::with_mocked_responses(mock_dir, wd_dir(r,"file.txt", as_df = TRUE))
  
  expect_equal(nrow(df), 1)
  expect_equal(df$contenttype, "text/plain")
})

test_that("dir error", {
  r <- httr2::request("https://cloud.example.com/")
  expect_error(
    httr2::with_mocked_responses(mock_dir, wd_dir(r,"testx"))
  )
})
