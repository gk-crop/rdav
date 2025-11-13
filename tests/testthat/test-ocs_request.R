test_that("api request", {
  r <- httr2::request("https://example.com/sub/d/remote.php/dav/files/johndoe")
  ar <- ocs_request(r, "share")

  expect_equal(ar$url,
               "https://example.com/sub/d/ocs/v2.php/apps/files_sharing/api/v1")
  expect_equal(ar$headers$`OCS-APIRequest`, "true")
})

test_that("warning", {
  expect_warning(
    ocs_warnings("none", 11, "no", "item"),
    "no"
  )
})
