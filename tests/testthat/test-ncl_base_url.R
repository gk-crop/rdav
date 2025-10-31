test_that("nextcloud base url", {
  url <- ncl_baseurl("example.com", "johndoe", "sub/dir/")
  expect_equal(url, "https://example.com/sub/dir/remote.php/dav/files/johndoe")
})

test_that("nextcloud share url", {
  url <- ncl_shareurl("example.com", "johndoe", "sub/dir/")
  expect_equal(url, "https://example.com/sub/dir/public.php/dav/files/johndoe")
})

test_that("nextcloud share url from public linkd", {
  url <- ncl_shareurl_from_publicurl("https://example.com/s/johndoe")
  expect_equal(url, "https://example.com/public.php/dav/files/johndoe")
})

test_that("nextcloud share url from public linkd", {
  url <- ncl_shareurl_from_publicurl("https://example.com/s/johndoe")
  expect_equal(url, "https://example.com/public.php/dav/files/johndoe")
})

test_that("username from public linkd", {
  user <-
    ncl_username_from_url("https://example.com/public.php/dav/files/johndoe")
  expect_equal(user, "johndoe")
})
