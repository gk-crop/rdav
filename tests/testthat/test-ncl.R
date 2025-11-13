test_that("nextcloud base url", {
  url <- ncl_baseurl("example.com", "johndoe")
  expect_equal(url, "https://example.com/remote.php/dav/files/johndoe")
})

test_that("nextcloud base url - path prefix", {
  url <- ncl_baseurl("example.com", "johndoe", "sub/dir/")
  expect_equal(url, "https://example.com/sub/dir/remote.php/dav/files/johndoe")
})

test_that("nextcloud base url - path prefix, no trailing slash", {
  url <- ncl_baseurl("example.com", "johndoe", "sub/dir")
  expect_equal(url, "https://example.com/sub/dir/remote.php/dav/files/johndoe")
})

test_that("nextcloud base url - spaces, umlauts", {
  url <- ncl_baseurl("example.com", "john@dö", "sub dir")
  expect_equal(
    url,
    "https://example.com/sub%20dir/remote.php/dav/files/john%40d%C3%B6"
  )
})

test_that("nextcloud share url", {
  url <- ncl_shareurl("example.com", "johndoe")
  expect_equal(url, "https://example.com/public.php/dav/files/johndoe")
})

test_that("nextcloud share url - path prefix", {
  url <- ncl_shareurl("example.com", "johndoe", "sub/dir/")
  expect_equal(url, "https://example.com/sub/dir/public.php/dav/files/johndoe")
})

test_that("nextcloud share url from public link", {
  url <- ncl_shareurl_from_publicurl("https://example.com/s/johndoe")
  expect_equal(url, "https://example.com/public.php/dav/files/johndoe")
})

test_that("nextcloud share url from public link - path prefix", {
  url <- ncl_shareurl_from_publicurl("https://example.com/sub/dir/s/johndoe")
  expect_equal(url, "https://example.com/sub/dir/public.php/dav/files/johndoe")
})

test_that("username from public link", {
  user <-
    ncl_username_from_url("https://example.com/public.php/dav/files/johndoe")
  expect_equal(user, "johndoe")
})

test_that("username from public link - path prefix", {
  user <-
    ncl_username_from_url("https://example.com/sd/public.php/dav/files/johndoe")
  expect_equal(user, "johndoe")
})

test_that("username from public link - path prefix, subfolder, ", {
  user <-
    ncl_username_from_url("https://example.com/sd/public.php/dav/files/jd/sub/")
  expect_equal(user, "jd")
})
