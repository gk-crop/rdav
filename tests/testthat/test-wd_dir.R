test_that("dir works", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(mock_dir, wd_dir(r, "test")),
    c("a.csv", "file.txt", "tmp")
  )
})

test_that("dir file works", {
  r <- httr2::request("https://cloud.example.com/")
  expect_equal(
    httr2::with_mocked_responses(mock_dir, wd_dir(r, "file.txt")),
    character(0)
  )
})

test_that("dir full path works", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(
      mock_dir, wd_dir(r, "test", full_names = TRUE)
    ),
    c("test/a.csv", "test/file.txt", "test/tmp/")
  )
})

test_that("dir as_df works", {
  r <- httr2::request("https://cloud.example.com")
  df <- httr2::with_mocked_responses(mock_dir, wd_dir(r, "test", as_df = TRUE))
  expect_equal(nrow(df), 3)
  expect_equal(df$contenttype, c("text/csv", "text/plain", NA))
})

test_that("dir file as_df works", {
  r <- httr2::request("https://cloud.example.com")
  df <- httr2::with_mocked_responses(
    mock_dir, wd_dir(r, "file.txt", as_df = TRUE)
  )
  expect_equal(nrow(df), 1)
  expect_equal(df$contenttype, "text/plain")
})

test_that("dir error", {
  r <- httr2::request("https://cloud.example.com/")
  expect_warning(
    httr2::with_mocked_responses(mock_dir, wd_dir(r, "testx")),
    "Not Found"
  )
})

test_that("dir main", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(mock_dir, wd_dir(r)),
    character(0)
  )
})
