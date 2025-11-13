test_that("is dir works", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(mock_isdir, wd_isdir(r, "test")),
    TRUE
  )
})

test_that("is not dir works", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(mock_isdir, wd_isdir(r, "file.txt")),
    FALSE
  )
})

test_that("dir  warning", {
  r <- httr2::request("https://cloud.example.com")
  expect_warning(
    httr2::with_mocked_responses(mock_isdir, wd_isdir(r, "testx")),
    "Not Found"
  )
})
