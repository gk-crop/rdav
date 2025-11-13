test_that("share_info", {
  r <- httr2::request("https://cloud.example.com")
  df <- httr2::with_mocked_responses(
    mock_share,
    ocs_share_info(r, 10, columns = c("permissions"))
  )
  rows <- nrow(df)
  cols <- ncol(df)
  expect_equal(rows, 11)
  expect_equal(cols, 4)
})

test_that("share_info wrong", {
  r <- httr2::request("https://cloud.example.com")
  expect_warning(
    httr2::with_mocked_responses(
      mock_share,
      ocs_share_info(r, 11, columns = c("permissions"))
    ),
    "Not Found"
  )
})
