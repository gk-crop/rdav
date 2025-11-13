test_that("child shares folder", {
  r <- httr2::request("https://cloud.example.com")
  df <- httr2::with_mocked_responses(mock_share,
                                     ocs_child_shares(r, "exchange"))
  rows <- nrow(df)

  expect_equal(
    rows, 11
  )
})

test_that("child shares wrong folder", {
  r <- httr2::request("https://cloud.example.com")
  expect_warning(
    httr2::with_mocked_responses(mock_share, ocs_child_shares(r, "exchanges")),
    "File or folder does not exist: exchanges"
  )
})

test_that("shares folder", {
  r <- httr2::request("https://cloud.example.com")
  df <- httr2::with_mocked_responses(
    mock_share,
    ocs_shares(r, "exchange", columns = c("permissions"))
  )
  rows <- nrow(df)
  cols <- ncol(df)
  expect_equal(rows, 11)
  expect_equal(cols, 4)
})

test_that("shares folder no df", {
  r <- httr2::request("https://cloud.example.com")
  lst <- httr2::with_mocked_responses(
    mock_share,
    ocs_shares(r, "exchange", as_df = FALSE)
  )
  rows <- length(lst)
  expect_equal(rows, 11)
})
