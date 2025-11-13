test_that("find users", {
  r <- httr2::request("https://cloud.example.com")
  df <-     httr2::with_mocked_responses(
    mock_users,
    ocs_find_users(r, "Doe")
  )
  row <- nrow(df)
  expect_equal(row, 2)
})

test_that("find users - list", {
  r <- httr2::request("https://cloud.example.com")
  df <-     httr2::with_mocked_responses(
    mock_users,
    ocs_find_users(r, "Doe", as_df = FALSE)
  )
  row <- length(df)
  expect_equal(row, 2)
})

test_that("find users wrong", {
  r <- httr2::request("https://cloud.example.com")
  df <-     httr2::with_mocked_responses(
    mock_users,
    ocs_find_users(r, "Dee")
  )
  expect_equal(df, NULL)
})
