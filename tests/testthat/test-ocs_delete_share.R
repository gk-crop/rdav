test_that("delete", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(
      mock_share,
      ocs_delete_share(r, 10)
    ),
    TRUE
  )
})

test_that("delete wrong", {
  r <- httr2::request("https://cloud.example.com")
  expect_warning(
    httr2::with_mocked_responses(
      mock_share,
      ocs_delete_share(r, 11)
    ),
    "Share does not exist: 11"
  )
})
