test_that("sendmail", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(
      mock_share,
      ocs_send_mail(r, 10)
    ),
    TRUE
  )
})

test_that("sendmail password", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(
      mock_share,
      ocs_send_mail(r, 10, password = "abc")
    ),
    TRUE
  )
})

test_that("sendmail wrong", {
  r <- httr2::request("https://cloud.example.com")
  expect_warning(
    httr2::with_mocked_responses(
      mock_share,
      ocs_send_mail(r, 11)
    ),
    "Not Found"
  )
})
