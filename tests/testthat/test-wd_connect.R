test_that("connection works", {
  resp <- httr2::with_mocked_responses(
    mock_response_connect,
    wd_connect("https://cloud.example.com", "abc", "123") |>
      httr2::req_method("HEAD") |>
      httr2::req_perform()
  )
  expect_equal(httr2::resp_status(resp), 200)
})

test_that("url wrong", {
  expect_error(
    httr2::with_mocked_responses(
      mock_response_connect,
      wd_connect("https://no.example.com", "abc", "123") |>
        httr2::req_method("HEAD") |>
        httr2::req_error(is_error = \(e) FALSE) |>
        httr2::req_perform()
    )
  )
})

test_that("credentials are wrong", {
  expect_error(resp <- httr2::with_mocked_responses(
    mock_response_connect,
    wd_connect("https://cloud.example.com", "abc", "1234") |>
      httr2::req_method("HEAD") |>
      httr2::req_error(is_error = \(e) FALSE) |>
      httr2::req_perform()
  ),
  "Unauthorized"
  )
})
