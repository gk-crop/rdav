# mock response
webdav_mock_response_connect <- function(req) {
  if (req$method != "HEAD") {
    httr2::response(body = 405)
  } else {
    if (req$url != "https://cloud.example.com") {
      httr2::response(status_code = 404)
    } else {
      if (req$headers$Authorization != "Basic YWJjOjEyMw==") {
        httr2::response(status_code = 401)
      } else {
        httr2::response(status_code = 200)
      }
    }
  }
}


test_that("connection works", {
  resp <- httr2::with_mocked_responses(
    webdav_mock_response_connect,
    wd_connect("https://cloud.example.com", "abc", "123") |>
      httr2::req_method("HEAD") |>
      httr2::req_perform()
  )
  expect_equal(httr2::resp_status(resp), 200)
})

test_that("url is changed", {
  expect_error(
    httr2::with_mocked_responses(
      webdav_mock_response_connect,
      wd_connect("https://no.example.com", "abc", "123") |>
        httr2::req_method("HEAD") |>
        httr2::req_error(is_error = \(e) FALSE) |>
        httr2::req_perform()
    )
  )
})

test_that("credential are changed", {
  expect_error(resp <- httr2::with_mocked_responses(
    webdav_mock_response_connect,
    wd_connect("https://cloud.example.com", "abc", "1234") |>
      httr2::req_method("HEAD") |>
      httr2::req_error(is_error = \(e) FALSE) |>
      httr2::req_perform()
  ),
  "Unauthorized"
  )
})
