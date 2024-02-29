mock_delete <- function(req) {
  if(req$method != "DELETE") {
    httr2::response(body=405)
  } else {
    if(req$url != "https://cloud.example.com/deleteme") {
      httr2::response(status_code=404)
    } else {
      httr2::response(status_code=200)
    }
  }
}


test_that("delete works", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(mock_delete, wd_delete(r,"deleteme")),
    TRUE
  )
})

test_that("delete warning", {
  r <- httr2::request("https://cloud.example.com")
  expect_warning(
    httr2::with_mocked_responses(mock_delete, wd_delete(r,"deletemex")),
    'Not Found'
  )
})

test_that("delete return false", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(mock_delete, suppressWarnings(wd_delete(r,"deletemex"))),
    FALSE
  )
})
