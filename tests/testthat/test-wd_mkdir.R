mock_mkdir <- function(req) {
  if(req$method != "MKCOL") {
    httr2::response(body=405)
  } else {
    if(req$url == "https://cloud.example.com/new") {
      httr2::response(status_code=200)
    } else {
      httr2::response(status_code=409)
    }
  }
}


test_that("mkdir works", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(mock_mkdir, wd_mkdir(r,"new")),
    TRUE
  )
})

test_that("mkdir warning", {
  r <- httr2::request("https://cloud.example.com")
  expect_warning(
    httr2::with_mocked_responses(mock_mkdir, wd_mkdir(r,"old/new")),
    'Conflict'
  )
})

test_that("mkdir return false", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(mock_mkdir, suppressWarnings(wd_mkdir(r,"old/new"))),
    FALSE
  )
})