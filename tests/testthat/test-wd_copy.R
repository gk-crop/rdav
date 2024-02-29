mock_copy <- function(req) {
  if (req$method != "COPY") {
    httr2::response(body = 405)
  } else {
    if (req$url == "https://cloud.example.com/file.txt") {
      if (req$headers$Destination == "https://cloud.example.com/newfile.txt") {
        httr2::response(status_code = 200)
      } else if (req$headers$Overwrite == "F") {
        httr2::response(status_code = 409)
      } else {
        httr2::response(status_code = 200)
      }
    } else {
      httr2::response(status_code = 404)
    }
  }
}

test_that("copy works", {
  r <- httr2::request("https://cloud.example.com/")
  expect_equal(
    httr2::with_mocked_responses(
      mock_copy,
      wd_copy(r, "file.txt", "newfile.txt")
    ),
    TRUE
  )
})

test_that("copy warning", {
  r <- httr2::request("https://cloud.example.com")
  expect_warning(
    httr2::with_mocked_responses(
      mock_copy, wd_copy(r, "filex.txt", "newfile.txt")
    ),
    "File could not be copied. Maybe the file already exists. Not Found"
  )
})

test_that("copy return false", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(
      mock_copy, suppressWarnings(wd_copy(r, "filex.txt", "newfile.txt"))
    ),
    FALSE
  )
})



test_that("copy overwrite warning", {
  r <- httr2::request("https://cloud.example.com")
  expect_warning(
    httr2::with_mocked_responses(
      mock_copy,
      wd_copy(r, "file.txt", "existingfile.txt", overwrite = FALSE)
    ),
    "File could not be copied. Maybe the file already exists. Conflict"
  )
})

test_that("copy overwrite return false", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(
      mock_copy,
      suppressWarnings(
        wd_copy(r, "file.txt", "existingfile.txt", overwrite = FALSE)
      )
    ),
    FALSE
  )
})
