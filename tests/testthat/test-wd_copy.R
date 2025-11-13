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
