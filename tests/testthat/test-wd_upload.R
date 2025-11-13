test_that("upload works", {
  r <- httr2::request("https://cloud.example.com/")
  local <- paste0(tempdir(), "/ul")
  dir.create(local)
  dir.create(paste0(local, "/test"))
  file.create(paste0(local, "/test/abc.txt"))
  expect_equal(
    httr2::with_mocked_responses(mock_upload, wd_upload(r, local, "dir")),
    c("dir/test/abc.txt")
  )
  unlink(local, recursive = TRUE)
})

test_that("upload file works", {
  r <- httr2::request("https://cloud.example.com/")
  local <- paste0(tempdir(), "/ul")
  dir.create(local)
  file.create(paste0(local, "/file.txt"))
  expect_equal(
    httr2::with_mocked_responses(
      mock_upload, wd_upload(r, paste0(local, "/file.txt"), "test")
    ),
    c("test/file.txt")
  )
  unlink(local, recursive = TRUE)
})

test_that("upload without target works", {
  r <- httr2::request("https://cloud.example.com/")
  local <- paste0(tempdir(), "/ul")
  dir.create(local)
  dir.create(paste0(local, "/test"))
  file.create(paste0(local, "/test/abc.txt"))
  expect_equal(
    httr2::with_mocked_responses(mock_upload, wd_upload(r, local)),
    c("ul/test/abc.txt")
  )
  unlink(local, recursive = TRUE)
})


test_that("upload missing warning works", {
  r <- httr2::request("https://cloud.example.com/")
  local <- paste0(tempdir(), "/ul")
  dir.create(local)
  file.create(paste0(local, "/file.txt"))
  expect_warning(
    httr2::with_mocked_responses(
      mock_upload, wd_upload(r, paste0(local, "/file.txt"), "missing")
    ),
    "Not Found"
  )
  unlink(local, recursive = TRUE)
})

test_that("upload missing works", {
  r <- httr2::request("https://cloud.example.com/")
  local <- paste0(tempdir(), "/ul")
  dir.create(local)
  file.create(paste0(local, "/file.txt"))
  expect_equal(
    httr2::with_mocked_responses(
      mock_upload,
      suppressWarnings(
        wd_upload(r, paste0(local, "/file.txt"), "missing")
      )
    ),
    character(0)
  )
  unlink(local, recursive = TRUE)
})
