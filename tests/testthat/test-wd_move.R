mock_move <- function(req) {
  if(req$method != "MOVE") {
    httr2::response(body=405)
  } else {
    if(req$url == "https://cloud.example.com/file.txt") {
      if(req$headers$Destination=="https://cloud.example.com/newfile.txt") {
        httr2::response(status_code=200)
      } else if (req$headers$Overwrite=="F"){
        httr2::response(status_code=409)
      }
      else {
        httr2::response(status_code=200)
      }
    } else {
      httr2::response(status_code=404)
    }
  }
}

test_that("move works", {
  r <- httr2::request("https://cloud.example.com/")
  expect_equal(
    httr2::with_mocked_responses(mock_move, wd_move(r, "file.txt", "newfile.txt")),
    TRUE
  )
})

test_that("move warning", {
  r <- httr2::request("https://cloud.example.com")
  expect_warning(
    httr2::with_mocked_responses(mock_move, wd_move(r,"filex.txt","newfile.txt")),
    'File could not be moved. Maybe target file already exists. Not Found'
  )
})

test_that("move return false", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(mock_move, suppressWarnings(wd_move(r,"filex.txt","nefile.txt"))),
    FALSE
  )
})

test_that("move overwrite warning", {
  r <- httr2::request("https://cloud.example.com")
  expect_warning(
    httr2::with_mocked_responses(mock_move, wd_move(r,"file.txt","existingfile.txt", overwrite=FALSE)),
    'File could not be moved. Maybe target file already exists. Conflict'
  )
})

test_that("move overwrite return false", {
  r <- httr2::request("https://cloud.example.com")
  expect_equal(
    httr2::with_mocked_responses(mock_move, suppressWarnings(wd_move(r,"file.txt","existingfile.txt",overwrite=FALSE))),
    FALSE
  )
})