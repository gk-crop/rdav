mock_upload <- function(req) {
  if(req$method == "MKCOL") {
    httr2::response(body=200)
  } else if (req$method == "PUT") {
    
      httr2::response(status_code=200)
    
  } else {
    httr2::response(status_cod=405)
  }
}

test_that("upload works", {
  r <- httr2::request("https://cloud.example.com/")
  local = paste0(tempdir(),"/ul")
  dir.create(local)
  dir.create(paste0(local,"/test"))
  file.create(paste0(local,"/test/abc.txt"))
  expect_equal(
    httr2::with_mocked_responses(mock_upload, wd_upload(r, local, "dir")),
    c("dir/test/abc.txt")
  )
  
  unlink(local, recursive=TRUE)
})
