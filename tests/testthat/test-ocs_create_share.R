test_that("create share extended", {
  r <- httr2::request("https://cloud.example.com")
  df <- httr2::with_mocked_responses(
    mock_share,
    ocs_create_share(r, "exchange", 3, expire_date = "2000-01-01",
                     password = "abc123xyz789", attributes = "xyz")
  )
  rows <- nrow(df)
  expect_equal(
    rows, 1
  )
})

test_that("create share extended error", {
  r <- httr2::request("https://cloud.example.com")
  expect_error(
    httr2::with_mocked_responses(
      mock_share,
      ocs_create_share(r, "exchange", 1)
    ),
    "For user or group shares the parameter 'shareWith' is required"
  )
})

test_that("create share link", {
  r <- httr2::request("https://cloud.example.com")
  df <- httr2::with_mocked_responses(
    mock_share,
    ocs_create_share_link(r, "exchange", expire_date = "2000-01-01",
                          password = "abc123xyz789")
  )
  rows <- nrow(df)
  expect_equal(
    rows, 1
  )
})

test_that("create share link short password", {
  r <- httr2::request("https://cloud.example.com")
  expect_error(
    httr2::with_mocked_responses(
      mock_share,
      ocs_create_share_link(r, "exchange", password = "abc")
    ),
    "Your password is too short."
  )
})

test_that("create share link wrong folder", {
  r <- httr2::request("https://cloud.example.com")
  expect_warning(
    httr2::with_mocked_responses(
      mock_share,
      ocs_create_share_link(r, "exchanges")
    ),
    "File / folder could not be shared: exchanges"
  )
})


test_that("create share user", {
  r <- httr2::request("https://cloud.example.com")
  df <- httr2::with_mocked_responses(
    mock_share,
    ocs_create_share_user(r, "exchange", user = "jackdoe")
  )
  rows <- nrow(df)
  expect_equal(
    rows, 1
  )
})

test_that("create share group", {
  r <- httr2::request("https://cloud.example.com")
  df <- httr2::with_mocked_responses(
    mock_share,
    ocs_create_share_group(r, "exchange", group = "jackdoe")
  )
  rows <- nrow(df)
  expect_equal(
    rows, 1
  )
})

test_that("create share mail", {
  r <- httr2::request("https://cloud.example.com")
  df <- httr2::with_mocked_responses(
    mock_share,
    ocs_create_share_mail(r, "exchange", email = "jackdoe@example.com")
  )
  rows <- nrow(df)
  expect_equal(
    rows, 1
  )
})

test_that("create share federated", {
  r <- httr2::request("https://cloud.example.com")
  df <- httr2::with_mocked_responses(
    mock_share,
    ocs_create_share_federated(r, "exchange", cloud_id = "jackdoe")
  )
  rows <- nrow(df)
  expect_equal(
    rows, 1
  )
})

test_that("permissions to int", {
  expect_equal(permissions_to_int(c("read,write", "share")), 19)
  expect_equal(permissions_to_int(c(1, 2, 16, 16, 1)), 19)
  expect_equal(permissions_to_int(list("a")), 1)
})

test_that("permissions to chr", {
  expect_equal(
    permissions_to_chr(c(1, 3, 17)),
    c("read", "read,update", "read,share")
  )
})
