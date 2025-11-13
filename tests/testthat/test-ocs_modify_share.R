test_that("modify share", {
  r <- httr2::request("https://cloud.example.com")
  df <- httr2::with_mocked_responses(
    mock_share,
    ocs_modify_share(r, 10, password = "abcdefghijklm",
                     permissions = 2, public_upload = TRUE,
                     expire_date = "2002-01-01",
                     label = "hi", note = "hello",
                     send_mail = FALSE, at = "xyz")
  )
  rows <- nrow(df)
  expect_equal(
    rows, 11
  )
})

test_that("modify share warnings", {
  r <- httr2::request("https://cloud.example.com")
  expect_warning(
    httr2::with_mocked_responses(
      mock_share,
      ocs_modify_share(r, 12, password = "abcdefghijklm")
    )
  )

  expect_error(
    httr2::with_mocked_responses(
      mock_share,
      ocs_modify_share(r, 10, password = "abc")
    )
  )

  expect_warning(
    httr2::with_mocked_responses(
      mock_share,
      ocs_modify_share(r, 12, permissions = 2)
    )
  )

  expect_warning(
    httr2::with_mocked_responses(
      mock_share,
      ocs_modify_share(r, 12, public_upload = TRUE)
    )
  )

  expect_warning(
    httr2::with_mocked_responses(
      mock_share,
      ocs_modify_share(r, 12, expire_date = "2002-01-01")
    )
  )

  expect_warning(
    httr2::with_mocked_responses(
      mock_share,
      ocs_modify_share(r, 12, label = "hi")
    )
  )

  expect_warning(
    httr2::with_mocked_responses(
      mock_share,
      ocs_modify_share(r, 12, note = "hello")
    )
  )

  expect_warning(
    httr2::with_mocked_responses(
      mock_share,
      ocs_modify_share(r, 12, send_mail = FALSE)
    )
  )

  expect_warning(
    httr2::with_mocked_responses(
      mock_share,
      ocs_modify_share(r, 12, at = "xyz")
    )
  )
})
