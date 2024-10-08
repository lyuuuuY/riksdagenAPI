test_that("Large query works", {
  result <- get_top_10_nouns(start_date = "2015-01-01", end_date = "2024-01-01", 
                             type_speech = "", party = "", member = "0866055333028", size = 10000)
  expect_true(length(result) > 0)
})

test_that("API handles invalid parameters", {
  expect_error(
    get_top_10_nouns(
      start_date = "invalid-date",
      end_date = "invalid-date",
      type_speech = "invalid-type",
      party = "invalid-party",
      member = "invalid-member",
      size = 100
    )
  )
})

test_that("Function handles specific inputs", {
  result <- get_top_10_nouns(
    start_date = "2015-01-01",
    end_date = "2024-01-01",
    type_speech = "Nej",
    party = "sd",
    member = "086684470825",  
    size = 50 
  )
  expect_true(length(result) > 0, "The result should not be empty")
  expect_true(length(result) <= 10, "The result should contain up to 10 nouns")
})
  

test_that("Function returns consistent results for the same inputs", {
  result1 <- get_top_10_nouns(
    start_date = "2015-01-01",
    end_date = "2024-01-01",
    type_speech = "Nej",
    party = "sd",
    member = "086684470825",  
    size = 50 
  )
  
  result2 <- get_top_10_nouns(
    start_date = "2015-01-01",
    end_date = "2024-01-01",
    type_speech = "Nej",
    party = "sd",
    member = "086684470825",  
    size = 50 
  )
  expect_equal(result1, result2)
})
  