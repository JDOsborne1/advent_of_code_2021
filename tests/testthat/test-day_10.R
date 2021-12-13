input_10 <- readr::read_lines(system.file("extdata", "input_10_test.txt", package = "aoc2021"))

test_that("first_part_works", {
  expect_equal(
          input_10 |>
                  clear_legal_values() |>
                  find_and_score_illegal_closes()
          , 26397L
         )
})

test_that("second part works", {
        expect_equal(
                input_10 |>
                        clear_legal_values() |>
                        find_and_score_incomplete_lines()
                , 288957L
        )
})

test_that("string_reversal_works", {
        expect_equal(
                str_reverse("abcdef")
                , "fedcba"
        )


})

