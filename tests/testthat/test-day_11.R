input_11 <- readr::read_lines(system.file("extdata", "input_11_test.txt", package = "aoc2021"))

test_that("part 1 works", {
  expect_equal(
          input_11 |>
                  parse_octopus_positions() |>
                  find_total_flashes(.after = 100)
          ,
                1656
          )
})
