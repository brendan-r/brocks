context("brocks")


# html tri ----------------------------------------------------------------

test_that("html_tri", {

  ht1 <- html_tri(1:-1)

  ht2 <- html_tri(1:-1, symbols = c("up" = "&#128522;", "down" = "&#128542;",
                                    "nochange" = "&#128528;"))

  exp_res2 <-
    c("<a style='color:green'>&#128522;</a><a>1</a>",
      "<a style='color:black'>&#128528;</a><a>0</a>",
      "<a style='color:red'>&#128542;</a><a>-1</a>" )

  exp_res1 <-
    c("<a style='color:green'>&#9650;</a><a>1</a>",
      "<a style='color:black'>&#9632;</a><a>0</a>",
      "<a style='color:red'>&#9660;</a><a>-1</a>")

  expect_equal(ht1, exp_res1)
  expect_equal(ht2, exp_res2)

})

test_that("misc", {
  expect_equal(ac_se(c(TRUE, FALSE)), 0.219089, tolerance = 0.002)


})

