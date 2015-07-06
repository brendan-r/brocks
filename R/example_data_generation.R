
n <- 100
set.seed(1)

likert_vals <- c("0 - Not At All Likely", 1:4, "5 - Neutral", 6:9,
                 "10 - Extremely Likely")

gender <- c("M", "m", "F", "F", "Man", "Woman", "Female", "male", "Female", "Male", "<NA>", "n/a", "")

income <- c("$0 - $40k", "$41k - $80k", "$81k - $100k", "$100k+")

dob <- as.Date(
  runif(n, Sys.Date() - 70 * 365, Sys.Date() - 365 * 18),
  origin = "1970-01-01"
)

test_data <- data.frame(
  rec    = sample(likert_vals, replace = TRUE, size = n),
  gender = sample(gender,      replace = TRUE, size = n),
  income = sample(income,      replace = TRUE, size = n),
  dob    = dob
)

save(test_data, file = "data/test_data.rda")


