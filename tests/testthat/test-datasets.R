test_that("example datasets load", {
  ds <- c("autoboost.res.x.adapt",
          "autoboost.res.x",
          "autoboost.res.x2.adapt",
          "autoboost.res.x2",
          "Cascade_confidence",
          "Cascade_example",
          "fastboost.res.x.adapt",
          "fastboost.res.x",
          "fastboost.res.x2.adapt",
          "fastboost.res.x2",
          "results_simuls_reverse_engineering_v3"
  )
  if (length(ds) == 0L) skip("No datasets in data/")
  for (d in ds) {
    expect_silent(data(list = d, package = "SelectBoost", envir = environment()))
  }
})

