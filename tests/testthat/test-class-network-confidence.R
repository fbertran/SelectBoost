test_that("network.confidence class: exact slots and minimal valid object", {
  skip_if_not_installed("methods")
  expect_true(methods::isClass("network.confidence"))

  slots <- methods::getSlots("network.confidence")
  expect_equal(sort(names(slots)), sort(c("network.confidence","name","F","time_pt","cv.subjects")))

  expect_identical(unname(slots[["network.confidence"]]), "matrix")
  expect_identical(unname(slots[["name"]]), "character")
  expect_identical(unname(slots[["F"]]), "array")
  expect_identical(unname(slots[["time_pt"]]), "numeric")
  expect_identical(unname(slots[["cv.subjects"]]), "logical")

  # Construct a minimal valid object
  nc <- methods::new("network.confidence",
                     network.confidence = matrix(0, 2, 2),
                     name = "toy",
                     F = array(0, dim = c(2, 2, 1)),
                     time_pt = 0,
                     cv.subjects = FALSE)

  expect_s4_class(nc, "network.confidence")
  expect_equal(dim(nc@network.confidence), c(2L, 2L))
  expect_type(nc@name, "character")
  expect_equal(dim(nc@F), c(2L, 2L, 1L))
  expect_type(nc@time_pt, "double")
  expect_true(methods::validObject(nc))
})
