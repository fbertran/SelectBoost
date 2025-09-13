test_that("boost.normalize centers columns and L2-normalizes", {
  set.seed(1)
  X <- matrix(rnorm(20), nrow = 5, ncol = 4)
  colnames(X) <- letters[1:4]

  Xn <- boost.normalize(X)
  # original names saved, new names are '1:ncol'
  expect_identical(attr(Xn, "orig.colnames"), colnames(X))
  expect_identical(colnames(Xn), as.character(seq_len(ncol(X))))

  # columns centered
  expect_true(all(abs(colMeans(Xn, na.rm=TRUE)) < 1e-8))

  # L2 norm of each column is 1 (up to tolerance)
  l2 <- sqrt(colSums(Xn^2))
  expect_true(all(abs(l2 - 1) < 1e-8))
})

test_that("boost.compcorrs matches base cor() on normalized X", {
  set.seed(2)
  X <- matrix(rnorm(30), nrow = 6, ncol = 5)
  Xn <- boost.normalize(X)
  C1 <- boost.compcorrs(Xn, corrfunc = "cor")
  C2 <- cor(Xn)
  expect_equal(C1, C2, tolerance = 1e-12)
  expect_true(is.matrix(C1))
  expect_equal(dim(C1), c(ncol(X), ncol(X)))
})

test_that("boost.correlation_sign simply takes the sign of entries", {
  M <- matrix(c(-2, -0.1, 0, 0.2, 5), nrow = 1)
  S <- boost.correlation_sign(M)
  expect_equal(S, sign(M))
})

test_that("boost.Xpass returns a normalized transformation matrix", {
  Xpass <- boost.Xpass(5, 4)
  expect_true(is.matrix(Xpass))
  expect_equal(dim(Xpass), c(5, 4))
  # Each column has L2 norm 1
  l2 <- sqrt(colSums(Xpass^2))
  expect_true(all(abs(l2 - 1) < 1e-8))
})

