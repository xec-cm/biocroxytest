#' Select samples in the requested order
#'
#' @param x A SummarizedExperiment.
#' @param samples Sample names in the order to return.
#' @return A SummarizedExperiment with the selected columns.
#' @export
#' @testCases sample-alignment
#' list(
#'   cases = list(
#'     memory = list(suite = "tests", setup = {
#'       x <- example_experiment()
#'       expect_true(is.matrix(SummarizedExperiment::assay(x)))
#'     }),
#'     hdf5 = list(suite = "longtests", setup = {
#'       x <- example_experiment()
#'       file <- withr::local_tempfile(fileext = ".h5")
#'       SummarizedExperiment::assay(x) <- HDF5Array::writeHDF5Array(
#'         SummarizedExperiment::assay(x), file, name = "counts", with.dimnames = TRUE
#'       )
#'       expect_s4_class(SummarizedExperiment::assay(x), "HDF5Matrix")
#'     })
#'   ),
#'   test = {
#'     selected <- align_samples(x, c("s3", "s1"))
#'     expect_identical(colnames(selected), c("s3", "s1"))
#'     expect_identical(as.integer(SummarizedExperiment::assay(selected)), c(31L, 11L))
#'     expect_identical(selected$condition, c("treated", "control"))
#'   }
#' )
align_samples <- function(x, samples) x[, samples]
