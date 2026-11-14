#' Select samples in the requested order
#'
#' @param x A SummarizedExperiment.
#' @param samples Sample names in the order to return.
#' @return A SummarizedExperiment with the selected columns.
#' @export
#' @tests
#' x <- example_experiment()
#' selected <- align_samples(x, c("s3", "s1"))
#' expect_identical(colnames(selected), c("s3", "s1"))
#' expect_identical(as.integer(SummarizedExperiment::assay(selected)), c(31L, 11L))
#' expect_identical(selected$condition, c("treated", "control"))
align_samples <- function(x, samples) x[, samples]
