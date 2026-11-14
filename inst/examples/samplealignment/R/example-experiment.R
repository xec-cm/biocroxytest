#' Create three samples with known measurements
#'
#' @return A SummarizedExperiment with one gene and three samples.
#' @export
example_experiment <- function() {
  samples <- c("s1", "s2", "s3")
  counts <- matrix(c(11L, 21L, 31L), nrow = 1L,
                   dimnames = list("gene1", samples))
  metadata <- data.frame(condition = c("control", "control", "treated"),
                         row.names = samples)
  SummarizedExperiment::SummarizedExperiment(
    assays = list(counts = counts), colData = metadata
  )
}
