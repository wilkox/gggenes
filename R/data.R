#' A set of example genes.
#'
#' Genes for example purposes only.
#'
#' @format A data frame with 72 rows and six variables:
#' \describe{
#'   \item{molecule}{the genome}
#'   \item{gene}{the name of the gene}
#'   \item{start}{the start position of the gene}
#'   \item{end}{the end position of the gene}
#'   \item{strand}{the strand of the gene}
#'   \item{orientation}{the orientation of the gene}
#' }
#' 
#' example_subgenes (143 rows) also contains:
#' \describe{
#'   \item{subgene}{the name of the subgene}
#'   \item{from}{the start position of the subgene segment}
#'   \item{to}{the end position of the subgene segment}
#' }
"example_genes"

#' @rdname example_genes
"example_subgenes"
