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
"example_genes_polar"

#' A set of example dummy alignment genes.
#'
#' Dummy genes, similar to those generated with `make_alignment_dummies()`, for
#' example purposes only.
#'
#' @format A data frame with eight rows and three variables:
#' \describe{
#'   \item{molecule}{the genome}
#'   \item{start}{the start position of the dummy}
#'   \item{end}{the end position of the dummy}
#'   \item{gene}{the name of the dummy gene}
#' }
"example_dummies"

#' @rdname example_genes
"example_subgenes"

#' @rdname example_genes
"example_subgenes_polar"

#' A set of example genetic features.
#'
#' Genetic features for example purposes only.
#'
#' @format A data frame with 23 rows and five variables:
#' \describe{
#'   \item{molecule}{the genome}
#'   \item{name}{the name of the feature}
#'   \item{type}{the type of the feature}
#'   \item{position}{the position of the feature}
#'   \item{forward}{is the feature oriented, and if so in the forward direction?}
#' }
"example_features"

#' @rdname example_features
"example_features_polar"

#' A set of example terminators.
#'
#' Terminators for example purposes only.
#'
#' @format A data frame with two rows and three variables:
#' \describe{
#'   \item{molecule}{the genome}
#'   \item{name}{the name of the terminator}
#'   \item{position}{the position of the terminator}
#' }
"example_terminators"

#' @rdname example_terminators
"example_terminators_polar"
