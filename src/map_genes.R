#' Load gene map object.
#'
#' @return The gene map object.
#' @export
#'
load_gene_map<-function() {
  readRDS(gene_map, file="../../data.derived/immunophenotype_gene_mapping.rds")  
}


#' Map gene symbol to immunphenotype class
#'
#' Gene symbols belong to a particular immunophenotype class. The gene_map defines
#' the mappings. Here, the gene symbols are mapped to classes in the form of a
#' `GENE=CLASS` list.
#'
#' @param x A list of gene symbols to map to classes.
#'
#' @return List of gene=class pairs, for mapping genes onto immunoclasses.
#' @export
#'
map_gene_class<-function(x) {
  
  load_gene_map() %>%
    dplyr::left_join(tibble::tibble(GENE=x), by=c("Approved symbol"="GENE")) %>%
    dplyr::select(`Approved symbol`, `Class`) %>%
    tibble::deframe()
}

#' Map gene symbol to display gene name
#'
#' Genes are a mess (nomenclature-wise). This maps the official gene symbol
#' (used for clustering) onto an alternate set of display names which are more
#' amenable to human consumption. This is because names change and people remember
#' the original or "unofficial" name better.
#'
#' @param x A list of gene symbols to map to display names.
#'
#' @return A list of display names (in the order of input).
#' @export
map_gene_display_name<-function(x) {
  load_gene_map() %>%
    dplyr::left_join(tibble::tibble(GENE=x), by=c("Approved symbol"="GENE")) %>%
    dplyr::select(`Approved symbol`, `Symbol to Display`) %>%
    tibble::deframe()
}