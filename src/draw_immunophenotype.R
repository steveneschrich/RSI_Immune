#' Draw the immunophenotype graph
#' 
#' Provide the function with the matrix to graph and it generates the
#' immunophenotype graph.
#'
#' @param x Matrix of genes in columns
#' @param classes The class assignments for genes
#' @param legend_title If needed, a legend for title
#' @param annotations A list of annotations for rows.
#' @param annotation_name Name for the annotation column.
#' @param ... Any other parameters to pass to Heatmap.
#'
#' @return A Heatmap object.
#' @export
#'
draw_immunophenotype<-function(x,
                               classes,
                               legend_title="", 
                               annotations=c(), 
                               annotation_name="", 
                               gene_labels = colnames(x),
                               cluster_columns = FALSE,
                               ...) {
  assertthat::assert_that(all(names(classes) %in% colnames(x)))
  classes <-  classes[intersect(colnames(x),names(classes))]
  assertthat::assert_that(all(names(classes) == colnames(x)))
  
  Heatmap(x,
          cluster_column_slices = FALSE,
          cluster_columns = cluster_columns,
          cluster_rows = FALSE,
          column_split=classes,
          show_row_dend = FALSE,
          show_column_dend = FALSE,
          column_names_side = "top",
          width = unit(8, "in"), height = unit(3, "in"),
          border=TRUE,
          column_gap=unit(5, "mm"),
          column_names_gp = gpar(fontsize=7),
          row_names_gp = gpar(fontsize = 7),
          row_order = order(annotations[rownames(x)], decreasing=TRUE),
          row_names_side="left",
          column_labels = gene_labels,
          left_annotation = rowAnnotation(annotation = annotations[rownames(x)], 
                                          col=list(annotation = color_gradient(
                                            annotations,
                                            min="#EEEEEE", mid = "#c2a5cf", max = "#7b3294"
                                          )),                                        
                                          annotation_label = annotation_name,
                                          annotation_name_gp = gpar(fontsize = 6),
                                          annotation_name_rot = 0,
                                          annotation_name_side = "bottom",
                                          simple_anno_size = unit(0.25,"cm"),
                                          annotation_legend_param=list(title_gp=gpar(fontsize=7),
                                                                       labels_gp = gpar(fontsize = 6))
          ),
          heatmap_legend_param = list(title = legend_title, 
                                      title_gp=gpar(fontsize=7),
                                      labels_gp = gpar(fontsize = 6)
          ),
          ...
  )
}


#' Annotate gene immunophenotype class
#'
#' A set of genes belong to the immunophenotyping signature in three
#' different classes. This function takes a list of genes and returns
#' a list of these classes.
#' 
#' This exists because eventually as a library, this will be an important of package
#' data rather than be provided by the user.
#' @param x A list of gene names
#' @param classses
#' 
#' @return A named list of classes (in form Gene=Class).
#' @export
#'
annotate_immunophenotype_class<-function(x, classes) {
  classes[x]
  #tibble::enframe(x, name=NULL, value="Gene") %>%
  #  dplyr::left_join(gene_map, by=c("Gene"="Symbol to Display")) %>%
  #  dplyr::select(Gene, Class) %>%
  #  tibble::deframe()
}

