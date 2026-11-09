#' it selects cells with the highest total read counts
#' @param input_file path to the input csv file
#' @param output_file path to the output csv file
#' @param n number of cells to keep
#' @return a list containinig the number of kept cells and the number of genes
#' @export
filterTopCells<- function(input_file, output_file, n){
  data<- read.csv(input_file, header = TRUE, row.names = 1)
  library_size<- colSums(data)
  if (n>=ncol(data)){
    filtered_data<- data
  } else {
    ordered_cells<- order(library_size, decreasing = TRUE)
    selected_cells<- ordered_cells[1:n]
    filtered_data<- data[, selected_cells, drop=FALSE]
  }
  write.csv(filtered_data, output_file, row.names = TRUE)
  return(list(n_cells_kept=ncol(filtered_data),
              n_genes=nrow(filtered_data),
              n_cells=ncol(filtered_data)
  ))
}
