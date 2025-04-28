#' Make ASPM runs
#' 
#' \code{extract_NLL_npars_gradient} This function generates ASPM
#' 
#' @export

extract_NLL_npars_gradient<- function(file_path) {
  # will go the a .par file and get the number of estimated parameters, NLL and  final gradient
  #
  # Read the first line of the file
  first_line <- readLines(file_path, n = 1)
  
  # Use regular expressions to extract numeric values
  numbers <- as.numeric(unlist(regmatches(first_line, gregexpr("[0-9]+(\\.[0-9]+)?", first_line))))
  
  # Return as a named list
  return(list(
    num_parameters = numbers[1],
    objective_value = numbers[2],
    max_gradient = numbers[3]
  ))
}