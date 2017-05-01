#' Convert .mesh files from FEBio to .lis files required by Bonemat. Outputs 
#' two files: elements and nodes
#' @author Scott Telfer \email{scott.telfer@gmail.com}
#' @param x filepath to .mesh file
#' @param output_folder folder to output .lis files to. If not given, the files 
#' will be exported to the same directory as the original .mesh file
#' @param append_name Determines if the output filenames will use the naem of 
#' the orginal file as their prefix 
mesh2lis <- function(x, output_folder, append_name = TRUE) {
  # packages
  require(readr)
  require(stringr)
  
  # test input
  if (is.character(x) == FALSE)
    stop("filepath needs to be a character string")
  if (strsplit(basename(x), "\\.")[[1]][2] != "mesh")
    stop("file needs to be of type '.mesh'")
  
  # import mesh data
  mesh_file <- readLines(x)
  vert_line_no <- which(str_detect(mesh_file, "Vertices") == TRUE)
  no_vert <- as.numeric(mesh_file[vert_line_no + 1])
  tet_line_no <- which(str_detect(mesh_file, "Tetrahedra") == TRUE)
  no_tet <- as.numeric(mesh_file[tet_line_no + 1])
  nodes <- read_tsv(x, col_names = FALSE, skip = vert_line_no + 1, 
                    n_max = no_vert)
  elements <- read_tsv(x, col_names = FALSE, skip = tet_line_no + 1, 
                       n_max = no_tet)
  
  # format data frames
  nodes <- data.frame(1:no_vert, nodes[, 1:3])
  elements <- data.frame(1:no_tet, elements)
  
  # export mesh data to .lis files
  if (missing(output_folder) == FALSE) {
    if (is.character(output_folder) == FALSE)
      stop("filepath needs to be a character string")
    if (append_name == TRUE) {
      name <- strsplit(basename(x), "\\.")[[1]][1]
      node_path <- paste0(output_folder, "/", name, "_nodes.lis")
      element_path <- paste0(output_folder, "/", name, "_elements.lis")
    }
    if (append_name == FALSE) {
      node_path <- paste0(output_folder, "/nodes.lis")
      element_path <- paste0(output_folder, "/elements.lis" )
    }
  } else {
    directory <- dirname(x)
    if (append_name == TRUE) {
      name <- strsplit(basename(x), "\\.")[[1]][1]
      node_path <- paste0(directory, "/", name, "_nodes.lis")
      element_path <- paste0(directory, "/", name, "_elements.lis")
    }
    if (append_name == FALSE) {
      node_path <- paste0(directory, "/nodes.lis")
      element_path <- paste0(directory, "/elements.lis" )
    }
  }
  write.table(nodes, node_path, sep = "\t", row.names = FALSE, 
              col.names = FALSE)
  write.table(elements, element_path, sep = "\t", row.names = FALSE,
              col.names = FALSE)
}


###############################################################################
# =============================================================================
# END OF SCRIPT
# =============================================================================
###############################################################################

