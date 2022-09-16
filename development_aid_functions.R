# useful r functions for development

# CHECK TYPES OF OBJECT
check_types <- function(object) {
  cat("OBJECT NAME:", deparse(substitute(object)), "\n")
  cat("typeof:", typeof(object), "\n")
  cat("class:", class(object), "\n")
  cat("mode:", mode(object), "\n")
  cat("length:", length(object), "\n")
  cat("number of dimensions:", length(dim(object)), "\n")
  cat("dimensions:", dim(object),  "\n")
}

# changes strings that start with a digit into variable names
fix_digit_names <- function(x, insertion, pos_idx = 0)
{
  # NOTE: Need to assign it to the x variable e.g. x <- tf_fixer(x, insertion)
  fun_insert <- function(x, pos, insert)
  {
    # Function inserts 'insertion' argument at the 0 position
    gsub(paste0("^(.{", pos, "})(.*)$"), paste0("\\1", insert, "\\2"), x)
  }
  
  test_vec <- grepl("^[[:digit:]+]", x)
  for (i in 1:length(x))
  {
    if (test_vec[i] == TRUE)
    {
      x[i] <- fun_insert(x[i], pos_idx, as.character(insertion))
    }
  }
  return(x)
}

# Source file: "ev_es_1.12.r"
name_as_string <- function(x)
{
  # Returns the name of whatever you pass as a character string 
  # Primary purpose is to get string representation of function and variable names 
  # Added benefit is anything passed as x will come out as 'x'
  deparse(substitute(x))
}
# Source file: "ev_es_1.12.r"
# function inserts a characters at a given position in a string
fun_insert <- function(x, pos, insert)
{
  # Function inserts 'insertion' argument at the 0 position
  gsub(paste0("^(.{", pos, "})(.*)$"), paste0("\\1", insert, "\\2"), x)
}

# Designed to store many individual files of "results"
# can either store data or contruct string directories for storage
# source file: "ev_sec_0.4.r"
store_results <- function(results,
                          cd_root,
                          icb_level = "",
                          return_type = "",
                          type = c("data", "directory"),
                          mode = "csv",
                          rowNames = TRUE,
                          colNames = TRUE) {
  # function writes results in individual ".csv" files
  # constructs the basic directory
  cd_trunk <- paste0(cd_root, icb_level, "/", return_type, "/")
  # retrieves sub-grouping
  keys <- names(results)
  if (type == "data") {
    if (mode == "csv") {
      # STORE THE RESULTS
      for (i in seq_along(results)) {
        write.csv(results[[i]],
                  file = paste0(cd_trunk, keys[[i]], ".csv"),
                  row.names = rowNames
        )
      }
    } else if (mode == "zoo") {
      # STORE THE RESULTS
      for (i in seq_along(results)) {
        zoo::write.zoo(
          results[[i]],
          file = paste0(cd_trunk, keys[[i]], ".csv"),
          row.names = rowNames,
          col.names = colNames
        )
      }
    } else if (type == "directory") {
      d_list <- vector(mode = "character", length = length(results))
      # fills "d_list" with directories
      for (i in seq_along(results)) {
        d_list[[i]] <- paste0(cd_trunk, keys[[i]], ".csv")
      }
      # writes "d_list" as .txt file
      write.table(
        d_list,
        file = paste0(cd_trunk, icb_level, "_", return_type, "_", "cd.txt"),
        quote = TRUE,
        row.names = FALSE,
        col.names = FALSE
      )
    }
  }
}