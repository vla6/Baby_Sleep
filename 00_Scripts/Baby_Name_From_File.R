#
# Get the baby name part of a string (generally a filename
# with a prefix)
#

require(tools)

Baby_Name_From_File <- function (fn_str, prefix_pattern) {
  return(as.character(gsub(prefix_pattern, "",
                           file_path_sans_ext(fn_str))))
}