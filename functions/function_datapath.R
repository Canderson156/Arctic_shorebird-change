# makes a nicer looking call when using data_path to make relative paths for both computer

datapath <- function(rel_path) {
  paste(data_path, rel_path, sep = "")
}
