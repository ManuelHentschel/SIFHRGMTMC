
makeMacroFile <- function(texFile, entries, prefix='val'){
  if(!is.list(entries)){
    entries <- as.list(entries)
  }
  entryNames <- names(entries)
  if(is.null(entryNames)){
    stop('`entries` need to be a named list!')
  }
  lines <- c()
  for(name in entryNames){
    entry <- as.character(entries[[name]])
    line <- sprintf('\\newcommand{\\%s%s}{%s}', prefix, name, entry)
    lines <- c(lines, line)
  }
  ret <- paste0(lines, collapse = '\n')
  cat(ret, '\n', sep='', file=texFile)
}
