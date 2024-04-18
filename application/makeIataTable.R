
N_COLS_IATAS <- 13

makeIataTable <- function(IATAs, ncols = N_COLS_IATAS, prefix='\\texttt{', postfix='}'){
  IATAs <- paste0(prefix, IATAs, postfix)
  rowEntries <- split(IATAs, ceiling(seq_along(IATAs) / ncols))
  rows <- sapply(rowEntries, paste0, collapse=' & ')
  txt <- paste0('    ', rows, collapse=' \\\\\n')
  header <- paste0('\\begin{tabular}{', strrep('l', ncols), '}\n')
  txt <- paste0(
    header,
    txt,
    '\n\\end{tabular}\n'
  )
  return(txt)
}

