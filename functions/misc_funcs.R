
explain_code_safe <- function(code){

  tmp <- icd::explain_code(code)

  if (length(tmp)==0) {tmp <- NA}

  tmp
}
