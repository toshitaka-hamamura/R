####generate random numbers and characters####
digits = 0:9
createRandString<- function() {
  v = c(sample(letters, 5, replace = TRUE),
        sample(digits, 4, replace = TRUE),
        sample(letters, 1, replace = TRUE))
  return(paste0(v,collapse = ""))
}
createRandString()
