

text_to_binary <- function(text) {
  chars <- unlist(strsplit(text, NULL))
  
  binary_sequence <- sapply(chars, function(char) {
    binary_char <- intToBits(utf8ToInt(char))
    binary_string <- paste(rev(as.integer(binary_char)[1:8]), collapse = "")
    return(binary_string)
  })
  
  binary_sequence <- paste(binary_sequence, collapse = "")
  
  return(binary_sequence)
}

# Binary -> DNA
char2dna <- function(binary_text) {

  codon_map <- list(
    '00' = 'CG', '01' = 'CC', '10' = 'GT', '11' = 'AT'
  )
  

  binary_pairs <- strsplit(binary_text, NULL)[[1]]
  
  dna_sequence <- character()
  
  # Loop
  for (i in seq(1, length(binary_pairs), by = 2)) {
    binary_pair <- paste0(binary_pairs[i], binary_pairs[i + 1])
    if (!is.null(codon_map[[binary_pair]])) {
      dna_sequence <- c(dna_sequence, codon_map[[binary_pair]])
    } else {
      dna_sequence <- c(dna_sequence, 'NN') # Default codon for unknown pairs
    }
  }
  

  dna_sequence <- paste(dna_sequence, collapse = "")
  
  return(dna_sequence)
}

# Function binary -> string
transform_binary_string <- function(s) {
  # Split
  chars <- strsplit(s, "")[[1]]
  
  result <- character()
  
  # Char loop
  for (i in seq(1, length(chars), by = 2)) {
    # Combine the current and next character with a hyphen
    pair <- paste(chars[i], chars[i + 1], sep = "-")
    # Append the pair to the result vector
    result <- c(result, pair)
  }
  
  # Combine the pairs with newline characters
  final_result <- paste(result, collapse = "\n")
  
  return(final_result)
}

# Example usage
text <- "Hello World!"
binary_sequence <- text_to_binary(text)
dna_sequence <- char2dna(binary_sequence)


print(dna_sequence)
cat(text, 'BECOMES:\n', transform_binary_string(dna_sequence))
