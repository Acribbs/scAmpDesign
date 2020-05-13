

reverse_comp <- function(seq){
  toupper(Biostrings::reverseComplement(Biostrings::DNAString(seq)))
}


# A function to parse the transcript with id to generate a sequence
ensembl_trid_seq <- function(id){

}

ncbi_ref_seq <- function(id){
  seq <- ape::read.GenBank(id)

  full_seq <- as.character(seq)
  paste(full_seq[[1]], collapse = "")
}

