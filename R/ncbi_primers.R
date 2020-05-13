

# NCBI
ncbi_primer_design <- function(id){
  ref <- ncbi_ref_seq(id)
  ref <- reverse_comp(ref)
  # Extract polyA
  polyA <- nchar(stringr::str_extract(ref, "T+"))

  # final position of polyA
  loc <- as.integer(stringr::str_locate(ref, "T+")[,'end'])

  # without polyA seq
  ref_nopolya <- substr(ref, loc+1, nchar(ref))



}
