# NCBI
ncbi_primer_design_input <- function(id){
  ref <- ncbi_ref_seq(id)
  ref <- reverse_comp(ref)
  # Extract polyA
  polyA <- nchar(stringr::str_extract(ref, "T+"))

  # final position of polyA
  loc <- as.integer(stringr::str_locate(ref, "T+")[,'end'])

  # without polyA seq
  ref_nopolya <- substr(ref, loc+1, nchar(ref))

  # Add the extra sequence
  ext_seq <- substr(ref_nopolya, 1, 500)

  # Input sequence for primer3
  paste0("AAGCAGTGGTATCAACGCAGAGTAC", "[nnnnnnnnnnnnNNNNNNNNTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT", substr(ref, 1, 200+polyA), "]", substr(ext_seq, 500-300, 500))


}

# ENSEMBL
ensembl_primer_design_input <- function(id,
                                  host=NULL,
                                  dataset=NULL,
                                  type="ensembl_transcript_id_version"){


  ref <- ensembl_ref_seq(id = id, host = host, dataset = dataset, type=type)
  ref <- reverse_comp(ref)
  # Extract polyA
  polyA <- nchar(stringr::str_extract(ref, "T+"))

  # final position of polyA
  loc <- as.integer(stringr::str_locate(ref, "T+")[,'end'])

  # without polyA seq
  ref_nopolya <- substr(ref, loc+1, nchar(ref))

  # Add the extra sequence
  ext_seq <- substr(ref_nopolya, 1, 500)

  # Input sequence for primer3
  paste0("AAGCAGTGGTATCAACGCAGAGTAC", "[nnnnnnnnnnnnNNNNNNNNTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT", substr(ref, 1, 200+polyA), "]", substr(ext_seq, 500-300, 500))

}
