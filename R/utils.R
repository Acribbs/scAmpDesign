

reverse_comp <- function(seq){
  tryCatch(if(length(seq) == 0){TRUE}, finally=print("Looks like there may be a problem, did you specify a real Ensembl reference?
                                                     Otherwise the server may be down. If an output is produced please ignore this warning"))
  toupper(Biostrings::reverseComplement(Biostrings::DNAString(seq)))
}


ncbi_ref_seq <- function(id){
  seq <- tryCatch(ape::read.GenBank(id), finally=print("Looks like there may be a problem, did you specify a real NCBI reference?
                                                       Otherwise GenBank may be down"))

  full_seq <- as.character(seq)
  paste(full_seq[[1]], collapse = "")
}

ensembl_ref_seq <- function(id, host, dataset,
                            type){

  ensembl <- biomaRt::useMart("ensembl", host = host)
  ensembl = biomaRt::useDataset(dataset, mart=ensembl)
  filters = biomaRt::listFilters(ensembl)
  attributes = biomaRt::listAttributes(ensembl)
  mart <- biomaRt::useMart("ensembl",dataset=dataset)
  seq = biomaRt::getSequence(id=id, type=type, seqType="cdna", mart = mart)
  seq$cdna

}
