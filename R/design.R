test <- ensembl_primer_design(id="ENST00000376207.10")
# Need to check for input then decide which function to run, whether it is ncbi or ensembl transcript id

test <- ncbi_primer_design("NC_045512")



foo = .callP3NreadOrg(seq = test,
                      Tm = c(58, 60, 62),
                      name = "Adamc")
