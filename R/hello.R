# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.

library("biomaRt")
# Normal host is uresponsive
host = "http://uswest.ensembl.org/"

listMarts(host = "http://uswest.ensembl.org/")

ensembl <- useMart("ensembl", host = "http://uswest.ensembl.org/")

datasets <- listDatasets(ensembl)
head(datasets)
ensembl = useDataset("hsapiens_gene_ensembl",mart=ensembl)
filters = listFilters(ensembl)
filters[1:10,]


attributes = listAttributes(ensembl)
attributes[1:5,]




mart <- useMart("ensembl",dataset="hsapiens_gene_ensembl")
seq = getSequence(id="ENST00000376207.10", type="ensembl_transcript_id_version", seqType="cdna", mart = mart)
seq$cdna



foo = .callP3NreadOrg(seq = test,
                      Tm = c(58, 60, 62),
                      name = "Adamc")
