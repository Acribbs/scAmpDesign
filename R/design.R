#' Initiate the primer design for a single gene
#'
#' This function will design a primer set to use with targeted single-cell sequencing.
#'
#' @param ncbi An NBCI Reference. For example, the SARS-COV2 is NC_045512.
#' @param transcript_id_version An Ensembl transcript id with version number. For example,
#'   the main transcript for FOXP3 is ENST00000376207.10.
#' @param primer3 The location of primer3_core. Example:  primer3='/Users/adamcribbs/miniconda3/bin/primer3_core'
#' @param thermo.param The location of the thermodynamic parameters folder. Currently within the R inst/extdata
#' @param settings text file for parameters for primer 3. Currently within the inst/extdata
#' @return A set of primer pairs
#' @examples
#' primer_design(ncbi="NC_045512")
#' @export
single_primer_design <- function(ncbi=NULL, transcript_id_version=NULL,
                                 primer3=NULL,
                                 thermo.param=system.file("extdata/primer3_config/", package="scAmpDesign"),
                                 settings=system.file("extdata/primer3_settings.txt", package="scAmpDesign")){

  # Specify the location of primer3_core
  if(length(is.na(primer3)) == 0){
    stop("Please specify the location of primer3_core. For example: primer3='/Users/adamcribbs/miniconda3/bin/primer3_core'")
  }

  # Select either ensembl or ncbi design
  if(length(is.na(ncbi)) == 0 & length(is.na(transcript_id_version)) == 0){
    stop("Please select a NCBI Reference Sequence or Ensembl transcript id with version")
  }
  if(length(is.na(ncbi)) != 0 & length(is.na(transcript_id_version)) != 0){
    stop("You have specified both an NCBI Reference Sequence and Ensembl transcript id with version,
         please specify only one")
  }
  if(length(is.na(ncbi)) != 0 & length(is.na(transcript_id_version)) == 0){
    seq <- ncbi_primer_design_input(id=ncbi)
  }
  if(length(is.na(ncbi)) == 0 & length(is.na(transcript_id_version)) != 0){
    seq <- ensembl_primer_design_input(id=transcript_id_version)
  }
  primer3_output = .callP3NreadOrg(seq = seq,
                                   Tm = c(58, 60, 62),
                                   name = "Adamc",
                                   primer3=primer3,
                                   )
}




