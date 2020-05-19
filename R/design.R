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
#' @param ensembl_host Specify the host for ensembl. Defaults to "http://uswest.ensembl.org/".
#' @param ensembl_dataset Specify the datasdets to be accessed by ensembl. Defaults to "hsapiens_gene_ensembl"
#' @importFrom magrittr %>%
#' @return A set of primer pairs
#' @examples
#' single_primer_design(ncbi="NC_045512", primer3="/Users/adamcribbs/miniconda3/bin/primer3_core")
#' single_primer_design(transcript_id_version="ENST00000376207.10", primer3="/Users/adamcribbs/miniconda3/bin/primer3_core")
#' @export
single_primer_design <- function(ncbi=NULL, transcript_id_version=NULL,
                                 primer3=NULL,
                                 thermo.param=system.file("extdata/primer3_config/", package="scAmpDesign"),
                                 settings=system.file("extdata/primer3_settings.txt", package="scAmpDesign"),
                                 ensembl_host="http://uswest.ensembl.org/",
                                 dataset="hsapiens_gene_ensembl"){

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
    seq <- ensembl_primer_design_input(id=transcript_id_version, host=ensembl_host, dataset=dataset)
  }
  primer3_output = .callP3NreadOrg(seq = seq,
                                   Tm = c(58, 60, 62),
                                   name = "Adamc",
                                   primer3=primer3,
                                   thermo.param=thermo.param,
                                   settings=settings
                                   )
}



#' Initiate the primer design for a single gene - will return multiple primers.
#'
#' This function will design a primer set to use with targeted single-cell sequencing.
#'
#' @param ncbi An NBCI Reference. For example, the SARS-COV2 is NC_045512.
#' @param transcript_id_version An Ensembl transcript id with version number. For example,
#'   the main transcript for FOXP3 is ENST00000376207.10.
#' @param primer3 The location of primer3_core. Example:  primer3='/Users/adamcribbs/miniconda3/bin/primer3_core'
#' @param thermo.param The location of the thermodynamic parameters folder. Currently within the R inst/extdata
#' @param settings text file for parameters for primer 3. Currently within the inst/extdata
#' @param ensembl_host Specify the host for ensembl. Defaults to "http://uswest.ensembl.org/".
#' @param ensembl_dataset Specify the datasdets to be accessed by ensembl. Defaults to "hsapiens_gene_ensembl"
#' @param num_primers Specify the number of primers to return. Defaults to 5.
#' @return A set of primer pairs
#' @examples
#' multiple_primer_design(ncbi="NC_045512", primer3="/Users/adamcribbs/miniconda3/bin/primer3_core")
#' multiple_primer_design(transcript_id_version="ENST00000376207.10", primer3="/Users/adamcribbs/miniconda3/bin/primer3_core")
#' @export
multiple_primer_design <- function(ncbi=NULL, transcript_id_version=NULL,
                                 primer3=NULL,
                                 thermo.param=system.file("extdata/primer3_config/", package="scAmpDesign"),
                                 settings=system.file("extdata/primer3_settings.txt", package="scAmpDesign"),
                                 ensembl_host="http://uswest.ensembl.org/",
                                 dataset="hsapiens_gene_ensembl",
                                 num_primers=5){

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
    seq <- ensembl_primer_design_input(id=transcript_id_version, host=ensembl_host, dataset=dataset)
  }

  output <- list()

  excluded_regions <- ""

  for (i in seq(num_primers)){

    primer3_output = .callP3NreadOrg(seq = seq,
                                     Tm = c(58, 60, 62),
                                     name = "Adamc",
                                     primer3=primer3,
                                     thermo.param=thermo.param,
                                     settings=settings,
                                     excluded_regions=excluded_regions)

    output[[i]] <- primer3_output

    right_primer_pos <- primer3_output %>%
      select(PRIMER_RIGHT_pos) %>% unlist %>%  as.integer()

    right_primer_len <- primer3_output %>%
      select(PRIMER_RIGHT_len) %>% unlist %>%  as.integer()

    excluded_regions <- paste0(excluded_regions, sprintf(" %s,%s", right_primer_pos, right_primer_len))
  }

  print(output)
  final_output <- dplyr::bind_rows(output)

  return(final_output)
}


