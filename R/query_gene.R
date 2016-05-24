
#' Wormbase gene summary
#'
#' Using a given list of gene names, the Wormbase REST API is queried
#' to create a data frame containing a description of the gene,
#' and other important information.
#'
#' @param  gene name or list of gene names in C. elegans to search for.
#' @return data frame of five columns: Gene name, gene type (protein, rRNA, etc),
#' genomic location, wormbase gene summary, and references. If the gene is not
#' found in C. elegans, an empty row will be returned.
#'
#' @export

wb_gene <- function(...) {

  genes <- unlist(list(...))

  resultapply <- do.call(rbind, lapply(genes, function(query) {

    req <- httr::GET(paste("http://www.wormbase.org/species/c_elegans/gene/", query,
                           sep = ""), config = httr::content_type_json())

    httr::warn_for_status(req)

    # parse data into result, catch accessing a webpage/gene that doesn't exist=
    result <- tryCatch(httr::content(req)$results[[1]],
                       error = function(e) {
                         message(paste("gene", query, "does not exist"))
                         return(NULL)})
    df <- dplyr::data_frame(Gene=character(), Gene_type=character(), Gene_location=character(), Summary=character())
    df$References <- list()

    if (is.null(result)) {
      #return empty data frame==================================================
    } else if (result$taxonomy$species == "elegans") {
      # Continue here, get overview info========================================
      req <- httr::GET(paste("http://www.wormbase.org/rest/widget/gene/", result$name$id,
                             "/overview", sep = ""), config = httr::content_type_json())
      raw_text <- httr::content(req, "parsed")
      Sum_text <- raw_text$fields$concise_description$data$text
      gene_type <- raw_text$fields$classification$data$type

      if (is.null(Sum_text)) {
        Sum_text <- ""
      }

      # get references==========================================================
      req <- httr::GET(paste("http://www.wormbase.org/rest/widget/gene/", result$name$id,
                             "/references", sep = ""), config = httr::content_type_json())
      refs <- sapply((httr::content(req, "parsed"))$results,function(x) { x$name$id})

      # get gene location=======================================================
      req <- httr::GET(paste("http://www.wormbase.org/rest/widget/gene/", result$name$id, "/location", sep = ""),
                       config = httr::content_type_json())
      loc = gsub("\\.\\.", "-", (httr::content(req, "parsed"))$fields$genomic_position$data[[1]]$label)
      tempdf <- dplyr::data_frame(Gene=query, Gene_type=gene_type, Gene_location=loc, Summary=Sum_text, References=list(refs))

    } else {
      message(paste("gene", query, "not found in C. elegans"))
    }
  }))
  return(resultapply)
}

#' Wormbase gene variants
#'
#' Using a given gene name, the Wormbase REST API is queried to create a data
#' frame containing information on the different variants of the gene, and the
#' associated strains.
#'
#' @param name of C. elegans gene to search for.
#' @return data frame of nine columns: Wormbase variant ID, allele name, isoform ID,
#' locations (exon, intron, etc), molecular change (deletion, subsitution, etc),
#' gene effect (missense, nonsense, etc), amino acid change, amino acid position,
#' and associated strain. All empty fields contain NA as a character. If the gene
#' is not found in C. elegans, an empty data frame will be returned.
#'
#' @export

wb_variants <- function(gene) {

  req <- httr::GET(paste("http://www.wormbase.org/species/c_elegans/gene/", gene, sep = ""),
                   config = httr::content_type_json())

  httr::warn_for_status(req)

  # parse data into result, catch accessing a webpage/gene that doesn't exist===
  result <- tryCatch(httr::content(req)$results[[1]],
                     error = function(e) {
                       message("gene does not exist")
                       return(NULL)})
  # Construct empty dataframe===================================================
  df <- dplyr::data_frame(id=character(), allele=character(), isoform_id=character(), locations=character(),
                          molecular_change=character(), gene_effect=character(), aa_change=character(),
                          aa_position=character(), strain=character())

  # No gene was found, return empty dataframe===================================
  if (is.null(result)) {
    return(df)
  }

  #check if gene is in C. elegans===============================================
  if (result$taxonomy$species == "elegans") {
    # Continue here, get overview info==========================================
    req <- httr::GET(paste("http://www.wormbase.org/rest/widget/gene/", result$name$id,
                           "/genetics", sep = ""), config = httr::content_type_json())
    raw_text <- httr::content(req, "parsed")

    # Clean up data into a tidy data frame======================================
    as.data.frame(do.call(rbind, raw_text$fields$alleles$data)) -> df
    dplyr::rowwise(df) %>%
    dplyr::mutate(gene_effect = ifelse(is.null(effects), as.character(NA), unlist(effects))) %>%
    dplyr::mutate(id = variation[1], allele = variation[2]) %>%
    dplyr::mutate(isoform_id = ifelse(is.null(isoform), as.character(NA), unlist(isoform))) %>%
    dplyr::mutate(aa_change = ifelse(is.null(aa_change), as.character(NA), aa_change)) %>%
    dplyr::mutate(aa_position = ifelse(is.null(aa_position), as.character(NA), aa_position)) %>%
    dplyr::mutate(strain = ifelse(is.null(strain), as.character(NA), unlist(strain)[1])) %>%
    dplyr::select(id, allele, isoform_id, locations, molecular_change, gene_effect, aa_change, aa_position, strain) -> df

  } else {
    # Gene not found in C. elegans, print message and return empty data frame===
    message("gene not found in C. elegans")
  }

  return(df)
}

#' Wormbase gene sequence
#'
#' Using a given gene name, the Wormbase REST API is queried to create a data
#' frame containing a gene's DNA and protein sequence for each isoform.
#'
#' @param name of C. elegans gene to search for
#' @return data frame of three columns: ID of the isoform, gene DNA sequence (exons
#' are uppercase while introns/UTRs are lowercase), and protein sequence. If the gene
#' is not found in C. elegans, an empty data frame will be returned.
#'
#' @export

wb_sequence <- function(gene) {
  req <- httr::GET(paste("http://www.wormbase.org/species/c_elegans/gene/", gene, sep = ""),
                   config = httr::content_type_json())

  httr::warn_for_status(req)

  # parse data into result, catch accessing a webpage/gene that doesn't exist===
  result <- tryCatch(httr::content(req)$results[[1]],
                     error = function(e) {
                       message("gene does not exist")
                       return(NULL)})

  df <- dplyr::data_frame(isoform_id=character(), gene_sequence=character(),
                          protein_sequence=character())

  if (is.null(result)) {
    return(df)
  }

  #check if gene is in C. elegans===============================================
  if (result$taxonomy$species == "elegans") {
    req <- httr::GET(paste("http://www.wormbase.org/rest/widget/gene/", result$name$id,
                           "/sequences", sep = ""), config = httr::content_type_json())
    raw_text <- httr::content(req, "parsed")

    # get all isoforms data=====================================================
    isoforms <- lapply(raw_text$fields$gene_models$data$table, function(x) {
      isoform_id <- x$cds$text$id
      # get sequence============================================================
      req <- httr::GET(paste("http://www.wormbase.org/rest/widget/transcript/",
                             isoform_id, "/sequences", sep = ""),
                       config = httr::content_type_json())
      raw_text <- httr::content(req, "parsed")
      # remove html tags from sequence==========================================
      gsub("<.*?>|\n", "", raw_text$fields$print_sequence$data[[1]]$sequence) -> gene_seq
      return(c(isoform_id, gene_seq, raw_text$fields$print_sequence$data[[3]]$sequence))
    })

    # turn data into data frame=================================================
    nCol <- max(vapply(isoforms, length, 0))
    data <- lapply(isoforms, function(row) c(row, rep(NA, nCol-length(row))))
    data <- matrix(unlist(data), nrow=length(data), ncol=nCol, byrow=TRUE)
    data.frame(data) -> df
    colnames(df) <- c("isoform_id", "gene_sequence", "protein_sequence")
  } else {
    # Gene not found in C. elegans, print message and return empty data frame===
    message("gene not found in C. elegans")
  }

  return(df)
}
