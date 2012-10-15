#' Write a fasta file to directory to use for whatever, e.g., running Clustal 
#' 		for multiple sequences alignment. 
#' 
#' @param sequences A list of sequences. 
#' @param fileout A file to write to, e.g., mysequences.fas
#' @examples \dontrun{
#' # Note that they are all the same sequence
#' myseqs <- c("TCTTATTTACAATAGGAGGATTATCAGGAATTATATTATCAAATTCATCTATTGATATTATACTACACGATACTTATTACGTTATTGGACACTTTCATTATGTACTCTCAATA",
#' 		"TCTTATTTACAATAGGAGGATTATCAGGAATTATATTATCAAATTCATCTATTGATATTATACTACACGATACTTATTACGTTATTGGACACTTTCATTATGTACTCTCAATA",
#' 		"TCTTATTTACAATAGGAGGATTATCAGGAATTATATTATCAAATTCATCTATTGATATTATACTACACGATACTTATTACGTTATTGGACACTTTCATTATGTACTCTCAATA")
#' names(myseqs) <- c("Apis_mellifera","Homo sapiens","Helianthus annuus")
#' write_fasta(myseqs, "myseqs.fas")
#' }
#' @export
write_fasta <- function (sequences, fileout) 
{
	outfile <- file(description = fileout, open = "w")
	write.oneseq <- function(sequence, name) {
		if(grepl("\\s", name)){name<-gsub("\\s", "_", name)} else{name<-name}
		writeLines(paste(">", name, sep = ""), outfile)
		writeLines(sequence[[1]], outfile)
	}
	sapply(seq_len(length(sequences)), function(x) write.oneseq(
		sequence=as.character(sequences[[x]]), name=names(sequences[x])))
	close(outfile)
}