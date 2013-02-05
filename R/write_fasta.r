#' Write a fasta file to directory to use for whatever, e.g., running Clustal 
#' 		for multiple sequences alignment. 
#' 
#' @param sequences A list of sequences. 
#' @param fileout A file to write to, e.g., mysequences.fas
#' @param format one of Bayesian or maxlik (for maximum likelihood).
#' @examples \dontrun{
#' # Note that they are all the same sequence
#' myseqs <- c("TCTTATTTACAATAGGAGGATTATCAGGAATTATATTATCAAATTCATCTATTGATATTATACTACACGATACTTATTACGTTATTGGACACTTTCATTATGTACTCTCAATATCTTATTTACAATAGGAGGATTATCAGGAATTATATTATCAAATTCATCTATTGATATTATACTACACGATACTTATTACGTTATTGGACACTTTCATTATGTACTCTCAATA",
#' 	"TCTTATTTACAATAGGAGGATTATCAGGAATTATATTATCAAATTCATCTATTGATATTATACTACACGATACTTATTACGTTATTGGACACTTTCATTATGTACTCTCAATATCTTATTTACAATAGGAGGATTATCAGGAATTATATTATCAAATTCATCTATTGATATTATACTACACGATACTTATTACGTTATTGGACACTTTCATTATGTACTCTCAATA",
#' 	"TCTTATTTACAATAGGAGGATTATCAGGAATTATATTATCAAATTCATCTATTGATATTATACTACACGATACTTATTACGTTATTGGACACTTTCATTATGTACTCTCAATATCTTATTTACAATAGGAGGATTATCAGGAATTATATTATCAAATTCATCTATTGATATTATACTACACGATACTTATTACGTTATTGGACACTTTCATTATGTACTCTCAATA")
#' names(myseqs) <- c("Apis_mellifera","Homo sapiens","Helianthus annuus")
#' write_fasta(sequences=myseqs, fileout="myseqs.fas")
#' }
#' @export
write_fasta <- function (sequences, fileout, format = 'bayesian') 
{
	outfile <- file(description = fileout, open = "w")
	write_oneseq <- function(sequence, name) {
		if(grepl("\\s", name)){name<-gsub("\\s", "_", name)} else{name<-name}
		writeLines(paste(">", name, sep = ""), outfile)
		writeLines(sequence[[1]], outfile)
	}
	write_oneseq_ml <- function(sequence, name) {
		if(grepl("\\s", name)){name<-gsub("\\s", "_", name)} else{name<-name}
		writeLines(paste(name, " ", sequence[[1]], sep = ""), outfile)
	}
	if(format=="bayesian"){
		sapply(seq_len(length(sequences)), function(x) write_oneseq(
			sequence=as.character(sequences[[x]]), name=names(sequences[x])))
		close(outfile)
	} else
	{
		numtaxa <- length(sequences)
		numchars <- nchar(sequences[[1]])
		writeLines(paste(numtaxa, " ", numchars, "\n", sep = ""), outfile)
		sapply(seq_len(length(sequences)), function(x) write_oneseq_ml(
			sequence=as.character(sequences[[x]]), name=names(sequences[x])))
		close(outfile)
	}
}