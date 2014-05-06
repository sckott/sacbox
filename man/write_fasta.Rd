\name{write_fasta}
\alias{write_fasta}
\title{Write a fasta file to directory to use for whatever, e.g., running Clustal
		for multiple sequences alignment.}
\usage{
write_fasta(sequences, fileout, format = "bayesian")
}
\arguments{
  \item{sequences}{A list of sequences.}

  \item{fileout}{A file to write to, e.g., mysequences.fas}

  \item{format}{one of Bayesian or maxlik (for maximum
  likelihood).}
}
\description{
Write a fasta file to directory to use for whatever, e.g.,
running Clustal for multiple sequences alignment.
}
\examples{
\dontrun{
# Note that they are all the same sequence
myseqs <- c("TCTTATTTACAATAGGAGGATTATCAGGAATTATATTATCAAATTCATCTATTGATATTATACTACACGATACTTATTACGTTATTGGACACTTTCATTATGTACTCTCAATATCTTATTTACAATAGGAGGATTATCAGGAATTATATTATCAAATTCATCTATTGATATTATACTACACGATACTTATTACGTTATTGGACACTTTCATTATGTACTCTCAATA",
	"TCTTATTTACAATAGGAGGATTATCAGGAATTATATTATCAAATTCATCTATTGATATTATACTACACGATACTTATTACGTTATTGGACACTTTCATTATGTACTCTCAATATCTTATTTACAATAGGAGGATTATCAGGAATTATATTATCAAATTCATCTATTGATATTATACTACACGATACTTATTACGTTATTGGACACTTTCATTATGTACTCTCAATA",
	"TCTTATTTACAATAGGAGGATTATCAGGAATTATATTATCAAATTCATCTATTGATATTATACTACACGATACTTATTACGTTATTGGACACTTTCATTATGTACTCTCAATATCTTATTTACAATAGGAGGATTATCAGGAATTATATTATCAAATTCATCTATTGATATTATACTACACGATACTTATTACGTTATTGGACACTTTCATTATGTACTCTCAATA")
names(myseqs) <- c("Apis_mellifera","Homo sapiens","Helianthus annuus")
write_fasta(sequences=myseqs, fileout="myseqs.fas")
}
}

