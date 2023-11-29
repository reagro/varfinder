\name{match_rf}

\alias{match_rf}

\title{
Match field samples to reference with IBS
}

\description{
Describe
}


\usage{
match_rf(d, MAF_cutoff, SNP_mr, sample_mr, IBS_cutoff, 
			inb_method="mom.visscher", cpus=1)
}
 
\arguments{
\item{d}{data.frame with combined and recoded reference and field data}
\item{MAF_cutoff}{numeric. Minor Allele Frequency cutoff. Only use makers with a MAF of at least this value. A SNP minor allele frequency of 0.05 is often used in the literature}
\item{SNP_mr}{numeric. Markers with a higher missing rate are not used. A SNP missing rate 0.9 is a low threshold, 0.2 is a high threshold}
\item{sample_mr}{numeric. Sample Missing Rate}
\item{IBS_cutoff}{numeric. Cutoff}
\item{inb_method}{character. Inbreeding method. One of "mom.weir", "mom.visscher", "mle", "gcta1", "gcta2", "gcta3"}
\item{cpus}{positive integer}
}

\value{
list
}

\examples{
fr <- system.file("ex/reference.csv", package="varfinder")
fs <- system.file("ex/survey.csv", package="varfinder")
dref <- data.table::fread(fr)
dsmp <- data.table::fread(fs)
comb <- combine_rf(dref, dsmp)
rec <- recode_rf(comb, biallelic=TRUE, missflags="-")
ibs <- match_rf(rec, MAF_cutoff=0.05, SNP_mr=0.2, sample_mr=0.2, IBS_cutoff=.8)
}




