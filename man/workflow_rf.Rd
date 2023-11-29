\name{workflow_rf}

\alias{workflow_rf}

\title{
Single workflow
}

\description{
High level function that reads raw input files, and runs \code{\link{combine_rf}}, \code{\link{recode_rf}}, \code{\link{workflow_rf}} and writes the output to an excel file
}


\usage{
workflow_rf(ref_file, fld_file, country, crop, year="", outdir=".", 
		MAF_cutoff=0.05, SNP_mr=0.2, sample_mr=0.2, IBS_cutoff=.8, 
		biallelic=TRUE, missflags="-")
}
 
\arguments{
\item{ref_file}{logical}
\item{fld_file}{character}
\item{country}{character}
\item{crop}{character}
\item{year}{character}
\item{outdir}{character}
\item{biallelic}{logical}
\item{missflag}{character}

\item{MAF_cutoff}{numeric. Minor Allele Frequency cutoff. Only use makers with a MAF of at least this value. A SNP minor allele frequency of 0.05 is often used in the literature}
\item{SNP_mr}{numeric. Markers with a higher missing rate are not used. A SNP missing rate 0.9 is a low threshold, 0.2 is a high threshold}
\item{sample_mr}{numeric. Sample Missing Rate}
\item{IBS_cutoff}{numeric. Cutoff}
\item{inb_method}{character. Inbreeding method. One of "mom.weir", "mom.visscher", "mle", "gcta1", "gcta2", "gcta3"}
\item{cpus}{positive integer}
}

\value{
The filename written to disk
}


