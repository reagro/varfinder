\name{combine}

\alias{combine_rf}


\title{
Combine reference and field samples marker data
}

\description{
Combine reference and field samples marker data.frames. Each should have the three identifies columns, the first being the marker ID, the second ..., the third ... 
All other columns should be samples (genotypes)
}


\usage{
combine_rf(ref, fld)
}
 
\arguments{
\item{ref}{data.frame with the genetic reference data. Rows are genotypes. Columns have the markers}
\item{fld}{data.frame with the field data. Rows are genotypes. Columns have the markers}
}

\value{
data.frame
}


