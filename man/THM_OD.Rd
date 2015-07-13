\name{THM_OD}
\alias{THM_OD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
THM_OD(amount = 375000, rate = 0.2145, f_monthly = 200, f_disburs = 0.01, mode = "value")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{amount}{
%%     ~~Describe \code{amount} here~~
}
  \item{rate}{
%%     ~~Describe \code{rate} here~~
}
  \item{f_monthly}{
%%     ~~Describe \code{f_monthly} here~~
}
  \item{f_disburs}{
%%     ~~Describe \code{f_disburs} here~~
}
  \item{mode}{
%%     ~~Describe \code{mode} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (amount = 375000, rate = 0.2145, f_monthly = 200, f_disburs = 0.01, 
    mode = "value") 
{
    res <- matrix(0, nrow = 12, ncol = 3)
    res[1, 1] <- amount
    for (i in 1:12) {
        res[i, 2] <- res[i, 1] * (1 + rate/12) + f_monthly
        if (i == 12) {
            res[i, 3] <- res[i, 2]
        }
        else {
            res[i + 1, 1] <- res[i, 2]
            res[i, 3] <- 0
        }
    }
    res[12, 3] <- res[12, 2]
    THM <- IRR(amount, res[, 3])
    if (mode == "table") {
        return(res)
    }
    else {
        return(THM)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
