\name{THM_CC}
\alias{THM_CC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
THM_CC(limit = 1e+05, mindue = 0.05, rate = 0.33, f_monthly = 200, f_yearly = 0, mode = "value")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{limit}{
%%     ~~Describe \code{limit} here~~
}
  \item{mindue}{
%%     ~~Describe \code{mindue} here~~
}
  \item{rate}{
%%     ~~Describe \code{rate} here~~
}
  \item{f_monthly}{
%%     ~~Describe \code{f_monthly} here~~
}
  \item{f_yearly}{
%%     ~~Describe \code{f_yearly} here~~
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
function (limit = 1e+05, mindue = 0.05, rate = 0.33, f_monthly = 200, 
    f_yearly = 0, mode = "value") 
{
    res <- matrix(0, nrow = 12, ncol = 4)
    res[1, 1] = limit - f_yearly
    for (i in 1:12) {
        res[i, 2] <- res[i, 1] * rate/12
        res[i, 3] <- res[i, 1] + res[i, 2] + f_monthly
        if (i == 12) {
            res[i, 4] <- res[i, 3]
        }
        else {
            res[i, 4] <- res[i, 3] * mindue
            res[i + 1, 1] <- res[i, 3] - res[i, 4]
        }
    }
    colnames(res) <- c("Start_amount", "Interest", "End_amount", 
        "Mindue")
    THM <- IRR(res[1, 1], res[, 4])
    return(if (mode == "table") {
        return(res)
    } else {
        return(round(THM, 4))
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
