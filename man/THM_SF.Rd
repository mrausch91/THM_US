\name{THM_SF}
\alias{THM_SF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
THM_SF(amount = 5e+05, rate = 0.173, tenor = 12, f_monthly = 0, f_disburs = 20000, mode = "value")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{amount}{
%%     ~~Describe \code{amount} here~~
}
  \item{rate}{
%%     ~~Describe \code{rate} here~~
}
  \item{tenor}{
%%     ~~Describe \code{tenor} here~~
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
function (amount = 5e+05, rate = 0.173, tenor = 12, f_monthly = 0, 
    f_disburs = 20000, mode = "value") 
{
    return(THM_PL(amount - f_disburs, rate, tenor, f_monthly, 
        f_disburs = 0, mode))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
