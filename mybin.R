#' Function for Binomial Distribution
#'
#' @param iter interation
#' @param n size
#' @param p p value
#'
#' @return mybin function from lab 5
#' @export
#'
#' @examples
#' mybin(iter = 800, n = 10, p = .5)
mybin=function(iter=100,n=10, p=0.5){
  sam.mat=matrix(NA,nr=n,nc=iter, byrow=TRUE)
  succ=c()
  for( i in 1:iter){
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ,levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation",
          xlab="Number of successes")
  succ.tab/iter
}
