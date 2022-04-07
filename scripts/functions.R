
# Absolute loss function --------------------------------------------------

abs_loss <- function(p){
  d <- seq(0,1, length.out=length(p))
  sapply(d, function(d) sum(abs(d - p)/sum(p)))
}
plot_abs_loss <- function(loss, samples){
  d <- seq(0,1, length.out=length(loss))
  y <- min(loss) ; x <- d[which.min(loss)] 
  plot(d, loss, type="l") ; points(x,y, pch=20)
  text(x, y+0.1, paste0("(",round(x, digits = 2),",",round(y, digits = 2),")"))
  abline(v=median(samples), lty=2)
  abline(v=mean(samples), lty=3)
  legend("bottomleft", legend = c("median", "mean"), lty = c(2,3))
}

loss <- abs_loss(samples)
# Check: vale = median
plot_abs_loss(loss)

# Quadratric loss function ------------------------------------------------

quad_loss <- function(p){
  d <- seq(0,1, length.out=length(p))
  sapply(d, function(d) sum((d - p)^2/sum(p)))
}
plot_quad_loss <- function(loss, samples){
  d <- seq(0,1, length.out=length(loss))
  y <- min(loss) ; x <- d[which.min(loss)] 
  plot(d, loss, type="l") ; points(x,y, pch=20)
  text(x, y+0.1, paste0("(",round(x, digits = 2),",",round(y, digits = 2),")"))
  abline(v=median(samples), lty=2)
  abline(v=mean(samples), lty=3)
  legend("bottomleft", legend = c("median", "mean"), lty = c(2,3))
}



