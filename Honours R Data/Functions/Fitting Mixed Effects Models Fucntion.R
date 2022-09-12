
## to make nice even versions of variabes for plotting
seq_func<-function(x)(seq(min(x, na.rm=T), max(x, na.rm=T), length.out=100))


### plot with 95% CI using lmer objects
lmer.predict<-function(mod, newdat, se.mult, binom=NULL, poisson=NULL){
pvar1 <- diag(as.matrix(newdat) %*% tcrossprod(vcov(mod),as.matrix(newdat)))
newdat$y<- as.matrix(newdat) %*% fixef(mod)  
newdat <- data.frame(newdat, plo = newdat$y-(se.mult*sqrt(pvar1)), phi = newdat$y+(se.mult*sqrt(pvar1)))

## if you have used binomial errors then this will back transform logits to the probability scale
if(binom==T) {
		newdat$y<-plogis(newdat$y); newdat$plo<-plogis(newdat$plo); newdat$phi<-plogis(newdat$phi)
		} else 

## if you have used poisson errors or have log-transformed your response, then this will back transform to the original scale (e.g. abundance)
if(poisson==T) {
		newdat$y<-exp(newdat$y); newdat$plo<-exp(newdat$plo); newdat$phi<-exp(newdat$phi)
		} 
return(with(newdat, data.frame(y, phi, plo)))
}


plot.CI.func<- function(x.for.plot, pred, upper, lower, env.colour, env.trans=NA, line.colour, line.weight, line.type){
colour.rgb<-col2rgb(col=env.colour)  
polygon.coords<-data.frame(rbind(cbind(x.for.plot[1], lower[1]), 
							cbind(x.for.plot, upper), 
							cbind(x.for.plot, lower)[rev(order(x.for.plot)),]))
names(polygon.coords)<-c("x", "y")							
polygon(polygon.coords$x, polygon.coords$y, col=rgb(red=colour.rgb["red",],blue=colour.rgb["blue",], green=colour.rgb["green",] , alpha=env.trans, maxColorValue = 255), border=NA)
lines(x.for.plot, pred, col=line.colour, lwd=line.weight, lty=line.type)         
} 




