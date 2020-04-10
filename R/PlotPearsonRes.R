#' Plot Pearson residuals for SS runs
#' 
#' \code{PlotPearsonRes} This function makes Pearson residual plots for a specified fishery
#' 
#' @export

PlotPearsonRes<-function(Rep=Rep, myFleet=myFleet, Path=Path){
  
  lendat <- Rep$lendbase
  tmp<- lendat %>% filter(Kind=="LEN",Fleet==myFleet)
  boxplot(split(tmp$Pearson, tmp$Bin), xlab="Length class (quarter)", ylab="Pearson residuals",ylim=c(-4,4))
  boxplot(split(tmp$Pearson, tmp$Yr), xlab="Length class (quarter)", ylab="Pearson residuals",ylim=c(-4,4))
  
  png(paste0(Path,"PearsonRes.png"), width = 5000, height = 4000, res = 600)
  par(mfrow=c(2,2), mar=c(4,4,2,2))
  layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), 
         widths=c(3,1), heights=c(1,1))
  
  boxplot(split(tmp$Pearson, tmp$Yr/4+1974.75), xlab="Year", range = 0.000001, ylab="Pearson residuals",ylim=c(-0.5,0.5), outline = FALSE)
  lines(range(tmp$Bin,-1,50), rep(0,2), lty=3, col=2)
  
  boxplot(split(tmp$Pearson, tmp$Bin), xlab="Length class (cm)", range = 0.000001, ylab="Pearson residuals",ylim=c(-0.5,0.5), outline = FALSE)
  lines(range(tmp$Bin,-1,50), rep(0,2), lty=3, col=2)
  #boxplot(split(tmp$Pearson, tmp$YearClass), xlab="Year class", ylab="Pearson residuals")
  #lines(range(tmp$Bin,-1,150), rep(0,2), lty=3, col=2)
  
  #Bottom panel is the normal quantile-quantile plot for residuals, with
  #the 1:1 line, horizontal lines give the 5, 25, 50, 75, and 95 percentiles.
  qqnorm(y=tmp$Pearson, main="")
  qqline(y=tmp$Pearson)
  q1 <- quantile(tmp$Pearson,0.05)
  lines(c(-5,5), rep(q1,2), lty=2)
  q1 <- quantile(tmp$Pearson,0.25)
  lines(c(-5,5), rep(q1,2), lty=2)
  q1 <- quantile(tmp$Pearson,0.5)
  lines(c(-5,5), rep(q1,2), lty=2)
  q1 <- quantile(tmp$Pearson,0.75)
  lines(c(-5,5), rep(q1,2), lty=2)
  q1 <- quantile(tmp$Pearson,0.95)
  lines(c(-5,5), rep(q1,2), lty=2)
  
  dev.off()
}

