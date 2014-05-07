require(ggplot2)
require(reshape)
require(RColorBrewer)

require(gplots)

load(file="label_prop.Rdata")

my.barplot <- function(df, title, legend.title, fixed.legend=T) {
  
  op <- par(mar=c(5, 6, 4, 2) + 0.1)
  
  if (fixed.legend) {
    ylim <- c(0, 1)  
  } else {
    vals <- as.vector(as.matrix(df))
    ylim <- range(pretty(c(vals * 1.04, vals / 1.04)))
  }
  
  # First get the grid in the background.
  barplot2(
    t(as.matrix(df)),
    beside=T,
    ylim=ylim,
    col="white",
    plot.grid=TRUE,
    axes=F,
    axisnames=F
  )

  n <- ncol(df)
  angle <- 0 + 45 * 1:n
  density <- rep(30, n)
  col <- c(brewer.pal(5, "Set1")[-5], "darkgoldenrod")
  col <- col[seq(col)%%5 + 1]
  if (n == 5) {
    angle <- c(0, angle[-n])
    density <- c(100, density[-n]) 
  } else {
    col <- col[-1]
  }
    # Plot for real.
  barplot2(
    t(as.matrix(df)), 
    beside=T, 
    ylim=ylim,
    col = col,
    angle = angle,
    density = density, 
    ylab = title, 
    add=T,
    yaxt="n"
  )
  box()
  ticks <- round(axTicks(2), 2)
  axis(2, at=ticks, labels = paste(ticks * 100, "%", sep=""), las=1)

  legend(
    x="topleft",
    inset = 0.04,
    legend=colnames(df), 
    angle=angle, density=density,
    fill = col,
    xjust=1,
    yjust=1,
    title=legend.title,
    bg="white"
  )
}

pdf("~/label_dotprod/recall_at_1_versus_baseline.pdf", height=4.6, width=7)
my.barplot(recall.at.1.over.baseline[,-1], 
           "Lift of Edge-Explain over K=20\n",
           legend.title="K")
dev.off()


pdf("~/label_dotprod/recall_at_3_versus_baseline.pdf", height=4.6, width=7)
my.barplot(recall.at.3.over.baseline[,-1], 
           "Lift of Edge-Explain over K=20\n",
           legend.title="K")
dev.off()


pdf("~/label_dotprod/recall_at_1_versus_LP.pdf", height=4.6, width=7)
my.barplot(recall.at.1.over.lp, 
           "Lift of Edge-Explain over\nLabel Propagation\n",
           legend.title="K",
           fixed.legend=F)
dev.off()


pdf("~/label_dotprod/recall_at_3_versus_LP.pdf", height=4.6, width=7)
my.barplot(recall.at.3.over.lp, 
           "Lift of Edge-Explain over\nLabel Propagation\n",
           legend.title="K",
           fixed.legend=F)
dev.off()


pdf("~/label_dotprod/effectAlpha.pdf", height=4.6, width=7)
my.barplot(recall.at.1.effect.alpha, 
           expression(atop(paste(plain("Lift of Edge-Explain over "), 
                      alpha==0.1),
                      phantom("0"))),
           legend.title=expression(alpha),
           fixed.legend=F)
dev.off()

multi.line.plot <- function(df, xlab, ylab) {
  op <- par(mar=c(5, 6, 4, 2) + 0.1)
  
  xvals <- as.numeric(gsub('^X', '', colnames(df)))
  cols <- c(brewer.pal(5, "Set1")[-5], "darkgoldenrod")
  plot(
    rep(xvals, nrow(df)), 
    as.vector(as.matrix(df)),
    type="p",
    pch="",
    las=1,
    xlab=xlab,
    ylab=ylab
  )

  grid(col="black")
  ltys = c(1,2,4,5,6)
  for (ii in 1:nrow(df)) {
    lines(
      xvals,
      df[ii,],
      type="b",
      col=cols[ii],
      lwd=3,
      lty=ltys[ii],
      pch=ii
    )
  }

  legend(
    x="topleft",
    inset = 0.04,
    legend=rownames(df), 
    lty=ltys[1:nrow(df)],
    pch=1:nrow(df),
    col=cols[1:nrow(df)],
    xjust=1,
    yjust=1,
    lwd=2,
    bg="white"
  )
}


pdf("~/label_dotprod/runningTime.pdf", height=4.6, width=7)
multi.line.plot(
  running.time,
  ylab="Running time (minutes)",
  xlab="Number of friends K"
)
dev.off()


pdf("~/label_dotprod/prob_correct_inference.pdf", height=4.6, width=7)
multi.line.plot(
  t(prob.correct)[,1:5],
  xlab="Fraction of friends sharing user's true label",
  ylab="Probability of correct inference"
)
dev.off()
