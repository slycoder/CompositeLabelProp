process.data.frame <- function() {
  df <- read.delim(pipe('pbpaste'), dec=',')
  df$X <- factor(
    df$X, 
    levels=c(
      "Hometown", 
      "Current city", 
      "High school", 
      "College", 
      "Employer"
    )
  )  
  rownames(df) <- df$X
  df <- df[,-1]
  colnames(df) <- gsub('^[^0-9]*', '', colnames(df))
  df
}
recall.at.1.over.lp <- process.data.frame()
recall.at.3.over.lp <- process.data.frame()

recall.at.1.over.baseline <- process.data.frame()
recall.at.3.over.baseline <- process.data.frame()

recall.at.1.effect.alpha <- process.data.frame()

running.time <- read.delim(pipe("pbpaste"), sep="\t")
rownames(running.time) <- running.time$X
running.time <- running.time[-1]

prob.correct <- read.delim(pipe("pbpaste"), dec=',')
rownames(prob.correct) <- prob.correct[,1]
prob.correct <- prob.correct[,-1]
prob.correct <- prob.correct[,c(1,4,2,5,3)]
colnames(prob.correct) <- gsub('\\.', ' ', colnames(prob.correct))

save(prob.correct, running.time, 
     recall.at.1.over.lp, recall.at.3.over.lp,
     recall.at.1.over.baseline, recall.at.3.over.baseline,
     recall.at.1.effect.alpha, file="label_prop.Rdata")
