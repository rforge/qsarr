outliers <- function( ... ){
qsar.data.full=cbind(qsar.activity,qsar.descriptors)
qsar.data.na <- subset(qsar.data.full, select=colMeans(is.na(qsar.data.full)) == 0)
qsar.data.nzvdesc <- caret::nearZeroVar(qsar.data.na)
qsar.data.nzv <- qsar.data.na[, -qsar.data.nzvdesc]

##
qsar.data.correlation <- cor(qsar.data.nzv)
qsar.data.descriptors.correlation.75 <- caret::findCorrelation(qsar.data.correlation, cutoff = .90)
qsar.data.cor75 <- qsar.data.nzv[,-qsar.data.descriptors.correlation.75]

s#pre- process data
preProcValues <- caret::preProcess(qsar.data.cor75, method = c("center", "scale"))
qsar.data.cor <- stats::predict(preProcValues, qsar.data.cor7)
#definir numero de N

#
outlier.scores <- lofactor(qsar.data.cor, k=3)
plot(density(outlier.scores))
outlier.scores.index <- order(outlier.scores, decreasing=T)
hist.outliers<-hist(outlier.scores,main="Distribution of Outliers", col="lightblue")
hist.outliers <- as.data.frame(hist.outliers$counts)
freq.outliers <- print(hist.outliers)
###frequencia menor que 10%
n.outlier <- nrow(qsar.data.cor)*0.05
n.outlier.remove <- sum(hist.outliers[hist.outliers<n.outlier]) 
outlier.index <- order(outlier.scores, decreasing=T)[1:n.outlier.remove]
molecules.outliers <- print(outlier.index)
###plot PCA com autliers
outliers <- outlier.index 
n <- nrow(qsar.data.cor)
labels <- 1:n
labels[-unlist(outliers)] <- "."
biplot(prcomp(qsar.data.cor), cex=.8, xlabs=labels)
outlier.mat <- qsar.data.cor[c(-outlier.index),]
outlier.mat.a <- qsar.data.cor9[c(-outlier.index),]
### falta colocar quando varicao =0
qsar.activity.backup <- qsar.activity
qsar.activity <- outlier.mat.a[,1]
outlier.mat.n <- ncol(outlier.mat)
qsar.descriptors.cor <- outlier.mat[,2:outlier.mat.n]
#####exporting
.GlobalEnv[["qsar.activity.backup"]] <- qsar.activity.backup
.GlobalEnv[["qsar.activity"]] <- qsar.activity
.GlobalEnv[["qsar.descriptors.cor"]] <- qsar.descriptors.cor
}