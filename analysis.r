
# RUN: source("analysis.r")

# Load data
dirname     = dirname(sys.frame(1)$ofile)
filename    = paste(dirname,"/investigation.csv", sep="")
logname     = paste(dirname,"/analysis.log", sep="")
outname     = paste(dirname,"/analysis.csv", sep="")
pdfname     = paste(dirname,"/analysis.pdf", sep="")
data        = read.csv(filename, sep=";", dec=",")
data$simp   = data$eq/data$sym

# All output also to logfile
sink(logname, split=TRUE)

# Apply filters to the data
filt1       = subset(data,  vp != "V8b")   
filt2       = subset(filt1, filter < 0.5)

# Run some descriptive statistics
library(psych)
cat("\nDescriptive statistics of considered cases\n")
print(describe(filt2))
cat("\nDescriptive statistics of means of considered subjects\n")
print(describe(aggregate(cbind(exp, time) ~ vp, FUN=mean, data=filt1)))

# Check correlations of independents
cat("\nCorrelations of independents\n")
print(cor(cbind(filt2$sym, filt2$eq, filt2$lvls)))

# Check that experience has an influence
cat("\nTesting for significant exp*sym effects\n")
lmExp = lm(time ~ exp*sym, data=filt2)
print(summary(lmExp))

# Create regression object with repeated measures (mean and sym-slopes per subject)
library(nlme)
lmNull      = lme(           time ~ 1,           random = ~(0+sym) | vp, data = filt2, method = "ML")
lmLvls      = update(lmNull, time ~ 1+lvls)

# Output summaries
cat("\nCoefficients of model with levels\n")
print(coef(lmLvls))

# Comparison of models
cat("\nSignificance tests\n")
print(summary(lmLvls))

# Data for plots
data2 = aggregate(cbind(exp, time) ~ vp, FUN=mean, data=filt1)
rownames(data2) = data2$vp
data2 = data.frame(data2, coef(lmLvls))
write.table(data2, file=outname, sep=";", dec=".", row.names=FALSE, quote=FALSE)

# Do the plots
par(mfrow=c(2,1))
plot(data2$exp, data2$time)
plot(data2$exp, data2$sym)
dev.copy2pdf(device=dev.cur(), file=pdfname)

# Reset output redirection
sink()