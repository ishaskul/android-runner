rm(list=ls())
library(ggplot2)
library(effsize)
library(gridExtra)

# loading data
data = read.table("./final_table.csv", header = TRUE, sep = ",", dec = ".")

data$site = as.factor(data$url)
data$crit = as.factor(data$crit)
data$browser = as.factor(data$browser)

# Data exploration

# Descriptive statistics
by(data[data$browser=="chrome", ], data[data$browser=="chrome", ]$crit, summary)

by(data[data$browser=="firefox", ], data[data$browser=="firefox", ]$crit, summary)

by(data[data$browser=="opera", ], data[data$browser=="firefox", ]$crit, summary)

get_std <- function(data, browser, crit) {
  print(sd(data[which(data$browser == browser & data$crit==crit), ]$fid))
  print(sd(data[which(data$browser == browser & data$crit==crit), ]$lcp))
  print(sd(data[which(data$browser == browser & data$crit==crit), ]$cls))
  print(sd(data[which(data$browser == browser & data$crit==crit), ]$e))
}

get_std(data, "chrome", 'no')
get_std(data, "firefox", 'no')
get_std(data, "opera", 'no')
get_std(data, "chrome", 'std')
get_std(data, "firefox", 'std')
get_std(data, "opera", 'std')
get_std(data, "chrome", 'strict')
get_std(data, "firefox", 'strict')
get_std(data, "opera", 'strict')

# Box plots
p <- ggplot(data, aes(x = crit, y=fid, group=crit)) +
  geom_boxplot() + xlab("Privacy Preserving Settings") + ylab("FID (ms)") + ggtitle("FID (ms), Privacy Preserving Settings not applied and applied") + theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
p1 = p + stat_summary(fun=mean, geom="point", shape=23, size=4) + theme(text = element_text(size = 15)) + scale_x_discrete(labels=c("no", "std", "strict"))

p <- ggplot(data, aes(x = crit, y=lcp, group=crit)) +
  geom_boxplot() + xlab("Privacy Preserving Settings") + ylab("LCP (ms)") + ggtitle("LCP (ms), Privacy Preserving Settings not applied and applied") + theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold")) +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
p2 = p + stat_summary(fun=mean, geom="point", shape=23, size=4) + theme(text = element_text(size = 15)) + scale_x_discrete(labels=c("no", "std", "strict"))

p <- ggplot(data, aes(x = crit, y=cls, group=crit)) +
  geom_boxplot() + xlab("Privacy Preserving Settings") + ylab("CLS") + ggtitle("CLS (ms), Privacy Preserving Settings not applied and applied") + theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
p3 = p + stat_summary(fun=mean, geom="point", shape=23, size=4) + theme(text = element_text(size = 15)) + scale_x_discrete(labels=c("no", "std", "strict"))

p <- ggplot(data, aes(x = crit, y=e, group=crit)) +
  geom_boxplot() + xlab("Privacy Preserving Settings") + ylab("Energy (J)") + ggtitle("Energy (J), Privacy Preserving Settings not applied and applied") + theme(plot.title = element_text(size=13, hjust = 0.5, face = "bold"))
p4 = p + stat_summary(fun=mean, geom="point", shape=23, size=4) + theme(text = element_text(size = 15)) + scale_x_discrete(labels=c("no", "std", "strict"))

grid.arrange(p1, p2, p3, p4, ncol=2)

################################################ Density plots fid lcp cls e
#Chrome
data$lcp = as.numeric(data$lcp)
data$fid = as.numeric(data$fid)
data$cls = as.numeric(data$cls)

par("mar")
par(mfrow=c(4,2))
par(mar=c(2,2,2,2))
plot(density(data[which(data$browser == "chrome" & data$crit=='no'), ]$fid), 
     xlab="Load time (ms)", ylim=c(0,0.03), col="blue", 
     main="Density plot for FID in Chrome",
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
lines(density(data[which(data$browser == "chrome" & data$crit=='std'), ]$fid), col="red")
lines(density(data[which(data$browser == "chrome" & data$crit=='strict'), ]$fid), col="green")
legend("topright",
       c("no", "std", "strict"),
       fill=c("red","blue", "green"),
       cex=1
)

plot(density(data[which(data$browser == "chrome" & data$crit=='no'), ]$lcp), 
     xlab="Load time (ms)", ylim=c(0,0.001), col="blue", 
     main="Density plot for LCP in Chrome",
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
lines(density(data[which(data$browser == "chrome" & data$crit=='std'), ]$lcp), col="red")
lines(density(data[which(data$browser == "chrome" & data$crit=='strict'), ]$lcp), col="green")
legend("topright",
       c("no", "std", "strict"),
       fill=c("red","blue", "green"),
       cex=1
)

plot(density(data[which(data$browser == "chrome" & data$crit=='no'), ]$cls), 
     xlab="CLS (ms)", ylim=c(0,1000), col="blue", 
     main="Density plot for CLS in Chrome",
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
lines(density(data[which(data$browser == "chrome" & data$crit=='std'), ]$cls), col="red")
lines(density(data[which(data$browser == "chrome" & data$crit=='strict'), ]$cls), col="green")
legend("topright",
       c("no", "std", "strict"),
       fill=c("red","blue", "green"),
       cex=1
)

plot(density(data[which(data$browser == "chrome" & data$crit=='no'), ]$e), 
     xlab="Energy (J)", ylim=c(0,0.03), col="blue", 
     main="Density plot for energy in Chrome",
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
lines(density(data[which(data$browser == "chrome" & data$crit=='std'), ]$e), col="red")
lines(density(data[which(data$browser == "chrome" & data$crit=='strict'), ]$e), col="green")
legend("topright",
       c("no", "std", "strict"),
       fill=c("red","blue", "green"),
       cex=1
)

# Firefox
plot(density(data[which(data$browser == "firefox" & data$crit=='no'), ]$fid), 
     xlab="Load time (ms)", ylim=c(0,0.03), col="blue", 
     main="Density plot for FID in firefox",
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
lines(density(data[which(data$browser == "firefox" & data$crit=='std'), ]$fid), col="red")
lines(density(data[which(data$browser == "firefox" & data$crit=='strict'), ]$fid), col="green")
legend("topright",
       c("no", "std", "strict"),
       fill=c("red","blue", "green"),
       cex=1
)

plot(density(data[which(data$browser == "firefox" & data$crit=='no'), ]$lcp), 
     xlab="Load time (ms)", ylim=c(0,0.001), col="blue", 
     main="Density plot for LCP in firefox",
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
lines(density(data[which(data$browser == "firefox" & data$crit=='std'), ]$lcp), col="red")
lines(density(data[which(data$browser == "firefox" & data$crit=='strict'), ]$lcp), col="green")
legend("topright",
       c("no", "std", "strict"),
       fill=c("red","blue", "green"),
       cex=1
)

plot(density(data[which(data$browser == "firefox" & data$crit=='no'), ]$cls), 
     xlab="CLS (ms)", ylim=c(0,1000), col="blue", 
     main="Density plot for CLS in firefox",
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
lines(density(data[which(data$browser == "firefox" & data$crit=='std'), ]$cls), col="red")
lines(density(data[which(data$browser == "firefox" & data$crit=='strict'), ]$cls), col="green")
legend("topright",
       c("no", "std", "strict"),
       fill=c("red","blue", "green"),
       cex=1
)

plot(density(data[which(data$browser == "firefox" & data$crit=='no'), ]$e), 
     xlab="Energy (J)", ylim=c(0,0.03), col="blue", 
     main="Density plot for energy in firefox",
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
lines(density(data[which(data$browser == "firefox" & data$crit=='std'), ]$e), col="red")
lines(density(data[which(data$browser == "firefox" & data$crit=='strict'), ]$e), col="green")
legend("topright",
       c("no", "std", "strict"),
       fill=c("red","blue", "green"),
       cex=1
)

# Opera
plot(density(data[which(data$browser == "opera" & data$crit=='no'), ]$fid), 
     xlab="Load time (ms)", ylim=c(0,0.03), col="blue", 
     main="Density plot for FID in opera",
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
lines(density(data[which(data$browser == "opera" & data$crit=='std'), ]$fid), col="red")
lines(density(data[which(data$browser == "opera" & data$crit=='strict'), ]$fid), col="green")
legend("topright",
       c("no", "std", "strict"),
       fill=c("red","blue", "green"),
       cex=1
)

plot(density(data[which(data$browser == "opera" & data$crit=='no'), ]$lcp), 
     xlab="Load time (ms)", ylim=c(0,0.001), col="blue", 
     main="Density plot for LCP in opera",
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
lines(density(data[which(data$browser == "opera" & data$crit=='std'), ]$lcp), col="red")
lines(density(data[which(data$browser == "opera" & data$crit=='strict'), ]$lcp), col="green")
legend("topright",
       c("no", "std", "strict"),
       fill=c("red","blue", "green"),
       cex=1
)

plot(density(data[which(data$browser == "opera" & data$crit=='no'), ]$cls), 
     xlab="CLS (ms)", ylim=c(0,1000), col="blue", 
     main="Density plot for CLS in opera",
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
lines(density(data[which(data$browser == "opera" & data$crit=='std'), ]$cls), col="red")
lines(density(data[which(data$browser == "opera" & data$crit=='strict'), ]$cls), col="green")
legend("topright",
       c("no", "std", "strict"),
       fill=c("red","blue", "green"),
       cex=1
)

plot(density(data[which(data$browser == "opera" & data$crit=='no'), ]$e), 
     xlab="Energy (J)", ylim=c(0,0.03), col="blue", 
     main="Density plot for energy in opera",
     cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
lines(density(data[which(data$browser == "opera" & data$crit=='std'), ]$e), col="red")
lines(density(data[which(data$browser == "opera" & data$crit=='strict'), ]$e), col="green")
legend("topright",
       c("no", "std", "strict"),
       fill=c("red","blue", "green"),
       cex=1
)

################################################# Check normality fid lcp cls e

check_normality <- function(dataset_to_eval, x, title) {
  #plot(density(dataset_to_eval), xlab=x, main=paste("Density plot for", title, sep=" "))
  qqnorm(dataset_to_eval, main=paste("Normal Q-Q Plot for", title, sep=" "), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  print(shapiro.test(dataset_to_eval))
}
par(mfrow=c(3,4))
par(mar=c(2,2,2,2))
# Chrome, critical = 0
check_normality(data[which(data$browser == "chrome" & data$crit=="no"), ]$fid, "FID (ms)", "chrome, crit = no, fid")
check_normality(data[which(data$browser == "chrome" & data$crit=="no"), ]$lcp, "LCP (ms)", "chrome, crit = no, lcp")
check_normality(data[which(data$browser == "chrome" & data$crit=="no"), ]$cls, "CLS (ms)", "chrome, crit = no, cls")
check_normality(data[which(data$browser == "chrome" & data$crit=="no"), ]$e, "Energy usage (J)", "chrome, crit = no, e")

# Chrome, critical = 1
check_normality(data[which(data$browser == "chrome" & data$crit=="std"), ]$fid, "Loading time (ms)", "chrome, crit = std, fid")
check_normality(data[which(data$browser == "chrome" & data$crit=="std"), ]$lcp, "First paint (ms)", "chrome, crit = std, lcp")
check_normality(data[which(data$browser == "chrome" & data$crit=="std"), ]$cls, "First contentful paint (ms)", "chrome, crit = std, cls")
check_normality(data[which(data$browser == "chrome" & data$crit=="std"), ]$e, "Energy usage (J)", "chrome, crit = std, e")

# Chrome, critical = 2
check_normality(data[which(data$browser == "chrome" & data$crit=="strict"), ]$fid, "Loading time (ms)", "chrome, crit = strict, fid")
check_normality(data[which(data$browser == "chrome" & data$crit=="strict"), ]$lcp, "First paint (ms)", "chrome, crit = strict, lcp")
check_normality(data[which(data$browser == "chrome" & data$crit=="strict"), ]$cls, "First contentful paint (ms)", "chrome, crit = strict, cls")
check_normality(data[which(data$browser == "chrome" & data$crit=="strict"), ]$e, "Energy usage (J)", "chrome, crit = 1, e")

# firefox, critical = 0
check_normality(data[which(data$browser == "firefox" & data$crit=="no"), ]$fid, "FID (ms)", "firefox, crit = no, fid")
check_normality(data[which(data$browser == "firefox" & data$crit=="no"), ]$lcp, "LCP (ms)", "firefox, crit = no, lcp")
check_normality(data[which(data$browser == "firefox" & data$crit=="no"), ]$cls, "CLS (ms)", "firefox, crit = no, cls")
check_normality(data[which(data$browser == "firefox" & data$crit=="no"), ]$e, "Energy usage (J)", "firefox, crit = no, e")

# firefox, critical = 1
check_normality(data[which(data$browser == "firefox" & data$crit=="std"), ]$fid, "Loading time (ms)", "firefox, crit = std, fid")
check_normality(data[which(data$browser == "firefox" & data$crit=="std"), ]$lcp, "First paint (ms)", "firefox, crit = std, lcp")
check_normality(data[which(data$browser == "firefox" & data$crit=="std"), ]$cls, "First contentful paint (ms)", "firefox, crit = std, cls")
check_normality(data[which(data$browser == "firefox" & data$crit=="std"), ]$e, "Energy usage (J)", "firefox, crit = std, e")

# firefox, critical = 2
check_normality(data[which(data$browser == "firefox" & data$crit=="strict"), ]$fid, "Loading time (ms)", "firefox, crit = strict, fid")
check_normality(data[which(data$browser == "firefox" & data$crit=="strict"), ]$lcp, "First paint (ms)", "firefox, crit = strict, lcp")
check_normality(data[which(data$browser == "firefox" & data$crit=="strict"), ]$cls, "First contentful paint (ms)", "firefox, crit = strict, cls")
check_normality(data[which(data$browser == "firefox" & data$crit=="strict"), ]$e, "Energy usage (J)", "firefox, crit = 1, e")

# opera, critical = 0
check_normality(data[which(data$browser == "opera" & data$crit=="no"), ]$fid, "FID (ms)", "opera, crit = no, fid")
check_normality(data[which(data$browser == "opera" & data$crit=="no"), ]$lcp, "LCP (ms)", "opera, crit = no, lcp")
check_normality(data[which(data$browser == "opera" & data$crit=="no"), ]$cls, "CLS (ms)", "opera, crit = no, cls")
check_normality(data[which(data$browser == "opera" & data$crit=="no"), ]$e, "Energy usage (J)", "opera, crit = no, e")

# opera, critical = 1
check_normality(data[which(data$browser == "opera" & data$crit=="std"), ]$fid, "Loading time (ms)", "opera, crit = std, fid")
check_normality(data[which(data$browser == "opera" & data$crit=="std"), ]$lcp, "First paint (ms)", "opera, crit = std, lcp")
check_normality(data[which(data$browser == "opera" & data$crit=="std"), ]$cls, "First contentful paint (ms)", "opera, crit = std, cls")
check_normality(data[which(data$browser == "opera" & data$crit=="std"), ]$e, "Energy usage (J)", "opera, crit = std, e")

# opera, critical = 2
check_normality(data[which(data$browser == "opera" & data$crit=="strict"), ]$fid, "Loading time (ms)", "opera, crit = strict, fid")
check_normality(data[which(data$browser == "opera" & data$crit=="strict"), ]$lcp, "First paint (ms)", "opera, crit = strict, lcp")
check_normality(data[which(data$browser == "opera" & data$crit=="strict"), ]$cls, "First contentful paint (ms)", "opera, crit = strict, cls")
check_normality(data[which(data$browser == "opera" & data$crit=="strict"), ]$e, "Energy usage (J)", "opera, crit = 1, e")

################################################# Wilcoxon signed rank tests fid lcp cls e
wilcox.test(data[which(data$browser == "chrome" & data$crit=="no"), ]$fid, 
            data[which(data$browser == "chrome" & data$crit=="std"), ]$fid,
            paired=TRUE)
wilcox.test(data[which(data$browser == "chrome" & data$crit=="no"), ]$lcp, 
            data[which(data$browser == "chrome" & data$crit=="std"), ]$lcp,
            paired=TRUE)
wilcox.test(data[which(data$browser == "chrome" & data$crit=="no"), ]$cls, 
            data[which(data$browser == "chrome" & data$crit=="std"), ]$cls,
            # data[which(data$browser == "chrome" & data$crit=="strict"), ]$cls,
            paired=TRUE)
wilcox.test(data[which(data$browser == "chrome" & data$crit=="no"), ]$e, 
            data[which(data$browser == "chrome" & data$crit=="std"), ]$e, 
            # data[which(data$browser == "chrome" & data$crit=="strict"), ]$e,
            paired=TRUE)

wilcox.test(data[which(data$browser == "firefox" & data$crit=="no"), ]$fid, 
            data[which(data$browser == "firefox" & data$crit=="std"), ]$fid,
            # data[which(data$browser == "firefox" & data$crit=="strict"), ]$fid,
            paired=TRUE)
wilcox.test(data[which(data$browser == "firefox" & data$crit=="no"), ]$lcp, 
            data[which(data$browser == "firefox" & data$crit=="std"), ]$lcp,
            # data[which(data$browser == "firefox" & data$crit=="strict"), ]$lcp,
            paired=TRUE)
wilcox.test(data[which(data$browser == "firefox" & data$crit=="no"), ]$cls, 
            data[which(data$browser == "firefox" & data$crit=="std"), ]$cls,
            # data[which(data$browser == "firefox" & data$crit=="strict"), ]$cls,
            paired=TRUE)
wilcox.test(data[which(data$browser == "firefox" & data$crit=="no"), ]$e, 
            data[which(data$browser == "firefox" & data$crit=="std"), ]$e, 
            # data[which(data$browser == "firefox" & data$crit=="strict"), ]$e,
            paired=TRUE)

wilcox.test(data[which(data$browser == "opera" & data$crit=="no"), ]$fid, 
            data[which(data$browser == "opera" & data$crit=="std"), ]$fid,
            # data[which(data$browser == "opera" & data$crit=="strict"), ]$fid,
            paired=TRUE)
wilcox.test(data[which(data$browser == "opera" & data$crit=="no"), ]$lcp, 
            data[which(data$browser == "opera" & data$crit=="std"), ]$lcp,
            # data[which(data$browser == "opera" & data$crit=="strict"), ]$lcp,
            paired=TRUE)
wilcox.test(data[which(data$browser == "opera" & data$crit=="no"), ]$cls, 
            data[which(data$browser == "opera" & data$crit=="std"), ]$cls,
            # data[which(data$browser == "opera" & data$crit=="strict"), ]$cls,
            paired=TRUE)
wilcox.test(data[which(data$browser == "opera" & data$crit=="no"), ]$e, 
            data[which(data$browser == "opera" & data$crit=="std"), ]$e, 
            # data[which(data$browser == "opera" & data$crit=="strict"), ]$e,
            paired=TRUE)

# Cliff delta
cliff.delta(data[which(data$browser == "chrome" & data$crit=="std"), ]$fid, data[which(data$browser == "chrome" & data$crit=="no"), ]$fid)
cliff.delta(data[which(data$browser == "chrome" & data$crit=="std"), ]$lcp, data[which(data$browser == "chrome" & data$crit=="no"), ]$lcp)
cliff.delta(data[which(data$browser == "chrome" & data$crit=="std"), ]$cls, data[which(data$browser == "chrome" & data$crit=="no"), ]$cls)
cliff.delta(data[which(data$browser == "chrome" & data$crit=="std"), ]$e, data[which(data$browser == "chrome" & data$crit=="no"), ]$e)


cliff.delta(data[which(data$browser == "firefox" & data$crit=="std"), ]$fid, data[which(data$browser == "firefox" & data$crit=="no"), ]$fid)
cliff.delta(data[which(data$browser == "firefox" & data$crit=="std"), ]$lcp, data[which(data$browser == "firefox" & data$crit=="no"), ]$lcp)
cliff.delta(data[which(data$browser == "firefox" & data$crit=="std"), ]$cls, data[which(data$browser == "firefox" & data$crit=="no"), ]$cls)
cliff.delta(data[which(data$browser == "firefox" & data$crit=="std"), ]$e, data[which(data$browser == "firefox" & data$crit=="no"), ]$e)

cliff.delta(data[which(data$browser == "opera" & data$crit=="std"), ]$fid, data[which(data$browser == "opera" & data$crit=="no"), ]$fid)
cliff.delta(data[which(data$browser == "opera" & data$crit=="std"), ]$lcp, data[which(data$browser == "opera" & data$crit=="no"), ]$lcp)
cliff.delta(data[which(data$browser == "opera" & data$crit=="std"), ]$cls, data[which(data$browser == "opera" & data$crit=="no"), ]$cls)
cliff.delta(data[which(data$browser == "opera" & data$crit=="std"), ]$e, data[which(data$browser == "opera" & data$crit=="no"), ]$e)

