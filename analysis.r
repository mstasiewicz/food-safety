#preliminaries ####
#load plyr library for creating group counts, check 2013 against fsis summary page
library(plyr)
library(reshape2)
library(doBy)
library(ggplot2)
library(ggthemes)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <-
  function(...,
           plotlist = NULL,
           file,
           cols = 1,
           layout = NULL) {
    library(grid)
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                       ncol = cols,
                       nrow = ceiling(numPlots / cols))
    }
    
    if (numPlots == 1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]],
              vp = viewport(
                layout.pos.row = matchidx$row,
                layout.pos.col = matchidx$col
              ))
      }
    }
  }

#load data and clean up ####
data <- read.csv("data_cleaned_7-8-16.csv", stringsAsFactors = T)
summary(data)
data <- data[,-1] #drop the index row
#Now no records are duplicated.  Great!
#clean up the recalled values; make all text NA, make all 0's an NA
data$rec_text <- data$recalled
data$recalled <- as.numeric(gsub(",","",data$rec_text))
data$recalled[data$recalled==0] <- NA
  #subset(data, select=c(rec_text, recalled)) #looks OK
#clean up recovered; make all text NA, so a 0 is a known 0.
data$recovered_text <- data$recovered
data$recovered <- as.numeric(as.character(data$recovered))
  #subset(data, select = c(recovered, recovered_text)) #looks OK
#clean up the recall classes
table(data$reason, data$recall_class) #make all Listeria & STEC Class I; was done previously
  #data$recall_class[data$reason == "Listeria" | data$reason == "STEC"] <- "Class I"
  levels(data$reason)[levels(data$reason)=="E. coli"] <- "STEC" #make E.coli STEC
table(data$reason, data$recall_class) #worked
#check other
data$recall_class <- factor(data$recall_class)
summary(data)
#load(".Rdata")

#create the variable transformations####
data$recalled_log <- log(data$recalled + 1, 10)
data$recovered_log <- log(data$recovered + 1, 10)
data$Pr_recov <- data$recovered / data$recalled
data$Pr_recov_log <- data$recovered_log - data$recalled_log
data$year_n <- data$year-1994
summary(data)

#write the new data####
#write.csv(data, file="data_transformed_7-8-16.csv")

#create the figures####
#figure 1 - barplot####
data$reason<- factor( data$reason, levels=levels(data$reason)[ order( table(data$reason) , decreasing=T ) ] )
f1 <- ggplot(data, aes(x=reason, fill=recall_class)) + geom_bar()+  labs(x = "Reason for Recall", y = "Count of Recalls")#, title="Fig 1")
f1 <-  f1 + theme_bw() + theme(axis.text.x=element_text(angle=50, vjust=0.5)) + scale_fill_discrete(name="Recall Class") + theme(legend.position=c(.9,.8)) 
f1 
ggsave(f1, file="outputs/F1-Hist of reason and class counts.pdf")
ggsave(f1, file="outputs/F1-Hist of reason and class counts.jpg")
f1_bw <- f1 +  scale_fill_grey(name="Recall Class")
f1_bw
ggsave(f1_bw, file="outputs/F1bw-Hist of reason and class counts.pdf")
ggsave(f1_bw, file="outputs/F1bw-Hist of reason and class counts.jpg")

#figure 2 - histograms####
ds <- subset(data, select = c(recalled_log, recovered_log))
dm <- melt(ds)
levels(dm$variable) <- c("Product Recalled", "Product Recovered")
dm$isZero <- dm$value == 0
f2 <- ggplot(dm, aes(x=value, fill=isZero)) + geom_histogram(binwidth = 0.5) + labs(x="Product Recalled or Recovered [log10(pounds)]", y="Count of Recalls")#, title="Fig 2")
f2 <- f2 + facet_wrap( ~ variable, ncol = 1 ) + theme_bw() + theme(legend.position="none") #+ scale_fill_grey()
f2
ggsave(f2, file="outputs/F2-Hist of recall size.pdf")
ggsave(f2, file="outputs/F2-Hist of recall size.jpg")
f2_bw <- f2 + scale_fill_grey()
ggsave(f2_bw, file="outputs/F2bw-Hist of recall size.pdf")
ggsave(f2_bw, file="outputs/F2bw-Hist of recall size.jpg")

#figure 3 - proportion recovered####
data$recov_class <- "recovered <= 100%"
data$recov_class[ data$recovered_log == 0 ] <- "recovered = 0 lbs"
data$recov_class[ data$Pr_recov_log > 0 ] <- "recovered > 100%"

f3 <- ggplot(data=data, aes(x = Pr_recov_log, fill = recov_class)) + geom_histogram(binwidth = .25, right=T)
f3 <- f3 + theme_bw() + labs(x = "Proportion of Product Recovered [log10(pounds)]", y = "Count of Recalls") + #, title="Fig 3") + 
  theme(legend.position=c(.2,.85) ) + scale_fill_discrete(name="Recovery Category")
f3
ggsave(f3, file="outputs/F3-Hist of Pr recovered.pdf")
ggsave(f3, file="outputs/F3-Hist of Pr recovered.jpg")
f3_bw <- f3 + scale_fill_grey(name="Recovery Category")
ggsave(f3_bw, file="outputs/F3bw-Hist of Pr recovered.pdf")
ggsave(f3_bw, file="outputs/F3bw-Hist of Pr recovered.jpg")

table(subset(data, !is.na(recovered_log) ,select=recov_class))
summary(subset(data, recovered_log > 0, select="recovered_log"))
sd(subset(data, recovered_log > 0, select="recovered_log")[,])

#figure 4 - Correlation of recalled and recovered####
summary(subset(data, recovered_log>0)) #834 values, 15 NAs in recalled log; 819 remain therefore 817 df
mod <- lm(recovered_log ~ recalled_log, data=subset(data, recovered_log>0))
summary(mod) #regression is significant; int 0.08733, slope=0.83360
f4 <- ggplot( data=data, aes(x=recalled_log, y=recovered_log, color=recov_class) ) + theme_bw()
f4 <- f4 + geom_point() + labs(x="Product Recalled [log10(pounds)]", y="Product Recovered [log10(pounds)]")#, title="F4")
f4 <- f4 + geom_abline(slope = 1, intercept=0, color="black") + 
  geom_abline(slope = mod$coefficients[2], intercept = mod$coefficients[1], color="grey") +
  theme(legend.position=c(.2,.85) ) + scale_color_discrete(name="Recovery Category")
f4
ggsave(f4, file="outputs/F4-Recov v Recalled.pdf")
ggsave(f4, file="outputs/F4-Recov v Recalled.jpg")
f4_bw <- f4 + scale_color_grey(name="Recovery Category")
ggsave(f4_bw, file="outputs/F4bw-Recov v Recalled.pdf")
ggsave(f4_bw, file="outputs/F4bw-Recov v Recalled.jpg")

#figure 5 - Sizes by year####
f5_rca <- ggplot( aes(x = factor(year), y = recalled_log) , data=data) + geom_boxplot(notch=T, size=T)+
  theme_bw() + labs(x = "", y = "Product Recalled [log10(pounds)]", title="A")
f5_rca 
f5_ct <- ggplot( aes( x = factor(year)), data = data) + geom_bar() + 
  theme_bw() + labs(x = "Year", y = "Count of Recalls", title="D")
f5_ct
f5_rcv <- ggplot( aes(x = factor(year), y = recovered_log) , data=data) + geom_boxplot(notch=T, size=T) +
  theme_bw() + labs(x = "", y = "Product Recovered [log10(pounds)]", title="B")
f5_rcv
f5_pr <- ggplot( aes(x = factor(year), y = Pr_recov_log) , data=data) + geom_boxplot(notch=T, size=T) +
  theme_bw() + labs(x = "", y = "Proportion Recovered [log10(Recalled-Rocovered)]", title="C")
f5_pr
#pdf(file="outputs/f5 - Recalled.pdf", h=6, w=8); multiplot(f5a, f5b); dev.off()
#pdf(file="outputs/f5 - Amts over time.pdf", h=16, w=8); multiplot(f5a, f5c, f5d, f5b); dev.off()
#graphics.off()

#figure 6 - Frequence of rare events####
#look into number of outbreaks above a given size
data$recalled_above_1E8 <- data$recalled > 1E8
data$recalled_above_1E7 <- data$recalled > 1E7
data$recalled_above_1E6 <- data$recalled > 1E6
data$recalled_above_1E5 <- data$recalled > 1E4

#lots of cool stuff to be done with weighting
p <- ggplot(data, aes(x=factor(year))) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p + geom_bar() + labs(title = "Outbreaks > year") 
p <- ggplot() +  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p + geom_bar(data = data, aes(x=factor(year))) 
p + geom_bar(data = data, aes(x=factor(year), fill=factor(recalled_above_1E8)))

p + geom_bar(data = data, aes(x=factor(year))) + facet_wrap(~ recalled_above_1E8, ncol=1,  scales="free_y")  
p + geom_bar(data = data, aes(x=factor(year))) + facet_wrap(~ recalled_above_1E7, ncol=1,  scales="free_y")  
p + geom_bar(data = data, aes(x=factor(year))) + facet_wrap(~ recalled_above_1E6, ncol=1,  scales="free_y")  
p + geom_bar(data = data, aes(x=factor(year))) + facet_wrap(~ recalled_above_1E5, ncol=1,  scales="free_y")  

#would be better to plot as counts 
abv8 <- table(data$year, data$recalled_above_1E8)[,2]
abv7 <- table(data$year, data$recalled_above_1E7)[,2]
abv6 <- table(data$year, data$recalled_above_1E6)[,2]
abv5 <- table(data$year, data$recalled_above_1E5)[,2]
d <- data.frame(abv8, abv7, abv6)#, abv5)
d$year <- row.names(d)
m <- melt(d, id="year")
m$variable <- factor(m$variable)
levels(m$variable) <- relevel(m$value, "abv6")
p <- ggplot(data = m, aes(x = year, weight = value, fill = variable)) + theme_bw() + geom_bar(position="dodge")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(y = "Count of outbreaks above 1EX", title="Supp Fig 1")
p
p <- ggplot(data = m, aes(x = year, weight = value, fill = variable)) + theme_bw() 
p + geom_bar(position="identity", alpha = 0.5) + scale_fill_discrete()
theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(y = "Count of outbreaks above 1EX", title="Supp Fig 1")
p
dt <- data.frame(thr=abv6, year = names(abv6))
dt2 <- data.frame(thr=abv7, year = names(abv7))
dt3 <- data.frame(thr=abv8, year = names(abv8))

p <- ggplot(NULL, aes(year,thr)) + theme_bw()
pf <- p + geom_bar( aes(fill="abv6") , data=dt , stat="identity") +
  geom_bar( aes(fill="abv7") , data=dt2 , stat="identity") +
  geom_bar( aes(fill="abv8") , data=dt3 , stat="identity") 
pf <- pf + labs(y = "Count of outbreaks above threshold size", x = "year")#, title="Supp Fig 1")
pf <- pf +  scale_fill_brewer(name="Threshold [pounds]", labels=c("10^6","10^7","10^8")) + theme(legend.position=c(.1,.85) )
pf
ggsave(pf, file = "outputs/F6 - rare events.pdf") #don't appear to be increasing
ggsave(pf, file = "outputs/F6 - rare events.jpg") #don't appear to be increasing
pf_bw <- pf + scale_fill_grey(name="Threshold [pounds]", labels=c("10^6","10^7","10^8"), start=0.8, end = 0.2)
pf_bw
ggsave(pf_bw, file = "outputs/F6bw - rare events.pdf") #don't appear to be increasing
ggsave(pf_bw, file = "outputs/F6bw - rare events.jpg") #don't appear to be increasing

#table 1 - Univariate stats####
#source("C:/Users/mstasie.UOFI/Google Drive/r/univariate statistics.r")
sumfun <- function(x, ...){
  c(l=length(x),m=mean(x, ...), med=median(x,...), sd = sd(x, ...), max=max(x, ...))
}
#Table 1 recalled ####
#class summary
ds <- subset(data, recall_class!="None Assigned")
a <- summaryBy(recalled_log ~ recall_class, data=data, FUN=sumfun, na.rm=T)#, mean, median, sd))
b <- summaryBy(recalled ~ recall_class, data=data, FUN=sum, na.rm=T)#, mean, median, sd))
fit_lm <- lm(recalled_log ~ recall_class, data=ds)
#summary(fit_lm)
co <- coef(fit_lm)
get_p<-function(fit_lm){
  p_coef <- summary(fit_lm)$coefficients[,4]
  x <- summary(fit_lm)
  pv <- pf(x$fstatistic[1], x$fstatistic[2], x$fstatistic[3], lower.tail = FALSE)
  p_coef[1] <- pv
  return(p_coef)}
p_coef <- get_p(fit_lm)
CLASS <- cbind(a,sum=log10(b$recalled.sum), coef=c(co, NA),pval=c(p_coef,NA) )
colnames(CLASS) <- c("Covariate", "n", "mean", "med", "sd", "max", "sum", "coef", "pval")
CLASS
#Season summary
a <- summaryBy(recalled_log ~ Season, data=data, FUN=sumfun, na.rm=T)#, mean, median, sd))
b <- summaryBy(recalled ~ Season, data=data, FUN=sum, na.rm=T)#, mean, median, sd))
fit_lm <- lm(recalled_log ~ Season, data=data)
summary(fit_lm)
co <- coef(fit_lm)
p_coef <- get_p(fit_lm)
SEASON <- cbind(a,sum=log10(b$recalled.sum), coef=c(co),pval=c(p_coef) )
colnames(SEASON) <- c("Covariate", "n", "mean", "med", "sd", "max", "sum", "coef", "pval")
SEASON
#reason summary
levels(data$reason)
data$reason <- relevel(data$reason,"Undeclared Allergen")
levels(data$reason)
a <- summaryBy(recalled_log ~ reason, data=data, FUN=sumfun, na.rm=T)#, mean, median, sd))
b <- summaryBy(recalled ~ reason, data=data, FUN=sum, na.rm=T)#, mean, median, sd))
fit_lm <- lm(recalled_log ~ reason, data=data)
summary(fit_lm)
co <- coef(fit_lm)
p_coef <- get_p(fit_lm)
REASON <- cbind(a,sum=log10(b$recalled.sum), coef=c(co),pval=c(p_coef) )
colnames(REASON) <- c("Covariate", "n", "mean", "med", "sd", "max", "sum", "coef", "pval")
REASON
#year summary
a <- summaryBy(recalled_log ~ year, data=data, FUN=sumfun, na.rm=T)#, mean, median, sd))
b <- summaryBy(recalled ~ year, data=data, FUN=sum, na.rm=T)#, mean, median, sd))
fit_lm <- lm(recalled_log ~ factor(year), data=data)
summary(fit_lm)
co <- coef(fit_lm)
p_coef <- get_p(fit_lm)
YEAR <- cbind(a,sum=log10(b$recalled.sum), coef=c(co),pval=c(p_coef) )
colnames(YEAR) <- c("Covariate", "n", "mean", "med", "sd", "max", "sum", "coef", "pval")
YEAR$Covariate <- factor(YEAR$Covariate) 
YEAR
#species summary
a <- summaryBy(recalled_log ~ species, data=data, FUN=sumfun, na.rm=T)#, mean, median, sd))
b <- summaryBy(recalled ~ species, data=data, FUN=sum, na.rm=T)#, mean, median, sd))
fit_lm <- lm(recalled_log ~ species, data=data)
summary(fit_lm)
co <- coef(fit_lm)
p_coef <- get_p(fit_lm)
SPECIES <- cbind(a,sum=log10(b$recalled.sum), coef=c(co),pval=c(p_coef) )
colnames(SPECIES) <- c("Covariate", "n", "mean", "med", "sd", "max", "sum", "coef", "pval")
SPECIES
#full setdata$R
all <- data.frame(Covariate="Full", n=length(data$recalled), mean=mean(data$recalled_log, na.rm = T), med=median(data$recalled_log, na.rm = T),
                  sd=sd(data$recalled_log, na.rm = T), max=max(data$recalled_log, na.rm = T), sum=log10(sum(data$recalled, na.rm = T)), coef=NA, pval=NA)
ALL <- rbind(all,CLASS, SPECIES, REASON, SEASON,YEAR)
ALL
ALL$pval_txt <- round(ALL$pval,3)
ALL$pval_txt[ALL$pval<0.001] <- "<0.001"
ALL$pval_txt
#Table 1 recovered ####
#class summary
data_hold <- data
summary(data_hold)
data <- subset(data, !is.na(data$recovered_log))
summary(data)
a <- summaryBy(recovered_log ~ recall_class, data=data, FUN=sumfun, na.rm=T)#, mean, median, sd))
b <- summaryBy(recovered ~ recall_class, data=data, FUN=sum, na.rm=T)#, mean, median, sd))
fit_lm <- lm(recovered_log ~ recall_class, data=ds)
#summary(fit_lm)
co <- coef(fit_lm)
get_p<-function(fit_lm){
  p_coef <- summary(fit_lm)$coefficients[,4]
  x <- summary(fit_lm)
  pv <- pf(x$fstatistic[1], x$fstatistic[2], x$fstatistic[3], lower.tail = FALSE)
  p_coef[1] <- pv
  return(p_coef)}
p_coef <- get_p(fit_lm)
CLASS <- cbind(a,sum=log10(b$recovered.sum), coef=c(co, NA),pval=c(p_coef,NA) )
colnames(CLASS) <- c("Covariate", "n", "mean", "med", "sd", "max", "sum", "coef", "pval")
CLASS
#Season summary
a <- summaryBy(recovered_log ~ Season, data=data, FUN=sumfun, na.rm=T)#, mean, median, sd))
b <- summaryBy(recovered ~ Season, data=data, FUN=sum, na.rm=T)#, mean, median, sd))
fit_lm <- lm(recovered_log ~ Season, data=data)
summary(fit_lm)
co <- coef(fit_lm)
p_coef <- get_p(fit_lm)
SEASON <- cbind(a,sum=log10(b$recovered.sum), coef=c(co),pval=c(p_coef) )
colnames(SEASON) <- c("Covariate", "n", "mean", "med", "sd", "max", "sum", "coef", "pval")
SEASON
#reason summary
a <- summaryBy(recovered_log ~ reason, data=data, FUN=sumfun, na.rm=T)#, mean, median, sd))
b <- summaryBy(recovered ~ reason, data=data, FUN=sum, na.rm=T)#, mean, median, sd))
fit_lm <- lm(recovered_log ~ reason, data=data)
summary(fit_lm)
co <- coef(fit_lm)
p_coef <- get_p(fit_lm)
REASON <- cbind(a,sum=log10(b$recovered.sum), coef=c(co),pval=c(p_coef) )
colnames(REASON) <- c("Covariate", "n", "mean", "med", "sd", "max", "sum", "coef", "pval")
REASON
#year summary
a <- summaryBy(recovered_log ~ year, data=data, FUN=sumfun, na.rm=T)#, mean, median, sd))
b <- summaryBy(recovered ~ year, data=data, FUN=sum, na.rm=T)#, mean, median, sd))
fit_lm <- lm(recovered_log ~ factor(year), data=data)
summary(fit_lm)
co <- coef(fit_lm)
p_coef <- get_p(fit_lm)
YEAR <- cbind(a,sum=log10(b$recovered.sum), coef=c(co),pval=c(p_coef) )
colnames(YEAR) <- c("Covariate", "n", "mean", "med", "sd", "max", "sum", "coef", "pval")
YEAR$Covariate <- factor(YEAR$Covariate) 
YEAR
#species summary
a <- summaryBy(recovered_log ~ species, data=data, FUN=sumfun, na.rm=T)#, mean, median, sd))
b <- summaryBy(recovered ~ species, data=data, FUN=sum, na.rm=T)#, mean, median, sd))
fit_lm <- lm(recovered_log ~ species, data=data)
summary(fit_lm)
co <- coef(fit_lm)
p_coef <- get_p(fit_lm)
SPECIES <- cbind(a,sum=log10(b$recovered.sum), coef=c(co),pval=c(p_coef) )
colnames(SPECIES) <- c("Covariate", "n", "mean", "med", "sd", "max", "sum", "coef", "pval")
SPECIES
#full setdata$R
allrec <- data.frame(Covariate="Full", n=length(data$recovered), mean=mean(data$recovered_log, na.rm = T), med=median(data$recovered_log, na.rm = T),
                     sd=sd(data$recovered_log, na.rm = T), max=max(data$recovered_log, na.rm = T), sum=log10(sum(data$recovered, na.rm = T)), coef=NA, pval=NA)
ALLrec <- rbind(allrec,CLASS, SPECIES, REASON, SEASON,YEAR)
ALLrec
ALLrec$pval_txt <- round(ALLrec$pval,3)
ALLrec$pval_txt[ALLrec$pval<0.001] <- "<0.001"
ALLrec$pval_txt

#output####
write.csv(ALL, file="outputs/T1-Basic Desc Stats.csv")
write.csv(ALLrec, file="outputs/T1-Basic Desc Stats-recovered.csv")

data <- data_hold

#table 2 - Multivariate model; CAN'T DO TYPE III error in R####
#include indivdually signifiant factors, year reason species class
summary(ds)
fit_lm <- glm(recalled_log ~ reason + species + factor(year)+  recall_class , data=ds)
summary(fit_lm)
summary(aov(fit_lm))
fit_lm <- glm(recalled_log ~ recall_class + reason + species + factor(year) , data=ds)
summary(fit_lm)
summary(aov(fit_lm))
fit_lm
#try a type 3; comparable to JMP
library(car)
?Anova
t2 <- Anova(fit_lm, type=2)
t2
t3 <- Anova(fit_lm, type=3)
t3
aov(t3)
summary(t3)
summary(aov(fit_lm))
summary(aov(t2))

t1 <- Anova(fit_lm, type=1)
t1

summary(t3)  
fit_lm <- lme(recalled_log ~ recall_class + reason + species + factor(year) , data=ds)
?lme
anova.lme(fit_lm, type="marginal")
#diff based on order because this is in the incorrect Type I error system. 

#figure 5 - meat production####
#http://www.ers.usda.gov/data-products/livestock-meat-domestic-data.aspx
#date 6/24/16, make all values number
p <- read.csv("MeatStatsFull.csv", skip=2)
summary(p); names(p)
#clean up
names(p)[1] <- "ID"
p <- p[-(c(1:2,679)),]
head(p) 
foo <- data.frame(do.call('rbind', strsplit(as.character(p$ID),'-',fixed=TRUE)))
names(foo) <- c("month", "year")
p <- cbind(p, foo)
summary(p)
names(p)[names(p)=="Beef.3..1"] <-"Beef" 
names(p)[names(p)=="Pork.3..1"] <-"Pork" 
names(p)[names(p)=="Total.poultry.4..5..6."] <-"Poultry" 
names(p)
#get totals by year
ps <- subset(p, select=c(year, Beef, Pork, Poultry))
#summary(ps)
a <- summaryBy(Beef ~ year, data=ps, FUN=sum, na.rm=T)#, mean, median, sd))
b <- summaryBy(Pork ~ year, data=ps, FUN=sum, na.rm=T)#, mean, median, sd))
c <- summaryBy(Poultry ~ year, data=ps, FUN=sum, na.rm=T)#, mean, median, sd))
psu <- cbind(a, b$Pork.sum ,c$Poultry.sum)
names(psu) <- c("Year", "Beef", "Pork", "Poultry")
psu$"Sum" <- apply(psu[,2:4],1,FUN=sum)
pm <- melt(psu, id.vars="Year")
names(pm)[2:3] <- c("Commodity", "Production")
pm$Production <- pm$Production/1000
p <- ggplot(pm , aes(x=Year, y=Production, group=Commodity) )
#p <- p + geom_line(aes(color=Commodity))
#p <- p + geom_line(aes(linetype=Commodity))
p <- p + geom_line(aes(linetype=Commodity)) + scale_linetype_manual(values = c(2,3,4,1))
p <- p %+% subset(pm, as.vector(Year)>1993&as.vector(Year)<2016)
figS <- p + labs(x="Year", y="U.S. FSIS Inspected Production [Billion Pounds]", title="E") + theme_bw()
figS <- figS + theme(legend.position = c(.9, .7))
figS# + geom_line(linetype=Commodity)
ggsave(figS, file="outputs/FigS-US Meat Production.pdf")
ggsave(figS, file="outputs/FigS-US Meat Production.jpg")
ps <- subset(pm, as.vector(Year)>1993&as.vector(Year)<2016)
pc <- dcast(ps, Year ~ Commodity, margins=T, fun.aggregate = sum)
write.csv(pc, "outputs/US Meat Production.csv")
#write the plots
pdf(file="outputs/f5altbw - Amts over time.pdf", h=12, w=8); multiplot(f5_rca, f5_ct, figS); dev.off()
jpeg(file="outputs/f5altbw - Amts over time.jpg", h=12, w=8, units= "in", res=600); multiplot(f5_rca, f5_ct , figS); dev.off()
pdf(file="outputs/f5allbw - Amts over time.pdf", h=20, w=8); multiplot(f5_rca, f5_rcv, f5_pr, f5_ct , figS); dev.off()
jpeg(file="outputs/f5allbw - Amts over time.jpg", h=20, w=8, units= "in", res=600); multiplot(f5_rca, f5_rcv, f5_pr, f5_ct , figS); dev.off()
graphics.off()
