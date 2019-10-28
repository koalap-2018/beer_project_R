#here we install some new pacakages
install.packages("corrplot")
install.packages("ggfortify")


#here we load libraries tthat we need to use
library(ggplot2)
library(gplots)
library(dplyr)
library(gridExtra)
library(GGally)
library(corrplot)
theme_set(theme_minimal(20))
library(ggfortify)

# Load the Data
getwd()
setwd("C:/Users/correap/Documents/Rclass/project_R_UDA")
beer<-read.csv("recipeData.csv")


#here we get familiar with the data

str(beer)
summary(beer)
#Here we convert some of the data we want to work on to numeric type, we check these
#conversions at the end
beer$BoilGravity<-as.numeric(beer$BoilGravity)
beer$MashThickness<-as.numeric(beer$MashThickness)
beer$PitchRate<-as.ntumeric(beer$PitchRate)
beer$PrimaryTemp<-as.numeric(beer$PrimaryTemp)


#here we replace the N/A with NA so that we can do stats
beer$BoilGravity[beer$BoilGravity=="N/A"]<-"NA"
beer$MashThickness[beer$MashThickness=="N/A"]<-"NA"
beer$PitchRate[beer$PitchRate=="N/A"]<-"NA"
beer$PrimaryTemp[beer$PrimaryTemp=="N/A"]<-"NA"


#here we will investigat the color variable that is from 0-40 (40 being hte darkest)
summary(beer$Color)
ggplot(data = beer, aes(x = Color)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(limits = c(0, 186), breaks = seq(0, 186, 40))


#this transformation makes the distribution of the data look much clearer

ggplot(aes(log10(beer$Color+1)),data=beer)+
  geom_histogram(binwidth = 0.01) +
  geom_histogram(col='black', 
                 aes(fill=..count..))+
  scale_fill_gradient("Count", low="green", high="red")

#here we will plot the ABV
summary(beer$ABV)
ggplot(aes(x = ABV), data = beer) +
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(limits = c(0,20), breaks = seq(0,20, 5))
  
summary(beer$IBU)

ggplot(aes(x = IBU), data = beer) +
  geom_histogram(binwidth = 0.5)+
  xlim(0,500)+
  ylim(0,1000)
  

ggplot(aes(log10(x = beer$IBU+1)), data = beer) +
  geom_histogram(binwidth = 0.01)+
  ylim(0,1500)


#box plotS OF ABV divided by brewmethod and sugar scale
summary(beer$ABV)
#save them into one boxplot  
p1<-qplot(x=BrewMethod, y=ABV, 
      data=subset(beer,ABV<=8&ABV>=3),
      geom='boxplot', color=BrewMethod)+
     coord_cartesian(ylim=c(2,8))

p2<-qplot(x=SugarScale, y=ABV, 
      data=subset(beer,ABV<=8&ABV>=3),
      geom='boxplot', color=SugarScale)+
  coord_cartesian(ylim=c(2,8))

grid.arrange(p1,p2, ncol=1)



#prety graph of brewer methods and colored with the sugarscale
#here might be better to do pie charts instead of this
ggplot(beer, aes(BrewMethod, fill = SugarScale)) + 
  geom_bar()


#here we plot distributions by BoilTIme 
ggplot(data = beer, aes(x = BoilTime)) +
  geom_histogram(binwidth = 5)+
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, 20))

#here we plot OG and add a line for the median
ggplot(data = beer, aes(x = OG)) +
  geom_histogram(binwidth = 0.01)+
  coord_cartesian(xlim=c(1,1.5))+
  geom_vline(aes(xintercept=median(OG, na.rm=T)),   # Ignore NA values for median
             color="red", linetype="dashed", size=1)

#here we plot FG
summary(beer$FG)

ggplot(data = beer, aes(x = FG)) +
  geom_histogram(binwidth = 0.005)+
  coord_cartesian(xlim=c(0.95,1.1))+
  geom_vline(aes(xintercept=median(FG, na.rm=T)),   # Ignore NA values for median
             color="red", linetype="dashed", size=1)



##Here some bivariate analysis
#here we play with the IBU vs ABV... here you might need to subset the data??
ggplot(aes(x=log10(IBU+1), y=ABV), data=subset(beer,ABV>3)) +
  geom_point() +
  geom_smooth()+
  ggtitle("Bitterness vs alcohol content")



#here we play with the IBU vs ABV... here you might need to subset the data??
ggplot(aes(x=log10(Color+1), y=ABV), data=beer) +
  geom_point() +
  geom_smooth()+
  ggtitle("Bitterness vs alcohol content")

#Plot now color and ABV
 ggplot(aes(x=ABV, y=log10(Color+1), data=beer)) +
  geom_point() +
  geom_jitter(alpha=1/5, position=position_jitter(h=0))+
  stat_smooth(method="lm") +
  ggtitle("COlor vs alcohol")

#plot 95 percentile of the data
ggplot(aes(x=ABV, y=log10(Color+1)), data=beer)+
  geom_jitter(alpha=1/20, position=position_jitter(h=0))+
  coord_cartesian(xlim=c(0,20),ylim=c(0,2))+
  geom_smooth(method='lm',color='red')+
  geom_point()


  geom_jitter(alpha=1/50)+
  xlim(0,quantile(beer$ABV,0.95))+
  ylim(0,quantile(beer$Color,0.95))+
  geom_smooth(method='lm',color='blue')
  
#OG vs FG
ggplot(aes(x=OG, y=FG), data=beer)+
    coord_cartesian(xlim=c(0,25),ylim=c(0,10))+
    geom_point()+
    geom_smooth(method='lm',color='blue')+
    ggtitle("Original vs Final Gravities")+
   theme(plot.title = element_text(hjust = 0.5))
    
  
#OG vs FG and facet by ABV_ranges
ggplot(aes(x=OG, y=FG), data=subset(beer,!is.na(ABV_ranges)))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ABV_ranges)+
  ggtitle("Original vs Final Gravities")+
  theme(plot.title = element_text(hjust = 0.5))


#here we calculate the correlation with the most common ABV ranges
with(subset(beer,ABV<=8&ABV>=3), cor.test(ABV,IBU,method='spearman'))

#here we calculate the correlation coeficient
cor.test(beer$ABV,beer$IBU, method='pearson',alternative='greater')

#this is the color correlation
cor.test(beer$ABV,beer$Color, method='pearson',alternative='greater')
#ploting with ABV ranges
ggplot(aes(x=ABV, y=log10(IBU+1)), 
       data=subset(beer, ABV<=8&ABV>=3))+
  geom_jitter(alpha=1/5000, position=position_jitter(h=0))+
  geom_smooth(method='lm',color='red',size=3)+
  geom_point(size=0.1)


#grouping by boiling time
boiling_groups<-group_by(beer,BoilTime)
beer_by_boil<-summarise(boiling_groups,
                        ABV_mean=mean(ABV),
                        ABV_median=median(ABV),
                        n=n())
beer_by_boil<-arrange(beer_by_boil,BoilTime)

head(beer_by_boil)

#ploting the gruping by subsetting higher than 
ggplot(aes(x=BoilTime, y=ABV_mean),data=(beer_by_boil))+
  geom_point()+
  xlab("Boil Time")+
  ylab("ABV_mean")+
  geom_smooth(method='lm',color='red',size=2)+
  ggtitle("Boil Time vs ABV_mean")

#here we calculate the correlation coeficient for the mean of ABV and IBU
cor.test(beer_by_boil$ABV_mean,beer_by_boil$BoilTime, method='pearson',alternative='greater')







#ploting the gruping by subsetting higher than 60
ggplot(aes(x=BoilTime, y=ABV_mean),data=subset(beer_by_boil,BoilTime>60))+
  geom_line()+
  xlab("Boil Time")+
  ylab("ABV_mean")+
  ggtitle("Boil Time vs ABV")


#grouping by IBU time
IBU_groups<-group_by(beer,IBU)
beer_by_IBU<-summarise(IBU_groups,
                        ABV_mean=mean(ABV),
                        ABV_median=median(ABV),
                        n=n())
beer_by_IBU<-arrange(beer_by_IBU,IBU)
tail(beer_by_IBU)
head(IBU_groups$ABV_ranges)
#ploting the IBU vs ABV
summary(beer$IBU)

ggplot(aes(x=IBU, y=ABV_median),data=(beer_by_IBU))+
  geom_point()+
  xlab("IBU")+
  ylab("ABV_median")+
  xlim(1,500)
  ylim(1,30)
  ggtitle("IBU vs ABV")

beer_by_IBUNOzeros <- beer_by_IBU[-c(1), ]
head(beer_by_IBUNOzeros)
beer_by_IBUlog10<-log10(beer_by_IBUNOzeros+1)

#here we plot the 2 variables and the correlation.... it seems positive
ggplot(aes(x=IBU, y=ABV_mean),data=(beer_by_IBUlog10))+
  geom_point(size=0.5)+
  geom_jitter(alpha=1/500)+
  xlim(0,quantile(beer_by_IBUlog10$IBU,0.95))+
  ylim(0,quantile(beer_by_IBUlog10$ABV_mean,0.95))+
  geom_smooth(method='lm',color='blue')+
  xlab('log10IBU+1')
  ggtitle("IBU vs ABV_mean")

  
#this is a good graph for IBU vs ABV_mean!!!!
  ggplot(aes(x=IBU, y=ABV_mean),data=(beer_by_IBUlog10))+
    geom_point(size=0.5)+
    geom_jitter(alpha=1/500)+
    geom_smooth(method='lm',color='blue')+
    xlab('log10IBU+1')+
    ggtitle("IBU vs ABV_mean")+
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
#here we calculate the correlation coeficient for the mean of ABV and IBU
cor.test(beer_by_IBUNOzeros$ABV_mean,beer_by_IBUNOzeros$IBU, method='pearson',alternative='greater')


#grouping by OG
OG_groups<-group_by(beer,OG)
beer_by_OG<-summarise(OG_groups,
                       ABV_mean=mean(ABV),
                       ABV_median=median(ABV),
                       n=n())
beer_by_OG<-arrange(beer_by_OG,OG)
summary(beer_by_OG)


#plot ABV mean by OG
ggplot(aes(x=OG, y=ABV_mean),data=(beer_by_OG))+
  geom_point()+
  geom_smooth(method='lm',color='blue')+
  coord_cartesian(xlim = c(1,34), ylim = c(0,30))+
  ggtitle("OG vs ABV_mean")+
  theme(plot.title = element_text(hjust = 0.5))


#here we calculate the correlation coeficient for the mean of ABV and IBU
cor.test(beer_by_OG$ABV_mean,beer_by_OG$OG, method='pearson',alternative='greater')


#group by STYLE
Style_groups<-group_by(beer,Style)
summary(Style_groups)
# Simple Bar Plot 


# top-10 common styles
top_10_styles <- beer %>%
  count(Style) %>%
  top_n(10)%>%
  arrange(n, Style)


#top 10 bar plot
top_10_stylesDF<- as.data.frame(top_10_styles)
ggplot(top_10_styles, aes(x = reorder(Style, n), y = n)) + 
  geom_bar(stat = "identity")+
  xlab("Styles of beer")+
  ylab("Frecuency")+
  ggtitle("Fequency of beer style")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


unique(beer$BoilTime)
#it owuld be interesting to see hwo the ABU is correlated with common beers... do ppl prefer more comoon because
#of alcohol content??


# top-10 beer with BRew method... later on if we figure out with ABV 'groups' we can do this
#here it would be good that the names are set vertically instead of the way they are!!
top_10_stylesALL <- beer %>%
  count(Style) %>%
  top_n(10) %>%
  arrange(n, Style) %>%
  mutate(Style = factor(Style, levels = unique(Style)))
beer %>%
  filter(Style %in% top_10_stylesALL$Style) %>%
  mutate(Style = factor(Style, levels = rev(top_10_styles$Style))) %>%
  ggplot(aes(x = Style,fill = BrewMethod))+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  geom_bar()

#now i want to subset the ABV for ranges of ABV
#need to transform it into integers first for it to be ranges 
beer$ABV_int<-as.integer(beer$ABV)
#here we arbitrarily set the ranges
beer$ABV_ranges<-cut(beer$ABV_int, c(0,3,4,5,6,8,10,50))
#We are just checjing the ranges counts
table(beer$ABV_ranges)
head(beer$SugarScale)
##Here we create a df with the ABV ranges and arrange them by order
ABV_ranges_count <- beer %>%
  count(ABV_ranges) %>%
  top_n(10) %>%
  arrange(n, ABV_ranges)
#here we check and realize there are 168 with NA values... since this is small number 
#I remove this from the DF
head(ABV_ranges_count)
ABV_ranges_count_NONA<-ABV_ranges_count[-c(1), ]

# Here we plot a pie chart with Percentages of ranges
slices <- c(ABV_ranges_count_NONA$n) 
lbls <- c("Extremely high(>10)", "Very low(0-3)", "Very High(8-10)", "High(6-8)", "Low(3-4)", "Medium High(5-6)", "Medium(4-5)")
pct <- round(ABV_ranges_count_NONA$n/sum(ABV_ranges_count_NONA$n)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=heat.colors(length(lbls)),
    main="ABV ranges") 

# top-10 beer with ABV ranges
#here it would be good that the names are set vertically instead of the way they are!!
top_10_stylesALL <- beer %>%
  count(Style) %>%
  top_n(10) %>%
  arrange(n, Style) %>%
  mutate(Style = factor(Style, levels = unique(Style)))
beer %>%
  filter(Style %in% top_10_stylesALL$Style) %>%
  mutate(Style = factor(Style, levels = rev(top_10_styles$Style))) %>%
  ggplot(aes(x = Style,fill = ABV_ranges))+
  geom_bar()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

#here we facet by ABV_ranges
top_10_stylesALL <- beer %>%
  count(Style) %>%
  top_n(10) %>%
  arrange(n, Style) %>%
  mutate(Style = factor(Style, levels = unique(Style)))
beer %>%
  filter(Style %in% top_10_stylesALL$Style) %>%
  mutate(Style = factor(Style, levels = levels(top_10_styles$Style))) %>%
  ggplot(aes(x = Style))+
  geom_bar()+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  facet_wrap(~ABV_ranges)

#facet IBU by ABV ranges
ggplot(aes(x = IBU), data = subset(beer, !is.na(ABV_ranges))) +
  geom_histogram() +
  geom_histogram(binwidth = 0.5)+
  coord_cartesian(xlim = c(0,300))+
  facet_wrap(~ABV_ranges)



#END OF BIVARIATE... BEGGING OF MULTAVARITE
#here we are going to plot 2x2 correalations of variables
#set seed
set.seed(1836)
#First we have to get rid of some columns that are not relevant for this kind of a nalysis
beer_subset <- beer[,6:20]
#here we drop the categorical variables
beer_subset$Color<- NULL
beer_subset$SugarScale<- NULL
beer_subset$BrewMethod<- NULL
#Replace the 'N/As'
beer_subset[beer_subset=='N/A']<- NA
#is.na(beer_subset)
#head(beer_subset)
#Here we remove all missing values accorss rows
beer_subset_Clean <- beer_subset[complete.cases(beer_subset[,1:12]), ]
#here we transform these variables to numeric
beer_subset_Clean$BoilGravity<-as.numeric(beer_subset_Clean$BoilGravity)
beer_subset_Clean$MashThickness<-as.numeric(beer_subset_Clean$MashThickness)
beer_subset_Clean$PitchRate<-as.numeric(beer_subset_Clean$PitchRate)
beer_subset_Clean$PrimaryTemp<-as.numeric(beer_subset_Clean$PrimaryTemp)

#here we do plot histogrames of 2x2 variables of the beer subset:
beer_subset <- pf[, c('age', 'dob_year', 'dob_month', 'gender', 'tenure')]
names(pf_subset)
ggpairs(beer_subset_Clean[sample.int(nrow(beer_subset_Clean), 1000),])


##THIS IS WHERE THE MULTIVARIATE ANALYSIS BEGINS
#here we are going to plot a correclation matrix of the clean data
corr_beer_matrix<-cor(beer_subset_Clean)
corrplot::corrplot(corr_beer_matrix)

#PCA analysis of the data
beer_pca <- prcomp(beer_subset_Clean)
#Here we get an idea of the principal components and STDEV
summary(beer_pca)
#here we standarize the variances becase of scales
beer_pca1_stan<- prcomp(beer_subset_Clean, scale. = TRUE)
summary(beer_pca1_stan)
summary_pca_stan<-summary(beer_pca1_stan)

#with the ggforty packge you can plot and calculate PCA easily
beer_pca1_stan$sdev
cumsum( beer_pca1_stan$sdev^2 / sum( beer_pca1_stan$sdev^2) )
plot( cumsum( pca$sdev^2 / sum( pca$sdev^2) ), type="o", ylim=c(0:1), ylab="Varianza", xlab="PC" )
#here we plot the cumulative proportions vs PC components
plot(cumsum( beer_pca1_stan$sdev^2 / sum( beer_pca1_stan$sdev^2) ), type="o", ylim=c(0:1), ylab="Cumulative proportion", xlab="PC#" )
#this would plot the 2 fisrst components as set by ABV_ranges
autoplot(prcomp(beer_subset, scale. = TRUE), data = beer, colour = 'ABV_ranges')

#this would plot the 2 compotnets by SugarScale without a scale of the data into consideration
autoplot(prcomp(beer_subset_Clean), data = beer, colour = 'SugarScale')

autoplot(prcomp(beer_subset_Clean), data = beer, colour = 'ABV_ranges')

#this would plot the 2 compotnets by SugarScale witha scale of the data into consideration
#ONLY this works now
autoplot(prcomp(beer_subset_Clean, scale. = TRUE))

#Here will plot PCA manyalt
beer <- beer[var]
scores <- predict( pca, beerpre )[,1:2]

###########
#FINAL PLOTS
#PLOT1
# Here we plot a pie chart with Percentages of ranges
slices <- c(ABV_ranges_count_NONA$n) 
lbls <- c("(>10)", "(0-3)", "(8-10)", "(6-8)", "(3-4)","(5-6)", "(4-5)")
pct <- round(ABV_ranges_count_NONA$n/sum(ABV_ranges_count_NONA$n)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=heat.colors(length(lbls)),
    main="ABV ranges proportion in beer recipe")
legend("topleft", c("Extremely high(>10)", "Very low(0-3)", "Very High(8-10)", "High(6-8)", "Low(3-4)",
                     "Medium High(5-6)", "Medium(4-5)"), cex=0.6, fill=heat.colors(length(lbls)))


#PLOT2
#here we are going to plot a correclation matrix of the clean data
corr_beer_matrix<-cor(beer_subset_Clean)
corrplot::corrplot(corr_beer_matrix,title="Correlation Matrix",mar=c(0,0,1,0))