##############FINAL DAMSEL CODE##############################################################
####Load packages and Load Data####

library(rsq)
library(ggplot2)
library(RcmdrMisc)
library(png)
library(grid)


#Path to datasheet
damsel_data <- read.csv("C:\\Users\\poone\\Dropbox\\FBQ Moorea 2020\\Prickly Fish\\data_analysis_28Feb20_FINAL.csv")


####Testing for Collinearity and Normality of Continuous Explanatory Variables####
#In order to run a linear model, we had to first test that the assumptions of  linear model were met. This means that the explanatory variables cannot be colinear and must be normally distributed. The response variable was also tested for normality. We set our threshold for colinearity at a correlation coefficient of 0.7. Distributions of variables were plotted to check for for any strong deviations from normality.

####Checking Collinearity####

#filter only continuous data
cont_damsel_data <- as.data.frame(c(damsel_data["Temp_Celsius"], damsel_data["fish_size_cm"], damsel_data["Start_Dist_cm"], damsel_data["percent_coral_cover"], damsel_data["rugosity_sum_cm"], damsel_data["urchin_density"]))

#correlation between variables
cor(cont_damsel_data)
#rugosity and percent coral cover have a higher correlation of 0.64, so we will remove rugosity from our final model (similar results in Chan et al 2018)


####Checking Distributions####

#Temperature
hist(damsel_data$Temp_Celsius)
hist(log10(damsel_data$Temp_Celsius))
#slightly skewed right, log doesn't fix it. leaving untransformed

#Fish Size
hist(damsel_data$fish_size_cm)
#normal. okay untransformed

#FID
hist(damsel_data$FID_cm)
#normal. okay to use

#Starting Distance
hist(damsel_data$Start_Dist_cm)
#normal. okay to use 

#Percent Coral Cover
hist(damsel_data$percent_coral_cover)
#skewed left. not transforming

#Urchin Density
hist(damsel_data$urchin_density)
hist(log10(damsel_data$urchin_density+1))
#slightly skewed but will leave untransformed because log transform doesn't help

####Descriptive Statistics of Explanatory Variables including Mean, Standard Deviation, and Ranges####


#Mean and standard deviation for independent and continuous variables included in final model
numSummary(damsel_data[,c("fish_size_cm", "Start_Dist_cm","Temp_Celsius","percent_coral_cover","urchin_density"), drop=FALSE],statistics=c("mean","sd","quantiles"))

#Ranges for Independent Variables 

#Fish Size (cm)
paste(min(damsel_data$fish_size_cm),",", max(damsel_data$fish_size_cm))

#Starting Distance (cm)  
paste(min(damsel_data$Start_Dist_cm),",",max(damsel_data$Start_Dist_cm))

#Temperature (Degrees Celsius)
paste(min(damsel_data$Temp_Celsius),",",max(damsel_data$Temp_Celsius))

#Percent Coral Cover
paste(min(damsel_data$percent_coral_cover), ",", max(damsel_data$percent_coral_cover))

#Urchin Density (Urchins/m^2)  
paste(min(damsel_data$urchin_density), ",", max(damsel_data$urchin_density))


 ####Final Model####
#A linear model was fit to the explanatory variables of interest. We used the adjusted R-squared value to determine how well the model fit the data, and also looked at what variables showed significance.

#Explanatory variables: Start distance, Temperature, Continuity of Bommie, Percent Coral Cover, Fish Size, Urchin Density, interaction between starting distance and urchin density, interaction between fish size and percent coral cover
fm <- lm(FID_cm~ Start_Dist_cm+ Temp_Celsius+ cont_or_iso+ percent_coral_cover+fish_size_cm+ urchin_density+Start_Dist_cm*urchin_density+ fish_size_cm*percent_coral_cover, data=damsel_data)

##Summary of Linear Model
summary.lm(fm)

#model explains 33.4% of the variation in the data

##Analysis of Variance for Linear Model
damsel_anova <- anova(fm)

##Partial R^2 for Explanatory Variables was determined to see how much of the variation each variable in the model explained. 
rsq.partial(fm)

####Checking Linear Model Assumptions ####
#A histogram of the residuals of the model and a qqplot were plotted to check for normality, an assumption of a linear model. Fitted values were also plotted against residuals to confirm there were no obvious pattern. 

r <- residuals(fm)
f <- fitted(fm)

#Histogram of Residuals
hist(r)

#Qqplot of Residuals 
qqnorm(r)

##Fitted Values against Residuals 
plot(f,r)

####Plot of Main Effects####
#To visualize the interaction between urchin density and starting distance on explaining variation in FID, we split urchin density by greater or fewer urchins. The median of urchin density was taken. All urchin densities less than the median were called "Fewer Urchins" and all densities greater than or equal to the mean were called "More Urchins". The relationship between FID and SD was plotted separately between the two groups as a way to look at the interaction.


#creating new column in data
damsel_data$urchin_density_split <- NA

#Fewer than the median is "fewer urchins"
damsel_data$urchin_density_split[damsel_data$urchin_density < median(damsel_data$urchin_density)] <- "Fewer Urchins"

#Greater than or equal to the median is "more urchins"
damsel_data$urchin_density_split[damsel_data$urchin_density >= median(damsel_data$urchin_density)] <- "More Urchins"
damsel_plot <- ggplot(damsel_data,aes(x=Start_Dist_cm, y=FID_cm, group=urchin_density_split))+
  geom_point(aes(shape=damsel_data$urchin_density_split),size=3, show.legend = F)+
  geom_smooth(method=lm, se=T, aes(linetype=damsel_data$urchin_density_split), show.legend = F)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Starting Distance (cm)")+
  ylab("Flight Initiation Distance (cm)")+
  #theme(legend.position = c(0.2, 0.8))+
  theme(legend.title=element_blank())+
  theme(text = element_text(size = 20, colour = "black"))+
  coord_fixed(ratio=1, expand=T, clip="on")+
  scale_x_continuous(breaks = seq(150,300,25))+
  theme(axis.text.x= element_text(colour = "black"))+
  theme(axis.text.y= element_text(colour="black"))


ggsave("damsels.png", damsel_plot)

#adding image to plot
urchin_image <- png::readPNG("C:/Users/poone/Dropbox/FBQ Moorea 2020/Prickly Fish/Clip art/urchin_clipart.png")
g <-  rasterGrob(urchin_image, interpolate=TRUE)
damsel_image <- png::readPNG("C:/Users/poone/Dropbox/FBQ Moorea 2020/Prickly Fish/Clip art/fish.png")
h <- rasterGrob(damsel_image, interpolate=TRUE)
urchin_plot <- damsel_plot+
  annotation_custom(grob=g, xmin=271, xmax=279, ymin=99, ymax=110)+
  annotation_custom(grob=g, xmin=278, xmax=286, ymin=99, ymax=110)+
  annotation_custom(grob=g, xmin=274, xmax=282, ymin=102, ymax=113)+
  annotation_custom(grob=g, xmin=288, xmax=295, ymin=90, ymax=100)+
  annotation_custom(grob=h, xmin=180, xmax=220, ymin=100, ymax=145)

ggsave("damsels_urchins.png", urchin_plot)


####Size Calibrations####

#To ensure that the fize sizes were estimated correctly, we had the observer estimate 9 objects in the size range of the fish. Estimations were done underwater and from approximately the range of starting distances used. The observer was more comfortable estimating in inches, so estimates were converted to centimeters. The true size of the object was measured, and the correlation between the two values was noted using a linear model. 

True_Size_in <- c(2,2.5,1.75,4.75,1.25,3,2.75,5,4.5)
True_Size_cm <- True_Size_in*2.54
Estimated_Size_in <- c(2,2.5,1,4,1,3,3,4.5,4.5)
Estimated_Size_cm <- Estimated_Size_in*2.54
size_calibrations <- data.frame(True_Size_cm,Estimated_Size_cm)
model<- lm(Estimated_Size_cm~True_Size_cm, data=size_calibrations)
summary.lm(model)


#Plotting correlatioin between estimated and true values
scatterplot(size_calibrations$True_Size_cm,size_calibrations$Estimated_Size_cm, smooth=F, main="Size Calibrations", xlab="True Size (cm)", ylab="Estimated Size (cm)")
