setwd("C:/Users/davin/Documents/PhD/Writing/Heterogeneity/Supp_Data/YTK_growth_arrest/GA_growth_curves/")
library(readxl)
library(growthcurver)
library(tidyverse)
library(reshape2)
library(openxlsx)
library(plater)
library(directlabels)
library(gtools)
library(gridExtra)
library(tictoc)
library(ggpubr)

#plater import
plated <- read_plate(
  file = "230323_layout.csv",
  well_ids_column = "Wells")

#take away OD column from the plater tibble and transpose
platerbind <- plated[,1:2] %>% t()
##x<-(t(platerbind))
##aggregate(x,)


####ggplotting####
#save formatting as a theme
mytheme<-theme(axis.text.x =element_text(size=7),
               axis.text.y =element_text(size=5),
               axis.title =element_blank(),
               axis.line = element_line(size=1.2),
               axis.ticks = element_line(size=1.2),
               plot.title = element_text(size=7),
               strip.text = element_text(size=5),
               strip.text.x = element_text(margin = margin(.05, 0, .05, 0, "cm"), size=8),
               legend.position = "none")

#####functionise plotting#####
ggplotify.bc<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point(size=1) + 
    geom_line() +
    xlab("Hours") + 
    ## ylab("Blank-corrected 0D600") +
    facet_wrap(~variable, ncol=3) +
    theme_bw() +
    mytheme +
    scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 4)) +
    scale_y_continuous(limits = c(-0.1,0.9), breaks = seq(-0.1, 0.9, by = 0.2))}



#####FUNCTIONS#####
#extract OD function
##extractify.OD <- function(f){
##  excel <-read_excel(f, col_names=FALSE, 
##                     range = "B2:M9", col_types = c("numeric", 
##                                                     "numeric", "numeric", "numeric", 
##                                                   "numeric", "numeric", "numeric", 
##                                                 "numeric", "numeric", "numeric", 
##                                               "numeric", "numeric"))
##  transv <- as.vector((t(excel)))
##  return(transv)}

extractify.OD <- function (f){
  csv <- read.csv(f)
  ods <- csv[c(1:8),c(2:13)]
  transv <- as.vector(t(ods))
  return(transv)
}



#####SORTFILES#####
#folder for files containing OD

sortfiles<-mixedsort(list.files(pattern = '230323_t'))
sortfiles

#tidy up function - REMEMBER TO CHANGE TIME POINTS
tidy.up<-function(newseries){
  Time<-c("0","1","2","3","4","5","6","7","8","8.5")
  tns<-t(newseries)
  nice<-rbind(platerbind[2,], tns)
  samples<-as.data.frame(nice[c(1,3:nrow(nice)),], col.names=TRUE, stringsAsFactors=FALSE)
  colnames(samples)<-samples[1,]
  samples_clean<-samples[2:nrow(samples),]
  merged<-cbind(Time,samples_clean, stringsAsFactors=FALSE) 
  fornum <- merged[!is.na(names(merged))]
  num<- fornum %>% mutate_if(sapply(fornum, is.character), as.numeric)
  write.csv(num, "tidy_new.csv", row.names=FALSE) #######CHANGE FILE NAME
}


#check extractify
##length(sortfiles)
##extractify.OD(sortfiles[[22]])

####series####
#series dataframe for loop
series <- data.frame(well=c("A01","A02","A03","A04","A05","A06","A07","A08","A09","A10","A11","A12",
                            "B01","B02","B03","B04","B05","B06","B07","B08","B09","B10","B11","B12",
                            "C01","C02","C03","C04","C05","C06","C07","C08","C09","C10","C11","C12",
                            "D01","D02","D03","D04","D05","D06","D07","D08","D09","D10","D11","D12",
                            "E01","E02","E03","E04","E05","E06","E07","E08","E09","E10","E11","E12",
                            "F01","F02","F03","F04","F05","F06","F07","F08","F09","F10","F11","F12",
                            "G01","G02","G03","G04","G05","G06","G07","G08","G09","G10","G11","G12",
                            "H01","H02","H03","H04","H05","H06","H07","H08","H09","H10","H11","H12"))

#loop
sortfiles
for(f in sortfiles){series[[f]]<-cbind(extractify.OD(f))}
tidy.up(series)
tidied<-read.csv(file = "tidy_new.csv")



####Extraction, blank-correction and plotting#####---------------------------------

#####extraction#####

#extract sms
sm1<- select(tidied,1,ends_with("sm1"))
names(sm1)[names(sm1) == "blank_sm1"] <- "blank"

sm2<- select(tidied,1,ends_with("sm2"))
names(sm2)[names(sm2) == "blank_sm2"] <- "blank"

sm3<- select(tidied,1,ends_with("sm3"))
names(sm3)[names(sm3) == "blank_sm3"] <- "blank"

sm4<- select(tidied,1,ends_with("sm4"))
names(sm4)[names(sm4) == "blank_sm4"] <- "blank"

sm5<- select(tidied,1,ends_with("sm5"))
names(sm5)[names(sm5) == "blank_sm5"] <- "blank"

sm6<- select(tidied,1,ends_with("sm6"))
names(sm6)[names(sm6) == "blank_sm6"] <- "blank"

sm7<- select(tidied,1,ends_with("sm7"))
names(sm7)[names(sm7) == "blank_sm7"] <- "blank"





#####blank-correction#####
#note: [,3:ncol(df)-1] means 2nd column to 2nd to last column. [,3:(ncol(df)-1)] means 3rd column to 2nd to last column
sm1_bc<-sm1
sm1_bc[,2:(ncol(sm1_bc)-1)] = sm1_bc[,2:(ncol(sm1_bc)-1)] - sm1_bc[,ncol(sm1_bc)]
sm2_bc<-sm2
sm2_bc[,2:(ncol(sm2_bc)-1)] = sm2_bc[,2:(ncol(sm2_bc)-1)] - sm2_bc[,ncol(sm2_bc)]
sm3_bc<-sm3
sm3_bc[,2:(ncol(sm3_bc)-1)] = sm3_bc[,2:(ncol(sm3_bc)-1)] - sm3_bc[,ncol(sm3_bc)]
sm4_bc<-sm4
sm4_bc[,2:(ncol(sm4_bc)-1)] = sm4_bc[,2:(ncol(sm4_bc)-1)] - sm4_bc[,ncol(sm4_bc)]
sm5_bc<-sm5
sm5_bc[,2:(ncol(sm5_bc)-1)] = sm5_bc[,2:(ncol(sm5_bc)-1)] - sm5_bc[,ncol(sm5_bc)]
sm6_bc<-sm6
sm6_bc[,2:(ncol(sm6_bc)-1)] = sm6_bc[,2:(ncol(sm6_bc)-1)] - sm6_bc[,ncol(sm6_bc)]
sm7_bc<-sm7
sm7_bc[,2:(ncol(sm7_bc)-1)] = sm7_bc[,2:(ncol(sm7_bc)-1)] - sm7_bc[,ncol(sm7_bc)]



#melting
sm1_melt <- melt(sm1_bc, id.vars = "Time")
sm2_melt <- melt(sm2_bc, id.vars = "Time")
sm3_melt <- melt(sm3_bc, id.vars = "Time")
sm4_melt <- melt(sm4_bc, id.vars = "Time")
sm5_melt <- melt(sm5_bc, id.vars = "Time")
sm6_melt <- melt(sm6_bc, id.vars = "Time")
sm7_melt <- melt(sm7_bc, id.vars = "Time")



#plotting
sm1_plot<-ggplotify.bc(sm1_melt)
sm2_plot<-ggplotify.bc(sm2_melt)
sm3_plot<-ggplotify.bc(sm3_melt)
sm4_plot<-ggplotify.bc(sm4_melt)
sm5_plot<-ggplotify.bc(sm5_melt)
sm6_plot<-ggplotify.bc(sm6_melt)
sm7_plot<-ggplotify.bc(sm7_melt)




####ggarrange####

ggarrange(sm1_plot,sm2_plot,sm3_plot,sm4_plot,sm5_plot,sm6_plot,
          ncol = 3, nrow = 2)


##allplots<- list(sm1_plot,sm2_plot,sm3_plot,sm4_plot,sm5_plot,sm6_plot,sm7_plot)

##allplots


#make plot list for pdf generation
##plots<-list(sm1_plot,sm2_plot,sm3_plot,sm4_plot,sm5_plot,sm6_plot,sm7_plot,sm8_plot,sm9_plot,sm10_plot,sm11_plot,sm12_plot)


#individual plot generation in graphics window
sm1_plot
sm2_plot
sm3_plot
sm4_plot
sm5_plot
sm6_plot
sm7_plot


##--------------------------------REAL-TIME SCRIPT END------------------------------##






