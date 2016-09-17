# call source file with all required packages.
library(choroplethr)
library(Hmisc)         # to describe the data
library(ggmap)
library(plotly)
library('dplyr')      # for data manipulation
library('tidyr')      # for reshaping data
library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)
library("maps")
library(ggmap)
library(dplyr)

#Read the data
Crimedata = read.csv("C:/Users/harin/Documents/crime.csv")

# To understand what columns have nulls and types and details of all the columns in the data
summary(Crimedata)

# Dc_Dist           Psa                Dispatch_Date_Time    Dispatch_Date      Dispatch_Time    
# Min.   : 1.00   2      :339997   7/31/2014 14:28 :     35   8/4/2010 :    758   16:24:00:   1187  
# 1st Qu.: 9.00   1      :317755   5/1/2010 16:36  :     31   8/3/2010 :    754   16:47:00:   1164  
# Median :16.00   3      :285013   8/23/2012 12:04 :     26   7/27/2010:    751   16:36:00:   1162  
# Mean   :17.05   4      : 40516   11/9/2012 8:56  :     20   5/3/2011 :    740   16:32:00:   1152  
# 3rd Qu.:24.00   A      :  6693   5/1/2010 16:34  :     19   6/29/2011:    733   16:37:00:   1152  
# Max.   :92.00   K      :  4168   11/16/2013 10:04:     18   5/1/2010 :    727   16:26:00:   1151  
# (Other): 54433   (Other)         :1048426   (Other)  :1044112   (Other) :1041607  
# Hour           Dc_Key                                         Location_Block     UCR_General  
# Min.   : 0.00   Min.   :1.998e+11   4600 BLOCK E ROOSEVELT BLVD           :   2681   Min.   : 100  
# 1st Qu.: 9.00   1st Qu.:2.010e+11   1000 BLOCK MARKET ST                  :   1888   1st Qu.: 600  
# Median :14.00   Median :2.012e+11   5200 BLOCK FRANKFORD AVE              :   1419   Median : 800  
# Mean   :13.26   Mean   :2.012e+11   1300 BLOCK MARKET ST                  :   1413   Mean   :1276  
# 3rd Qu.:19.00   3rd Qu.:2.013e+11   1600 BLOCK S CHRISTOPHER COLUMBUS BLVD:   1389   3rd Qu.:1800  
# Max.   :23.00   Max.   :2.017e+11   0 BLOCK N 52ND ST                     :   1301   Max.   :2600  
# (Other)                               :1038484                 
# Text_General_Code  Police_Districts     Month             Lon              Lat       
# All Other Offenses            :205462   Min.   : 1.00    2010-08: 19193   Min.   :-75.28   Min.   :39.87  
# Thefts                        :125083   1st Qu.: 8.00    2010-07: 18716   1st Qu.:-75.19   1st Qu.:39.96  
# Other Assaults                :123282   Median :12.00    2010-05: 18354   Median :-75.16   Median :39.99  
# Vandalism/Criminal Mischief   : 97158   Mean   :11.98    2010-06: 18109   Mean   :-75.15   Mean   :39.99  
# Theft from Vehicle            : 79492   3rd Qu.:17.00    2011-07: 17929   3rd Qu.:-75.12   3rd Qu.:40.03  
# Narcotic / Drug Law Violations: 65100   Max.   :22.00    2011-08: 17913   Max.   :-74.96   Max.   :40.14  
# (Other)                       :352998   NA's   :8510     (Other):938361   NA's   :7233     NA's   :7233   

# To find what Crimes are present and a Total of 32 different types of crimes are filed.
unique(Crimedata$Text_General_Code)

# # 
# [1] Other Assaults                         
# [2] All Other Offenses                     
# [3] Weapon Violations                      
# [4] Thefts                                 
# [5] Burglary Non-Residential               
# [6] Aggravated Assault Firearm             
# [7] Theft from Vehicle                     
# [8] Disorderly Conduct                     
# [9] Vandalism/Criminal Mischief            
# [10] Arson                                  
# [11] Fraud                                  
# [12] Robbery No Firearm                     
# [13] Vagrancy/Loitering                     
# [14] Motor Vehicle Theft                    
# [15] Recovered Stolen Motor Vehicle         
# [16] Robbery Firearm                        
# [17] Embezzlement                           
# [18] Rape                                   
# [19] DRIVING UNDER THE INFLUENCE            
# [20] Forgery and Counterfeiting             
# [21] Narcotic / Drug Law Violations         
# [22] Burglary Residential                   
# [23] Other Sex Offenses (Not Commercialized)
# [24] Liquor Law Violations                  
# [25] Aggravated Assault No Firearm          
# [26] Homicide - Criminal                    
# [27] Gambling Violations                    
# [28] Prostitution and Commercialized Vice   
# [29] Public Drunkenness                     
# [30] Receiving Stolen Property              
# [31] Homicide - Gross Negligence            
# [32] Offenses Against Family and Children  

#Create Week days 
Crimedata$day <- weekdays(as.Date(Crimedata$Dispatch_Date))

#Create Year to check year wise analysis
Crimedata$Year <- year(as.Date(Crimedata$Dispatch_Date)) 

#Subset data based on different types of Crimes 
Thefts <- subset(Crimedata, Text_General_Code == "Thefts" | Text_General_Code == "Rape" )
Thefts_Rape <- subset(Crimedata, Text_General_Code == "Thefts" | Text_General_Code == "Rape" )
Homicide <- subset(Crimedata, Text_General_Code == "Homicide - Criminal" )
MTheft = subset(Thefts, day="Monday")
TTheft = subset(Thefts, day="Tuesday")
Homicide =  na.exclude(Homicide)
Monday  =  subset(Homicide , day =="Monday") 
Tuesday = subset(Homicide , day =="Tuesday") 
Wednesday= subset(Homicide , day =="Wednesday") 
Thursday= subset(Homicide , day =="Thursday") 

#Creating Map pdf to check the steerts where most crimes occured
pdf("P_Map.pdf")
Pmap <- qmap("philadelphia", zoom = 14, color = "bw")
dev.off()


pdf("Theft and Rape Locations.pdf")
Pmap +
  geom_point(aes(x = Lon, y = Lat, colour = factor(Thefts$Text_General_Code) , Legend = "Representation"),
             data = Thefts_Rape) +ggtitle("Theft & Rape Locations")
dev.off()

pdf("Theft Locations.pdf")
Pmap +
  geom_point(aes(x = Lon, y = Lat, colour = "red"),
             data = Thefts) +ggtitle("Theft Locations")
dev.off()

pdf("Homicide Locations.pdf")
Pmap +
  geom_point(aes(x = Lon, y = Lat, colour = "red", size=2),
             data = Homicide) +ggtitle("Homicide Locations")
dev.off()

pdf("Contour_Homicide.pdf")
Pmap +
  stat_density2d(
    aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..),
     bins = 4, data = Homicide,
    geom = "polygon"
  ) + ggtitle("Contour/Polygon Representation of Homicide Locations")
dev.off()

pdf("Contour_Eachday_Hom.pdf")
Pmap +
  stat_density2d(aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..),
                 bins = 5, geom = "polygon",
                 data = Homicide) +
  scale_fill_gradient(low = "black", high = "red") +
  facet_wrap(~ day) + ggtitle(" Hom Locations on each day")
dev.off()

pdf("Contour_Hom.pdf")
Pmap +
  stat_density2d(aes(x = Lon, y = Lat, fill = ..level.., alpha = ..level..),
                 bins = 2, geom = "polygon",
                 data = Monday) +
  scale_fill_gradient(low = "black", high = "red")
dev.off()

# Create a  graph that shows if crimes are increased or decresed over years
ggplot(data=Crimedata, aes(x=Year)) +   geom_bar(colour="black", fill="blue") +
  ylab('Count') +   facet_wrap(~Text_General_Code)


# Create a  graph that shows if Crimes in different Districts are increased or decresed over years
ggplot(data=Crimedata, aes(x=Year)) +
  geom_bar(colour="black", fill="purple") +
  ylab('Count') +
  facet_wrap(~Police_Districts)
