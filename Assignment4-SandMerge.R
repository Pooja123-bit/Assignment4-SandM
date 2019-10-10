library("tidyverse")
load(file="fish_data.Rdata")
f=fish;rm(fish)

#tapply & merge

#Using the tapply function, find the mean of 'parcel.density.m3' 
#for each transect and assign the outcome to an object

M<-tapply(f$parcel.density.m3,f$transect.id, mean)
M

#Convert the object to a data frame

df.M<- as.data.frame(M)
df.M

#Rename the column with the density values to something more descriptive

colnames(df.M) = "mean density"  
df.M

#Assign the row names of the data frame to be the values 
#in a new field "transect"

transect=rownames(df.M)
rownames(df.M)=NULL
M1.df=cbind(transect,df.M)
M1.df

#Repeat the above steps, but this time using the tapply function
#to find the standard deviation of 'parcel.density.m3'

J<-tapply(f$parcel.density.m3,f$transect.id, sd)
J

df.J<- as.data.frame(J)
df.J

colnames(df.J) = "sd density"  
df.J

transect=rownames(df.J)
rownames(df.J)=NULL
J1.df=cbind(transect,df.J)
J1.df

#Using the merge function, combine the data frames with the mean 
#and standard deviation to create one, new data frame that has three columns (mean density, sd density, transect)

P1.df<-merge(x=M1.df, y=J1.df, by = "transect")
P1.df

#Repeat the above steps, but this time using the tapply function to find the count of observations for each transect for 'parcel.density.m3''

C<-tapply(f$parcel.density.m3,f$transect.id, sum)
C

df.C<- as.data.frame(C)
df.C

colnames(df.C)="count"
df.C

transect=rownames(df.C)
rownames(df.C)=NULL
C1.df=cbind(transect,df.C)
C1.df

#Using the merge function, combine the data frames with the mean and standard deviation to create
#one, new data frame that has three columns (mean density, sd density, count, and transect).

P2.df<-merge(x=P1.df, y=C1.df, by = "transect")
P2.df

#Summarize & join

#Using the group_by and summarise functions (tidyverse package), 
#find the mean of 'parcel.density.m3'for each transect and assign the outcome to an object.

library(tidyverse)
f %>% group_by(transect.id) %>% summarize(parcel.density.m3 = mean(parcel.density.m3, na.rm = TRUE))

MP<- f %>% group_by(transect.id) %>% summarize(parcel.density.m3 = mean(parcel.density.m3, na.rm = TRUE))
MP

#Convert the object to a data frame

df.MP<-as.data.frame(MP)
df.MP

#Rename the column with the density values to something more descriptive

names(df.MP)[names(df.MP) == "parcel.density.m3"] <- "mean density"  
df.MP

#Assign the row names of the data frame to be the values in a 
#new field "transect"

colnames(df.MP)[1]="transect"
df.MP

#Repeat the above steps, but this time using the tapply function 
#to find the standard deviation of'parcel.density.m3'

JP<-f %>% group_by(transect.id) %>% summarize(parcel.density.m3 = sd(parcel.density.m3, na.rm = TRUE))
JP

df.JP<-as.data.frame(JP)
df.JP

names(df.JP)[names(df.JP) == "parcel.density.m3"] <- "sd density"  
df.JP

colnames(df.JP)[1]="transect"
df.JP

#Using the join function (tidyverse package), combine the data 
#frames with the mean and standard deviation to create one, new data frame that has three columns (mean density, sd density, transect)

Q1.df<- full_join(df.MP, df.JP, by = "transect")
Q1.df

#Repeat the above steps, but this time using the tapply function 
#to find the count of observations for each transect for 'parcel.density.m3''.

CP<-f %>% group_by(transect.id) %>% summarize(parcel.density.m3 = sum(parcel.density.m3, na.rm = TRUE))
CP

#Another way to summarize
AP=aggregate(x=f$parcel.density.m3, by = list(f$transect.id), FUN = sum)
AP

df.CP<-as.data.frame(CP)
df.CP

names(df.CP)[names(df.CP) == "parcel.density.m3"] <- "count"  
df.CP

colnames(df.CP)[1]="transect"
df.CP

#Using the join function, combine the data frames with the mean 
#and standard deviation to create one, new data frame that has three columns (mean density, sd density, count, and transect).

Q2.df<-full_join(Q1.df, df.CP, by = "transect")
Q2.df

#Select any 2 fields (e.g. area, depth, year, transect) in the 
#fish_data.Rdata to group by

S1=tapply(f$parcel.length.m, list(f$area_fac), FUN = fivenum)
S1

S2=tapply(f$parcel.length.m, list(f$depth_fac), FUN = fivenum)
S2