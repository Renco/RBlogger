#Link for this code original post 
#http://rud.is/b/2015/08/27/coloring-and-drawing-outside-the-lines-in-ggplot/

##Get libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(scales)
library(gridExtra)
library(grid)           

#Fetching data 
url <- "http://apps.npr.org/dailygraphics/graphics/women-cs/data.csv"
fil <- "gender.csv"
if (!file.exists(fil)) download.file(url,fil) #Learn This is to download
#data to a local file, you still need to read data from that file into
#workplace 

gender <- read.csv(fil) 

glimpse(gender) #This is handy, it depends on package dplyr

tail(gender)

#Replace tk values into NAs, which is R-compatible
#gender <- mutate_each(gender,funs(as.numeric))
#another handy function in dplyr
#somehow doesn't work with this data
#I write something with same outcome 
gender <- mutate_each(gender, funs(as.character))
gender <- mutate_each(gender, funs(as.numeric))
#to change factor into numeric, change them into character first 
#gender[46:48,3:4] <- 1
#gender <- mutate_each(gender,funs(as.numeric(as.character(dfA1))))

##sometimes as.numeric change values into factors, 
##this prevent that from happening 

#Change column names 
colnames(gender) <- str_replace(colnames(gender),"\\."," ")

gender_long <- mutate(gather(gender, area, value, -date),
                      area=factor(area, levels=colnames(gender)[2:5],
                                  ordered=TRUE))
#TidyR is another greate package we can use

#Colors
gender_colors <- c('#11605E', '#17807E', '#8BC0BF','#D8472B')
names(gender_colors) <- colnames(gender)[2:5]

gender_long["value"] <- as.numeric(gender_long["value"])

##GGPLOT
chart_title <- expression(atop("What Happened To Women in Computer Science?",
                               atop(italic("% Of Women Majors, By Field"))))

gg <- ggplot(gender_long)
gg <- gg + geom_line(aes(x=date, y=value, group=area, color=area))
gg <- gg + scale_color_manual(name="", values=gender_colors)
#gg <- gg + scale_y_continuous(label=percent)
gg <- gg + labs(x=NULL, y=NULL, title=chart_title)
gg <- gg + theme_bw(base_family="Helvetica")
gg <- gg + theme(axis.ticks.y=element_blank())
gg <- gg + theme(panel.border=element_blank())
gg <- gg + theme(legend.key=element_blank())
gg


#####testtest