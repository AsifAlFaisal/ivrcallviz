## Copyright - Asif Al Faisal, CIMMYT-Bangladesh


setwd("D:\\CIMMYT\\Khaled Vai")

library(readxl)
library(ggplot2)
library(tidyverse)
library(scales)
library(RColorBrewer)

ivr.data <- read_excel("190904 - IVR_ Calls Report.xlsx",sheet = "IVR_All Calls_Farmers")

typeof(ivr.data)

ivr.data <- as.data.frame(ivr.data)

subset.cols <- c("UID","Gender","Union","A/B_Group","cast_rain", "Advisory_Deployed","Message_total_duration",
                 "Call_Status","Message_Listened_Duration",	"times_listen_message",	
                 "Call_Fail_Reason_Descrip",	"call_back", "Source_farmers_phone_ number")

ivr.data.subset <- ivr.data[,subset.cols]

cols.factor <- c("Gender","Union","A/B_Group", "cast_rain", "Advisory_Deployed", "Call_Status",
                  "Call_Fail_Reason_Descrip",	"call_back","Source_farmers_phone_ number")


ivr.data.subset[cols.factor] <- lapply(ivr.data.subset[cols.factor], factor)

str(ivr.data.subset)

unique.farmer <- ivr.data.subset[which(!duplicated(ivr.data.subset$UID)),]

## Plotting each union based of gender


ug_plot <- ggplot(unique.farmer, aes(x= Gender,  fill=Gender)) + 
  geom_bar(position = "dodge", alpha = 0.8) +
  geom_text(aes(label=..count..),stat = "count", position = position_dodge(0.9), vjust=-0.4) + 
  xlab("Union") + 
  ylab("Number of farmers") + 
  ggtitle("Farmers demographic information by union (Total farmers = 1372)") +
  scale_fill_manual(values = c("#31a354","#2b83ba"))+
  facet_wrap(~Union) 
  # theme(axis.title.x = element_text(size=15),
  #       axis.title.y = element_text(size=15),
  #       axis.text.x = element_text(size=10),
  #       axis.text.y = element_text(size=10),
  #       
  #       plot.title = element_text(size=20),
  #       
  #       legend.title = element_text(size=12),
  #       legend.text = element_text(size=12))

ug_plot +theme_bw()



## Call Status Success or Failed


call_plot <- ggplot(ivr.data.subset, aes(x= Gender,  fill=Call_Status)) + 
  geom_bar(position = "dodge2",alpha=0.8) +
  geom_text(aes(label=..count..),stat = "count", position = position_dodge(0.9), vjust=-0.4) + 
  xlab("Union") + 
  ylab("Number of calls") + 
  ggtitle("Number of calls recieved vs failed (Total generated call = 18321)") +
  scale_fill_manual("Call status",values = c("#d53e4f","#1a9850"))+
  facet_wrap(~Union)

call_plot + theme_bw()



# ## Number of Advisories Deployed
# 
# 
# adv_plot <- ggplot(ivr.data.subset, aes(x= Union,  fill=Advisory_Deployed)) + 
#   geom_bar(position = "dodge2", alpha=0.8) +
#   geom_text(aes(label=..count..),stat = "count", position = position_dodge(0.9), vjust=-0.4) + 
#   xlab("Union") + 
#   ylab("Number of advisories deployed") + 
#   ggtitle("Advisories deployment by union") + 
#   scale_fill_manual("Advisories deployment",values = c("#d53e4f","#1a9850"))
# 
# adv_plot + theme_bw()


## Call Failure Reason

call.fail.subset <- ivr.data.subset[!is.na(ivr.data.subset$Call_Fail_Reason_Descrip),]

failure_plot <- ggplot(call.fail.subset, aes(x= Call_Fail_Reason_Descrip, 
                                             fill=Call_Fail_Reason_Descrip)) + 
  geom_bar(alpha=0.7) +
  geom_text(aes(label=..count..),stat = "count", position = position_dodge(0.9), vjust=-0.4) + 
  xlab("Reasons for call failure") + 
  ylab("Number of calls") + 
  ggtitle("Reasons for call failure (Total failed call = 7693)")  + 
  scale_fill_manual("Call failure reason",
                    values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')) + 
  coord_flip()

failure_plot + theme_bw()


## Call duration distribution graph more than 1 time

msg.dur.subset <- ivr.data.subset[!is.na(ivr.data.subset$Message_Listened_Duration),]
#msg.dur.subset <- msg.dur.subset[msg.dur.subset$times_listen_message >= 2,]
msg.dur.subset <- msg.dur.subset[msg.dur.subset$times_listen_message >= 1,]


## Density Curve By Union

 # nrow(msg.dur.subset[msg.dur.subset$Union=="Betagi Sankipur",])
 # nrow(msg.dur.subset[msg.dur.subset$Union=="Choto Bighai",])
 # nrow(msg.dur.subset[msg.dur.subset$Union=="Gulisakhali",])

dist_dur_plot <- ggplot(msg.dur.subset, aes(x=Message_Listened_Duration, fill=Union)) + 
  geom_density(alpha=.5) + 
  xlab("Duration of message listened (seconds)") + 
  ylab("Proportion of the listeners") + 
  ggtitle("Farmers who listened the whole message & more by union (Total farmer=6979)") + 
  scale_fill_manual(labels=c("Betagi sankipur(2465)","Choto bighai(2529)","Gulisakhali(1985)"),
                    values = c("#b2182b","#762a83","#00441b"))+
  facet_wrap(~Union) + theme_bw()

dist_dur_plot

## Histogram By Union

hist_dur_plot <- ggplot(msg.dur.subset, aes(x=Message_Listened_Duration, fill=Union)) + 
  geom_histogram(position='identity', alpha=0.6, color='black', bins = 20) + 
  xlab("Duration of message listened (seconds)") + 
  ylab("Number of farmers") + 
  ggtitle("Farmers who listened the whole message & more by union (Total farmer=6979)") + 
  scale_fill_manual(labels=c("Betagi sankipur(2465)","Choto bighai(2529)","Gulisakhali(1985)"),
                    values = c("#b2182b","#762a83","#00441b"))+
  facet_wrap(~Union) + theme_bw()

hist_dur_plot


## Density Curve by Gender
#nrow(msg.dur.subset[msg.dur.subset$Gender=="Female",])

dist_dur_plot <- ggplot(msg.dur.subset, aes(x=Message_Listened_Duration, fill=Gender)) + 
  geom_density(alpha=0.4) + 
  xlab("Duration of message listened (seconds)") + 
  ylab("Proportion of the listeners") + 
  ggtitle("Farmers who listened the whole message & more by gender (Total farmer=6979)") +
  scale_fill_manual(labels = c("Female(586)", "Male(6393)"), values = c("#b2182b","#00441b"))+
  facet_wrap(~Gender)
dist_dur_plot + theme_bw()

## Histogram By Gender

#nrow(msg.dur.subset[msg.dur.subset$Gender=="Male",])

hist_dur_plot <- ggplot(msg.dur.subset, aes(x=Message_Listened_Duration, fill=Gender)) + 
  geom_histogram(position='identity', alpha=0.6, color='black', bins = 20) + 
  #facet_wrap(~Gender) + 
  xlab("Duration of message listened (seconds)") + 
  ylab("Number of farmers") +
  ggtitle("Farmers who listened the whole message & more by gender (Total farmer=6979)") +
  scale_fill_manual(labels = c("Female(586)", "Male(6393)"), values = c("#b2182b","#00441b"))+
  facet_wrap(~Gender)

hist_dur_plot + theme_bw()


## Density Curve by Gender grouped by A/B Group
#nrow(msg.dur.subset[msg.dur.subset$Gender=="Female",])

dist_dur_plot <- ggplot(msg.dur.subset, aes(x=Message_Listened_Duration, fill=Gender)) + 
  geom_density(alpha=0.4) + 
  xlab("Duration of message listened (seconds)") + 
  ylab("Proportion of the listeners") + 
  ggtitle("Group A/B farmers who listened the whole message & more by gender (Total farmer=6979)") +
  scale_fill_manual(labels = c("Female(586)", "Male(6393)"), values = c("#b2182b","#00441b"))+
  #facet_wrap(~Gender)+
  facet_wrap(Gender~`A/B_Group`)
dist_dur_plot + theme_bw()

## Histogram By Gender grouped by A/B Group
#nrow(msg.dur.subset[msg.dur.subset$Gender=="Male",])

hist_dur_plot <- ggplot(msg.dur.subset, aes(x=Message_Listened_Duration, fill=Gender)) + 
  geom_histogram(position='identity', alpha=0.6, color='black', bins = 20) + 
  #facet_wrap(~Gender) + 
  xlab("Duration of message listened (seconds)") + 
  ylab("Number of farmers") + 
  ggtitle("Group A/B farmers who listened the whole message & more by gender (Total farmer=6979)") +
  scale_fill_manual(labels = c("Female(586)", "Male(6393)"), values = c("#b2182b","#00441b"))+
  #facet_wrap(~Gender)+
  facet_wrap(Gender~`A/B_Group`) + theme_bw()

hist_dur_plot




## Density Curve by A/B Group

#nrow(msg.dur.subset[msg.dur.subset$`A/B_Group`=="A",])
#nrow(msg.dur.subset[msg.dur.subset$`A/B_Group`=="B",])

dist_dur_plot <- ggplot(msg.dur.subset, aes(x=Message_Listened_Duration, fill=`A/B_Group`)) + 
  geom_density(alpha=0.3) + 
  xlab("Duration of message listened (seconds)") + 
  ylab("Proportion of the listeners") + 
  ggtitle("Farmers who listened the whole message & more by A/B group (Total farmer=6979)") +
  scale_fill_manual("A or B Group",
                    labels = c("Group-A(3189)", "Group-B(3790)"),
                    values = c("#b2182b","#00441b"))+
  facet_wrap(~`A/B_Group`) + 
  theme_bw()
dist_dur_plot

## Histogram By A/B Group

hist_dur_plot <- ggplot(msg.dur.subset, aes(x=Message_Listened_Duration, fill=`A/B_Group`)) + 
  geom_histogram(position='identity', alpha=0.3, color='black', bins = 30) + 
  #facet_wrap(~`A/B_Group`) + 
  xlab("Duration of message listened (seconds)") + 
  ylab("Number of farmers") + 
  ggtitle("Farmers who listened the whole message & more by A/B group (Total farmer=6979)") +
  scale_fill_manual("A or B Group",
                    labels = c("Group-A(3189)", "Group-B(3790)"),
                    values = c("#b2182b","#00441b"))+
  facet_wrap(~`A/B_Group`) + 
  theme_bw()
hist_dur_plot



## Density Curve Listen Duration

dist_dur_plot <- ggplot(msg.dur.subset, aes(x=Message_Listened_Duration)) + 
  geom_density(alpha=0.4,fill="#762a83") + 
  xlab("Duration of message listened (seconds)") + 
  ylab("Proportion of the listeners") + 
  ggtitle("Farmers who listened the whole message & more (Total farmer=6979)") +
  theme_bw()

dist_dur_plot

## Histogram Listen Duration


hist_dur_plot <- ggplot(msg.dur.subset, aes(x=Message_Listened_Duration)) + 
  geom_histogram(position='identity', alpha=0.4, color='black', bins = 40, fill="#762a83") +
  xlab("Duration of message listened (seconds)") + 
  ylab("Number of farmers") + 
  ggtitle("Farmers who listened the whole message & more (Total farmer=6979)") +
  theme_bw()

hist_dur_plot




## Source of Farmers Phone Numbers

num_source_plot <- ggplot(ivr.data.subset, aes(`Source_farmers_phone_ number`)) +
  geom_bar(stat = "count") + 
  coord_flip() + 
  geom_text(aes(label=..count..),stat = "count", position = position_dodge(0.9), hjust=-0.2) + 
  xlab("Farmers number collection source") + 
  ylab("Number of farmers") + 
  ggtitle("Sources of farmers number collection (Total numbers = 18321)") + 
  theme_bw()
num_source_plot



## Bubble plot of call listen duration

msg.dur.subset2 <- msg.dur.subset

msg.dur.subset2$times_listen_message <- as.integer(msg.dur.subset2$times_listen_message)

#colnames(msg.dur.subset2)[which(names(msg.dur.subset2) == "Message_Listened_Duration")] <- "Message listened duration"

bubble_plot <- ggplot(msg.dur.subset2, aes(y=times_listen_message, x=Message_Listened_Duration, 
                           fill = Message_Listened_Duration)) +
  geom_point(aes(size = Message_Listened_Duration), alpha=0.5, shape=21, color="black")+
  scale_size(range = c(1, 20))+
  xlab("Duration of message listened (seconds)") +
  ylab("Number of times message listened") + 
  ggtitle("Message listen duration by union grouped by gender")+
  scale_y_continuous(breaks=c(0,1,2,3,4), limits = c(0,4)) +
  scale_x_continuous(breaks=c(30,60,90,120,150), limits = c(30,160)) +
  guides(size = FALSE) +
  facet_grid(Union~Gender)+
  labs(fill="Message duration")+
  theme_bw()
bubble_plot



# ## Density Curve by Advisory Deployed
# 
# nrow(msg.dur.subset[msg.dur.subset$Advisory_Deployed=="NO",])
# 
# dist_dur_plot <- ggplot(msg.dur.subset, aes(x=Message_Listened_Duration, 
#                                             fill= Advisory_Deployed)) + 
#   geom_density(alpha=0.3) + 
#   xlab("Duration of Message Listed") + 
#   ylab("Proportion of the Listeners") + 
#   ggtitle("Farmers who Listened the Message Two times & More by Advisory Deployed (Total Farmer=296)") +
#   theme(axis.title.x = element_text(size=15),
#         axis.title.y = element_text(size=15),
#         axis.text.x = element_text(size=10),
#         axis.text.y = element_text(size=10),
#         
#         plot.title = element_text(size=20),
#         
#         legend.title = element_text(size=12),
#         legend.text = element_text(size=12)) + 
#   scale_fill_manual("Advisories Deployed or Not",
#                     labels = c("NO (282)","YES (14)"),
#                     values = c("#b2182b","#00441b")) +
#   facet_wrap(~Advisory_Deployed)
# dist_dur_plot
# 
# ## Histogram By Advisory Deployed
# 
# hist_dur_plot <- ggplot(msg.dur.subset, aes(x=Message_Listened_Duration, fill=Advisory_Deployed)) + 
#   geom_histogram(position='identity', alpha=0.6, color='black', bins = 30) + 
#   #facet_wrap(~Advisory_Deployed) + 
#   xlab("Duration of Message Listed") + 
#   ylab("Number of Farmers")  + 
#   ggtitle("Farmers who Listened the Message Two times & More by Advisory Deployed (Total Farmer=296)") +
#   theme(axis.title.x = element_text(size=15),
#         axis.title.y = element_text(size=15),
#         axis.text.x = element_text(size=10),
#         axis.text.y = element_text(size=10),
#         
#         plot.title = element_text(size=20),
#         
#         legend.title = element_text(size=12),
#         legend.text = element_text(size=12)) + 
#   scale_fill_manual("Advisories Deployed or Not",
#                     labels = c("NO (282)","YES (14)"),
#                     values = c("#b2182b","#00441b")) +
#   facet_wrap(~Advisory_Deployed)
# hist_dur_plot


## Plotting each union based of callback


clback_plot <- ggplot(ivr.data.subset, aes(x= call_back,  fill=call_back)) + 
  geom_bar(position = "dodge", alpha = 0.8) +
  geom_text(aes(label=..count..),stat = "count", position = position_dodge(0.9), vjust=-0.4) + 
  xlab("Union") + 
  ylab("Number of farmers") + 
  ggtitle("Number of calls returned (Total farmers = 18321)") +
  scale_fill_manual(values = c("#d53e4f","#1a9850"))+
  facet_wrap(~Union) 

clback_plot +theme_bw()




