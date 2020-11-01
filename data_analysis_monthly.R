attach(Datathon_COVID_Completed)
bp_dataframe = data.frame(
  date = as.Date(Date),
  count = total_count
)
View(bp_dataframe)
class(bp_dataframe$date)
jan<- subset(bp_dataframe,date >= "2020-01-01" & date <="2020-01-31")
jan_count = jan$count
feb<- subset(bp_dataframe,date >= "2020-02-01" & date <="2020-02-29")
feb_count = feb$count
mar<- subset(bp_dataframe,date >= "2020-03-01" & date <="2020-03-31")
mar_count = mar$count
april<- subset(bp_dataframe,date >= "2020-04-01" & date <="2020-04-30")
april_count = april$count
may<- subset(bp_dataframe,date >= "2020-05-01" & date <="2020-05-31")
may_count = may$count
June<- subset(bp_dataframe,date >= "2020-06-01" & date <="2020-06-30")
June_count = June$count
July<- subset(bp_dataframe,date >= "2020-07-01" & date <="2020-07-31")
July_count = July$count
Aug<- subset(bp_dataframe,date >= "2020-08-01" & date <="2020-08-31")
Aug_count = Aug$count
Sept<- subset(bp_dataframe,date >= "2020-09-01" & date <="2020-09-30")
Sept_count = Sept$count
Oct<- subset(bp_dataframe,date >= "2020-10-01" & date <="2020-10-31")
Oct_count = Oct$count
analyse_dataset = list(jan_count,feb_count,mar_count,april_count,may_count,June_count,July_count,Aug_count,Sept_count,Oct_count)
View(analyse_dataset)
boxplot(jan_count,feb_count,mar_count,april_count,may_count,June_count,July_count,Aug_count,Sept_count,Oct_count)
attach(district_count)
library(ggplot2)
western_df = data.frame(
  west_province = c("colombo","gampaha","kalutara"),
  count= c(district_count$`Colombo - Total`,district_count$`Gampaha - Total`,district_count$`Kalatura - Total`)
)
bar_1<-ggplot(data=western_df, aes(x=west_province, y=count)) + 
  geom_bar(stat="identity",width=0.5, fill="steelblue")+
  geom_text(aes(label=round((count)/sum(count)*100,digits=4)), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
bar_1 

northern_df = data.frame(
  northern_province = c("Jaffna","kilinochchi","mannar","mullaitivu","vavuniya"),
  count= c(district_count$`Jaffna - Total`,district_count$Kilinochchi,district_count$`Mannar - Total`,district_count$Mullaitivu,district_count$`Vavuniya - Total`)
)
bar_2<-ggplot(data=northern_df, aes(x=northern_province, y=count)) + 
  geom_bar(stat="identity",width=0.5, fill="lightgreen")+
  geom_text(aes(label=round((count)/sum(count)*100,digits=4)), vjust=1.6, color="black", size=3.5)+
  theme_minimal()
bar_2 

southern_df = data.frame(
  southern_province = c("galle","hambantota","matara"),
  count= c(district_count$`Galle - Total`,district_count$`Hambantota - Total`,district_count$`Matara - Total`)
)
bar_3<-ggplot(data=southern_df, aes(x=southern_province, y=count)) + 
  geom_bar(stat="identity",width=0.5, fill="darkblue")+
  geom_text(aes(label=round((count)/sum(count)*100,digits=4)), vjust=1.6, color="white", size=3.5)+
  theme_minimal()
bar_3

Uva_df = data.frame(
  Uva_province = c("Badulla","monaragala"),
  count= c(district_count$`Badulla - Total`,district_count$`Moneragala - Total`)
)
bar_4<-ggplot(data=Uva_df, aes(x=Uva_province, y=count)) + 
  geom_bar(stat="identity",width=0.5, fill="green")+
  geom_text(aes(label=round((count)/sum(count)*100,digits=4)), vjust=1.6, color="black", size=3.5)+
  theme_minimal()
bar_4


Eastern_df = data.frame(
  Eastern_province = c("trincomalee","Batticaloa","Ampara"),
  count= c(district_count$`Trincomalee - Total`,district_count$`Battocaloa - Total`,district_count$`Ampara - Total`)
)
bar_5<-ggplot(data=Eastern_df, aes(x=Eastern_province, y=count)) + 
  geom_bar(stat="identity",width=0.5, fill="blue")+
  geom_text(aes(label=round((count)/sum(count)*100,digits=4)), vjust=1.6, color="black", size=3.5)+
  theme_minimal()
bar_5


sabragamuwa_df = data.frame(
  sabragamuwa_province = c("Kegalle","Ratnapura"),
  count= c(district_count$`Kegalle - Total`,district_count$`Ratnapura - Total`)
)
bar_7<-ggplot(data=sabragamuwa_df, aes(x=sabragamuwa_province, y=count)) + 
  geom_bar(stat="identity",width=0.5, fill="steelblue")+
  geom_text(aes(label=round((count)/sum(count)*100,digits=4)), vjust=1.6, color="black", size=3.5)+
  theme_minimal()
bar_7

northwestern_df = data.frame(
  Northwstern_province = c("Puttalam","Kurunegala"),
  count= c(district_count$`Puttalam - Total`,district_count$`Kurunegala - Total`)
)
bar_8<-ggplot(data=northwestern_df, aes(x=Northwstern_province, y=count)) + 
  geom_bar(stat="identity",width=0.5, fill="skyblue")+
  geom_text(aes(label=round((count)/sum(count)*100,digits=4)), vjust=1.6, color="black", size=3.5)+
  theme_minimal()
bar_8


Central_df = data.frame(
Central_province = c("Kandy","Matale","Nuwera Eliya"),
count= c(district_count$`Kandy - Total`,district_count$`Matale - Total`,district_count$`Nuwara Eliya - Total`)
)
bar_6<-ggplot(data=Central_df, aes(x=Central_province, y=count)) +
geom_bar(stat="identity",width=0.5, fill="steelblue")+
geom_text(aes(label=round((count)/sum(count)*100,digits=4)), vjust=1.6, color="black", size=3.5)+
theme_minimal()
bar_6


NorthCentral_df = data.frame(
NorthCentral_province = c("Anuradhapura","Polonnaruwa"),
count= c(district_count$`Anuradhapura - Total`,district_count$`Polonnaruwa - Total`)
)
bar_9<-ggplot(data=NorthCentral_df, aes(x=NorthCentral_province, y=count)) +
geom_bar(stat="identity",width=0.5, fill="steelblue")+
geom_text(aes(label=round((count)/sum(count)*100,digits=4)), vjust=1.6, color="black", size=3.5)+
theme_minimal()
bar_9

attach(stack_dataset)
ggplot(stack_dataset, aes(fill=District, y=Frequenzy, x=Province)) + 
  geom_bar(position="fill", stat="identity")+
  ylab("percentage *100")
