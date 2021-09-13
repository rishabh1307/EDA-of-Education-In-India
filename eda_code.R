
df_sec <- read.csv("2015_16_Statewise_Secondary.csv")

View(df_sec)
dim(df_sec)
names(df_sec)

df_ele <- read.csv("2015_16_Statewise_Elementary.csv")

View(df_ele)
dim(df_ele)
names(df_ele)

sum(is.na(df_sec))
sum(is.na(df_ele))

sum(duplicated(df_sec))
sum(duplicated(df_ele))



for(i in 1:ncol(df_sec)){
  df_sec[is.na(df_sec[,i]), i] <- mean(df_sec[,i], na.rm = TRUE)
}


for(i in 1:ncol(df_ele)){
  df_ele[is.na(df_ele[,i]), i] <- mean(df_ele[,i], na.rm = TRUE)
}

df_ele$TOTPOPULAT[19]
df_ele$TOTPOPULAT[19]<-991348/10
#outlier removal

library("ggplot2")
library(dplyr)
library(tidyr)

plot1 <- ggplot(df_sec, aes(x=tot_population, y=reorder(statname, tot_population))) + 
  geom_bar(stat = "identity",fill='brown')+  # Y axis is explicit. 'stat=identity'
  ggtitle("STATE VS TOTAL POPULATION") +
  xlab("TOTAL POPULATION") + ylab("STATENAME")

print(plot1)

#The plot provides an overview as to where we can focus our resources.

ggplot(data=df_sec, aes(x=reorder(statname, literacy_rate), y=literacy_rate, group=1)) +
  geom_line(linetype = "dashed")+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("STATE VS LITERACY RATE") +
  xlab("STATENAME") + ylab("LITERACY")+
  geom_text(aes(label=literacy_rate), vjust=-0.3, size=3.5)

#We can see almost all in India states have more than 50% literacy rate


ggplot(data=df_ele, aes(x=reorder(STATNAME,OVERALL_LI), y=OVERALL_LI, fill=SCHTOT)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("LITERACY RATE VS TOTAL SCHOOLS RELATION") +
  xlab("STATE") + ylab("LITERACY RATE")+
  geom_text(aes(label=OVERALL_LI), vjust=-0.3, size=3.5)



#We see a relation i.e states with high literacy rate have more no. of schools

df <- df_ele %>%
  select(STATNAME, SCHTOTG, SCHTOTP) %>%
  gather(key = "variable", value = "value", -STATNAME)

df

ggplot(df, aes(x=STATNAME, y=value, group=variable)) +
  geom_line(aes(color=variable))+
  geom_point(aes(color=variable))+
  theme(legend.position="top",axis.text.x = element_text(angle = 90))+
  scale_color_manual(labels = c("GOVT SCHOOL", "PRIVATE SCHOOL"), values = c("maroon", "orange"))+
  ggtitle("STATE VS SCHOOLS") +
  xlab("STATE") + ylab("SCHOOLS")
#No. of govt an private school comparison

#We will now see a Male vs Female vs Literacy rate comparison
df <- df_ele %>%
  select(STATNAME, MALE_LIT, FEMALE_LIT) %>%
  gather(key = "variable", value = "value", -STATNAME)

df

ggplot(data=df, aes(x=reorder(STATNAME, value), y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="top",axis.text.x = element_text(angle = 90))+
  scale_fill_discrete(name = "Variable", labels = c("FEMALE", "MALE"))+
  ggtitle("STATE VS GENDER LITERACY RATE") +
  xlab("STATE") + ylab("LITERACY RATE")


#Here we are going to create a new feature, DIFF_LIT, by subtracting the Female literacy rate from the male literacy rate. This will help us see the difference in male and female literacy rate for each state easily.

df_ele$diff_lit=df_ele$MALE_LIT-df_ele$FEMALE_LIT
df_ele$diff_lit
mean(df_ele$diff_lit)
# States with the least male and female literacy rates difference
head(df_ele[order(df_ele$diff_lit),c("STATNAME","diff_lit")],5)
# States with the most male and female literacy rates difference
head(df_ele[order(-df_ele$diff_lit),c("STATNAME","diff_lit")],5)
#We now check out how the North-East Indian states perform compared to the National Average

D=list('NAGALAND','MANIPUR','MIZORAM','ASSAM','TRIPURA','ARUNACHAL PRADESH','MEGHALAYA','SIKKIM')
D
for(i in D){
  z<-df_ele[df_ele$STATNAME==i,"diff_lit"]
  print(z)
}
message("male female literacy diff of north eastern states ",mean(z))
message("male female literacy diff national avg ",mean(df_ele$diff_lit))

#The avg in DIFF_LIT for north-eastern states (10.86) is much less than the national avg (13.89361111111111).

#write all inference

#We will be creating top_bottom, a dataframe that contains only the top 3 and bottom 3 states w.r.t. overal literacy rate. This will make plotting and analysis easy for us as we just have to explore these 6 states for answering this question.
#How are the top 3 states different from the bottom 3 states and what factors can the bottom 3 states work on to increase their literacy rate ?
df_ele[order(-df_ele$OVERALL_LI),"STATNAME"]
#remove telangana as it has lack of data 
top_bottom=df_ele[order(-df_ele$OVERALL_LI),]
View(top_bottom)
top_bottom=top_bottom[c(1,2,3,33,34,36),]  #removing telangana as it is newly formed
View(top_bottom)
#Now we will go over different features that we think affect our state's overall literacy rate

den=list()
den$state=top_bottom$STATNAME
den$density=(top_bottom$TOTPOPULAT/top_bottom$AREA_SQKM)*1000
den=as.data.frame(den)
den


ggplot(top_bottom, aes(x=diff_lit, y=reorder(STATNAME, diff_lit))) + 
  geom_bar(stat = "identity",fill="orange")+
  ggtitle("STATE VS GENDER LITERACY RATE DIFFERENCE") +
  xlab("LITERACY RATE DIFF") + ylab("STATE")
#DIFF_LIT says a lot. The differences are really high in states with low overall literacy rates. So even if the bottom most states have good male literacy rates, female literacy rates are really low and that takes their overall literacy rate down. Thus these states really need to work on educating their females and increasing their literacy rate.

top_bottom$P_PUR_POP=100-top_bottom$P_URB_POP

df <- top_bottom %>%
  select(STATNAME, P_URB_POP, P_PUR_POP) %>%
  gather(key = "variable", value = "value", -STATNAME)

df

ggplot(data=df, aes(x=reorder(STATNAME, value), y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position="top",axis.text.x = element_text(angle = 90))+
  scale_fill_discrete(name = "Variable", labels = c("RURAL POP", "URBAN POP"))+
  ggtitle("STATE VS URBAN/RURAL POP") +
  xlab("STATE") + ylab("POP")
#Here we see a pretty obvious but important component that differentiates our top 3 and bottom 3 states. The difference between rural and urban population is much much bigger in the bottom 3.

#The rural population percentage in the bottom 3 states is much more than the rural population percentage in the top 3. That's an important factor to note. People living in rural areas lead a very different life compared to the people living in urban areas. There is less motivation to go to school in rural areas as a lot of people tend to take up their parents profession or business. Also, often children are made to skip school and work at the farm.

#Sex ratio is the no. of females per 1000 males
ggplot(top_bottom, aes(x=STATNAME, y=SEXRATIO)) + 
  geom_bar(stat = "identity",fill="#009E73")+
  geom_text(aes(label=SEXRATIO), vjust=-0.3, size=3.5)+
  ggtitle("STATE VS SEX RATIO") +
  xlab("STATE") + ylab("SEX RATIO")

cor(top_bottom$OVERALL_LI, top_bottom$SEXRATIO)
#Clearly the sex ratio has nothing to do with the literacy level.The graphs dont show sex ratio affecting the literacy rate. Also the correlation's too weak.

ps<-top_bottom["STATNAME"]
ps$schtotpp<-top_bottom$SCHTOTP/top_bottom$SCHTOT
ps$schtotgp<-top_bottom$SCHTOTG/top_bottom$SCHTOT
ps[,]
ps$schtotpp[5]<-0.0787  #outlier removal

df <- ps %>%
  select(STATNAME, schtotgp, schtotpp) %>%
  gather(key = "variable", value = "value", -STATNAME)

df

ggplot(df, aes(x=STATNAME, y=value, group=variable)) +
  geom_line(aes(color=variable))+
  geom_point(aes(color=variable))+
  theme(legend.position="top",axis.text.x = element_text(angle = 90))+
  scale_color_manual(labels = c("GOVT SCHOOL", "PRIVATE SCHOOL"), values = c("maroon", "orange"))+
  ggtitle("STATE VS GOVT/PRIVATE SCHOOLS") +
  xlab("STATE") + ylab("SCHOOLS")

#From here we can clearly note the following:

#Lakshadweep has only govt schools.
#Kerala has the highest number of Pvt schools share in the total schools and is the only state here which has more Pvt schools than Govt schools. It has also got an exceptionally high number of Madarasa and the number of govt schools is much less than the national average.
#Bihar and Arunachal Pradesh have really less pvt schools compared to the govt schools. Their % of govt school is more than the national avg.
#Rajasthan has around 35% pvt schools which is large compared to its no. of govt schools.
#All of this just shows us that even the share of pvt and govt schools isn't related to the literacy rate.

tbd<-top_bottom["STATNAME"]
a<-top_bottom$C5_B+top_bottom$C5_G
b<-top_bottom$C6_B+top_bottom$C6_G
tbd$drop<-(a-b)/a
tbd

ggplot(tbd, aes(x=STATNAME, y=drop)) + 
  geom_bar(stat = "identity",fill="brown")+
  theme(legend.position="top",axis.text.x = element_text(angle = 90))+
  ggtitle("STATE VS DROP OUT RATE")+
  xlab("STATE") + ylab("DROP OUT RATE")

#We find the drop out rate from 8th to 9th(primary to secondary) a pretty important feature in distinguishing between the top 3 and bottom 3 states. It is an obvious reason but no one knew that there would be this much of a difference. The top 3 states have more admissions in 9th than they had in 8th while the bottom 3 have the opposite going on.

q=sum(df_ele$ENR1)
w=sum(df_sec$enr_5)
e=sum(df_sec$enr_7)

SCHTYPE <- c("PRIMARY", "SECONDARY","HR SECONDARY")
values = c(q,w,e)
percents <- round((values/sum(values))*100,1)

df<-as.data.frame(SCHTYPE)
df<-cbind(df,percents)

ggplot(df, aes(x = "", y = percents, fill = SCHTYPE)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()+
  #scale_fill_discrete(name = "SCHTYPE")+
  ggtitle("SCHOOL ENROLLMENT")

#school enrollment

q=sum(df_ele$SCH1)
w=sum(df_sec$sch_5)
e=sum(df_sec$sch_7)

SCHTYPE <- c("PRIMARY", "SECONDARY","HR SECONDARY")
values = c(q,w,e)
percents <- round((values/sum(values))*100,1)

df<-as.data.frame(SCHTYPE)
df<-cbind(df,percents)

ggplot(df, aes(x = "", y = percents, fill = SCHTYPE)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  theme_void()+
  #scale_fill_discrete(name = "SCHTYPE")+
  ggtitle("SCHOOLS IN INDIA")           

#type of schhols in india

a<-cbind(df_ele['SELE1'],df_sec[c('statname','electric_5','electric_7')])
z<-colSums(a[,c(1,3,4)])
z
z<-as.data.frame(z)
z$names<-c("PRIMARY","SECONDARY","HR SECONDARY")
z$z[1]<-z$z[1]/10 #outlier removal
ggplot(z, aes(x=names, y=z)) + 
  geom_bar(stat = "identity",fill='brown')+  # Y axis is explicit. 'stat=identity'
  ggtitle("SCHOOLS WITH ELECTRICITY") +
  xlab("SCHTYPE") + ylab("NO. SCHOOLS")
#schools wih electricity in india


