
###################
#### Read Data ####
###################


BS6<- read.csv('6Lmin.csv')
BS10<- read.csv('10Lmin.csv')
BS<- read.csv('6 and 10.csv')


################################
### Plot Density of Bubbles ####
################################

a <- ggplot(BS, aes(x = uM)) + 
  geom_density() +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x), 
    limits = c(1, 10000),
    labels = scales::trans_format("log10", scales::math_format(10^.x)), name = "Bubble Diameter (uM)") + 
  scale_y_continuous(name = "Count (n)") + theme_bw() + facet_grid(Flow ~ Type) + geom_density(alpha =0.2, fill = "antiquewhite3")

a
a + annotation_logticks(sides = "b") 
a + annotation_logticks(sides = "b") + theme(panel.grid.minor = element_blank()) + theme(strip.text = element_text(size=11), axis.title.x = element_text(size=12), axis.text.x  = element_text(size=11), axis.title.y = element_text(size=12), axis.text.y = element_text(size=11))




##################################
### Plot Histograms of Bubbles ####
##################################

Six <- subset(BS, BS$Flow=="6 Lmin")
Ten <- subset(BS, BS$Flow=="10 Lmin")

png(file = "6Lmin.png",width = 200, height = 75, res=300, units = "mm")
ggplot(Six, aes(x = uM)) + geom_histogram(binwidth=0.025, color="black", fill="white") +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    limits = c(10, 10000),    labels = scales::trans_format("log10", scales::math_format(10^.x)), name = "Bubble Diameter (uM)") +
  scale_y_continuous(name = "Count (n)") + theme_bw() + facet_grid(. ~ Type) + annotation_logticks(sides = "b") + theme(panel.grid.minor = element_blank()) + theme(strip.text = element_text(size=11), axis.title.x = element_text(size=12), axis.text.x  = element_text(size=11), axis.title.y = element_text(size=12), axis.text.y = element_text(size=11))
dev.off()


png(file = "10Lmin.png",width = 200, height = 75, res=300, units = "mm")
ggplot(Ten, aes(x = uM)) + geom_histogram(binwidth=0.025, color="black", fill="white") +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    limits = c(10, 10000),    labels = scales::trans_format("log10", scales::math_format(10^.x)), name = "Bubble Diameter (uM)") +
  scale_y_continuous(name = "Count (n)") + theme_bw() + facet_grid(. ~ Type) + annotation_logticks(sides = "b") + theme(panel.grid.minor = element_blank()) + theme(strip.text = element_text(size=11), axis.title.x = element_text(size=12), axis.text.x  = element_text(size=11), axis.title.y = element_text(size=12), axis.text.y = element_text(size=11))
dev.off()





##################################
### Plot Histogram of Bubbles ####
##################################

png(file = "6 and 10Lminv2.png",width = 175, height = 75, res=300, units = "mm")
ggplot(BS, aes(x = uM)) + geom_histogram(binwidth=0.025, color="black", fill="white") +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    limits = c(10, 10000),    labels = scales::trans_format("log10", scales::math_format(10^.x)), name = "Bubble Diameter (uM)") +
  scale_y_continuous(name = "Count (n)") + theme_bw() + facet_grid(Flow ~ Type) + annotation_logticks(sides = "b") + theme(panel.grid.minor = element_blank()) + theme(strip.text = element_text(size=11), axis.title.x = element_text(size=12), axis.text.x  = element_text(size=11), axis.title.y = element_text(size=12), axis.text.y = element_text(size=11))
dev.off()




#########################################
### Plot Volume Histograms of Bubbles ###
#########################################

Six <- subset(BS, BS$Flow=="6 Lmin")
Ten <- subset(BS, BS$Flow=="10 Lmin")

No_V6 <- subset(Six, Six$Type=="0V")
Vibr6 <- subset(Six, Six$Type=="3V")

c1 <- table(cut(No_V6$uM, breaks = seq(0, 10000, by = 50)))
c1
No_V_BD_VolFreq6<-as.data.frame(c1)
write.csv(No_V_BD_VolFreq6,'No_V_BD_VolFreq6.csv', row.names = TRUE)

c2 <- table(cut(Vibr6$uM, breaks = seq(0, 10000, by = 25)))
c2
Vibr_BD_VolFreq6<-as.data.frame(c2)
write.csv(Vibr_BD_VolFreq6,'Vibr_BD_VolFreq6.csv', row.names = TRUE)


No_V10 <- subset(Ten, Ten$Type=="0V")
Vibr10 <- subset(Ten, Ten$Type=="2.2V")

c3 <- table(cut(No_V10$uM, breaks = seq(0, 10000, by = 25)))
c3
No_V_BD_VolFreq10<-as.data.frame(c3)
write.csv(No_V_BD_VolFreq10,'No_V_BD_VolFreq10.csv', row.names = TRUE)

c4 <- table(cut(Vibr10$uM, breaks = seq(0, 10000, by = 25)))
c4
Vibr_BD_VolFreq10<-as.data.frame(c4)
write.csv(Vibr_BD_VolFreq10,'Vibr_BD_VolFreq10.csv', row.names = TRUE)



BS<- read.csv('BSVol.csv')
Six<- read.csv('No_V_BD_VolFreq6.csv')

ggplot(Six, aes(x = Diameter)) + geom_histogram(binwidth=0.025, color="black", fill="white") +
  geom_line(aes(y = CumVol)) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    limits = c(10, 10000),    labels = scales::trans_format("log10", scales::math_format(10^.x)), name = "Bubble Diameter (uM)") +
  scale_y_continuous(name = "Count (n)", sec.axis = sec_axis(~.*0.05, name="Cum Vol (mL)")) + 
  theme_bw() + annotation_logticks(sides = "b") + theme(panel.grid.minor = element_blank()) + theme(strip.text = element_text(size=11), axis.title.x = element_text(size=12), axis.text.x  = element_text(size=11), axis.title.y = element_text(size=12), axis.text.y = element_text(size=11))






#############################################
### Bin and generate Bubble counts 6Lmin ####
#############################################


No_V6 <- subset(Six, Six$Type=="0V")
Vibr6 <- subset(Six, Six$Type=="3V")

c1 <- table(cut(No_V6$uM, breaks = seq(0, 7000, by = 25)))
c1
No_V_BD_Freq6<-as.data.frame(c1)
No_V_BD_Freq6$Freq<-No_V_BD_Freq6$Freq*52 # Count multiplied by 52 because bubble sizing carried out for five nozzles in 7 cm water vs 60 cm - hence (18/5) x (60/7) x 1.67 = 52
write.csv(No_V_BD_Freq6,'No_V_BD_Freq6.csv', row.names = TRUE)

c2 <- table(cut(Vibr6$uM, breaks = seq(0, 7000, by = 25)))
c2
Vibr_BD_Freq6<-as.data.frame(c2)
Vibr_BD_Freq6$Freq<-Vibr_BD_Freq6$Freq*31
write.csv(Vibr_BD_Freq6,'Vibr_BD_Freq6.csv', row.names = TRUE)



##############################################
### Bin and generate Bubble counts 10Lmin ####
##############################################


No_V10 <- subset(Ten, Ten$Type=="0V")
Vibr10 <- subset(Ten, Ten$Type=="2.2V")

c3 <- table(cut(No_V10$uM, breaks = seq(0, 10000, by = 25)))
c3
No_V_BD_Freq10<-as.data.frame(c3)
No_V_BD_Freq10$Freq<-No_V_BD_Freq10$Freq*52 # Count multiplied by 52 because bubble sizing carried out for five nozzles in 7 cm water vs 60 cm - hence (18/5) x (60/7) x 1.67 = 52
write.csv(No_V_BD_Freq10,'No_V_BD_Freq10.csv', row.names = TRUE)

c4 <- table(cut(Vibr10$uM, breaks = seq(0, 10000, by = 25)))
c4
Vibr_BD_Freq10<-as.data.frame(c4)
Vibr_BD_Freq10$Freq<-Vibr_BD_Freq10$Freq*31
write.csv(Vibr_BD_Freq10,'Vibr_BD_Freq10.csv', row.names = TRUE)
