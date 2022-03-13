# Aim: Understand how the blight alerting model works 
# Jian Liu

# Very happy to discuss this with you, Steve. I am not particularly clever when
# it comes to developing spreadsheets, so my process relies on manual
# interventions!
# 
#I have attached: 
# Export - I export the information from our weather station to
#this file. 
# Download – I  copy the information from “Export” to the appropriate
#month in this file. Walnut blight MASTER – This links to “Download”. Or one
#could enter information manually! 
# Walnut blight 2021 2022 – This also links to
#“Download” and shows our record of when we should “Spray”. It then
#automatically resets the blight number to 1 a week after spraying. (Note that
#we no longer spray. So, this now gives us a record of what we are ‘saving’ in
#spray costs and time.)
#
#Our weather station is relatively cheap.  I note that some are available at
#about $100. I have no idea how useful they would be. However, all one needs is
#outdoor temperature and humidity to make the spreadsheet work. (Rain-fall is an
#optional extra.)
#
# I trust that some of this is helpful.
# 
library(readxl)
library(data.table)
library(magrittr)
library(ggplot2)

# skip the download part
# Download data from cliflo yet to be included 
# Started from download 
export <- read_excel("./Data/Download.xlsx") %>% 
  data.table()

summary(export)
colnames(export)
# Need Rainhr and RainDay 
# Need outtemp and outhumi
# And Time
cols <- c("Time", "OutTemp", "OutHumi",
          "RainDay", "RainHour")
DT <- export[,..cols]

# For each hourly result of relative humidity the spreadsheet calculates…
# If RH<85% it uses the default reducing factor of 0.995
# If RH>=85% then the multiplying factor is 1+((5^($D16/20))/100)
# It multiplies the previous score by this figure
# If the score drops below 1, it restores the score to 1
# Multiplier=IF(Humidity<85,0.995,1+((5^(Temperature/20))/100))
# Score = =IF(Score(previous)*Multiplier<1,1,Score(previous)*Muliplier)

DT[, Multiplier := ifelse(OutHumi<85, 0.995,
                          1+((5^(OutTemp/20))/100))
   ]
DT[, Score := 1]
for (i in 2:nrow(DT)){
  DT$Score[i] <- ifelse(DT$Score[i-1] * DT$Multiplier[i] < 1, 1,
                        DT$Score[i-1] * DT$Multiplier[i])
  # Started from 8th September 
  # Check spray column 
  # If the spray column = true, reset the score to 1 
  
  }
# str(DT)
DT %>% colnames()
legendcols <-c("red", "blue", "green")
names(legendcols) <-  c("Score", "OutTemp", "RainDay")
DT %>% 
  ggplot(aes(Time, Score)) +
  geom_line(aes(color= "Score"), size = 1.5) +
  geom_smooth(aes(y = OutTemp, color = "OutTemp"), span = 0.05, se = FALSE) +
  geom_line(aes(y = RainDay, color = "RainDay"), size = 1) +
  theme_linedraw() +
  scale_color_manual(name = "",
                     values = legendcols,
                     labels = c("Blight Score", "Temperature", "24 Hour Rain"))+
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        legend.key.width = unit(15, "mm"),
        legend.key = element_rect(colour =  "transparent", fill = "white"))


DT[, Spray := as.integer(NA)]
# Spray started to take affect one week after 
# 7 DAYS * 24 HOURS, SO 168 rows after 
spraycount <- 168

# Add spray
DT$Spray[20] <- 1L


if(spraycount <= nrow(DT)) {
  DT$Score[spraycount] <- ifelse(DT$Spray[i - 1] == 1L, 1, 
                                 DT$Score[spraycount])
  spraycount <- spraycount + 1 
}
