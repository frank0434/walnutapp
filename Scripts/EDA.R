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
library(clifro)


# add NIWA STATION --------------------------------------------------------

me <- cf_user("frank0434", askpass::askpass())
my.dts <- cf_datatype(c( 3, 4), #rainfall, temperature 
                      c( 1, 2),
                      list( 2, 4),
                      c(NA,NA))
# Broadfield weather station
agent_no <- as.integer(17603)
my.stations <- cf_station(agent_no)

cf.datalist <- cf_query(user = me,
                        datatype = my.dts,
                        station = my.stations,
                        start_date = paste(as.Date("2021-09-01"), "00"),
                        end_date = paste(as.Date("2021-09-30"), "23"))
View(cf.datalist)
Rain <- as.data.table(cf.datalist[[1]])
Temp <- as.data.table(cf.datalist[[2]])

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
no_spray <- DT %>% 
  ggplot(aes(Time, Score)) +
  geom_line(aes(color= "Score"), size = 1.5) +
  geom_smooth(aes(y = OutTemp, color = "OutTemp"), span = 0.05, se = FALSE) +
  geom_line(aes(y = RainDay, color = "RainDay"), size = 1)  +
  scale_color_manual(name = "",
                     values = legendcols,
                     labels = c("Blight Score", "Temperature", "24 Hour Rain"))+
  theme_walnut()

no_spray  

DT[, Spray := as.integer(NA)]
# Spray started to take affect one week after 
# 7 DAYS * 24 HOURS, SO 168 rows after 
spraylag <- 168
sprayeffect <- 169
# Add spray
DT$Spray[68] <- 1L


if(spraycount <= nrow(DT)) {
  for (i in sprayeffect:nrow(DT)){
    DT$Score[i] <- ifelse(is.na(DT$Spray[i - spraylag]), 
                          ifelse(DT$Score[i - 1] * DT$Multiplier[i] < 1, 1,
                          DT$Score[i - 1] * DT$Multiplier[i]),
                          1L)
    # Started from 8th September 
    # Check spray column 
    # If the spray column = true, reset the score to 1 
    
  }
  sprayeffect <- sprayeffect + 1 
}

post_spray <-  DT %>% 
  ggplot(aes(Time, Score)) +
  geom_line(aes(color= "Score"), size = 1.5) +
  geom_smooth(aes(y = OutTemp, color = "OutTemp"), span = 0.05, se = FALSE) +
  geom_line(aes(y = RainDay, color = "RainDay"), size = 1)  +
  scale_color_manual(name = "",
                     values = legendcols,
                     labels = c("Blight Score", "Temperature", "24 Hour Rain"))+
  theme_walnut()

post_spray