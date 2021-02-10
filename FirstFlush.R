
# library -----------------------------------------------------------------


rm(list= ls())
library(tidyverse)
library(readxl)
library(lubridate)
library(zoo)


# read in -----------------------------------------------------------------


dat <- read_excel("C:/Users/mhill/Documents/FirstFlush/2011-2012 LAB ANALYSES FOR PROCEEDINGS.xlsx", sheet = "2011-2012 LAB DATA") %>% 
  rename(ISCO_Number = 2,
         TSS_initial_g = 6,
         TSS_final_g = 7,
         Sample_Volume_ml = 8,
         TSS = 9,
         grab_storm = 17) %>% 
  mutate(
        Date_Sampled= as_date(Date_Sampled),
        Date_Analyzed= as_date(Date_Analyzed),
        NOTES = as.character(NOTES)
    )

# data management ---------------------------------------------------------


## get rid of all the grab samples in the data set by isolating only samples with sequence 
dat <- drop_na(dat, ISCO_Number)


## group the events and assign meaningful names
dat$ID <- interaction(dat$Date_Sampled, dat$SampleID, drop= TRUE)


## try to get rid of events that only have a couple of entries 
dat <- dat %>% 
  group_by(Date_Sampled, SampleID) %>% 
  filter(n() > 3)


## filter does weird things to the ordering, here order by group then sequence number
dat <- dat[order(dat$Season, dat$Date_Sampled, dat$ISCO_Number),]


## add a seasonal indicator column
yq <- as.yearqtr(as.yearmon(dat$Date_Sampled, "%m/%d/%Y") + 1/12)
dat$Season <- factor(format(yq, "%q"), levels = 1:4, 
                    labels = c("winter", "spring", "summer", "fall"))
dat$Quarter <- as.integer(dat$Season)


# visualization -----------------------------------------------------------

## panel of TSS values across events
ggplot(data =dat, aes(ISCO_Number, TSS, group= Season, color= Season))+
  #theme(legend.position = "none")+
  geom_line()+
  geom_point()+
  facet_wrap(~reorder(ID, Quarter))+
  coord_cartesian(ylim = c(0,5000))

### panel of TIP values across events
ggplot(data =dat, aes(ISCO_Number, TIP, group= Season, color= Season))+
  #theme(legend.position = "none")+
  geom_line()+
  geom_point()+
  facet_wrap(~reorder(ID, Quarter))+
  coord_cartesian(ylim = c(0,10))

### panel to TSS values across events
ggplot(data =dat, aes(ISCO_Number, Turbidity, group= Season, color= Season))+
  #theme(legend.position = "none")+
  geom_line()+
  geom_point()+
  facet_wrap(~reorder(ID, Quarter))+
  coord_cartesian(ylim = c(0,5000))






# import hydrographs from DF ----------------------------------------------

DFflow <- read_excel("W:/Home/bpoganski/public/Ag_Economic_Env_CoverCrops/Flow Data/discharge workbooks/NIFA.MUR1.discharge.xlsx", "Calculated",
                        range = cell_rows(c(1,178457:180079)))
DFflow <- DFflow[-1,]%>% 
 # mutate(Time = as.Date(Time)) %>% 
  mutate(`Flow Rate(AVG)`= as.numeric(`Flow Rate(AVG)`)) %>% 
  mutate(`Flow(TOT)` = as.numeric(`Flow(TOT)`))

DFflow <- DFflow[-c(1:178487),]

ggplot(DFflow)+
  geom_line(aes(Time, `calculated flow (L)`))+
  #geom_line(aes(Time, `Flow Rate(AVG)`*100, color= "red"))+
  scale_y_continuous(sec.axis = sec_axis(trans = ~.*100))+
  scale_x_datetime(date_breaks = "2 hours")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




ggplot(data =DFflow, aes(ISCO_Number, TIP, group= ID, color= ID))+
  theme(legend.position = "none")+
  geom_line()+
  geom_point()+
  facet_wrap(~ID)+
  coord_cartesian(ylim = c(0,10))


#  mutate( 
#    # changes the formatting of some of the columns to make R happier
#    Date = lubridate::mdy(Date),
#    # makes the dates as dates R will recognize
#    sampleid = as.factor(sampleid),
#    # Sets the sampleid column as a factor instead of text strings
#    farm = as.factor(farm),
#    # Sets the Farm column as a factor instead of text strings
#    treatment = as.factor(treatment),
#    # Sets the Treatment column as a factor instead of text strings
#    TUR_ntu = as.numeric(TUR_ntu),      
#    # Sets turbidity as a number instead of text strings 
#    #(I have no idea why it wasn't already)
#    sample_type = as.factor(sample_type)
#  )
#
## replace underscores in the column headers with periods
#names(dat) <- gsub(
#  x = names(dat), 
#  pattern = "\\_", 
#  replacement = "."
#)
#
#dat$season <- ifelse(
#  month(dat$Date) == 11 | month(dat$Date) ==  12 | month(dat$Date) == 1| 
#    month(dat$Date) == 2 |  month(dat$Date) == 3 | month(dat$Date) == 4,
#  "cover",
#  "cash")
#
#dat <- dat %>% 
#  mutate(season = as.factor(season))
#
