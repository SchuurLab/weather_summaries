######################################################################################
#                    HOBO summaries from Healy station.                               #
#                                                                                     #
#                 Created by Gerardo Celis March 2015                                 #
######################################################################################

# Required packages to run script. If not on computer download and install.
library(ggplot2)
library(data.table)
library(lubridate)
library(reshape2)
library(dplyr)


# Import HOBO LTER daily file created by 'HOBO combine all years.R' program.
setwd("~/Documents/Post Doc/Schuur/Data LTER")

load('Daily HOBO 2004 to 2014 Precip Gapfilled.Rdata')
all.d[, N := NULL]
load('Hobo Daily_2014-10-01_to_2015-09-08.RData')
HMD3[, Tmax := NULL]
HMD3[, Tmin := NULL]
HMD3[, gust_sp := NULL]

all.d <- rbind(all.d, HMD3)

# Create a season variable to divide winter and growing seasons.
all.d$season <- ifelse(all.d$DOY >= 121 & all.d$DOY < 274, "Growing", "Winter")

# Create a winter season for each winter
all.d$winter.year <- NA
all.d$winter.year <- ifelse(all.d$DOY <= 121 & all.d$year==2005 | all.d$DOY >= 275 & all.d$year==2004, "2004/2005", all.d$winter.year)
all.d$winter.year <- ifelse(all.d$DOY <= 121 & all.d$year==2006 | all.d$DOY >= 275 & all.d$year==2005, "2005/2006", all.d$winter.year)
all.d$winter.year <- ifelse(all.d$DOY <= 121 & all.d$year==2007 | all.d$DOY >= 275 & all.d$year==2006, "2006/2007", all.d$winter.year)
all.d$winter.year <- ifelse(all.d$DOY <= 121 & all.d$year==2008 | all.d$DOY >= 275 & all.d$year==2007, "2007/2008", all.d$winter.year)
all.d$winter.year <- ifelse(all.d$DOY <= 121 & all.d$year==2009 | all.d$DOY >= 275 & all.d$year==2008, "2008/2009", all.d$winter.year)
all.d$winter.year <- ifelse(all.d$DOY <= 121 & all.d$year==2010 | all.d$DOY >= 275 & all.d$year==2009, "2009/2010", all.d$winter.year)
all.d$winter.year <- ifelse(all.d$DOY <= 121 & all.d$year==2011 | all.d$DOY >= 275 & all.d$year==2010, "2010/2011", all.d$winter.year)
all.d$winter.year <- ifelse(all.d$DOY <= 121 & all.d$year==2012 | all.d$DOY >= 275 & all.d$year==2011, "2011/2012", all.d$winter.year)
all.d$winter.year <- ifelse(all.d$DOY <= 121 & all.d$year==2013 | all.d$DOY >= 275 & all.d$year==2012, "2012/2013", all.d$winter.year)
all.d$winter.year <- ifelse(all.d$DOY <= 121 & all.d$year==2014 | all.d$DOY >= 275 & all.d$year==2013, "2013/2014", all.d$winter.year)

# Create accumulative PAR variable
all.d$PAR.cum <- (all.d$PAR*86400)/1000000

# data checks
check <- all[is.na(Tair)==T, .N, by=list(DOY, year)]
check <- all[is.na(PAR)==T, .N, by=list(DOY, year)]

ggplot(check, aes(DOY, N))+geom_bar(stat="identity")+facet_wrap(~year)

check <- all.d[is.na(PAR)==T, .N, by=list(DOY, year)]
check <- all.d[is.na(PAR)==T]

c.year <- all.d[, list(PAR.cum = sum(PAR.cum, na.rm=T)),
                by = list(year)]

c.year <- all.d[, list(precip = sum(precip, na.rm=T)),
                by = list(year)]

c.year <- all.d[season == "Growing", list(precip = sum(precip, na.rm=T)),
                by = list(year)][order(year)]


all$month <- month(all$date)

all$month.l <- month(all$date, label=TRUE)
#all$month.l <- translator_vector[all$month]
#all$month.l <- ordered(all$month.l, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))



check <- all[is.na(Tair)==T, .N, by=list(DOY, year)]
check <- all[is.na(PAR)==T, .N, by=list(DOY, year)]

ggplot(check, aes(DOY, N))+geom_bar(stat="identity")+facet_wrap(~year)

check <- all.d[is.na(PAR)==T, .N, by=list(DOY, year)]
check <- all.d[is.na(PAR)==T]

c.year <- all.d[, list(precip = sum(precip, na.rm=T)),
                        by = list(year)]

c.year <- all.d[season == "Growing", list(precip = sum(precip, na.rm=T)),
                by = list(year)][order(year)]


all.g <- subset(all, season=="Growing")
all.g <- data.table(all.g)
c.year.growing <- all.g[season=="Growing", list(precip = sum(precip, na.rm=T)),
                      by = list(year, DOY, month, month.l)]

all.g$week <- week(all.g$time)
all.g$week.s <- ifelse(all.g$week >= 18, all.g$week - 17 )

c.year.growing.week <- all.g[season=="Growing", list(precip = sum(precip, na.rm=T)),
                        by = list(year, week.s)]


hobo.12.13 <- fread("Hobo Daily_2012-10-01_to_2013-09-30.csv")
hobo.12.13$month <- month(as.Date(hobo.12.13$date))

# Annual
hobo.12.13$year <- year(as.Date(hobo.12.13$date))
c.year <- hobo.12.13[, list(precip = sum(precip, na.rm=T)),
              by = list(year)]

hobo.12.13$season <- ifelse(hobo.12.13$DOY >= 121 & hobo.12.13$DOY < 274, "Growing", "Winter")
#hobo.12.13$winter.year <- ifelse(hobo.12.13$DOY <= 121 & hobo.12.13$year==2013 | hobo.12.13$DOY >= 275 & hobo.12.13$year==2012, "2012/2013", hobo.12.13$winter.year)
h.12.13 <- subset(hobo.12.13, season=="Growing")

h.12.13$month.l <- month(h.12.13$date, label=TRUE)
#h.12.13$month.l <- translator_vector[h.12.13$month]
#h.12.13$month.l <- ordered(h.12.13$month.l, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

h.12.13$week <- week(h.12.13$date)
h.12.13$week.s <- ifelse(h.12.13$week >= 18, h.12.13$week - 17 )

h.12.13 <- data.table(h.12.13)
h.12.13.week <- h.12.13[, list(precip = sum(precip, na.rm=T)),
                             by = list(year, week.s)]

c.year.growing <- rbind(c.year.growing, h.12.13[, .(year, DOY, month, month.l, precip)])

c.year.growing.week <- rbind(c.year.growing.week, h.12.13.week[, .(year, week.s, precip)])

# Export data
#write.csv(c.year.growing.week, "Growing season weekly precipitation 2004-2014.csv")

precip.growing <- c.year.growing[, list(precip.total = sum(precip, na.rm=T)),
                      by = list(year)]
mean(precip.growing$precip.total)
p <- ggplot(precip.growing, aes(factor(year), precip.total))+
  geom_bar(stat="identity", fill="blue")+
  #geom_point()+
  scale_y_continuous(breaks=seq(0, 650, by=50))+
  labs(title= "Growing season precipitation totals", y="Precipitation (mm)", x ="Year")+theme_bw()
p
ggsave(p, file="Precip Growing season yearly (2004-2014).pdf")

precip.growing.m <- c.year.growing[, list(precip.total = sum(precip, na.rm=T)),
                                 by = list(year, month, month.l)]
# Export data
#write.csv(precip.growing.m, "Growing season monthly precipitation 2004-2014.csv")

p <- ggplot(subset(precip.growing.m, month > 4 & month < 10), aes(year, precip.total, fill=factor(year)))+
  geom_bar(stat="identity")+
  scale_x_continuous(breaks=seq(from=2004, to=2014, by=2))+
  facet_grid(.~month.l)+
  labs(title= "Growing season Precipitation totals", y="precipitation (mm)", x ="Year", fill="Year")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45))
p
ggsave(p, file="Precip Growing season monthly (2004-2014).pdf")



# WindRose.R
require(ggplot2)
require(RColorBrewer)

plot.windrose <- function(data,
                          spd,
                          dir,
                          spdres = 2,
                          dirres = 30,
                          spdmin = 2,
                          spdmax = 20,
                          spdseq = NULL,
                          palette = "YlGnBu",
                          countmax = NA,
                          debug = 0){
  
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),                                               
                                            palette))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- c(seq(spdseq))
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")
    cat(speedcuts.colors, "\n")    
  }  
  
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (m/s)", 
                      values = spd.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank())
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(p.windrose)  
  
  # return the handle to the wind rose
  return(p.windrose)
}

p <- plot.windrose(spd = all.d$wind_sp,
                   dir = all.d$wind_di)

p <- plot.windrose(spd = all.d$wind_sp,
                   dir = all.d$wind_di,
                   spdseq = c(0,3,6,12,20))

wind <- all.d[is.na(wind_di)==F & is.na(wind_sp)==F]

p.wr2 <- plot.windrose(data = wind,
                       spd = "wind_sp",
                       dir = "wind_di",
                       spdseq = c(0,3,6,12,20),
                       dirres = 30,
                       debug = 0,
                       palette = "Reds")+theme_bw()

p.wr3 <- p.wr2 + facet_wrap(~month.l,
                            ncol = 3)

