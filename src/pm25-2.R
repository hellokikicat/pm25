rm(list=ls())
library(xts)
library(forecast)
library(reshape2)
library(ggplot2)

setwd('~')
setwd('pm25/data')

# Code for raw data combination and import commented out.
dataFileYears <- c(2007:2016)
# csvNames <- paste0('daily_88101_', dataFileYears, '.csv')
# 
# df <- data.frame()
# for(csv_i in csvNames) df <- rbind(df, read.csv(csv_i, stringsAsFactors = F))
# colTypes <- sapply(df, class)
# # dfRaw <- df
# for(i in seq_along(colTypes)) if (colTypes[i] == "character") df[,i] <- as.factor(unlist(df[,i]))
# 
# d <- df[df$Sample.Duration=='24 HOUR' & df$Pollutant.Standard=='PM25 24-hour 2006',-c(24, 29)]
# summary(d)
# dim(d)
# rm(dfRaw)
# 
# write.csv(d, file = 'combined_daily_88101_2007-2016.csv')

d <- read.csv('combined_daily_88101_2007-2016.csv')
d$Date <- as.Date(d$Date.Local)
summary(d)
dim(d)

# Compare columns AQI vs Arithmetic.Mean to see if they are the same. Conclusion: scaled differently, use AQI Column.
plot(d[1:100000, 'AQI'], d[1:100000, 'Arithmetic.Mean'])

# Look at distribution by states in the data:
stateCounts <- sort(table(d$State.Name), decreasing = T)
print(stateCounts)
barplot(stateCounts, main = 'Data Count by States', las=3)

# Remove States outside of continental US, i.e. Puerto Rico, Virgin Islands, Hawaii, and Alaska.
d <- d[!(d$State.Name %in% c('Puerto Rico', 'Virgin Islands', 'Hawaii', 'Alaska')),]
dim(d)

# Get the geolocation of all sites, the data count at each site, and the overall average AQI at each site:
# Site.Num and Local.Site.Name to get unique site locations
locMap0 <- aggregate(cbind(Latitude, Longitude) ~ Site.Num + Local.Site.Name, d, mean)
siteCount <- aggregate(County.Name ~ Site.Num + Local.Site.Name, d, length)
siteAvgAQI <- aggregate(AQI ~ Site.Num + Local.Site.Name, d, mean)

# A function to bubble plot on a US map with color gradient.
plotDotMapUS <- function(lon, lat, dotSizes=NULL, sizeScaleLab='Size Scale', dotColors=NULL, colorScaleLab = 'Color Scale', title='US Map', labs=NULL) {
  us <- map_data("state")
  rgAQI <- c(0,165)
  gg <- ggplot() + geom_polygon(data=us, aes(x=long, y=lat, group = group), fill="#ffffff", color="grey", size=0.1)
  if (is.null(dotSizes)) {
    if (is.null(dotColors)) {
      gg <- gg + geom_point(aes(x=lon, y=lat), color="gold2", size=3, na.rm = T)
    }
    else {
      gg <- gg + geom_point(aes(x=lon, y=lat, color=pmin(dotColors,rgAQI[2])), size=3, na.rm = T) + scale_colour_gradient2(low = 'green', mid = 'red', high = 'red', na.value = "#0000000F", midpoint = 100, limit = rgAQI, name = colorScaleLab)
    }
  }
  else {
    if (is.null(dotColors)) {
      gg <- gg + geom_point(aes(x=lon, y=lat, size=dotSizes, color="gold2", na.rm = T)) + scale_size(name=sizeScaleLab)
    }
    else {
      gg <- gg + geom_point(aes(x=lon, y=lat, size=dotSizes, color=pmin(dotColors,rgAQI[2])), na.rm = T) + scale_size(name=sizeScaleLab) + scale_colour_gradient2(low = 'green', mid = 'red', high = 'red', na.value = "#0000000F", midpoint = 100, limit = rgAQI, name = colorScaleLab)
    }
  }
  if (!is.null(labs)) {
    gg <- gg + geom_text( hjust=0.5, vjust=-0.7, aes(x=lon, y=lat, label=labs), colour='gold3', size=5 , na.rm = T)
  }
  gg <- gg + ggtitle(title)
  plot(gg)
}

# Plot all sites on a US map:
plotDotMapUS(locMap0$Longitude, locMap0$Latitude, siteCount[,3], 'Data Count', siteAvgAQI$AQI, 'Avg AQI', 'Locations of Measurement Sites')

# Reshape the data into unique measurement sites by day, average all AQI on the same day same site.
# Site.Num and Local.Site.Name used as merging key
dAQIBySite <- dcast(d, Date ~ Site.Num + Local.Site.Name, value.var = 'AQI', fun.aggregate = mean, na.rm=T)

# Converting into "zoo" time series
zAQIBySite <- zoo(dAQIBySite[,2:ncol(dAQIBySite)], dAQIBySite$Date)
# Observing lots of missing data:
plot(zAQIBySite[,1])
plot(zAQIBySite[,2])
plot(zAQIBySite[,3])
plot(zAQIBySite[,4])
plot(zAQIBySite[,5])
plot(zAQIBySite[,6])
plot(zAQIBySite[,7])

xAQIBySite <- as.xts(zAQIBySite)
# plot a random day on map
plotDotMapUS(locMap0$Longitude, locMap0$Latitude, dotColor=as.numeric(xAQIBySite['2008-02-03']), colorScaleLab='Avg AQI', title='PM2.5 AQI at All Sites on ')

# To see the effect through time, a movie can be made that can present the time series data in the most natural way to humans.
# I won't run the code here, let's just see the movie, which is the entire year of data in 2013.

# library(animation)
# 
# saveVideo({
#   startDate <- as.Date('2013-01-01')
#   endDate <- as.Date('2013-12-31')
#   ani.options(interval = 0.5)
#   for(di in (startDate:endDate)) {
#     someday <- as.numeric(xAQIBySite[as.Date(di)])
#     plotDotMapUS(locMap0$Longitude, locMap0$Latitude, dotColor=someday, colorScaleLab='Avg AQI', title=paste('PM2.5 AQI at All Sites on', as.Date(di)))
#     ani.pause()
#   }
# },
#   paste0('anim', startDate, '.mp4'),
#   ffmpeg = 'bin/ffmpeg.exe',
#   ani.width = 800, ani.height = 450)
# 

# In the movie 2013-01-22 is interesting because on that day NYC and DC area had very high but localized AQI, while other areas are green:
plotDotMapUS(locMap0$Longitude, locMap0$Latitude, dotColor=as.numeric(xAQIBySite['2013-01-22']), colorScaleLab='Avg AQI', title='PM2.5 AQI at All Sites')
# Then this localized high AQI air spread and influenced a large area in eastern US in the next 5 days:
plotDotMapUS(locMap0$Longitude, locMap0$Latitude, dotColor=as.numeric(xAQIBySite['2013-01-23']), colorScaleLab='Avg AQI', title='PM2.5 AQI at All Sites')
plotDotMapUS(locMap0$Longitude, locMap0$Latitude, dotColor=as.numeric(xAQIBySite['2013-01-24']), colorScaleLab='Avg AQI', title='PM2.5 AQI at All Sites')
# Now we can see how lots of places are getting bad air:
plotDotMapUS(locMap0$Longitude, locMap0$Latitude, dotColor=as.numeric(xAQIBySite['2013-01-25']), colorScaleLab='Avg AQI', title='PM2.5 AQI at All Sites')
plotDotMapUS(locMap0$Longitude, locMap0$Latitude, dotColor=as.numeric(xAQIBySite['2013-01-26']), colorScaleLab='Avg AQI', title='PM2.5 AQI at All Sites')
plotDotMapUS(locMap0$Longitude, locMap0$Latitude, dotColor=as.numeric(xAQIBySite['2013-01-27']), colorScaleLab='Avg AQI', title='PM2.5 AQI at All Sites')
plotDotMapUS(locMap0$Longitude, locMap0$Latitude, dotColor=as.numeric(xAQIBySite['2013-01-28']), colorScaleLab='Avg AQI', title='PM2.5 AQI at All Sites')
plotDotMapUS(locMap0$Longitude, locMap0$Latitude, dotColor=as.numeric(xAQIBySite['2013-01-29']), colorScaleLab='Avg AQI', title='PM2.5 AQI at All Sites')
plotDotMapUS(locMap0$Longitude, locMap0$Latitude, dotColor=as.numeric(xAQIBySite['2013-01-30']), colorScaleLab='Avg AQI', title='PM2.5 AQI at All Sites')
#Back to green
plotDotMapUS(locMap0$Longitude, locMap0$Latitude, dotColor=as.numeric(xAQIBySite['2013-01-31']), colorScaleLab='Avg AQI', title='PM2.5 AQI at All Sites')

# Conclusion from above observations from the raw data:
# 1) too many missing data, need to deal with it.
# 2) there does seem to be a trend where the air of locations will affect other surrounding regions in the next few days.
#    This means it makes sense to try to model this effect.


# Due to a huge proportion of missing daily data at many sites,
# I decided to combine sites that are close by together based on their exact location.
# This is achieved by doing a unsupervised K-Mean Clustering analysis on the Longitude and Latitude of site locations.
# K = 20 by trail & error to get a even distribution. I.e. a dot close to all major metropolitan areas, e.g. San Fransisco, Los Angeles, NYC, Seatle, Miami etc.
set.seed(3)
siteClusters <- kmeans(d[,c('Latitude', 'Longitude')], centers = 20)
# Here are the cluster centers:
plotDotMapUS(siteClusters$centers[,'Longitude'], siteClusters$centers[,'Latitude'], labs = row.names(siteClusters$centers), title = 'Cluster Centers')

# Now we are ready to extract features based on these clusters

############### Feature Extraction ################

# First use our clusters to classify all data points
d$Cluster <- fitted(siteClusters, method = 'classes')
d[,c('ClusterLat', 'ClusterLon')] <- fitted(siteClusters, method = 'centers')

# Average AQI and actual measurements for each cluster, each day.
dAQI0 <- dcast(d, Date ~ Cluster, value.var = 'AQI', fun.aggregate = mean, na.rm=T)
# From the summary we can see the number NA's for each cluster is not too much (max at 511). So we can be comfortable with 20 clusters.
print(summary(dAQI0))

# Filling missing values:

# None of them is missing the first day
print(head(dAQI0,1))
# A lot of them is missing the data from 2016-04-01 (row 3378) and forward, so let's remove the tail.
print(tail(dAQI0,65))
dAQI <- head(dAQI0, 3378)
# Check. But cluster 15 still has missing data in the last few days.
# Looks like 2016-01-31 is the last day all clusters have data. So let's cut the tail again (or we can copy the data down only for cluster 15)
print(tail(dAQI,65))
dAQI <- head(dAQI, 3318)
print(tail(dAQI))
# We are good to do the interpolations for missing values now that we have the end point data.

# To interpolate, we use the "zoo" package and use its time series interpolation function to do it for us.
zAQI0 <- zoo(dAQI[,2:ncol(dAQI)], dAQI$Date)
zAQI <- na.approx(zAQI0)

# Then convert the 'zoo' object to an 'xts' time series object for ease of windowing.
# 'zoo' is good for plotting, 'xts' is convenient for subsetting by dates.
xAQI <- as.xts(zAQI)

# Plot all clusters separately
plot.zoo(xAQI)
# Plot all clusters together..
plot.zoo(xAQI, screen=1, col=1:20)

# Plot 1 months of data:
plot.zoo(xAQI['2013-01-01/2013-02-01'])
plot.zoo(xAQI['2013-01-01/2013-02-01'], screen=1, col=1:20)

# Plot a certain day to test plotting
plotDotMapUS(siteClusters$centers[,'Longitude'], siteClusters$centers[,'Latitude'], sizeScaleLab = 'Avg AQI', dotSizes=as.numeric(xAQI['2013-02-23']), dotColors=as.numeric(xAQI['2013-02-23']), colorScaleLab='Avg AQI', labs = row.names(siteClusters$centers), title = 'Cluster Centers')

# # Let's make another video
# saveVideo({
#   startDate <- as.Date('2013-01-01')
#   endDate <- as.Date('2013-12-31')
#   ani.options(interval = 0.5)
#   # ani.options(interval = 0.5, ani.dev = function(...){return(png(bg = 'transparent', ...))} )
#   for(di in (startDate:endDate)) {
#     someday <- as.numeric(xAQI[as.Date(di)])
#     plotDotMapUS(siteClusters$centers[,'Longitude'], siteClusters$centers[,'Latitude'], dotColors=someday, colorScaleLab='Avg AQI', labs = row.names(siteClusters$centers), title=paste('PM2.5 AQI at All Clusters on', as.Date(di)))
#     ani.pause()
#   }
# },
#   paste0('animCluster', startDate, '.mp4'),
#   ffmpeg = 'bin/ffmpeg.exe',
#   ani.width = 800, ani.height = 450)




# Preparing lagged data to be used as features

# Select the number of days to lag: 1 to 3 days.
maxLag = 3
lags = seq_len(maxLag)

# Prepare data.frame for the actual training.
df0 <- as.data.frame(xAQI)

# Append columns for all cluster for each lag.
# These are our features!
for (l in lags) {
  dfLag <- as.data.frame(lag(xAQI, l))
  colnames(dfLag) <- paste0(colnames(dfLag), '.lag-', l)
  df0 <- cbind(df0, dfLag)
}

# When you lag data, the begining of the data will be NA's, so we remove the first 8 rows because we used lag 1 to 7 days
df <- df0[(maxLag+1):nrow(df0),]
usedDates <- index(xAQI)[(maxLag+1):length(index(xAQI))]
# Now we add 2 other dates related features: day of the week and month of the year
dfMonthDay <- data.frame(MoY = relevel(factor(months(usedDates), levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')), ref='January'), DoW = relevel(factor(weekdays(usedDates), levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')), ref='Monday'))
dfMonthDayNum <- sapply(dfMonthDay, as.numeric)


print(summary(df))
print(dim(df))
# This completes our feature selection.

#####################################################

############# Training of Models ##############

# Try RNN
# RNN is the model that's most suitable for this type of data.
# However the RNN package is still under development, it currently cannot handel this amount of data.

# Try Feed Forward Neural Network
library(nnet)

mNN <- nnet(x = cbind(df[,21:ncol(df)],dfMonthDayNum), y = df[,1:20], size = 20, linout = T, maxit = 2000, MaxNWts = 4000)

pNN <- predict(mNN, cbind(df[,21:ncol(df)],dfMonthDayNum))
zpNN <- zoo(pNN, usedDates)
plot(zpNN) # BAD!

# Try Random Forest, codes for training are commented due to long run time.
library(randomForest)

mRFs <- list()
pRF <- data.frame(row.names = usedDates)

# for (j in c(1:20)) {
#   cat('Training Cluster', j, '...\n')
#   mRFj <- randomForest(x = cbind(df[,21:ncol(df)],dfMonthDay), y = df[,j], ntree = 500)
#   pRFj <- predict(mRFj, cbind(df[,21:ncol(df)],dfMonthDay))
#   mRFs <- c(mRFs, list(mRFj))
#   pRF <- cbind(pRF, pRFj)
# }
# colnames(pRF) <- paste(c(1:20))
# save(mRFs, pRF, file = 'mRFs3.rda')

# Training takes almost 30min on my laptop. Saved the models in file.
load('mRFs3.rda')

zpRF <- zoo(pRF, usedDates)
xpRF <- as.xts(zpRF)

# Print Root mean squared error (RMSE):
cat('Root Mean Squared Error:', sqrt(mean((xAQI - xpRF)^2)), '\n')

plot.zoo(xpRF)
plot.zoo(xAQI) # compare with actual data

plot.zoo(xpRF['2013-01-01/2013-02-01'])
plot.zoo(xAQI['2013-01-01/2013-02-01']) # compare with actual data

# Let's see what these models tells us by plotting the variable importance charts for each model
varImpPlot(mRFs[[1]]) # all important features are from clusters around it by refering to the map
varImpPlot(mRFs[[6]]) # New York City, similar as above, short term surrounding areas are important
varImpPlot(mRFs[[3]]) # San Fransisco is influnced mostly by its own past experience and the seasons
varImpPlot(mRFs[[20]]) # Miami is similar as above
varImpPlot(mRFs[[8]]) # Chicago is again affected most by surrounding area

# A function to use the models to project into the future day by day starting with only 1 day of real data
projection <- function(ms, dfRow, nProj= 7, nCluster = 20, nLag = maxLag, nOtherFeat = 2) {
  dfProj <- dfRow
  for(i in seq_len(nProj)) {
    nextDate <- as.Date(row.names(dfRow)) + 1
    print(nextDate)
    nextRow <- dfRow
    nextRow[,c( (nCluster+1) : ((nLag+1)*nCluster) )] <- nextRow[,c( 1 : (nLag*nCluster) )]
    nextRow[,(nLag+1)*nCluster + 1] <- relevel(factor(months(nextDate), levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')), ref='January')
    nextRow[,(nLag+1)*nCluster + 2] <- relevel(factor(weekdays(nextDate), levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')), ref='Monday')
    for(ci in seq_len(nCluster)) {
      nextRow[,ci] <- predict(ms[[ci]], dfRow[,c( (nCluster+1) : ((nLag+1)*nCluster + nOtherFeat) )])
    }
    row.names(nextRow) <- paste(nextDate)
    dfProj <- rbind(dfProj, nextRow)
    dfRow <- nextRow
  }
  return(dfProj)
}

# a convenient data frame for projection
dfAll <- cbind(df, dfMonthDay)

######### Should we show this ??
# Let's take 2007-06-13 and project into the future to see if the severe AQI at cluster one has any effect on other locations.
prj1 <- projection(mRFs, dfAll['2013-03-29',])
zprj1 <- zoo(prj1[,1:20], as.Date(row.names(prj1)))
plot.zoo(cbind(xAQI['2013-03-29/2013-04-05'], zprj1), screens = (1:20), col = c(rep('red',20), rep('blue',20)), main = 'Red = Actual, Blue = Projection')
#################################

# Make up some experimental data to see the effect
testDay <- dfAll['2009-06-25',]
row.names(testDay) <- '2017-04-01' # change date to the future
testDay[,1:(20*(1+maxLag))] <- rep(50,160) # AQI=50 at all clusters except cluster 14
testDay[,14+(0:maxLag)*20] <- c(140, 120, 100, 60) # input scenario data: cluster 14 has severe AQI
testDay[,81] <- relevel(factor(months(as.Date('2017-04-01')), levels = c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December')), ref='January')
testDay[,82] <- relevel(factor(weekdays(as.Date('2017-04-01')), levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')), ref='Monday')

prjTest <- projection(mRFs, testDay)
zprjTest <- zoo(prjTest[,1:20], as.Date(row.names(prj1)))
plot(zprjTest)
# from the plots we can see clusters to the east of 14 (4, 6, 7, 10, 16) all have a impact from 14's high AQI.
# 7 is more immediate, rising on the next day; others clusters' increases are a few days delayed.
# Conclusion: this model can indeed tell this trend. 

