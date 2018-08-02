library(ggplot2)
library(scales)
library(grid)
library(forecast)
library(ggfortify)

# lead_cause_2001_all <- read.csv("lead-cause-2001-all.csv")

# Look at age-adjusted rates
suicide.rates.adj <- read.delim("suicide-rate-all.txt")
suicide.rates <- subset(suicide.rates.adj, select=c(Year, Deaths, Age.Adjusted.Rate))
colnames(suicide.rates)[2] <- "Suicides"
suicide.rates$Year <-paste(suicide.rates$Year, "01-01", sep="-") # Add dummy day to convert this variable to date format
suicide.rates$Year <- as.Date(suicide.rates$Year, "%Y-%m-%d")

# VIS 1.0 Plot yearly rates
ggplot(suicide.rates, aes(x=Year, y=Age.Adjusted.Rate)) +
  geom_line(color="blue") +
  geom_point() +
  scale_x_date(breaks=date_breaks("1 year"), labels=date_format("%Y"), expand=c(1/50, 0), limits=c(as.Date("1999-01-01"), as.Date("2016-01-01"))) +
  scale_y_continuous(breaks=seq(0, max(suicide.rates$Age.Adjusted.Rate) + 0.5, 0.5)) +
  labs(x="Year", y="Deaths per 100k", color="Year", title="U.S. Deaths by Suicide: Annual Age-Adjusted Rate") +
  theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=-45, hjust=0))

# Look at monthly data
suicide.mort.all.monthly <- read.delim("suicide-mort-all-monthly-1999-2016")

monthList <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
suicide.mort.all.monthly <- subset(suicide.mort.all.monthly, Injury.Intent == "Suicide", select=-c(Notes, Injury.Intent, Injury.Intent.Code, Population, Crude.Rate, X..of.Total.Deaths))
totalMonthly <- subset(suicide.mort.all.monthly.1999.2016, Notes =="Total" & Month != "", select=Deaths)
colnames(totalMonthly)[1] <- "Total.Deaths"

suicide.mort.all.monthly <- cbind(suicide.mort.all.monthly, totalMonthly)
suicide.mort.all.monthly$Perc.Total.Deaths <- suicide.mort.all.monthly$Deaths / suicide.mort.all.monthly$Total.Deaths

suicide.mort.all.monthly$Month <- rep(monthList, 18)
suicide.mort.all.monthly$Month <- factor(suicide.mort.all.monthly$Month, levels=unique(suicide.mort.all.monthly$Month))
suicide.mort.all.monthly$Year <- factor(suicide.mort.all.monthly$Year, levels=unique(suicide.mort.all.monthly$Year))
suicide.mort.all.monthly$Month.Code <- paste(suicide.mort.all.monthly$Month.Code, "01", sep="/") # Add dummy day to convert this variable to date format
suicide.mort.all.monthly$Month.Code <- as.Date(suicide.mort.all.monthly$Month.Code, "%Y/%m/%d")
colnames(suicide.mort.all.monthly)[5] <- "Suicides"

suicide.mort.all.monthly$Other.Causes <- suicide.mort.all.monthly$Total.Deaths - suicide.mort.all.monthly$Suicides

# VIS 1.1 Plot within-year suicide trends
ggplot(suicide.mort.all.monthly, aes(x=Month.Code, y=Suicides)) +
  geom_point() +
  # stat_smooth(span=0.1, se=TRUE, color="grey", alpha=0.2) +
  geom_line(color="blue") +
  scale_x_date(breaks=date_breaks("1 year"), labels=date_format("%b %y"), expand=c(1/40, 0)) +
  labs(x="Time", y="Number of Suicides", color="Year", title="U.S. Deaths by Suicide per Month: 1999-2016") +
  theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=-60, hjust=0))

# VIS 1.2 Plot within-year trends with percentage of total deaths
ggplot(suicide.mort.all.monthly, aes(x=Month.Code, y=Perc.Total.Deaths)) +
  geom_point() +
  # geom_line() +
  stat_smooth(method="lm", se=TRUE) +
  scale_x_date(breaks=date_breaks("1 year"), labels=date_format("%b %y"), expand=c(1/40, 0)) +
  scale_y_continuous(labels=percent_format()) +
  labs(x="Time", y="Percent of All Deaths per Month", color="Year", title="U.S. Suicides per Month from 1999-2016: Percentage of Total Deaths") +
  theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=-60, hjust=0))

# VIS 2.1.1 Multi plot
#Plot all deaths
pTotal <- ggplot(suicide.mort.all.monthly, aes(x=Month.Code, y=Total.Deaths)) +
  # Add vertical lines centered around the middle of winter and summer every year
  geom_vline(xintercept=seq(as.Date("1999-01-16"), as.Date("2017-02-01"), "year"), color="blue", size=12, alpha=0.06) + # Thicker lines split year in half
  geom_vline(xintercept=seq(as.Date("1999-07-16"), as.Date("2017-01-01"), "year"), color="orangered", size=12, alpha=0.06) +
  geom_vline(xintercept=seq(as.Date("1999-01-16"), as.Date("2017-02-01"), "year"), color="blue", size=6, alpha=0.075) + # Thinner lines split year into seasons
  geom_vline(xintercept=seq(as.Date("1999-07-16"), as.Date("2017-01-01"), "year"), color="orangered", size=6, alpha=0.075) +
  geom_vline(xintercept=seq(suicide.mort.all.monthly$Month.Code[1], as.Date("2017-01-01"), "year"), color="blue", size=.5, alpha=0.1) +
  geom_vline(xintercept=seq(suicide.mort.all.monthly$Month.Code[7], as.Date("2017-01-01"), "year"), color="orangered", size=.5, alpha=0.1) +
  geom_vline(xintercept=seq(suicide.mort.all.monthly$Month.Code[1], as.Date("2017-01-01"), "year"), linetype="dashed", color="black", size=.5, alpha=0.2) +
  geom_vline(xintercept=seq(suicide.mort.all.monthly$Month.Code[7], as.Date("2017-01-01"), "year"), linetype="dashed", color="black", size=.5, alpha=0.2) +
  annotate("text",
           x=c(as.Date("1999-01-16"),
               as.Date("1999-07-16")),
           y=max(suicide.mort.all.monthly$Total.Deaths), 
           hjust=0,
           vjust=0.5,
           size=6,
           label=c("Winter", "Summer"),
           color=c("midnightblue", "darkred"),
           angle=-90) +
  geom_line(color="black") +
  geom_point(color="black", alpha=0.8) +
  # stat_smooth(span=0.1, se=FALSE, color="seashell4") +
  scale_x_date(breaks=date_breaks("6 months"), labels=date_format("%B"), expand=c(1/40, 0), position="top") +
  labs(x="Time", y="Total Deaths", title="U.S. Total Deaths and Deaths by Suicide per Month: 1999-2016") +
  theme(text=element_text(family="Times", size=18), axis.title.x = element_blank(), axis.text.x=element_text(angle=-65, hjust=1))

# Suicides
pSuicide <- ggplot(suicide.mort.all.monthly, aes(x=Month.Code, y=Suicides)) +
  geom_vline(xintercept=seq(as.Date("1999-01-16"), as.Date("2017-02-01"), "year"), color="blue", size=12, alpha=0.06) + # Thicker lines split year in half
  geom_vline(xintercept=seq(as.Date("1999-07-16"), as.Date("2017-01-01"), "year"), color="orangered", size=12, alpha=0.06) +
  geom_vline(xintercept=seq(as.Date("1999-01-16"), as.Date("2017-02-01"), "year"), color="blue", size=6, alpha=0.075) + # Thinner lines split year into seasons
  geom_vline(xintercept=seq(as.Date("1999-07-16"), as.Date("2017-01-01"), "year"), color="orangered", size=6, alpha=0.075) +
  geom_vline(xintercept=seq(suicide.mort.all.monthly$Month.Code[1], as.Date("2017-01-01"), "year"), color="blue", size=.5, alpha=0.1) +
  geom_vline(xintercept=seq(suicide.mort.all.monthly$Month.Code[7], as.Date("2017-01-01"), "year"), color="orangered", size=.5, alpha=0.1) +
  geom_vline(xintercept=seq(suicide.mort.all.monthly$Month.Code[1], as.Date("2017-01-01"), "year"), linetype="dashed", color="black", size=.5, alpha=0.2) +
  geom_vline(xintercept=seq(suicide.mort.all.monthly$Month.Code[7], as.Date("2017-01-01"), "year"), linetype="dashed", color="black", size=.5, alpha=0.2) +
  geom_line(color="black") +
  geom_point(color="black", alpha=0.8) +
  # stat_smooth(span=0.1, se=FALSE, color="seashell4") +
  scale_x_date(breaks=date_breaks("1 year"), labels=date_format("%Y"), expand=c(1/40, 0)) +
  labs(x="Time", y="Number of Suicides") +
  theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=-65, hjust=0))

# Put them together and what do you get
grid.newpage()
grid.draw(rbind(ggplotGrob(pTotal), ggplotGrob(pSuicide), size = "first"))

# Just making some color palettes
greys <- c(seq(84, 28, -4))
greys <- c(paste("grey", greys, sep=""))
allgreys <- c(seq(84, 16, -4))
allgreys <-  c(paste("grey", allgreys, sep=""))

# VIS 2.2.1
# Plot monthly trends with separate regression lines for years
ggplot(suicide.mort.all.monthly, aes(x=Month, y=Suicides, color=Year, group=Year)) +
  scale_color_manual(values=allgreys) + 
  geom_point() +
  stat_smooth(se=FALSE) +
  labs(x="Month", y="Number of Suicides", color="Year", title="U.S. Deaths by Suicide per Month") +
  guides(color=guide_legend(reverse=TRUE)) +
  theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=-50, hjust=0))

# VIS 2.2.2
# Plot monthly trends with separate regression lines for years - highlight 2014
ggplot(suicide.mort.all.monthly, aes(x=Month, y=Suicides, color=Year, group=Year)) +
  scale_color_manual(values=c(greys, "red", "grey20", "grey16")) + 
  geom_point() +
  stat_smooth(se=FALSE) +
  geom_point(data=subset(suicide.mort.all.monthly, Year == "2014"), aes(x=Month, y=Suicides), color="red", size=3) +
  stat_smooth(data=subset(suicide.mort.all.monthly, Year == "2014"), aes(x=Month, y=Suicides), color="red", size=2, se=FALSE) +
  labs(x="Month", y="Number of Suicides", color="Year", title="U.S. Deaths by Suicide per Month") +
  guides(color=guide_legend(reverse=TRUE)) +
  theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=-50, hjust=0))

# VIS 2.2.3
# Plot monthly trends with separate lines connecting years - highlight 2014
ggplot(suicide.mort.all.monthly, aes(x=Month, y=Suicides, color=Year, group=Year)) +
  scale_color_manual(values=c(greys, "red", "grey20", "grey16")) + 
  geom_point() +
  geom_line() +
  geom_point(data=subset(suicide.mort.all.monthly, Year == "2014"), aes(x=Month, y=Suicides), color="red", size=3) +
  geom_line(data=subset(suicide.mort.all.monthly, Year == "2014"), aes(x=Month, y=Suicides), color="red", size=2) +
  labs(x="Month", y="Number of Suicides", color="Year", title="U.S. Deaths by Suicide per Month") +
  guides(color=guide_legend(reverse=TRUE)) +
  theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=-50, hjust=0))


# VIS 2.2.4
# Annotate historical event linked to suicide spike in Aug 2014
ggplot(suicide.mort.all.monthly, aes(x=Month, y=Suicides, color=Year, group=Year)) +
  scale_color_manual(values=c(greys, "red", "grey20", "grey16")) + 
  geom_point() +
  geom_line() +
  geom_point(data=subset(suicide.mort.all.monthly, Year == "2014"), aes(x=Month, y=Suicides), color="red", size=3) +
  geom_line(data=subset(suicide.mort.all.monthly, Year == "2014"), aes(x=Month, y=Suicides), color="red", size=2) +
  # geom_vline(xintercept=c(7.5, 8.5), linetype="dashed", color="red", size=.5, alpha=0.8) +
  annotate("rect", xmin=7, xmax=11, ymin=-Inf, ymax=Inf, fill="red", alpha=0.05) +
  annotate("text",
           x=7.1,
           y=2300,
           hjust=0,
           vjust=0.5,
           size=5,
           label="Robin Williams Suicide:\nAugust 11, 2014") +
  labs(x="Month", y="Number of Suicides", color="Year", title="U.S. Deaths by Suicide per Month") +
  guides(color=guide_legend(reverse=TRUE)) +
  theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=-50, hjust=0))

# Multivariate timeseries
suicide.time.series <- ts(subset(suicide.mort.all.monthly, select=c(Suicides, Perc.Total.Deaths, Total.Deaths)), frequency=12, start=1999)
plot(suicide.time.series)

suicide.ts.compare <- ts(subset(suicide.mort.all.monthly, select=c(Suicides)), frequency=12, start=1999)
plot(suicide.ts.compare)

# Univariate from before 2014 for forecast
suicide.ts.predict <- ts(subset(suicide.mort.all.monthly, Year != "2014" & Year != "2015" & Year != "2016", select=Suicides), frequency=12, start=1999)

# Use forecast package
suicideTimeFit14 <- auto.arima(suicide.ts.predict)
predict14 <- forecast(suicideTimeFit14, 36)
plot(predict14)

# Vis 3.0 Prediction plot
autoplot(predict14)

# Pull predicted values that were calculated using auto.arima and 1999-2013 data
forecastData <- subset(suicide.mort.all.monthly, Year == "2014" | Year == "2015" | Year == "2016", select=Month.Code)
forecastValues <- as.data.frame(summary(predict14))
forecastData <- cbind(forecastData, forecastValues[1])
colnames(forecastData)[2] <- "Suicides"
forecastData$Source <- "Predicted Values"

comparePlot14 <- subset(suicide.mort.all.monthly, select=c(Month.Code, Suicides))
comparePlot14$Source <- "Observed Data"
compareForecast14 <- rbind(comparePlot14, forecastData)

# VIS 3: Plot actual suicides vs number predicted excluding 2014 and beyond data
ggplot(compareForecast14, aes(x=Month.Code, y=Suicides, color=Source, group=Source)) +
  scale_color_manual(values=c("red", "blue")) +
  geom_point() +
  geom_line() +
  scale_x_date(breaks=date_breaks("1 month"), labels=date_format("%b %y"), expand=c(1/40, 0), limits=c(as.Date("2014-01-01"), as.Date("2015-01-01"))) +
  annotate("rect", xmin=as.Date("2014-07-01"), xmax=as.Date("2014-11-01"), ymin=-Inf, ymax=Inf, fill="red", alpha=0.05) +
  annotate("text",
           x=as.Date("2014-07-01"),
           y=2500,
           hjust=0,
           vjust=0.5,
           size=5,
           label="Robin Williams Suicide:\nAugust 11, 2014") +
  labs(x="Time", y="Number of Suicides", color="Source", title="2014 U.S. Deaths by Suicide: Actual Data vs. Predictions Based on 1999-2013 Data") +
  theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=-60, hjust=0))

# Look at most recent data from 2016-2017
mortLatest <- read.csv("suicide-mort-quarterly-16-17.csv")
suicideLatest <- subset(mortLatest, Indicator == "Suicide" & Rate.Type == "Age-adjusted", select=-Indicator)
suicideLatest <- na.omit(suicideLatest)
suicideLatest$Significant <- factor(suicideLatest$Significant, labels=c("Not significantly different", "Significantly different"))


ggplot(suicideLatest, aes(x=Year_Quarter, y=Rate, group=Type, color=Type, size=Significant, shape=Significant)) +
  scale_shape_manual(values=c(20, 17)) + 
  scale_size_manual(values=c(2.5, 4)) +
  geom_point() +
  geom_line(aes(color=Type), size=1) +
  scale_y_continuous(limits=c(0, 18), breaks=seq(0, 18, 2)) +
  labs(x="Quarter", y="Rate (Deaths per 100K)", color="Time Period", size="Compared to same time period in previous year", shape="Compared to same time period in previous year", title="U.S. Age-Adjusted Quarterly Suicide Rates: 2016-2017") +
  theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=-45, hjust=0), aspect.ratio=.9)


# Forecast
newestPredict <- ts(subset(suicide.rates, select=Age.Adjusted.Rate), frequency=1, start=1999)
futurePredict <- auto.arima(newestPredict)
futurePredict <- forecast(futurePredict, 1)
autoplot(futurePredict)

futureData <- as.data.frame(summary(futurePredict))

q4Predict <- subset(suicideLatest, Type == "12 months ending with quarter", select=c(Year_Quarter, Rate))
q4Predict$Source <- "Observed Data"

futurePredict <- data.frame(Year_Quarter="2017 Q4")
futurePredict <- cbind(futurePredict, futureData[1])
colnames(futurePredict)[2] <- "Rate"
futurePredict$Source <- "Predicted with 1999-2016 Data"

futurePredict <- rbind(q4Predict, futurePredict)

shortPredict <- ts(subset(suicideLatest, Type == "12 months ending with quarter", select=Rate), frequency=4, start=2016)
shortFuture <- ets(shortPredict)
shortFuture <- forecast (shortFuture, 1)
autoplot(shortFuture)
plot(shortPredict)

shortPoint <- as.data.frame(summary(shortFuture))
shortNewPoint <- data.frame(Year_Quarter="2017 Q4")
shortNewPoint <- cbind(shortNewPoint, shortPoint[1])
colnames(shortNewPoint)[2] <- "Rate"
shortNewPoint$Source <- "Predicted with 2016-Q1 - 2017-Q3 Quarterly Data"

futurePredict <- rbind(futurePredict, shortNewPoint)

# VIS 3.2 Plot predicted 2017 rate with and without recent data
ggplot(futurePredict, aes(x=Year_Quarter, y=Rate)) +
  geom_line(aes(group=futurePredict$Source), color="grey") +
  annotate("segment", 
           x=c(7, 7),
           xend=c(8, 8),
           y=c(13.8, 13.8),
           yend=c(13.79014, 13.6358),
           color="grey",
           linetype="dashed",
           size=1) +
  annotate("rect", xmin=5, xmax=7, ymin=-Inf, ymax=Inf, fill="red", alpha=0.05) +
  annotate("text",
           x=5.1,
           y=13.3,
           hjust=0,
           vjust=0,
           size=5,
           label="Chris Cornell Suicide:\nMay 18, 2017\n\nChester Bennington Suicide:\nJuly 20, 2017") +
  geom_point(aes(color=Source), size=4) +
  scale_color_manual(values=c("black", "dodgerblue", "red")) +
  scale_y_continuous(breaks=seq(0, max(futurePredict$Rate) + 0.5, 0.2)) +
  labs(x="Quarter", y="Deaths per 100k", color="Source", title="U.S. Deaths by Suicide: Age-Adjusted Rate for 12 Months Preceding Each Quarter") +
  theme(text=element_text(family="Times", size=18), axis.text.x=element_text(angle=-45, hjust=0), legend.justification=c(1, 1), legend.position=c(.4, .95))
