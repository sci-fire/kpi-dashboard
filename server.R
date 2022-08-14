#**********************************************************
#* CATEGORY	SHINY
#* GROUP	REPORTING
#* AUTHOR	LANCE HAYNIE <LHAYNIE@SCCITY.ORG>
#* FILE		SERVER.R
#**********************************************************
#Copyright Santa Clara City
#Developed for Santa Clara - Ivins Fire & Rescue
#Licensed under the Apache License, Version 2.0 (the "License");
#you may not use this file except in compliance with the License.#
#You may obtain a copy of the License at
#http://www.apache.org/licenses/LICENSE-2.0
#Unless required by applicable law or agreed to in writing, software
#distributed under the License is distributed on an "AS IS" BASIS,
#WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#See the License for the specific language governing permissions and
#limitations under the License.

library(shiny)
library(ggplot2)
library(dplyr)
library(sqldf)
library(lubridate)
library(ggplot2)
library(viridis)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(forcats)
library(tidyverse)
library(forecast)
library(profvis)
  
fy_start <- as.Date("2021-07-01")
fy_end <- as.Date("2022-06-30")

load("/analytics/publicsafety/scif/prod/data/response_times/response_times.Rdata")
response_times$date <- parse_date_time(response_times$date, "%m/%d%Y")
response_times$date <- as.Date(response_times$date)
response_times$year <- year(response_times$date)
response_times$month <- month(response_times$date)
response_times$city[is.na(response_times$city)] <- "WashCo"

load("/analytics/publicsafety/scif/prod/data//units.Rdata")

response_times_tmp <- sqldf(capture.output(cat("select * from response_times where year != ",as.character(year(now()))," and month != ",as.character(month(now())),sep='')))

incidents_fire <- sqldf("select count(number) as fire, year, month from response_times_tmp where type = 'Fire' group by year, month order by year, month")
incidents_medical <- sqldf("select count(number) as medical, year, month from response_times_tmp where type = 'Medical' group by year, month order by year, month")
incidents_other <- sqldf("select count(number) as other, year, month from response_times_tmp where type = 'Other' group by year, month order by year, month")

incidents <- merge(incidents_fire, incidents_medical, by.x=c("year", "month"), by.y=c("year", "month"))
incidents <- merge(incidents, incidents_other, by.x=c("year", "month"), by.y=c("year", "month"))
incidents <- incidents[order(incidents$year, incidents$month), ]

incident_ts <- ts(incidents, frequency = 12)
incident_d_fire <- decompose(incident_ts[,3])
incident_d_medical <- decompose(incident_ts[,4])
incident_d_other <- decompose(incident_ts[,5])

date6m <- seq(as.Date(cut(as.Date(now()), "month")), length = 2, by = "-6 month")[2] - 1
response_times6m <- sqldf(capture.output(cat("select * from response_times where date >= ", date6m, sep = "")))

turnout_fire <- as.numeric(sqldf("select avg(turnout_time) as avg from response_times6m where type = 'Fire' and turnout_time is not NULL"))
travel_fire <- as.numeric(sqldf("select avg(travel_time) as avg from response_times6m where type = 'Fire' and travel_time is not NULL"))
response_fire <- as.numeric(sqldf("select avg(response_time) as avg from response_times6m where type = 'Fire' and response_time is not NULL"))
turnout_medical <- as.numeric(sqldf("select avg(turnout_time) as avg from response_times6m where type = 'Medical' and turnout_time is not NULL"))
travel_medical <- as.numeric(sqldf("select avg(travel_time) as avg from response_times6m where type = 'Medical' and travel_time is not NULL"))
response_medical <- as.numeric(sqldf("select avg(response_time) as avg from response_times6m where type = 'Medical' and response_time is not NULL"))
turnout_other <- as.numeric(sqldf("select avg(turnout_time) as avg from response_times6m where type = 'Other'"))
travel_other <- as.numeric(sqldf("select avg(travel_time) as avg from response_times6m where type = 'Other'"))
response_other <- as.numeric(sqldf("select avg(response_time) as avg from response_times6m where type = 'Other'"))
santaclara_incidents <- as.numeric(sqldf("select count(*) as sum from response_times6m where city = 'Santa Clara'"))
ivins_incidents <- as.numeric(sqldf("select count(*) as sum from response_times6m where city = 'Ivins'"))
total_incidents <- as.numeric(sqldf("select count(*) as sum from response_times6m"))

date7m <- seq(as.Date(cut(as.Date(now()), "month")), length = 2, by = "-7 day")[2] - 1
response_times7d <- sqldf(capture.output(cat("select * from response_times where date >= ", date7m, sep = "")))
response_times7d$week_day <- weekdays(response_times7d$date)

turnout_7d <- as.numeric(sqldf("select avg(turnout_time) as avg from response_times7d where turnout_time is not NULL"))
travel_7d <- as.numeric(sqldf("select avg(travel_time) as avg from response_times7d where travel_time is not NULL"))
response_7d <- as.numeric(sqldf("select avg(response_time) as avg from response_times7d where response_time is not NULL"))
santaclara_incidents7d <- as.numeric(sqldf("select count(*) as sum from response_times7d where city = 'Santa Clara'"))
ivins_incidents7d <- as.numeric(sqldf("select count(*) as sum from response_times7d where city = 'Ivins'"))

date12m <- seq(as.Date(cut(as.Date(now()), "month")), length = 2, by = "-12 month")[2] - 1
county_incidents <- sqldf(capture.output(cat("select * from response_times where city not in ('Santa Clara', 'Ivins', 'Kayenta', 'St George', 'LaVerkin', 'Washington', 'Hurricane') and date >= ", date12m, sep = "")))
county_incidents$city[is.na(county_incidents$city)] <- "WashCo"
county_incidents$city[county_incidents$city == 'WashCo Santa Cl'] <- "WashCo"
county_incidents$city[county_incidents$city == 'WashCo StGeorge'] <- "WashCo"
county_incidents$city[county_incidents$city == 'WashCo Ivins'] <- "WashCo"

county_incidents_count <- as.numeric(sqldf("select count(*) as total from county_incidents"))
county_incidents_travel_time <- as.numeric(sqldf("select avg(travel_time) as travel_time from county_incidents"))*2
county_incidents_away_time <- as.numeric(sqldf("select avg(assignment_time) as assignment_time from county_incidents")) + county_incidents_travel_time

county_incident_table <- sqldf("select number as 'Incident Number',
                               nature as Nature,
                               city as City,
                               date,
                               paged as 'Paged Time',
                               enroute as 'En Route Time',
                               arrived as 'Arrived Time',
                               complete as 'Completed Time'
                               from county_incidents")
names(county_incident_table)[4] <- 'Date'

incidents12m <- sqldf(capture.output(cat("select * from response_times where date >= ", date12m, sep = "")))
incident_table <- sqldf("select number as 'Incident Number',
                               nature as Nature,
                               city as City,
                               date,
                               paged as 'Paged Time',
                               enroute as 'En Route Time',
                               arrived as 'Arrived Time',
                               complete as 'Completed Time'
                               from incidents12m")
names(incident_table)[4] <- 'Date'

fy_incidents <- sqldf(capture.output(cat("select * from response_times where date >= ", fy_start, " and date <= ",fy_end, sep = "")))

fy_incidents_tbl <- sqldf("select agency as Agency,
                          number as 'Incident Number',
                          callid as 'Call ID',
                          nature as Nature,
                          type as Type,
                          city as City,
                          date
                          from fy_incidents")
names(fy_incidents_tbl)[7] <- 'Date'

fy_incidents_sc <- sqldf("select * from fy_incidents where city = 'Santa Clara'")
fy_incidents_iv <- sqldf("select * from fy_incidents where city = 'Ivins'")
fy_incidents_o <- sqldf("select * from fy_incidents where city not in ('Santa Clara', 'Ivins')")

fy_fire_inc <- as.numeric(sqldf("select count(*) as total from fy_incidents where type = 'Fire'"))
fy_med_inc <- as.numeric(sqldf("select count(*) as total from fy_incidents where type = 'Medical'"))
fy_other_inc <- as.numeric(sqldf("select count(*) as total from fy_incidents where type = 'Other'"))

fy_santa_clara_inc <- as.numeric(sqldf("select count(*) as total from fy_incidents where city = 'Santa Clara'"))
fy_ivins_inc <- as.numeric(sqldf("select count(*) as total from fy_incidents where city = 'Ivins'"))
fy_otherarea_inc <- as.numeric(sqldf("select count(*) as total from fy_incidents where city not in ('Santa Clara', 'Ivins')"))

fy_santa_clara_inc_fire <- as.numeric(sqldf("select count(*) as total from fy_incidents where city = 'Santa Clara' and type = 'Fire'"))
fy_ivins_inc_fire <- as.numeric(sqldf("select count(*) as total from fy_incidents where city = 'Ivins' and type = 'Fire'"))
fy_other_inc_fire <- as.numeric(sqldf("select count(*) as total from fy_incidents where city not in ('Santa Clara', 'Ivins') and type = 'Fire'"))

fy_santa_clara_inc_med <- as.numeric(sqldf("select count(*) as total from fy_incidents where city = 'Santa Clara' and type = 'Medical'"))
fy_ivins_inc_med <- as.numeric(sqldf("select count(*) as total from fy_incidents where city = 'Ivins' and type = 'Medical'"))
fy_other_inc_med <- as.numeric(sqldf("select count(*) as total from fy_incidents where city not in ('Santa Clara', 'Ivins') and type = 'Medical'"))

fy_santa_clara_inc_other <- as.numeric(sqldf("select count(*) as total from fy_incidents where city = 'Santa Clara' and type = 'Other'"))
fy_ivins_inc_other <- as.numeric(sqldf("select count(*) as total from fy_incidents where city = 'Ivins' and type = 'Other'"))
fy_other_inc_other <- as.numeric(sqldf("select count(*) as total from fy_incidents where city not in ('Santa Clara', 'Ivins') and type = 'Other'"))

shinyServer(function(input, output) {
  output$turnout_fire <- renderValueBox({
    valueBox(
      value = formatC(turnout_fire, digits = 1, format = "f"),
      subtitle = "Turnout Time: Fire",
      icon = icon("fa-solid fa-fire"),
      if (turnout_fire >= 1.2) {
        color <- "yellow"
      } else if (turnout_fire >= 2) {
        color <- "red"
      } else {
        color <- "green"
      }
    )
  })

  output$travel_fire <- renderValueBox({
    valueBox(
      value = formatC(travel_fire, digits = 1, format = "f"),
      subtitle = "Travel Time: Fire",
      icon = icon("fa-solid fa-fire"),
      if (travel_fire >= 8) {
        color <- "yellow"
      } else if (travel_fire >= 10) {
        color <- "red"
      } else {
        color <- "green"
      }
    )
  })

  output$response_fire <- renderValueBox({
    valueBox(
      value = formatC(response_fire, digits = 1, format = "f"),
      subtitle = "Response Time: Fire",
      icon = icon("fa-solid fa-fire"),
      if (response_fire >= 11) {
        color <- "yellow"
      } else if (response_fire >= 15) {
        color <- "red"
      } else {
        color <- "green"
      }
    )
  })

  output$turnout_medical <- renderValueBox({
    valueBox(
      value = formatC(turnout_medical, digits = 1, format = "f"),
      subtitle = "Turnout Time: Medical",
      icon = icon("fa-solid fa-truck-medical"),
      if (turnout_medical >= 1) {
        color <- "yellow"
      } else if (turnout_medical >= 2) {
        color <- "red"
      } else {
        color <- "green"
      }
    )
  })

  output$travel_medical <- renderValueBox({
    valueBox(
      value = formatC(travel_medical, digits = 1, format = "f"),
      subtitle = "Travel Time: Medical",
      icon = icon("fa-solid fa-truck-medical"),
      if (travel_medical >= 8) {
        color <- "yellow"
      } else if (travel_medical >= 10) {
        color <- "red"
      } else {
        color <- "green"
      }
    )
  })

  output$response_medical <- renderValueBox({
    valueBox(
      value = formatC(response_medical, digits = 1, format = "f"),
      subtitle = "Response Time: Medical",
      icon = icon("fa-solid fa-truck-medical"),
      if (response_medical >= 11) {
        color <- "yellow"
      } else if (response_medical >= 15) {
        color <- "red"
      } else {
        color <- "green"
      }
    )
  })

  output$turnout_other <- renderValueBox({
    valueBox(
      value = formatC(turnout_other, digits = 1, format = "f"),
      subtitle = "Turnout Time: Other",
      icon = icon("fa-solid fa-hand-holding-medical"),
      if (turnout_other >= 1) {
        color <- "yellow"
      } else if (turnout_other >= 2) {
        color <- "red"
      } else {
        color <- "green"
      }
    )
  })

  output$travel_other <- renderValueBox({
    valueBox(
      value = formatC(travel_other, digits = 1, format = "f"),
      subtitle = "Travel Time: Other",
      icon = icon("fa-solid fa-hand-holding-medical"),
      if (travel_other >= 8) {
        color <- "yellow"
      } else if (travel_other >= 10) {
        color <- "red"
      } else {
        color <- "green"
      }
    )
  })

  output$response_other <- renderValueBox({
    valueBox(
      value = formatC(response_other, digits = 1, format = "f"),
      subtitle = "Response Time: Other",
      icon = icon("fa-solid fa-hand-holding-medical"),
      if (response_other >= 11) {
        color <- "yellow"
      } else if (response_other >= 15) {
        color <- "red"
      } else {
        color <- "green"
      }
    )
  })
  
  output$turnout_time7d <- renderValueBox({
    valueBox(
      value = formatC(turnout_7d, digits = 1, format = "f"),
      subtitle = "Turnout Time",
      icon = icon("fa-solid fa-fire"),
      if (response_other >= 11) {
        color <- "yellow"
      } else if (response_other >= 15) {
        color <- "red"
      } else {
        color <- "green"
      }
    )
  })
  
  output$travel_time7d <- renderValueBox({
    valueBox(
      value = formatC(travel_7d, digits = 1, format = "f"),
      subtitle = "Travel Time",
      icon = icon("fa-solid fa-fire"),
      if (response_other >= 11) {
        color <- "yellow"
      } else if (response_other >= 15) {
        color <- "red"
      } else {
        color <- "green"
      }
    )
  })
  
  output$response_time7d <- renderValueBox({
    valueBox(
      value = formatC(response_7d, digits = 1, format = "f"),
      subtitle = "Response Time",
      icon = icon("fa-solid fa-fire"),
      if (response_other >= 11) {
        color <- "yellow"
      } else if (response_other >= 15) {
        color <- "red"
      } else {
        color <- "green"
      }
    )
  })
  
  output$total_incidents7d <- renderValueBox({
    total_incidents <- as.numeric(sqldf("select count(*) as sum from response_times7d"))
    valueBox(
      value = formatC(total_incidents, digits = 0, format = "f"),
      subtitle = "Total Incidents",
      icon = icon("fa-solid fa-fire"),
      color <- "blue"
    )
  })
  
  output$santaclara_incidents7d <- renderValueBox({
    valueBox(
      value = formatC(santaclara_incidents7d, digits = 0, format = "f"),
      subtitle = "Santa Clara Incidents",
      icon = icon("fa-solid fa-fire"),
      color <- "blue"
    )
  })
  
  output$ivins_incidents7d <- renderValueBox({
    valueBox(
      value = formatC(ivins_incidents7d, digits = 0, format = "f"),
      subtitle = "Ivins Incidents",
      icon = icon("fa-solid fa-fire"),
      color <- "blue"
    )
  })  
  
  output$total_incidents <- renderValueBox({
    valueBox(
      value = formatC(total_incidents, digits = 0, format = "f"),
      subtitle = "Total Incidents",
      icon = icon("fa-solid fa-fire"),
      color <- "blue"
    )
  })
  
  output$santaclara_incidents <- renderValueBox({
    valueBox(
      value = formatC(santaclara_incidents, digits = 0, format = "f"),
      subtitle = "Santa Clara Incidents",
      icon = icon("fa-solid fa-fire"),
      color <- "blue"
    )
  })
  
  output$ivins_incidents <- renderValueBox({
    valueBox(
      value = formatC(ivins_incidents, digits = 0, format = "f"),
      subtitle = "Ivins Incidents",
      icon = icon("fa-solid fa-fire"),
      color <- "blue"
    )
  })
  
  output$county_incidents_count <- renderValueBox({
    valueBox(
      value = formatC(county_incidents_count, digits = 0, format = "f"),
      subtitle = "County Incidents",
      icon = icon("fa-solid fa-arrow-up-wide-short"),
      color <- "red"
    )
  })
    
  output$county_incidents_travel_time <- renderValueBox({
    valueBox(
      value = formatC(county_incidents_travel_time, digits = 1, format = "f"),
      subtitle = "Averge County Travel Time",
      icon = icon("fa-solid fa-truck-fast"),
      color <- "red"
    )
  })
      
  output$county_incidents_away_time <- renderValueBox({
    valueBox(
      value = formatC(county_incidents_away_time, digits = 1, format = "f"),
      subtitle = "Averge County Away Time",
      icon = icon("fa-solid fa-clock"),
      color <- "red"
    )
  })
  
  output$fy_fire_inc <- renderValueBox({
    valueBox(
      value = formatC(fy_fire_inc, digits = 0, format = "f"),
      subtitle = "FY Fire Incidents",
      icon = icon("fa-solid fa-fire"),
      color <- "red"
    )
  })
  
  output$fy_med_inc <- renderValueBox({
    valueBox(
      value = formatC(fy_med_inc, digits = 0, format = "f"),
      subtitle = "FY Medical Incidents",
      icon = icon("fa-solid fa-truck-medical"),
      color <- "blue"
    )
  })
  
  output$fy_other_inc <- renderValueBox({
    valueBox(
      value = formatC(fy_other_inc, digits = 0, format = "f"),
      subtitle = "FY Other Incidents",
      icon = icon("fa-solid fa-hand-holding-medical"),
      color <- "orange"
    )
  })
  
  output$fy_santa_clara_inc <- renderValueBox({
    valueBox(
      value = formatC(fy_santa_clara_inc, digits = 0, format = "f"),
      subtitle = "FY Santa Clara Incidents",
      icon = icon("fa-solid fa-fire"),
      color <- "blue"
    )
  })
  
  output$fy_ivins_inc <- renderValueBox({
    valueBox(
      value = formatC(fy_ivins_inc, digits = 0, format = "f"),
      subtitle = "FY Ivins Incidents",
      icon = icon("fa-solid fa-fire"),
      color <- "blue"
    )
  })
  
  output$fy_otherarea_inc <- renderValueBox({
    valueBox(
      value = formatC(fy_otherarea_inc, digits = 0, format = "f"),
      subtitle = "FY Other Area Incidents",
      icon = icon("fa-solid fa-fire"),
      color <- "blue"
    )
  })
  
  output$fy_santa_clara_inc_fire <- renderValueBox({
    valueBox(
      value = formatC(fy_santa_clara_inc_fire, digits = 0, format = "f"),
      subtitle = "FY Santa Clara Fire Incidents",
      icon = icon("fa-solid fa-fire"),
      color <- "red"
    )
  })
  
  output$fy_ivins_inc_fire <- renderValueBox({
    valueBox(
      value = formatC(fy_ivins_inc_fire, digits = 0, format = "f"),
      subtitle = "FY Ivins Fire Incidents",
      icon = icon("fa-solid fa-fire"),
      color <- "red"
    )
  })
  
  output$fy_other_inc_fire <- renderValueBox({
    valueBox(
      value = formatC(fy_other_inc_fire, digits = 0, format = "f"),
      subtitle = "FY Other Area Fire Incidents",
      icon = icon("fa-solid fa-fire"),
      color <- "red"
    )
  })
  
  output$fy_santa_clara_inc_med <- renderValueBox({
    valueBox(
      value = formatC(fy_santa_clara_inc_med, digits = 0, format = "f"),
      subtitle = "FY Santa Clara Medical Incidents",
      icon = icon("fa-solid fa-truck-medical"),
      color <- "blue"
    )
  })
  
  output$fy_ivins_inc_med <- renderValueBox({
    valueBox(
      value = formatC(fy_ivins_inc_med, digits = 0, format = "f"),
      subtitle = "FY Ivins Medical Incidents",
      icon = icon("fa-solid fa-truck-medical"),
      color <- "blue"
    )
  })
  
  output$fy_other_inc_med <- renderValueBox({
    valueBox(
      value = formatC(fy_other_inc_med, digits = 0, format = "f"),
      subtitle = "FY Other Area Medical Incidents",
      icon = icon("fa-solid fa-truck-medical"),
      color <- "blue"
    )
  })
  
  output$fy_santa_clara_inc_other <- renderValueBox({
    valueBox(
      value = formatC(fy_santa_clara_inc_other, digits = 0, format = "f"),
      subtitle = "FY Santa Clara Other Incidents",
      icon = icon("fa-solid fa-hand-holding-medical"),
      color <- "orange"
    )
  })
  
  output$fy_ivins_inc_other <- renderValueBox({
    valueBox(
      value = formatC(fy_ivins_inc_other, digits = 0, format = "f"),
      subtitle = "FY Ivins Other Incidents",
      icon = icon("fa-solid fa-hand-holding-medical"),
      color <- "orange"
    )
  })
  
  output$fy_other_inc_other <- renderValueBox({
    valueBox(
      value = formatC(fy_other_inc_other, digits = 0, format = "f"),
      subtitle = "FY Other Area Other Incidents",
      icon = icon("fa-solid fa-hand-holding-medical"),
      color <- "orange"
    )
  })

  output$turnout_dist <- renderPlot({
    turnout_time_hist <- hist(as.numeric(ms(response_times6m$turnout_time)) / 60, breaks = 100, plot = F)
    turnout_time_colors <- ifelse(turnout_time_hist$breaks < 1, rgb(0, 0.8, 0, 0.5), ifelse(turnout_time_hist$breaks >= 2, "red", rgb(0.2, 0.2, 0.2, 0.2)))
    plot(turnout_time_hist, col = turnout_time_colors, border = F, main = "", xlab = "", xlim = c(0.1, 5))
  })

  output$travel_dist <- renderPlot({
    travel_time_hist <- hist(as.numeric(ms(response_times6m$travel_time)) / 60, breaks = 100, plot = F)
    travel_time_colors <- ifelse(travel_time_hist$breaks < 8, rgb(0, 0.8, 0, 0.5), ifelse(travel_time_hist$breaks >= 12, "red", rgb(0.2, 0.2, 0.2, 0.2)))
    plot(travel_time_hist, col = travel_time_colors, border = F, main = "", xlab = "", xlim = c(0.1, 20))
  })

  output$response_dist <- renderPlot({
    response_time_hist <- hist(as.numeric(ms(response_times6m$response_time)) / 60, breaks = 100, plot = F)
    response_time_colors <- ifelse(response_time_hist$breaks < 8, rgb(0, 0.8, 0, 0.5), ifelse(response_time_hist$breaks >= 12, "red", rgb(0.2, 0.2, 0.2, 0.2)))
    plot(response_time_hist, col = response_time_colors, border = F, main = "", xlab = "", xlim = c(0.1, 20))
  })

  output$assignment_dist <- renderPlot({
    assignment_time_hist <- hist(as.numeric(ms(response_times6m$assignment_time)) / 60, breaks = 100, plot = F)
    assignment_time_colors <- rgb(0.2, 0.2, 0.2, 0.2)
    plot(assignment_time_hist, col = assignment_time_colors, border = F, main = "", xlab = "", xlim = c(0.1, 20))
  })
  
  output$city_summary <- renderPlot({
    ggplot(response_times6m,aes(x = fct_rev(fct_infreq(city)), y = ..count.., fill = ..count..)) + 
      coord_flip() +
      labs(x = "City/Area", y = "Total Calls", title = "", fill = "Calls") + 
      scale_fill_gradient(low="blue", high="navy") +
      geom_bar(width=0.9, stat = "count")
  })
  
  output$city_summary7d <- renderPlot({
    ggplot(response_times7d,aes(x = fct_rev(fct_infreq(city)), y = ..count.., fill = ..count..)) + 
      coord_flip() +
      labs(x = "City/Area", y = "Total Calls", title = "", fill = "Calls") + 
      scale_fill_gradient(low="blue", high="navy") +
      geom_bar(width=0.9, stat = "count")
  })
  
  output$incident_summary <- renderPlot({
    ggplot(response_times6m,aes(x = fct_rev(fct_infreq(nature)), y = ..count.., fill = ..count..)) + 
      coord_flip() +
      labs(x = "Nature", y = "Total Incidents", title = "", fill = "Total") + 
      scale_fill_gradient(low="blue", high="navy") +
      geom_bar(width=0.9, stat = "count")
  })
  
  output$incident_summary7d <- renderPlot({
    ggplot(response_times7d,aes(x = fct_rev(fct_infreq(nature)), y = ..count.., fill = ..count..)) + 
      coord_flip() +
      labs(x = "Nature", y = "Total Incidents", title = "", fill = "Total") + 
      scale_fill_gradient(low="blue", high="navy") +
      geom_bar(width=0.9, stat = "count")
  })
  
  output$weekday_summary7d <- renderPlot({
    ggplot(response_times7d,aes(x = fct_infreq(week_day), y = ..count.., fill = ..count..)) + 
      labs(x = "Day of Week", y = "Total Incidents", title = "", fill = "Total") + 
      scale_fill_gradient(low="blue", high="navy") +
      geom_bar(width=0.9, stat = "count")
  })
  
  output$county_area_plot <- renderPlot({
    ggplot(county_incidents,aes(x = fct_rev(fct_infreq(city)), y = ..count.., fill = ..count..)) + 
      coord_flip() +
      labs(x = "City/Area", y = "Total Calls", title = "", fill = "Calls") + 
      scale_fill_gradient(low="red", high="darkred") +
      geom_bar(width=0.9, stat = "count")
  })
  
  output$county_nature_plot <- renderPlot({
    ggplot(county_incidents,aes(x = fct_rev(fct_infreq(nature)), y = ..count.., fill = ..count..)) + 
      coord_flip() +
      labs(x = "Nature", y = "Total Incidents", title = "", fill = "Total") + 
      scale_fill_gradient(low="red", high="darkred") +
      geom_bar(width=0.9, stat = "count")
  })
  
  output$fy_incidents_sc_plot <- renderPlot({
    ggplot(fy_incidents_sc,aes(x = fct_rev(fct_infreq(nature)), y = ..count.., fill = ..count..)) + 
      coord_flip() +
      labs(x = "Nature", y = "Total Incidents", title = "", fill = "Total") + 
      scale_fill_gradient(low="blue", high="navy") +
      geom_bar(width=0.9, stat = "count")
  })
  
  output$fy_incidents_iv_plot <- renderPlot({
    ggplot(fy_incidents_iv,aes(x = fct_rev(fct_infreq(nature)), y = ..count.., fill = ..count..)) + 
      coord_flip() +
      labs(x = "Nature", y = "Total Incidents", title = "", fill = "Total") + 
      scale_fill_gradient(low="blue", high="navy") +
      geom_bar(width=0.9, stat = "count")
  })
  
  output$fy_incidents_o_plot <- renderPlot({
    ggplot(fy_incidents_o,aes(x = fct_rev(fct_infreq(nature)), y = ..count.., fill = ..count..)) + 
      coord_flip() +
      labs(x = "Nature", y = "Total Incidents", title = "", fill = "Total") + 
      scale_fill_gradient(low="blue", high="navy") +
      geom_bar(width=0.9, stat = "count")
  })
  
  output$fire_calls_boxplot <- renderPlot({
    boxplot(incident_ts[,3]~cycle(incident_ts),
            xlab="Month Number",
            ylab="Call Volume",
            col="red",
            border="darkred"
    )
  })
  
  output$medical_calls_boxplot <- renderPlot({
    boxplot(incident_ts[,4]~cycle(incident_ts),
            xlab="Month Number",
            ylab="Call Volume",
            col="blue",
            border="navy"
    )
  })
  
  output$other_calls_boxplot <- renderPlot({
    boxplot(incident_ts[,5]~cycle(incident_ts),
            xlab="Month Number",
            ylab="Call Volume",
            col="orange",
            border="darkorange"
    )
  })
  
  output$fire_decomp_plot <- renderPlot({
    plot(incident_d_fire,
         xlab = NULL,
         ylab = NULL,
         col="red")
  })
  
  output$medical_decomp_plot <- renderPlot({
    plot(incident_d_medical,
         xlab = NULL,
         ylab = NULL,
         col="blue")
  })
  
  output$other_decomp_plot <- renderPlot({
    plot(incident_d_other,
         xlab = NULL,
         ylab = NULL,
         col="orange")
  })
  
  output$units_table <- renderDataTable(units)
  output$county_incident_table <- renderDataTable(county_incident_table)
  output$incident_table <- renderDataTable(incident_table)
  output$fy_incidents_tbl <- renderDataTable(fy_incidents_tbl)
})