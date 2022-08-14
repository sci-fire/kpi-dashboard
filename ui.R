#**********************************************************
#* CATEGORY	SHINY
#* GROUP	REPORTING
#* AUTHOR	LANCE HAYNIE <LHAYNIE@SCCITY.ORG>
#* FILE		UI.R
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
library(shinydashboard)
library(shinydashboardPlus)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

header <- dashboardHeader(title = "SCIF KPI Dashboard")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Response Times", tabName = "responsetimes", icon = icon("fa-solid fa-gauge-high")),
    menuItem("Weekly Summary", tabName = "weekly", icon = icon("fa-solid fa-chart-line")),
    menuItem("Incident Stats", tabName = "incidents", icon = icon("fa-solid fa-chart-pie")),
    menuItem("Incident Detail", tabName = "incidentsdetail", icon = icon("fa-solid fa-table-list")),
    menuItem("FY Statistics", tabName = "fystats", icon = icon("fa-solid fa-calendar-plus")),
    menuItem("Forecasting", tabName = "forecast", icon = icon("fa-solid fa-arrow-trend-up")),
    menuItem("County Responses", tabName = "countyresponse", icon = icon("fa-solid fa-road"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "responsetimes",
      h4("Response Time Key Performance Indicators"),
      h6("Data Source: Spillman - Data Period: Rolling Six Months"),
      fluidRow(
        valueBoxOutput("turnout_fire"),
        valueBoxOutput("travel_fire"),
        valueBoxOutput("response_fire"),
        valueBoxOutput("turnout_medical"),
        valueBoxOutput("travel_medical"),
        valueBoxOutput("response_medical"),
        valueBoxOutput("turnout_other"),
        valueBoxOutput("travel_other"),
        valueBoxOutput("response_other")
      ),
      fluidRow(
        box(
          title = "Turnout Time Distribution",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("turnout_dist", height = "300px")
        ),
        box(
          title = "Travel Time Distribution",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("travel_dist", height = "300px")
        ),
        box(
          title = "Response Time Distribution",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("response_dist", height = "300px")
        ),
        box(
          title = "Assignment Time Distribution",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("assignment_dist", height = "300px")
        )
      )
    ),
    tabItem(
      tabName = "weekly",
      h4("Weekly Summary Dashboard"),
      h6("Data Source: Spillman - Data Period: Rolling Seven Days"),
      fluidRow(
        valueBoxOutput("turnout_time7d"),
        valueBoxOutput("travel_time7d"),
        valueBoxOutput("response_time7d"),
        valueBoxOutput("total_incidents7d"),
        valueBoxOutput("santaclara_incidents7d"),
        valueBoxOutput("ivins_incidents7d")
      ),
      fluidRow(
        box(
          title = "Incidents by Weekday",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("weekday_summary7d", height = "300px")
        ),
        box(
          title = "Incidents by City",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("city_summary7d", height = "300px")
        ),
        box(
          title = "Incidents by Nature",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("incident_summary7d", height = "600px")
        )
      )
    ),
    tabItem(
      tabName = "incidents",
      h4("Incident Statistics"),
      h6("Data Source: Spillman - Data Period: Rolling Six Months"),
      fluidRow(
        valueBoxOutput("total_incidents"),
        valueBoxOutput("santaclara_incidents"),
        valueBoxOutput("ivins_incidents"),
      ),
      fluidRow(
        box(
          title = "Incidents by City",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("city_summary", height = "750px")
        )
      ),
      fluidRow(
        box(
          title = "Incidents by Nature",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("incident_summary", height = "750px")
        )
      )
    ),
    tabItem(
      tabName = "incidentsdetail",
      h4("Incident Detail"),
      h6("Data Source: Spillman - Data Period: Rolling 12 Months"),
      fluidRow(
        column(12,
               dataTableOutput('incident_table')
        )
      ),
      h6("Note: Missing timestamps incidates dispatch did not receive that time to log.")
    ),
    tabItem(
      tabName = "fystats",
      h4("Fiscal Year Statistics"),
      h6("Data Source: Spillman - Data Period: Previous FY"),
      fluidRow(
        valueBoxOutput("fy_fire_inc"),
        valueBoxOutput("fy_med_inc"),
        valueBoxOutput("fy_other_inc"),
        valueBoxOutput("fy_santa_clara_inc"),
        valueBoxOutput("fy_ivins_inc"),
        valueBoxOutput("fy_otherarea_inc"),
        valueBoxOutput("fy_santa_clara_inc_fire"),
        valueBoxOutput("fy_ivins_inc_fire"),
        valueBoxOutput("fy_other_inc_fire"),
        valueBoxOutput("fy_santa_clara_inc_med"),
        valueBoxOutput("fy_ivins_inc_med"),
        valueBoxOutput("fy_other_inc_med"),
        valueBoxOutput("fy_santa_clara_inc_other"),
        valueBoxOutput("fy_ivins_inc_other"),
        valueBoxOutput("fy_other_inc_other"),
      ),
      fluidRow(
        box(
          title = "Santa Clara Incidents by Nature",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("fy_incidents_sc_plot", height = "750px")
        )
        ),
      fluidRow(
        box(
          title = "Ivins Incidents by Nature",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("fy_incidents_iv_plot", height = "750px")
        )
      ),
      fluidRow(
        box(
          title = "Other Area Incidents by Nature",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("fy_incidents_o_plot", height = "750px")
        )
      ),
      fluidRow(
        box(
          title = "County Incidents",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          column(12,
                 dataTableOutput('fy_incidents_tbl')
          )
        )
      )
      ),
    tabItem(
      tabName = "forecast",
      h4("Call Volume Forecasting"),
      h6("Data Source: Spillman - Data Period: Previous 12 Months"),
      fluidRow(
        box(
          title = "Fire Call Volume",
          width = 4,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("fire_calls_boxplot", height = "400px")
        ),
        box(
          title = "Medical Call Volume",
          width = 4,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("medical_calls_boxplot", height = "400px")
        ),
        box(
          title = "Other Call Volume",
          width = 4,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("other_calls_boxplot", height = "400px")
        )
      ),
      fluidRow(
        box(
          title = "Fire Call Volume Time Series Decomposition",
          width = 4,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("fire_decomp_plot", height = "400px")
        ),
        box(
          title = "Medical Call Volume Time Series Decomposition",
          width = 4,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("medical_decomp_plot", height = "400px")
        ),
        box(
          title = "Other Call Volume Time Series Decomposition",
          width = 4,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("other_decomp_plot", height = "400px")
        )
      )
    ),    
    tabItem(
      tabName = "countyresponse",
      h4("County Response Statistics"),
      h6("Data Source: Spillman - Data Period: Rolling 12 Months"),
      fluidRow(
        valueBoxOutput("county_incidents_count"),
        valueBoxOutput("county_incidents_travel_time"),
        valueBoxOutput("county_incidents_away_time")
      ),
      fluidRow(
        box(
          title = "County Incidents by Nature",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("county_nature_plot", height = "500px")
        ),
        box(
          title = "County Incidents by Area",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("county_area_plot", height = "500px")
        )
      ),
      fluidRow(
        box(
          title = "County Incidents",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          column(12,
                 dataTableOutput('county_incident_table')
          )
        )
      )
    )
  )
)

ui <- dashboardPage(
  title = "Santa Clara - Ivins Fire & Rescue",
  header,
  footer = dashboardFooter(left = "Data Refreshed Daily", right = "Santa Clara - Ivins Fire & Rescue"),
  sidebar,
  body,
  skin = "blue"
)
