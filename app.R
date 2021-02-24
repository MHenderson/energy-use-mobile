library(mobileCharts)
library(shiny)
library(shinyMobile)
library(simputation)
library(tidyverse)

dev_mode <- FALSE

if(dev_mode) {

  energy_2019 <- readxl::read_xlsx("energy.xlsx", sheet = "2019")
  energy_2020 <- readxl::read_xlsx("energy.xlsx", sheet = "2020")
  energy_2021 <- readxl::read_xlsx("energy.xlsx", sheet = "2021")

} else {

  googlesheets4::gs4_deauth()

  # should be a secret?
  sheet_id <- "15nKk44UVxxex7OrhdV3bZRTD0NsniclZmfwtqL0ls18"

  energy_2019 <- sheet_id %>%
    googlesheets4::range_read("2019")

  energy_2020 <- sheet_id %>%
    googlesheets4::range_read("2020")

  energy_2021 <- sheet_id %>%
    googlesheets4::range_read("2021")

}

energy <- bind_rows(energy_2019, energy_2020, energy_2021)

tidy_energy <- prep_tidy_energy(energy)

monthly_bills <- tidy_energy %>%
  filter(var == "cost") %>%
  group_by(fuel, year, month) %>%
  summarise(
    value = sum(value)
  ) %>%
  ungroup() %>%
  mutate(ymd = lubridate::ymd(paste(year, month, 1, sep = "-")))

annual_bill <- tidy_energy %>%
  filter(var == "cost") %>%
  group_by(fuel, year) %>%
  summarise(
    value = sum(value)
  ) %>%
  ungroup()

ui = f7Page(
  title = "My Energy Use",
  dark_mode = FALSE,
  #init = f7Init(skin = "ios", theme = "light"),
  allowPWA = TRUE,
  f7TabLayout(
    navbar = f7Navbar(
      title = "My Energy Use",
      hairline = TRUE,
      shadow = TRUE
    ),
    f7Tabs(
      animated = TRUE,
      #swipeable = TRUE,
      f7Tab(
        tabName = "Gas",
        icon = f7Icon("flame"),
        active = TRUE,
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Gas cost yesterday",
            textOutput("gas_cost_yesterday")
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
           title = "Gas usage yesterday",
           textOutput("gas_yesterday")
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Daily use",
            mobileOutput("gas_kwHPlot"),
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Total cost",
            mobileOutput("gas_totalPlot"),
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Monthly bill",
            mobileOutput("gas_monthlyPlot"),
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Annual bill",
            mobileOutput("gas_annualPlot"),
          )
        )
      ),
      f7Tab(
        tabName = "Electricity",
        icon = f7Icon("lightbulb"),
        active = FALSE,
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Electricity cost yesterday",
            textOutput("electricity_cost_yesterday")
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Electricity usage yesterday",
            textOutput("electricity_yesterday")
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Daily use",
            mobileOutput("electricity_kwHPlot"),
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Total cost",
            mobileOutput("electricity_totalPlot"),
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Monthly bill",
            mobileOutput("electricity_monthlyPlot"),
          )
        ),
        f7Shadow(
          intensity = 16,
          hover = TRUE,
          f7Card(
            title = "Annual bill",
            mobileOutput("electricity_annualPlot"),
          )
        )
      )
    )
  )
)

server <- function(input, output) {

  output$electricity_yesterday <- renderText({
    tidy_energy %>%
      filter(fuel == "electricity", var == "kwh") %>%
      tail(1) %>%
      pull(value) %>%
      paste("kwH")
  })

  output$electricity_cost_yesterday <- renderText({
    tidy_energy %>%
      filter(fuel == "electricity", var == "cost") %>%
      tail(1) %>%
      pull(value) %>%
      `/`(100) %>%
      round(2) %>%
      paste("GBP")
  })

  output$gas_yesterday <- renderText({
    tidy_energy %>%
      filter(fuel == "gas", var == "kwh") %>%
      tail(1) %>%
      pull(value) %>%
      paste("kwH")
  })

  output$gas_cost_yesterday <- renderText({
    tidy_energy %>%
      filter(fuel == "gas", var == "cost") %>%
      tail(1) %>%
      pull(value) %>%
      `/`(100) %>%
      round(2) %>%
      paste("GBP")
  })

  output$gas_kwHPlot <- render_mobile({
    tidy_energy %>%
      filter(fuel == "gas", var == "kwh") %>%
      mobile(aes(x = date, y = value, colour = supplier)) %>%
        mobile_point() %>%
        mobile_scale_x(type = "timeCat", tickCount = 5) %>%
        mobile_scale_y(type = "linear")
  })

  output$gas_totalPlot <- render_mobile({
    tidy_energy %>%
      filter(fuel == "gas", var == "total") %>%
        mobile(aes(x = date, y = value, colour = supplier)) %>%
          mobile_line(alpha = .5) %>%
          mobile_area() %>%
          mobile_scale_x(type = "timeCat", tickCount = 5) %>%
          mobile_scale_y(type = "linear")
    })

  output$electricity_kwHPlot <- render_mobile({
    tidy_energy %>%
      filter(fuel == "electricity", var == "kwh") %>%
      mobile(aes(x = date, y = value, colour = supplier)) %>%
        mobile_point() %>%
        mobile_scale_x(type = "timeCat", tickCount = 5) %>%
        mobile_scale_y(type = "linear")
  })

  output$electricity_totalPlot <- render_mobile({
    tidy_energy %>%
      filter(fuel == "electricity", var == "total") %>%
      mobile(aes(x = date, y = value, colour = supplier)) %>%
        mobile_line(alpha = .5) %>%
        mobile_area() %>%
        mobile_scale_x(type = "timeCat", tickCount = 5) %>%
        mobile_scale_y(type = "linear")
  })

  output$electricity_monthlyPlot <- render_mobile({
    monthly_bills %>%
      filter(fuel == "electricity") %>%
      mobile(aes(x = ymd, y = value)) %>%
        mobile_line(alpha = .5) %>%
        mobile_area() %>%
        mobile_point() %>%
        mobile_tooltip(snap = TRUE) %>%
        mobile_scale_x(type = "timeCat", tickCount = 5) %>%
        mobile_scale_y(type = "linear")
  })

  output$gas_monthlyPlot <- render_mobile({
    monthly_bills %>%
      filter(fuel == "gas") %>%
      mobile(aes(x = ymd, y = value)) %>%
        mobile_line(alpha = .5) %>%
        mobile_area() %>%
        mobile_point() %>%
        mobile_tooltip(snap = TRUE) %>%
        mobile_scale_x(type = "timeCat", tickCount = 5) %>%
        mobile_scale_y(type = "linear")
  })

  output$electricity_annualPlot <- render_mobile({
    annual_bill %>%
      filter(fuel == "electricity") %>%
      mobile(aes(x = year, y = value)) %>%
        mobile_line(alpha = .5) %>%
        mobile_area() %>%
        mobile_point() %>%
        mobile_tooltip(snap = TRUE) %>%
        mobile_scale_y(type = "linear")
  })

  output$gas_annualPlot <- render_mobile({
    annual_bill %>%
      filter(fuel == "gas") %>%
      mobile(aes(x = year, y = value)) %>%
        mobile_line(alpha = .5) %>%
        mobile_area() %>%
        mobile_point() %>%
        mobile_tooltip(snap = TRUE) %>%
        mobile_scale_y(type = "linear")
  })

}

shinyApp(ui = ui, server = server)