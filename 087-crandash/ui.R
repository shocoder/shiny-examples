# Shoaib: JS to set the timeout from client side
timeoutJS <- "
shinyjs.init = function() {
var timeoutWarningMsecs = 60000 * 1000;
var idleTimer;
function onTimeout() {
location.reload(true)
}
function startIdleTimer() {
if (idleTimer) clearTimeout(idleTimer);
idleTimer = setTimeout(onTimeout, timeoutWarningMsecs);
}
$(document).on('shiny:message shiny:inputchanged', startIdleTimer);
}"

# Shoaib: Add FullPage variable
FullPage <- dashboardPage(
  dashboardHeader(title = "cran.rstudio.com"),
  dashboardSidebar(

    # Shoaib: Collapse the Sidebar on pageload
    collapsed=TRUE,

    sliderInput("rateThreshold", "Warn when rate exceeds",
      min = 0, max = 50, value = 3, step = 0.1
    ),
    sidebarMenu(

      # Shoaib: Add an id to  sidebar menu
      id = "sbm",

      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Raw data", tabName = "rawdata")
    )
  ),
  dashboardBody(

    # Shoaib: Add an id to dashboardBody, use shinyjs and call the timeout function
    id = 'dashbody',
    useShinyjs(),
    extendShinyjs(text = timeoutJS),

    tabItems(
      tabItem("dashboard",
        fluidRow(
          valueBoxOutput("rate"),
          valueBoxOutput("count"),
          valueBoxOutput("users")
        ),
        fluidRow(
          box(
            width = 8, status = "info", solidHeader = TRUE,
            title = "Popularity by package (last 5 min)",
            bubblesOutput("packagePlot", width = "100%", height = 600)
          ),
          box(
            width = 4, status = "info",
            title = "Top packages (last 5 min)",
            tableOutput("packageTable")
          )
        )
      ),
      tabItem("rawdata",
        numericInput("maxrows", "Rows to show", 25),
        verbatimTextOutput("rawtable"),
        downloadButton("downloadCsv", "Download as CSV")
      )
    )
  )
)

