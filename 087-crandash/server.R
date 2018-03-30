function(input, output, session) {


  # Shoaib: validate credentials and display dashboard
  observeEvent(input$ok, {
    u <- input$username
    p <- input$password

    if(tolower(u) %in% golablUserID & tolower(p) %in% globalPassword){

      # Shoaib: You can read the session details here and write them to a database to track usage
      # Make sure you wrap this in a try/catch to add fault-tolerance to the code

      # sessionStartTime <<- as.character(Sys.time())
      # mydb <- dbConnect(MySQL(), user='test', password='test', dbname='test', host='test')
      # queryInsert <- paste0("insert into testSchema.testTable values('App_Name','",u,"','",sessionStartTime,"','')")
      # dbGetQuery(mydb, queryInsert)
      # dbDisconnect(mydb)


      # display the dashboard body and side bar if credentials match the entries in global.R file
      obs1$suspend()
      removeModal()
      shinyjs::show(id = "dashbody")
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")

    }else{
      # Throw an error
      showNotification("ERROR",duration = 10,type="error",action="Please enter a valid User ID and Password")
      showNotification("HELP",duration = 10,type="message",action="Please email the owner to receive valid credentials")
    }
  })



  # Shoaib: This function returns the UI for the login modal
  loginModal <- function(failed = FALSE) {
    modalDialog(
      textInput("username", "User:"),
      passwordInput("password", "Password:"),
      footer = tagList(
        actionButton("ok", "OK")
      )
    )
  }

  # Shoaib: This function hides the sidebar menu and dashboard body when session is created
  obs1 <- observe({
    shinyjs::hide(id = "sbm")
    shinyjs::hide(id = "dashbody")
    showModal(loginModal())
  })










  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  pkgStream <- packageStream(session)

  # Max age of data (5 minutes)
  maxAgeSecs <- 60 * 5

  # pkgData is a reactive expression that accumulates previous
  # values of pkgStream, discarding any that are older than
  # maxAgeSecs.
  pkgData <- packageData(pkgStream, maxAgeSecs)

  # dlCount is a reactive expression that keeps track of the total
  # number of rows that have ever appeared through pkgStream.
  dlCount <- downloadCount(pkgStream)

  # usrCount is a reactive expression that keeps an approximate
  # count of all of the unique users that have been seen since the
  # app started.
  usrCount <- userCount(pkgStream)

  # Record the time that the session started.
  startTime <- as.numeric(Sys.time())

  output$rate <- renderValueBox({
    # The downloadRate is the number of rows in pkgData since
    # either startTime or maxAgeSecs ago, whichever is later.
    elapsed <- as.numeric(Sys.time()) - startTime
    downloadRate <- nrow(pkgData()) / min(maxAgeSecs, elapsed)

    valueBox(
      value = formatC(downloadRate, digits = 1, format = "f"),
      subtitle = "Downloads per sec (last 5 min)",
      icon = icon("area-chart"),
      color = if (downloadRate >= input$rateThreshold) "yellow" else "aqua"
    )
  })

  output$count <- renderValueBox({
    valueBox(
      value = dlCount(),
      subtitle = "Total downloads",
      icon = icon("download")
    )
  })

  output$users <- renderValueBox({
    valueBox(
      usrCount(),
      "Unique users",
      icon = icon("users")
    )
  })

  output$packagePlot <- renderBubbles({
    if (nrow(pkgData()) == 0)
      return()

    order <- unique(pkgData()$package)
    df <- pkgData() %>%
      group_by(package) %>%
      tally() %>%
      arrange(desc(n), tolower(package)) %>%
      # Just show the top 60, otherwise it gets hard to see
      head(60)

    bubbles(df$n, df$package, key = df$package)
  })

  output$packageTable <- renderTable({
    pkgData() %>%
      group_by(package) %>%
      tally() %>%
      arrange(desc(n), tolower(package)) %>%
      mutate(percentage = n / nrow(pkgData()) * 100) %>%
      select("Package name" = package, "% of downloads" = percentage) %>%
      as.data.frame() %>%
      head(15)
  }, digits = 1)

  output$downloadCsv <- downloadHandler(
    filename = "cranlog.csv",
    content = function(file) {
      write.csv(pkgData(), file)
    },
    contentType = "text/csv"
  )

  output$rawtable <- renderPrint({
    orig <- options(width = 1000)
    print(tail(pkgData(), input$maxrows), row.names = FALSE)
    options(orig)
  })
}


