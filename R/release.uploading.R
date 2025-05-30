#' @title upload_releases
#' @import dplyr RSQLite DBI shiny DT svDialogs readxl
#' @description batch uploads tag release data
#' @export

upload_releases <- function(db = NULL, overwrite.tags = F,
                            oracle.user =if(exists("oracle.lobtag.user", inherits = T)) oracle.lobtag.user else NULL,
                            oracle.password = if(exists("oracle.lobtag.password", inherits = T)) oracle.lobtag.password else NULL,
                            oracle.dbname = if(exists("oracle.lobtag.server", inherits = T)) oracle.lobtag.server else NULL) {

  if(is.null(db)){return(base::message("You need to specify a database with db = "))}

  if(db %in% c("local","Local","LOCAL")){
    db = "local"
  }

  ## only install / load ROracle if the user chooses Oracle functionality
  if(db %in% "Oracle"){
  pkg <- "ROracle"
  if (!requireNamespace(pkg, quietly = TRUE)) {
    # If not installed, install the package
    install.packages(pkg)

    # Load the package after installing
    library(pkg, character.only = TRUE)
  } else {
    # If already installed, just load the package
    library(pkg, character.only = TRUE)
    }
  }
  #######################################################

  ##################################################################################################
  ##################################################################################################
  # Check if releases table already exists and create if not

  db_connection(db, oracle.user, oracle.password, oracle.dbname)

  table_name <- "LBT_RELEASES"

  ## look for existing table
  if(db %in% "Oracle"){
  query <- paste("SELECT COUNT(*) FROM user_tables WHERE table_name = '", table_name, "'", sep = "")
  }else{if(db %in% "local")query <- paste("SELECT COUNT(*) AS table_count FROM sqlite_master WHERE type='table' AND name='", table_name, "'", sep = "")}

  result <- dbGetQuery(con, query)

  # If the table does not exist, create it
  if (result[[1]] == 0) {
    print(paste0("Creating new table called: ",ifelse(db %in% "Oracle",paste0(oracle.user,"."),""),table_name))
    # Define the SQL statement to create the table
    sql_statement <- paste0("
    CREATE TABLE ",table_name," (
    SAMPLER VARCHAR2(100),
    SAMPLER_2 VARCHAR2(100),
    AFFILIATION VARCHAR2(100),
    VESSEL VARCHAR2(100),
    CAPTAIN VARCHAR2(100),
    PORT VARCHAR2(100),
    MANAGEMENT_AREA VARCHAR2(50),
    DAY VARCHAR2(50),
    MONTH VARCHAR2(50),
    YEAR VARCHAR2(50),
    TAG_COLOR VARCHAR2(50),
    TAG_PREFIX VARCHAR2(50),
    TAG_NUM VARCHAR2(50),
    TAG_ID VARCHAR2(50),
    CARAPACE_LENGTH VARCHAR2(50),
    SEX VARCHAR2(10),
    SHELL VARCHAR2(10),
    CLAW VARCHAR2(10),
    LAT_DEGREES VARCHAR2(50),
    LAT_MINUTES VARCHAR2(50),
    LON_DEGREES VARCHAR2(50),
    LON_MINUTES VARCHAR2(50),
    LATDDMM_MM VARCHAR2(20),
    LONDDMM_MM VARCHAR2(20),
    LAT_DD VARCHAR2(50),
    LON_DD VARCHAR2(50),
    REL_DATE VARCHAR2(50),
    COMMENTS VARCHAR2(1000)
)")

    # Execute the SQL statement
    dbSendQuery(con, sql_statement)

  }
  # Close the connection
  dbDisconnect(con)

####################################################################################################
###################################################################### MAIN FUNCTION:
### function for handling special characters
  escape_special_chars <- function(x) {
    if (is.character(x)) {
      # Escape single quotes (') and dashes (-) for Oracle
      x <- gsub("'", "''", x)
      x <- gsub("-", "\\-", x)
    }
    return(x)
  }

## Allow user to choose data file to upload
dlg_message("In the following window, choose an xlsx file containing your releases data")
file_path <- dlg_open(filter = dlg_filters["xls",])$res
#releases <- read.csv(file_path, na.strings = "")
releases <- read_xlsx(file_path, na = c("","NA"))
## Process / standardize the data table

##ccordinate decimal degrees and degrees minutes formatting done here
## account for negative degrees
releases$LAT_DEGREES = as.numeric(releases$LAT_DEGREES)
releases$LAT_MINUTES = as.numeric(releases$LAT_MINUTES)
releases$LON_DEGREES = as.numeric(releases$LON_DEGREES)
releases$LON_MINUTES = as.numeric(releases$LON_MINUTES)

releases$LATDDMM_MM = NA
releases$LAT_DD = NA
for(i in 1:nrow(releases)){
  if(!is.na(releases$LAT_DEGREES[i]) & !is.na(releases$LAT_MINUTES[i]) & is.numeric(releases$LAT_DEGREES[i]) &
     is.numeric(releases$LAT_MINUTES[i])){
    if(releases$LAT_DEGREES[i]<0){
      releases$LATDDMM_MM[i] = releases$LAT_DEGREES[i] * 100 - releases$LAT_MINUTES[i]
      releases$LAT_DD[i] = releases$LAT_DEGREES[i] - releases$LAT_MINUTES[i] / 60
    }else{
      releases$LATDDMM_MM[i] = releases$LAT_DEGREES[i] * 100 + releases$LAT_MINUTES[i]
      releases$LAT_DD[i] = releases$LAT_DEGREES[i] + releases$LAT_MINUTES[i] / 60
    }
  }
}

releases$LONDDMM_MM = NA
releases$LON_DD = NA
for(i in 1:nrow(releases)){
  if(!is.na(releases$LON_DEGREES[i]) & !is.na(releases$LON_MINUTES[i]) & is.numeric(releases$LON_DEGREES[i]) &
     is.numeric(releases$LON_MINUTES[i])){
    if(releases$LON_DEGREES[i]<0){
      releases$LONDDMM_MM[i] = releases$LON_DEGREES[i] * 100 - releases$LON_MINUTES[i]
      releases$LON_DD[i] = releases$LON_DEGREES[i] - releases$LON_MINUTES[i] / 60
    }else{
      releases$LONDDMM_MM[i] = releases$LON_DEGREES[i] * 100 + releases$LON_MINUTES[i]
      releases$LON_DD[i] = releases$LON_DEGREES[i] + releases$LON_MINUTES[i] / 60
    }
    }
}

##date column isn't 100% necessary but it's a good indication if things are going wrong
releases$REL_DATE = paste(releases$DAY, releases$MONTH, releases$YEAR, sep = "/")
releases$REL_DATE = format(as.Date(releases$REL_DATE, format = "%d/%m/%Y"), "%Y-%m-%d")
releases$TAG_ID = paste0(releases$TAG_PREFIX,releases$TAG_NUM)

## retrieve only selected variables if there are extra / differently ordered columns
select.names = c("SAMPLER"	,"SAMPLER_2",	"AFFILIATION","VESSEL",	"CAPTAIN","PORT",	"MANAGEMENT_AREA",	"DAY",	"MONTH",	"YEAR",	"TAG_COLOR",	"TAG_PREFIX",	"TAG_NUM", "TAG_ID", "CARAPACE_LENGTH",	"SEX",	"SHELL",	"CLAW",	"LAT_DEGREES",	"LAT_MINUTES",	"LON_DEGREES",	"LON_MINUTES", "LATDDMM_MM","LONDDMM_MM","LAT_DD","LON_DD","REL_DATE","COMMENTS")

rel <- dplyr::select(releases,(all_of(select.names)))
## clean variables for problematic characters
# rel$VESSEL = gsub("'","",rel$VESSEL)
# rel$PORT = gsub("'","",rel$PORT)
#\ rel$PORT = gsub("\\(","-",rel$PORT)
# rel$PORT = gsub("\\)","-",rel$PORT)

## error checking (Lobster specific, edit for other species / tagging programs):
# sex_values <- c(NA,0,1,2,3)
# shell_values <- c(NA, 1:7)
# claw_values <- c(NA,1,2,3)
# vnotch_values <- c(NA,"YES","NO")
# carapace_values <- c(NA, 40:150) #this one will alert the user but continue with upload.
# carapace_values_fsrs <- c(NA, 40:170) # fsrs samples some really big females as part of the v-notch program, so increase the threshold in this case.

########### Testing
# rel[1,26]= NA
#  rel[2,12]=NA
#  rel[3,12]=NA
#  rel[4,12]=NA
#  rel[5,13]=NA
# rel <- rbind(rel,rel[1,])
# rel <- rbind(rel,rel[1,])
# rel[6,27]=NA
# rel[7,26]=NA


## error checking (General):

bad_tag_pre = which(rel$TAG_PREFIX %in% NA)
bad_tag_num = which(rel$TAG_NUM %in% NA | as.numeric(rel$TAG_NUM) %in% NA)
repeat_tags = which(duplicated(rel$TAG_ID)==TRUE)

bad_lat = which(rel$LAT_DD %in% NA | nchar(as.character(rel$LAT_DD))<2 | !is.numeric(rel$LAT_DD))
bad_lon = which(rel$LON_DD %in% NA | nchar(as.character(rel$LON_DD))<2 | !is.numeric(rel$LON_DD))
sus_lon = which(rel$LON_DD>0) ## suspect longitudes not in the Western hemisphere
neg_lat.min = which(!is.na(rel$LAT_MINUTES) & rel$LAT_MINUTES<0)
neg_lon.min = which(!is.na(rel$LON_MINUTES) & rel$LON_MINUTES<0)
bad_date = which(rel$REL_DATE %in% NA)


error_out= ""
error_tab = NULL
warning_out= ""
warning_tab = NULL
return_error = FALSE
return_warning = FALSE
################################# Errors
if(length(bad_tag_pre) > 0){
  for(i in bad_tag_pre){
    error_out = paste(error_out, "\nMissing tag prefix for tag number:",rel$TAG_NUM[i],"at row:",i)
    error_tab = rbind(error_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Missing tag prefix"))
  }
  return_error = TRUE
}
if(length(bad_tag_num) > 0){
  for(i in bad_tag_num){
    error_out = paste(error_out, "\nBad or missing tag number at row:", i)
    error_tab = rbind(error_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Bad or missing tag number"))
  }
  return_error = TRUE
}
if(length(repeat_tags) > 0){
  for(i in repeat_tags){
    error_out = paste(error_out, "\nDuplicate tag:",rel$TAG_ID[i],"at row:",i)
    error_tab = rbind(error_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Duplicate tag prefix and number"))
  }
  return_error = TRUE
}
if(length(bad_lat) > 0){
  for(i in bad_lat){
    error_out = paste(error_out, "\nBad or missing latitude for tag:",rel$TAG_ID[i],"at row:",i)
    error_tab = rbind(error_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Bad or missing latitude"))
  }
  return_error = TRUE
}
if(length(bad_lon) > 0){
  for(i in bad_lon){
    error_out = paste(error_out, "\nBad or missing longitude for tag:",rel$TAG_ID[i],"at row:",i)
    error_tab = rbind(error_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Bad or missing longitude"))
  }
  return_error = TRUE
}
if(length(bad_date) > 0){
  for(i in bad_date){
    error_out = paste(error_out, "\nBad or missing date for tag:", rel$TAG_ID[i],"at row:",i)
    error_tab = rbind(error_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Bad or missing date"))
  }
  return_error = TRUE
}
if(length(neg_lat.min) > 0){
  for(i in neg_lat.min){
    error_out = paste(error_out, "\nNegative Latitude Minutes found for tag:", rel$TAG_ID[i],"at row:",i,"These should never be negative.")
    error_tab = rbind(error_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Negative Latitude Minutes"))
  }
  return_error = TRUE
}
if(length(neg_lon.min) > 0){
  for(i in neg_lon.min){
    error_out = paste(error_out, "\nNegative Longitude Minutes found for tag:", rel$TAG_ID[i],"at row:",i,"These should never be negative.")
    error_tab = rbind(error_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Negative Longitude Minutes"))
  }
  return_error = TRUE
}
############################## Warnings
if(length(sus_lon)>0){
  for(i in sus_lon){
    warning_out = paste(warning_out, "\nSuspicious positive longitudes (should be negative for Western hemisphere) for tags:", rel$TAG_ID[i],"at row:",i)
    warning_tab = rbind(warning_tab,c(i,rel$TAG_PREFIX[i],rel$TAG_NUM[i],"Suspicious positive longitude"))
  }
  return_warning = TRUE
}

if(return_error){
  colnames(error_tab)=c("Row","Tag Prefix","Tag Number","Error")
## Create interactive dialogue showing uploading errors and giving user option to download these in a table
  # Define the UI
  ui <- fluidPage(
    titlePanel("Uploading Errors"),
    mainPanel(
      # Display text from the string variable
      h3("Fix issues below and try uploading again:"),
      verbatimTextOutput("text_output"),

      # Button to download the table
      downloadButton("download_table", label = "Download Error Table")
    )
  )

  # Define the server logic
  server <- function(input, output) {
    # Display the text from the string variable
    output$text_output <- renderText({
      error_out
    })

    # Function to generate a downloadable file
    output$download_table <- downloadHandler(
      filename = function() {
        "releases_uploading_errors.csv"
      },
      content = function(file) {
        write.csv(error_tab, file,row.names = F)
      }
    )
  }

  # Create the Shiny app object
  return(shinyApp(ui = ui, server = server))
}


if(!return_error & return_warning){
    colnames(warning_tab)=c("Row","Tag Prefix","Tag Number","Warning")
    ## Create interactive dialogue showing uploading errors and giving user option to download these in a table
    # Define the UI
    ui <- fluidPage(
      titlePanel("Uploading Warnings"),
      mainPanel(
        uiOutput("dynamicUI")

        )
      )


    # Define the server logic
    server <- function(input, output) {
      # Initial content in the main panel
      output$dynamicUI <- renderUI({
        h3("Check issues below before proceeding with upload:")
      })
      output$dynamicUI <- renderUI({
        h4(verbatimTextOutput("text_output"),
          # Button to download the table
          downloadButton("download_table", label = "Download Warning Table"),
          # Button to proceed with upload
          actionButton(inputId = "upload_table", label = "Ignore Warnings and Upload Data")
        )
      })
      # Display the text from the string variable
      output$text_output <- renderText({
        warning_out
      })

      # Function to generate a downloadable file
      output$download_table <- downloadHandler(
        filename = function() {
          "releases_uploading_warnings.csv"
        },
        content = function(file) {
          write.csv(warning_tab, file,row.names = F)
        }
      )

      observeEvent(input$upload_table,{

        ###### db UPLOAD HERE. Check that entry doesn't already exist before uploading

        ### open db connection
       db_connection(db, oracle.user, oracle.password, oracle.dbname)

        ## check for already entered tags, then upload all new tag entries
        entered =NULL
        for(i in 1:nrow(rel)){
          sql <- paste0("SELECT * FROM ",table_name," WHERE TAG_ID= '",rel$TAG_ID[i],"'")
          check <- dbSendQuery(con, sql)
          existing_tag <- dbFetch(check)
          entered <- rbind(entered,existing_tag)
          dbClearResult(check)

          if(nrow(existing_tag)==0){

            rel$SAMPLER[i] = escape_special_chars(rel$SAMPLER[i])
            rel$SAMPLER_2[i] = escape_special_chars(rel$SAMPLER_2[i])
            rel$AFFILIATION[i] = escape_special_chars(rel$AFFILIATION[i])
            rel$VESSEL[i] = escape_special_chars(rel$VESSEL[i])
            rel$CAPTAIN[i] = escape_special_chars(rel$CAPTAIN[i])
            rel$PORT[i] = escape_special_chars(rel$PORT[i])
            rel$MANAGEMENT_AREA[i] = escape_special_chars(rel$MANAGEMENT_AREA[i])
            rel$COMMENTS[i] = escape_special_chars(rel$COMMENTS[i])
            sql <- paste("INSERT INTO ",table_name, " VALUES ('",rel$SAMPLER[i],"', '",rel$SAMPLER_2[i],"', '",rel$AFFILIATION[i],"', '",rel$VESSEL[i],"','",rel$CAPTAIN[i],"','",rel$PORT[i],"','",rel$MANAGEMENT_AREA[i],"','",rel$DAY[i],"','",rel$MONTH[i],"','",rel$YEAR[i],"','",rel$TAG_COLOR[i],"','",rel$TAG_PREFIX[i],"','",rel$TAG_NUM[i],"','",rel$TAG_ID[i],"','",rel$CARAPACE_LENGTH[i],"','",rel$SEX[i],"','",rel$SHELL[i],"','",rel$CLAW[i],"','",rel$LAT_DEGREES[i],"','",rel$LAT_MINUTES[i],"','",rel$LON_DEGREES[i],"','",rel$LON_MINUTES[i],"','",rel$LATDDMM_MM[i],"','",rel$LONDDMM_MM[i],"','",rel$LAT_DD[i],"','",rel$LON_DD[i],"','",rel$REL_DATE[i],"','",rel$COMMENTS[i],"')", sep = "")
            if(db %in% "local"){dbBegin(con)}
            result <- dbSendQuery(con, sql)
            dbCommit(con)
            dbClearResult(result)
          }else{
            if(overwrite.tags){
              update_query <- paste("UPDATE ", table_name,
    " SET SAMPLER = '", rel$SAMPLER[i], "',
        SAMPLER_2 = '", rel$SAMPLER_2[i], "',
        AFFILIATION = '", rel$AFFILIATION[i], "',
        VESSEL = '", rel$VESSEL[i], "',
        CAPTAIN = '", rel$CAPTAIN[i], "',
        PORT = '", rel$PORT[i], "',
        MANAGEMENT_AREA = '", rel$MANAGEMENT_AREA[i], "',
        DAY = '", rel$DAY[i], "',
        MONTH = '", rel$MONTH[i], "',
        YEAR = '", rel$YEAR[i], "',
        TAG_COLOR = '", rel$TAG_COLOR[i], "',
        TAG_PREFIX = '", rel$TAG_PREFIX[i], "',
        TAG_NUM = '", rel$TAG_NUM[i], "',
        CARAPACE_LENGTH = '", rel$CARAPACE_LENGTH[i], "',
        SEX = '", rel$SEX[i], "',
        SHELL = '", rel$SHELL[i], "',
        CLAW = '", rel$CLAW[i], "',
        LAT_DEGREES = '", rel$LAT_DEGREES[i], "',
        LAT_MINUTES = '", rel$LAT_MINUTES[i], "',
        LON_DEGREES = '", rel$LON_DEGREES[i], "',
        LON_MINUTES = '", rel$LON_MINUTES[i], "',
        LATDDMM_MM = '", rel$LATDDMM_MM[i], "',
        LONDDMM_MM = '", rel$LONDDMM_MM[i], "',
        LAT_DD = '", rel$LAT_DD[i], "',
        LON_DD = '", rel$LON_DD[i], "',
        REL_DATE = '", rel$REL_DATE[i], "',
        COMMENTS = '", rel$COMMENTS[i], "'
    WHERE TAG_ID = '", rel$TAG_ID[i], "'", sep = "")
      if(db %in% "local"){dbBegin(con)}
      result <- dbSendQuery(con, update_query)
      dbCommit(con)
      dbClearResult(result)
            }
          }

        }

        dbDisconnect(con)

        ### show interactive info window if there were any tags found to be already entered
        if(nrow(entered)>0 & !overwrite.tags){

          # Dynamically render new UI elements
          output$dynamicUI <- renderUI({
            fluidPage(
            tags$br(),
            h3("Upload Success! The following tags already exist in the database so were not uploaded:"),
            sidebarLayout(
              sidebarPanel(
                # Text box to display all TAG_NUM values
                textOutput("tag_values")
              ),

              mainPanel(
                # Display table based on selection
                DTOutput("table"),

                # Download button
                downloadButton("download_table", "Download Table of Existing Tags")
              )
            )
          )
          })

          # New server logic

            # Render unique TAG_NUM values in text box

            output$tag_values <- renderText({
              paste(unique(entered$TAG_ID), collapse = ", ")
            })

            # Render table based on selection
            output$table <- renderDT({
              datatable(entered)
            })

            # Download entire table
            output$download_table <- downloadHandler(
              filename = function() {
                paste("already_existing_tags", ".csv", sep = "")
              },
              content = function(file) {
                write.csv(entered, file, row.names = FALSE)
              }
            )

        }else{
          no.tags.found = " and none of the tags were found to already exist"
          if(overwrite.tags){no.tags.found = ""}
          output$dynamicUI <- renderUI({
            fluidPage(
            h3(pase0("All releases uploaded successfully! There were no errors",no.tags.found,". Close this window."))
            ) })
          }

      })

    }

    # Create the Shiny app object
    return(shinyApp(ui = ui, server = server))
  }


  if(!return_error & !return_warning){

  ###### db UPLOAD HERE. Check that entry doesn't already exist before uploading

  ### open db connection
  db_connection(db, oracle.user, oracle.password, oracle.dbname)

  ## check for already entered tags, then upload all new tag entries
  entered =NULL
  for(i in 1:nrow(rel)){
  sql <- paste0("SELECT * FROM ",table_name," WHERE TAG_ID= '",rel$TAG_ID[i],"'")
  check <- dbSendQuery(con, sql)
  existing_tag <- dbFetch(check)
  entered <- rbind(entered,existing_tag)
  dbClearResult(check)

  if(nrow(existing_tag)==0){

    ## handle any special characters such as apostrophes in names
    rel$SAMPLER[i] = escape_special_chars(rel$SAMPLER[i])
    rel$SAMPLER_2[i] = escape_special_chars(rel$SAMPLER_2[i])
    rel$AFFILIATION[i] = escape_special_chars(rel$AFFILIATION[i])
    rel$VESSEL[i] = escape_special_chars(rel$VESSEL[i])
    rel$CAPTAIN[i] = escape_special_chars(rel$CAPTAIN[i])
    rel$PORT[i] = escape_special_chars(rel$PORT[i])
    rel$MANAGEMENT_AREA[i] = escape_special_chars(rel$MANAGEMENT_AREA[i])
    rel$COMMENTS[i] = escape_special_chars(rel$COMMENTS[i])

    sql <- paste("INSERT INTO ",table_name, " VALUES ('",rel$SAMPLER[i],"', '",rel$SAMPLER_2[i],"', '",rel$AFFILIATION[i],"', '",rel$VESSEL[i],"','",rel$CAPTAIN[i],"','",rel$PORT[i],"','",rel$MANAGEMENT_AREA[i],"','",rel$DAY[i],"','",rel$MONTH[i],"','",rel$YEAR[i],"','",rel$TAG_COLOR[i],"','",rel$TAG_PREFIX[i],"','",rel$TAG_NUM[i],"','",rel$TAG_ID[i],"','",rel$CARAPACE_LENGTH[i],"','",rel$SEX[i],"','",rel$SHELL[i],"','",rel$CLAW[i],"','",rel$LAT_DEGREES[i],"','",rel$LAT_MINUTES[i],"','",rel$LON_DEGREES[i],"','",rel$LON_MINUTES[i],"','",rel$LATDDMM_MM[i],"','",rel$LONDDMM_MM[i],"','",rel$LAT_DD[i],"','",rel$LON_DD[i],"','",rel$REL_DATE[i],"','",rel$COMMENTS[i],"')", sep = "")
    if(db %in% "local"){dbBegin(con)}
    result <- dbSendQuery(con, sql)
    dbCommit(con)
    dbClearResult(result)

  }else{
    if(overwrite.tags){
      update_query <- paste("UPDATE ", table_name,
                            " SET SAMPLER = '", rel$SAMPLER[i], "',
        SAMPLER_2 = '", rel$SAMPLER_2[i], "',
        AFFILIATION = '", rel$AFFILIATION[i], "',
        VESSEL = '", rel$VESSEL[i], "',
        CAPTAIN = '", rel$CAPTAIN[i], "',
        PORT = '", rel$PORT[i], "',
        MANAGEMENT_AREA = '", rel$MANAGEMENT_AREA[i], "',
        DAY = '", rel$DAY[i], "',
        MONTH = '", rel$MONTH[i], "',
        YEAR = '", rel$YEAR[i], "',
        TAG_COLOR = '", rel$TAG_COLOR[i], "',
        TAG_PREFIX = '", rel$TAG_PREFIX[i], "',
        TAG_NUM = '", rel$TAG_NUM[i], "',
        CARAPACE_LENGTH = '", rel$CARAPACE_LENGTH[i], "',
        SEX = '", rel$SEX[i], "',
        SHELL = '", rel$SHELL[i], "',
        CLAW = '", rel$CLAW[i], "',
        LAT_DEGREES = '", rel$LAT_DEGREES[i], "',
        LAT_MINUTES = '", rel$LAT_MINUTES[i], "',
        LON_DEGREES = '", rel$LON_DEGREES[i], "',
        LON_MINUTES = '", rel$LON_MINUTES[i], "',
        LATDDMM_MM = '", rel$LATDDMM_MM[i], "',
        LONDDMM_MM = '", rel$LONDDMM_MM[i], "',
        LAT_DD = '", rel$LAT_DD[i], "',
        LON_DD = '", rel$LON_DD[i], "',
        REL_DATE = '", rel$REL_DATE[i], "',
        COMMENTS = '", rel$COMMENTS[i], "'
    WHERE TAG_ID = '", rel$TAG_ID[i], "'", sep = "")

      if(db %in% "local"){dbBegin(con)}
      result <- dbSendQuery(con, update_query)
      dbCommit(con)
      dbClearResult(result)
    }
  }

}

  dbDisconnect(con)

  ### show interactive info window if there were any tags found to be already entered
 if(nrow(entered)>0 & !overwrite.tags){

  # Define UI for application
  ui <- fluidPage(
    titlePanel("Upload Success!"),
    tags$br(),
    h4("The following tags already exist in the database so were not uploaded:"),
    sidebarLayout(
      sidebarPanel(
        # Text box to display all TAG_NUM values
        textOutput("tag_values")
      ),

      mainPanel(
        # Display table based on selection
        DTOutput("table"),

        # Download button
        downloadButton("download_table", "Download Table of Existing Tags")
      )
    )
  )


  # Define server logic
  server <- function(input, output) {
    # Render unique TAG_NUM values in text box
    output$tag_values <- renderText({
      paste(unique(entered$TAG_ID), collapse = ", ")
    })

    # Render table based on selection
    output$table <- renderDT({
      datatable(entered)
    })

    # Download entire table
    output$download_table <- downloadHandler(
      filename = function() {
        paste("already_existing_tags", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(entered, file, row.names = FALSE)
      }
    )
  }

  # Run the application
  shinyApp(ui = ui, server = server)


 }else{
   no.tags.found = " and none of the tags were found to already exist"
   if(overwrite.tags){no.tags.found = ""}
   dlg_message(paste0("All releases uploaded successfully! There were no errors",no.tags.found,". Close this window."))
  }

  }



#######  SCRAP
# new.names= c("Vessel", "Captain", "Port", "MANAGEMENT_AREA", "Sampler", "Sampler 2", "Affiliation", "Day",	"Month", "Year", "Tag Prefix",	"Tag Color", "Tag Num",	"Carapace Length",	"Sex",	"Shell", "Claw", "Lat Degrees",	"Lat Minutes",	"Lon Degrees",	"Lon Minutes", "latddmm.mm", "londdmm.mm", "latdd.dd", "londd.dd", "Date")

# library(dplyr)
# library(ROracle)
# library(shiny)
# library(DT)
# library(svDialogs)


}

