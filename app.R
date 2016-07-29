#!/usr/bin/Rscript
library(shiny)
library(DT)
library(RPostgreSQL)
library(yaml)
library(pool)


if(dir.exists('/config/conf.yml')){
    location <-'/config/conf.yml'
}else{
    location <-'conf.yml'
}
 configFile <- yaml.load_file(location)
### create the pool of SQL connections so it works for multiple people at a time
poolNames <- dbPool(
    drv = RPostgreSQL::PostgreSQL(),
    dbname = configFile$redshift$'db-name',
    host = configFile$redshift$host,
    user = configFile$redshift$user,
    password = configFile$redshift$password,
    port = configFile$redshift$port
)

ui <- fluidPage(
  titlePanel("Product Selection Tool"),
  fluidRow(
    column(3,
           
           textInput(inputId="pricemin", label="minimum price",value=10),
           textInput(inputId="pricemax", label="maximum price",value=30),
           selectInput(inputId  = "InFamily", label = "Product Family", 
                       choices = c("All","accessories","baby care","experiences","food & drink","gardens & outdoors",
                                   "haberdashery","health & beauty","home","jewellery","pet accessories",
                                   "prints, pictures & art","stationery & parties","toys, games & sports",
                                   "unknown"),selected="All"),
           uiOutput("ui"),
           actionButton("go", "Go") 
    ),
    column(3,           
           textInput(inputId = "keyword", label="search for title keyword",value=""),
           selectInput(inputId="OnSale",label = "include on sale products?",
                       choices = c("doesn\'t matter","Yes","no"),selected = "doesn\'t matter"),
           radioButtons(inputId="personalise",label="Can it be personalised?",
                        choices=c("don\'t care","yes","no"), selected="don\'t care")
    ), 
    column(3,
           radioButtons(inputId = "date", label = "product sales time frame", choices = c("recently published", "sales between dates"),
                        selected = "recently published"), 
           uiOutput("dateui")
           
    ),
    column(3,
           radioButtons(inputId="delivery",label="has free delivery?",
                        choices=c("Everything","yes","no"),selected="Everything"),
           radioButtons(inputId="deliveryExpress",label="has Express delivery?",
                        choices=c("Everything","yes","no"),selected="Everything")
    )
  ),
  textOutput("warning"),
  textOutput("warning2"),
  textOutput("count"),
  dataTableOutput("tab1")
  
) 

server <- function(input, output) { 
  ### render the drop down boxes for group. theses react to the selection from family
  output$ui <- renderUI({
    switch(input$InFamily,
           "All" = selectInput("group","Group",choices=c("All"),selected="All"),
           "accessories" = selectInput("group","Group",choices =c("all","accessory making","bags","clothing accessories",
                                                                  "footwear","headwear","luggage & travel", "personal accessories",
                                                                  "tech accessories"),selected="All"),
           "baby care" = selectInput("group","Group",choices =c("All","bathing & changing","clothing","feeding & nursing",
                                                                "other baby care","sleeping & travel"),
                                     selected="All"),
           "experiences" = selectInput("group","Group",choices =c("experiences"),selected="experiences"),
           "food & drink" = selectInput("group","Group",choices =c("All","alcoholic drinks","cakes & sweet treats",
                                                                   "chocolates & confectionery","hampers & sets",
                                                                   "make your own","sauces, dressings & condiments",
                                                                   "savouries","soft drinks"),selected="All"),
           "gardens & outdoors" = selectInput("group","Group",choices =c("All","cooking & heating","garden accessories",
                                                                         "garden plants & seeds","garden tools & equipment",
                                                                         "gardenwear","outdoor structures",
                                                                         "picnics & camping"),selected="All"),
           "haberdashery" = selectInput("group","Group",choices =c("All","knitting","other haberdashery",
                                                                   "sewing"),selected="All"),
           "health & beauty" = selectInput("group","Group",choices =c("All","bath & shower","beauty accessories","cosmetics",
                                                                      "dental","hair care","make your own","nail care",
                                                                      "shaving","skin care"),selected="All"),
           "home" = selectInput("group","Group",choices =c("All","barware","bath linen","bed linen",
                                                           "blinds & window accessories","cookware & bakeware",
                                                           "cutlery & utensils","decorative accessories","drinkware",
                                                           "fireplace","fresh cut flowers & house plants","furniture",
                                                           "glassware","kitchen accessories & gadgets","laundry",
                                                           "lighting","other homeware","painting & decorating",
                                                           "racks, hooks & rails","serveware","soft furnishings",
                                                           "storage","table linen","tableware",
                                                           "wall accessories"),selected="All"),
           "jewellery" = selectInput("group","Group",choices =c("All","jewellery","jewellery making",
                                                                "jewellery storage","watches"),selected="All"),
           "pet accessories" = selectInput("group","Group",choices =c("All","collars, leads & tags","food & feeding",
                                                                      "grooming & hygiene","other pet accessories",
                                                                      "pet beds, baskets & blankets",
                                                                      "pet clothing","pet play accessories",
                                                                      "pet shelters & travel"),selected="All"),
           "prints, pictures & art" = selectInput("group","Group",choices =c("All","framing",
                                                                             "prints, pictures & art"),selected="All"),
           "stationery & parties" = selectInput("group","Group",choices =c("All","albums, diaries & record books",
                                                                           "card crafting accessories","cards & invitations",
                                                                           "computer accessories","desk accessories",
                                                                           "party accessories","ribbon & wrapping",
                                                                           "seasonal decorations",
                                                                           "writing equipment & accessories"),selected="All"),
           "toys, games & sports" = selectInput("group","Group",choices =c("All","books","creative play & kits",
                                                                           "outdoor games & sports","pretend play",
                                                                           "toys & indoor games"),selected="All"),
           "unknown" = selectInput("group","Group",choices =c("unknown"),selected="unknown")
    )
  })
  
  output$dateui <- renderUI({
    switch(input$date,
           "recently published" = radioButtons(inputId="publishedDate",label="date options",choices=c("last week","last 2 weeks","last month")),
           "sales between dates" = dateRangeInput(inputId = "daterange", label = "sales window",start=Sys.Date()-14, end=Sys.Date(),min='2014-06-01',max=Sys.Date()-1)
    )
  })
  
  ### Create the reactive element. i.e. eberything here will go when the "go" button is selected.
  
  tab2 <- eventReactive(input$go, { 
    isolate(withProgress(message="Searching ...",{
      
      ### for some inputs, create the SQL query part for each selection.
      ### For sale
      if(input$OnSale == "yes"){
        sale <- 'true'
      }else{if(input$OnSale == "no"){
        sale <- 'false'
      }else{sale <- "\'True\',\'FALSE\'"} }
      
      ### select from family
      fam <- c("All",
               "accessories",
               "baby care",
               "clothing",
               "experiences",
               "food & drink",
               "garden & outdoors",
               "haberdashery",
               "health & beauty",
               "home",
               "jewellery",
               "pet accessories",
               "prints, pictures & art",
               "stationery & parties",
               "toys, games & sports",
               "unknown")
      if(input$InFamily == "All"){
        fam_key <- fam[-1]
        fam_keyword <- paste0(paste0("'",fam_key,collapse="\',"),"\'")
      }else{
        fam_key <- input$InFamily
        fam_keyword <- paste0('\'',fam_key,'\'')
      }
      
      ### Select group
      if(input$group == "All"){
        group_keyword <- " IS NOT NULL"
      }else{
        group_key <- input$group
        group_keyword <- paste0('IN (\'',group_key,'\')')
      }
      
      ### Is the item personalisible or not?
      if(input$personalise == "yes"){
        personalised <- " > 0"
      }else{
        if(input$personalise == "no"){
          personalised <- " = 0"
        } else {
          personalised <- " >= 0"}
      }
      
      ### free delivery?
      if(input$delivery == "yes"){
        deliveryOption <- " IN (\'Free\') "
      }else{
        if(input$delivery == "no"){
          deliveryOption <- " NOT IN (\'Free\') "
        }else{
          deliveryOption <- " IS NOT NULL" }
      }        
      
      ### Is there express delivery available?
      if(input$deliveryExpress == "yes"){
        deliver_express <-  " IN (\'TRUE\') "
      }else{ 
        if(input$deliveryExpress == "no"){
          deliver_express <- " IN (\'FALSE\') "
        }else{
          deliver_express <- " IS NOT NULL" }
      }
      
      ### product sales timeframe stuff
      if(input$date == "recently published"){
        if(input$publishedDate == "last week"){
          pubDate <- Sys.Date() - 7
          viewDate1 <- Sys.Date() - 7
          viewDate2 <- Sys.Date()
        }else{
          if(input$publishedDate == "last 2 weeks"){
            pubDate <- Sys.Date() - 14
            viewDate1 <- Sys.Date() - 14
            viewDate2 <- Sys.Date()
            
          }else{ 
            pubDate <- Sys.Date() - 28
            viewDate1 <- Sys.Date() - 28
            viewDate2 <- Sys.Date()
          }
          
        }
        
      }else{
        pubDate <- input$daterange[1]
        viewDate1 <- input$daterange[1]
        viewDate2 <- input$daterange[2]
      }
      
      print("before query 1")
      ### To aviod promoting products where the partner is on vacation, only select those not.
      query_hols <- 'SELECT DISTINCT noths.dimension_partner_.original_id as partner_id
      FROM noths.facts_partner_holidays
      JOIN noths.dimension_partner_
      ON noths.facts_partner_holidays.partner_dimension_id = noths.dimension_partner_.id
      WHERE (START_DATE_DIMENSION_ID <
      (SELECT ID
      FROM noths.dimension_date
      WHERE DATE = CURRENT_DATE)
      AND END_DATE_DIMENSION_ID <
      (SELECT ID
      FROM noths.dimension_date
      WHERE DATE = CURRENT_DATE))
      OR (START_DATE_DIMENSION_ID >
      (SELECT ID + 3
      FROM noths.dimension_date
      WHERE DATE = CURRENT_DATE)
      AND END_DATE_DIMENSION_ID >
      (SELECT ID + 3
      FROM noths.dimension_date
      WHERE DATE = CURRENT_DATE))'
      hols <- dbGetQuery(poolNames,query_hols)    
      notonhols <- paste0(paste0(hols$partner_id,collapse=","))
      
      print("after query 1")
      
      print("before query 2")
      ### Seelct products. Prereqa: Avilable, in stock ot made to order, partner is active.
      ### the rest come from the parameters input with the buttons.
      query_prod <- gsub("\n","",paste0('select product_name, product_code, partner_name,
                                        current_gross_price, gross_price_on_sale, delivery_time, delivery_class,
                                        url, family, current_availability, current_stock_status, partner_state, image_url, \"group\",
                                        has_express_delivery,target_age_range,number_of_options, partner_id,
                                        current_date-date(published_date) as days_live
                                        from product 
                                        WHERE current_gross_price BETWEEN ', input$pricemin, ' AND ', input$pricemax, '
                                        AND published_date >= date(\'' , as.character(pubDate), '\' 
                                        ) AND LOWER(product_name) LIKE (\'%', input$keyword ,'%\')
                                              AND currently_on_sale IN (',sale,')
                                        AND family IN (',fam_keyword,')
                                        AND \"group\" ', group_keyword,'
                                        AND number_of_options', personalised ,'
                                        AND delivery_class ', deliveryOption ,'
                                        AND has_express_delivery ', deliver_express
                                        ) )
      # and partner_id IN (', notonhols ,')'
      products <- dbGetQuery(poolNames,query_prod)    
      
      print("after query 2")
      
      ### If this search returns results, 
      if(dim(products)[1] > 0){
        product_codes <- paste0(paste0(products$product_code,collapse=","))
        
        
        print("before query 3")
        ### the products code from above are used int he next searches to limit the results found
        ### get the page views
        query_views <- gsub("\n","",paste0('select product_code, sum(number_of_views) as page_views 
                                           from noths.product_page_views_by_date  
                                           WHERE date BETWEEN \'', as.character(viewDate1), '\'
                                           AND \'', as.character(viewDate2), '\'
                                           AND product_code IN (',product_codes,')
                                           group by product_code') )
        views <- dbGetQuery(poolNames,query_views)  
        
        print("before query 4")
        ### get the ttv and number of checkouts
        query_trans <- gsub("\n","",paste0('select product_code, sum(ttv) as TTV,
                                           count(distinct checkout_id) as num_checkouts
                                           from transaction_line  
                                           WHERE date BETWEEN \'', as.character(viewDate1), '\'
                                           AND \'', as.character(viewDate2), '\'
                                           AND product_code IN (',product_codes,')
                                           group by product_code') )
        trans <- dbGetQuery(poolNames,query_trans)  
        print("after query 4")
        
        
        ### put the files together into one table. If there are any NAs, replace them with 0. 
        ### (i.e. if there are no page views or checkouts)
        part_file <- data.frame(products, page_views=views[match(products$product_code,views$product_code),-1]) 
        part_file$page_views <- replace(part_file$page_views,is.na(part_file$page_views),0) 
        
        if(dim(trans)[1] == 0){
          num_checkouts <- rep(0,dim(part_file)[1])
          ttv <- rep(0,dim(part_file)[1])
          full_file <- cbind(part_file,ttv,num_checkouts)
        }else{
          full_file <- data.frame(part_file, trans[match(part_file$product_code,trans$product_code),-1])
          full_file$ttv <- replace(full_file$ttv,is.na(full_file$ttv),0)
          full_file$num_checkouts <- replace(full_file$num_checkout,is.na(full_file$num_checkout),0)
          
        }
        
        ### Add in conversion 
        conversion <- ifelse(full_file$page_views < full_file$num_checkouts, 0, full_file$num_checkouts / full_file$page_views)
        conversion <- round(replace(conversion,is.na(conversion),0),2)
        
        ### Put the file together, select only the needed columns
        full_file2 <- cbind(full_file,conversion)
        
        if(input$date == "recently published"){
          ttv_per_day <- full_file2$ttv / full_file2$days_live
        }else{
          ttv_per_day <- full_file2$ttv / as.numeric(difftime(strptime(viewDate2, format="%Y-%m-%d"), strptime(viewDate1, format="%Y-%m-%d"), units="days"))
        }
        ttv_per_dayNorm <- (ttv_per_day - min(ttv_per_day)) / diff(range(ttv_per_day))
        conversionNorm <- (full_file2$conversion - min(full_file2$conversion)) / diff(range(full_file2$conversion))
        full_file2$impact <- conversionNorm*ttv_per_dayNorm
        full_file2 <- full_file2[order(-full_file2$impact),]
        full_file2$impact <- 1:nrow(full_file2)
        
        ### get the images and make the first column
        img <- full_file2[,which(names(full_file2) =="image_url")]
        productUrl <- full_file2[,which(names(full_file2) == "url")]
        image <- paste0("<a href = \"", productUrl, "\"> <img src=\"", img,"\" height=\"150\"></a>")
        newtab <- data.frame(cbind(image, full_file2))
        
        toSelect <- c("impact", "image", "product_name","product_code","partner_name","family","group","current_availability", "current_stock_status", "partner_state", "current_gross_price","gross_price_on_sale",
                      "delivery_time","delivery_class","page_views","ttv","num_checkouts","conversion")
        outputTable <-newtab[,match(toSelect,names(newtab))]
        
        outputTableNames <- c("Relative Impact", "Image", "Product", "Product Code", "Partner","Family","Group","Availability", "Stock Status", "Partner State", "Price","Sale Price",
                              "Delivery Time","Delivery Class","Page Views","TTV","Checkouts","Conversion")
        colnames(outputTable) <- outputTableNames
        
        outputTable
        
      }else{
        ### If there are no results from the search parameters, return a wanring to try different parameters 
        stop("There are no results for this query. Please try different search parameters.")}
    }))
  })  
  
  ### Outputs the 
  output$tab1 <- DT::renderDataTable({
    tab2() 
  },escape=FALSE, rownames = FALSE)
}

shinyApp(ui=ui,server=server)
