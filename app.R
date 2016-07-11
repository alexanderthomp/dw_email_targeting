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
#poolNames <- dbPool(
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
                           choices = c("doesn\'t matter","Yes","no"),selected = "doesn\'t matter")
        ), 
        column(3,
               dateInput(inputId="publishedDate",label="published after?",
                         value=as.character.Date(Sys.Date()-28)),
               dateRangeInput(inputId="rangeDates", label="Dates to get results over",
                              start = as.character.Date(Sys.Date()-28), end = as.character.Date(Sys.Date()-1)),
               radioButtons(inputId="personalise",label="Can it be personalised?",
                            choices=c("don\'t care","yes","no"), selected="don\'t care")
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
    
    ### Create the reactive element. i.e. eberything here will go when the "go" button is selected.
    
    tab2 <- eventReactive(input$go, { 
        
        if(dir.exists('/config/conf.yml')){
            location <-'/config/conf.yml'
        }else{
            location <-'conf.yml'
        }
        configFile <- yaml.load_file(location)
        ### create the pool of SQL connections so it works for multiple people at a time
        #poolNames <- dbPool(
        poolNames <- dbPool(
            drv = RPostgreSQL::PostgreSQL(),
            dbname = configFile$redshift$'db-name',
            host = configFile$redshift$host,
            user = configFile$redshift$user,
            password = configFile$redshift$password,
            port = configFile$redshift$port
        )
        
        
        
        isolate(withProgress(message="Searching ...",{
        ### some warnings in case the dates are miss set or not all objects will have been published over the date range
        if(as.character.Date(input$publishedDate) >  as.character.Date(as.character(input$rangeDates[2])))  {
            warningText <- "The published date is set after the end of the date range. Please change"
        }else{
            if(as.character.Date(input$publishedDate) <  as.character.Date(as.character(input$rangeDates[2])) && 
               as.character.Date(input$publishedDate) >  as.character.Date(as.character(input$rangeDates[1]))){
                warningText <- "Note: some products have not been published for the whole of this time period."
            }else{warningText <- ""}
        }
        output$warning <- renderText({warningText})
        
        
        ### for some inputs, create the SQL query part for each selection.
        ### For sale
        if(input$OnSale == "yes"){
            sale <- 'true'
        }else{if(input$OnSale == "no"){
            sale <- 'false'
        }else{sale <- "\'True\',\'FALSE\'"} }
        
        ### select from family
        fam <- c("All","accessories","baby care","experiences","food & drink","gardens & outdoors",
                 "haberdashery","health & beauty","home","jewellery","pet accessories",
                 "prints, pictures & art","stationary & parties","toys, games & sport",
                 "unknown")
        if(input$InFamily == "All"){
            fam_key <- fam[-1]
            fam_keyword <- paste0(paste0("'",fam_key,collapse="\',"),"\'")
        }else{
            fam_key <- input$InFamily
            fam_keyword <- paste0('\'',fam_key,'\'')
        }
        
        ### Seelct group
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
                                          url, family, partner_state, image_url, \"group\",
                                          has_express_delivery,target_age_range,number_of_options, partner_id,
                                          current_date-date(published_date) as days_live
                                          from product 
                                          WHERE current_availability = \'Available\' 
                                          AND current_stock_status IN (\'In Stock\',\'made to order\')
                                          AND partner_state = \'active\'
                                          AND current_gross_price BETWEEN ', input$pricemin, ' AND ', input$pricemax, '
                                          AND published_date > date(\'',as.character(input$publishedDate),'\'
                                          ) AND LOWER(product_name) LIKE (\'%', input$keyword ,'%\')
                            AND currently_on_sale IN (',sale,')
                                          AND family IN (',fam_keyword,')
                                          AND \"group\" ', group_keyword,'
                                          AND number_of_options', personalised ,'
                                          AND delivery_class ', deliveryOption ,'
                                          AND has_express_delivery ', deliver_express,'
                                          and partner_id IN (', notonhols ,')'
                                          ) )
        
        products <- dbGetQuery(poolNames,query_prod)    
        
        print("after query 2")
        
        ### If this search returns results, 
        if(dim(products)[1] > 0){
            product_codes <- paste0(paste0(products$product_code,collapse=","))
            
            
            print("before query 3")
            ### the products code from above are used int he next searches to limit the results found
            ### get the page views
            query_views <- gsub("\n","",paste0('select product_code, count(*) as page_views 
                                               from noths.product_page_views_by_date  
                                               WHERE date BETWEEN date(\'',as.character(input$rangeDates[1]),'\'
                                               ) AND date(\'',as.character(input$rangeDates[2]),' \'
                                      )
                                               AND product_code IN (',product_codes,')
                                               group by product_code') )
            views <- dbGetQuery(poolNames,query_views)  
            
            print("before query 4")
            ### get the ttv and number of checkouts
            query_trans <- gsub("\n","",paste0('select product_code, sum(ttv) as TTV,
                                               count(distinct checkout_id) as num_checkouts
                                               from transaction_line  
                                               WHERE date BETWEEN date(\'',as.character(input$rangeDates[1]),'\'
                                               ) AND date(\'',as.character(input$rangeDates[2]),' \'
                                            ) 
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
            conversion <- full_file$num_checkouts / full_file$page_views
            conversion <- round(replace(conversion,is.na(conversion),0),2)
            
            ### Put the file together, select only the needed columns
            full_file2 <- cbind(full_file,conversion)
            toSelect <- c("product_name","product_code","partner_name","family","group","current_gross_price","gross_price_on_sale",
                          "delivery_time","delivery_class","days_live","page_views","ttv","num_checkouts","conversion","url"  )
            small_file <-full_file2[,match(toSelect,names(full_file2))]
            
            ### get the images and make the first column
            img <- full_file[,which(names(full_file) =="image_url")]
            image <- paste0("<img src=\"", img," \" height=\"150\"></img>")
            newtab <- data.frame(cbind(image,small_file))
            
            print("before query 5")
            prod <- dbGetQuery(poolNames,'SELECT product_code,
                               CASE
                               WHEN published_date < date(\'2014-06-01\') THEN DATEDIFF(DAY, date(\'2014-06-01\'), CURRENT_DATE)
                               WHEN published_date >= date(\'2014-06-01\') THEN DATEDIFF(DAY, published_date, CURRENT_DATE)
                               END AS valid_days_live
                               FROM product
                               WHERE current_availability = \'Available\'
                               AND published_date IS NOT NULL
                               AND published_date != CURRENT_DATE')
            print("after query 5")
            
            print("before query 6")
            ctq <- dbGetQuery(poolNames,'SELECT product_code,
                              COUNT(DISTINCT checkout_id) AS total_checkouts,
                              SUM(ttv) AS total_ttv
                              FROM transaction_line
                              WHERE date > date(\'2014-06-01\')
                              GROUP BY product_code ')
            
            print("before query 7")
            pv <- dbGetQuery(poolNames,'SELECT product_code, SUM(number_of_views) AS total_page_views
                             FROM noths.product_page_views_by_date
                             WHERE date > (\'2014-06-01\')
                             GROUP BY product_code')
            
            
            db <- data.frame(prod, ctq[match(prod$product_code, ctq$product_code), ], pv[match(prod$product_code, pv$product_code), ])
            
            # replace NA total_checkouts, total_ttv & total_quantity with 0 
            db$total_checkouts <- replace(db$total_checkouts, is.na(db$total_checkouts), 0)
            db$total_ttv <- replace(db$total_ttv, is.na(db$total_ttv), 0)
            db$total_page_views <- replace(db$total_page_views, is.na(db$total_page_views), 0)
            
            # tidy up db
            duplicateColnames <- grep("product_code", colnames(db))[-1]
            db <- db[, -duplicateColnames]; rm(duplicateColnames)
            
            # create ttv by day
            db$ttv_per_day <- db$total_ttv / db$valid_days_live
            
            # create 'conversion' metric
            db$conversion <- ifelse(db$total_checkouts == 0 & db$total_page_views == 0, 0, db$total_checkouts / db$total_page_views)
            
            # remove products with conversion > 1 (can happen if page_views not tracked properly)
            db <- db[which(db$conversion <= 1), ]
            
            # normalise ttv per day globally
            db$ttv_per_dayNorm <- (db$ttv_per_day - min(db$ttv_per_day)) / diff(range(db$ttv_per_day))
            
            # normalise conversion globally (i.e., regardless of product family)
            db$conversionNorm <- (db$conversion - min(db$conversion)) / diff(range(db$conversion))
            
            db$impact <- db$conversionNorm*db$ttv_per_dayNorm
            db$impactRank <- rank(-db$impact)
            db <- db[, grep("product_code|impactRank", colnames(db))]
            
            ### add the impact next to the photo order by impact and re number imapct.
            newtab2 <- data.frame(newtab[,1],impact=db[match(newtab$product_code,db$product_code),-1],newtab[,-1])
            newtab2 <- data.frame(images=newtab[,1],impact=db[match(newtab$product_code,db$product_code),-1],newtab[,-1])
            newtab2 <- newtab2[order(newtab2$impact),]
            newtab2$impact <- 1:nrow(newtab2)
            newtab2
            
        }else{
            ### If there are no results from the search parameters, return a wanring to try different parameters 
            stop("There are no results for this query. Please try different search parameters.")}
        }))
    })  
    
    ### Outputs the 
    output$tab1 <- DT::renderDataTable({
        tab2() 
    },escape=FALSE)
}

shinyApp(ui=ui,server=server)

