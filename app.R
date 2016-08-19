#!/usr/bin/Rscript
library(shiny)
library(DT)
library(RPostgreSQL)
library(yaml)
library(pool)

### check for config file
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
    tags$head(includeScript("google-analytics.js")),
    includeCSS("www/PhuStyle.css"),
    navbarPage("Production Selection Tool",
               tabPanel("Tool",
                        fluidRow(
                            column(3,
                                   selectInput(inputId  = "InFamily", label = "Product Family", 
                                               choices = c("All","accessories","baby care","experiences","food & drink","garden & outdoors",
                                                           "haberdashery","health & beauty","home","jewellery","pet accessories",
                                                           "prints, pictures & art","stationery & parties","toys, games & sports",
                                                           "unknown"),selected="All"),
                                   uiOutput("group1ui"),
                                   selectInput(inputId  = "InFamily2", label = "Second Product Family", 
                                               choices = c("No second family","All","accessories","baby care","experiences","food & drink","garden & outdoors",
                                                           "haberdashery","health & beauty","home","jewellery","pet accessories",
                                                           "prints, pictures & art","stationery & parties","toys, games & sports",
                                                           "unknown"),selected="No second family"),
                                   uiOutput("group2ui"),
                                   selectInput(inputId  = "InFamily3", label = "Third Product Family", 
                                               choices = c("No third family","All","accessories","baby care","experiences","food & drink","garden & outdoors",
                                                           "haberdashery","health & beauty","home","jewellery","pet accessories",
                                                           "prints, pictures & art","stationery & parties","toys, games & sports",
                                                           "unknown"),selected="No third family"),
                                   uiOutput("group3ui"),
                                   
                                   actionButton("go", "Go") 
                            ),
                            column(3, 
                                   textInput(inputId="pricemin", label="minimum price",value=10),
                                   textInput(inputId="pricemax", label="maximum price",value=30),
                                   textInput(inputId = "keyword", label="search for title keyword",value=""),
                                   selectInput(inputId="OnSale",label = "include on sale products?",
                                               choices = c("doesn\'t matter","yes","no"),selected = "doesn\'t matter")
                                   
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
                                                choices=c("Everything","yes","no"),selected="Everything"),
                                   
                                   radioButtons(inputId="personalise",label="Can it be personalised?",
                                                choices=c("don\'t care","yes","no"), selected="don\'t care"),
                                   downloadButton('downloadAllData', 'Download full search'),
                                   downloadButton('downloadData', 'Download your Selections')
                            )
                        ),
                        #  ),
                        #  tabPanel("FAQs", uiOutput('FAQs'))
                        #  ),
                        tabsetPanel(
                            tabPanel("List View",dataTableOutput("tab1") ,value="list"),
                            tabPanel("Image Grid",htmlOutput("gridimage") ,value="grid"))
               ),
               tabPanel("FAQs", uiOutput('FAQs'))
               
    )
    
)

# generate list; ech element is a family and contains relevant groups
# advantages: runs in seconds; always shows latest Re:dash content
# naming: fg = family group 
query <- paste0("select family, \"group\" from product WHERE family <> \'\' group by family, \"group\"")
fg <- dbGetQuery(poolNames,query)
toAdd <- cbind(family = unique(fg$family), group = rep("All", length(unique(fg$family))))
fg <- rbind(fg, toAdd); fg <- fg[order(fg$family, fg$group), ]
fg <- split(fg$group, fg$family)
fg <- lapply(fg, as.character)



server <- function(input, output,session) { 
    ### render the drop down boxes for group. theses react to the selection from family
    output$group1ui <- renderUI({
        switch(input$InFamily,
               "All" = selectInput("group","Group",choices=c("All"),selected="All"),
               "accessories" = selectInput("group","Group",choices =c(unlist(fg["accessories"],use.names=F )),
                                           selected="All",multiple=TRUE),
               "baby care" = selectInput("group","Group",choices =c(unlist(fg["baby care"],use.names=F )),
                                         selected="All",multiple=TRUE),
               "clothing" = selectInput("group","Group",choices=c(unlist(fg["clothing"],use.names=F )),selected="All",multiple=TRUE),
               "experiences" = selectInput("group","Group",choices =c(unlist(fg["experiences"],use.names=F )),selected="experiences"),
               "food & drink" = selectInput("group","Group",choices =c(unlist(fg["food & drink"],use.names=F )),selected="All",multiple=TRUE),
               "garden & outdoors" = selectInput("group","Group",choices =c(unlist(fg["garden & outdoors"],use.names=F )),selected="All",multiple=TRUE),
               "haberdashery" = selectInput("group","Group",choices =c(unlist(fg["haberdashery"],use.names=F )),selected="All",multiple=TRUE),
               "health & beauty" = selectInput("group","Group",choices =c(unlist(fg["health & beauty"],use.names=F )),selected="All",multiple=TRUE),
               "home" = selectInput("group","Group",choices =c(unlist(fg["home"],use.names=F )),selected="All",multiple=TRUE),
               "jewellery" = selectInput("group","Group",choices =c(unlist(fg["jewellery"],use.names=F )),selected="All",multiple=TRUE),
               "pet accessories" = selectInput("group","Group",choices =c(unlist(fg["pet accessories"],use.names=F )),selected="All",multiple=TRUE),
               "prints, pictures & art" = selectInput("group","Group",choices =c(unlist(fg["prints, pictures & art"],use.names=F )),selected="All",multiple=TRUE),
               "stationery & parties" = selectInput("group","Group",choices =c(unlist(fg["stationery & parties"],use.names=F )),selected="All",multiple=TRUE),
               "toys, games & sports" = selectInput("group","Group",choices =c(unlist(fg["toys, games & sports"],use.names=F )),selected="All",multiple=TRUE),
               "unknown" = selectInput("group","Group",choices =c(unlist(fg["unknown"],use.names=F )),selected="unknown")
        )
    })
    
    output$group2ui <- renderUI({
        switch(input$InFamily2,
               "All" = selectInput("group2","Group 2",choices=c("All"),selected="All"),
               "accessories" = selectInput("group2","Group 2",choices =c(unlist(fg["accessories"],use.names=F )),selected="All",multiple=TRUE),
               "baby care" = selectInput("group2","Group 2",choices =c(unlist(fg["baby care"],use.names=F )),
                                         selected="All",multiple=TRUE),
               "fg" = selectInput("group2","Group 2",choices=c(unlist(fg["clothing"],use.names=F )),selected="All",multiple=TRUE),
               "experiences" = selectInput("group2","Group 2",choices =c("experiences"),selected="experiences"),
               "food & drink" = selectInput("group2","Group 2",choices =c(unlist(fg["food & drink"],use.names=F )),selected="All",multiple=TRUE),
               "garden & outdoors" = selectInput("group2","Group 2",choices =c(unlist(fg["garden & outdoors"],use.names=F )),selected="All",multiple=TRUE),
               "haberdashery" = selectInput("group2","Group 2",choices =c(unlist(fg["haberdashery"],use.names=F )),selected="All",multiple=TRUE),
               "health & beauty" = selectInput("group2","Group 2",choices =c(unlist(fg["health & beauty"],use.names=F )),selected="All",multiple=TRUE),
               "home" = selectInput("group2","Group 2",choices =c(unlist(fg["home"],use.names=F )),selected="All",multiple=TRUE),
               "jewellery" = selectInput("group2","Group 2",choices =c(unlist(fg["jewellery"],use.names=F )),selected="All",multiple=TRUE),
               "pet accessories" = selectInput("group2","Group 2",choices =c(unlist(fg["pet accessories"],use.names=F )),selected="All",multiple=TRUE),
               "prints, pictures & art" = selectInput("group2","Group 2",choices =c(unlist(fg["prints, pictures & art"],use.names=F )),selected="All",multiple=TRUE),
               "stationery & parties" = selectInput("group2","Group 2",choices =c(unlist(fg["stationery & parties"],use.names=F )),selected="All",multiple=TRUE),
               "toys, games & sports" = selectInput("group2","Group 2",choices =c(unlist(fg["toys, games & sports"],use.names=F )),selected="All",multiple=TRUE),
               "unknown" = selectInput("group2","Group 2",choices =c("unknown"),selected="unknown")
        )
    })
    
    output$group3ui <- renderUI({
        switch(input$InFamily3,
               "All" = selectInput("group3","Group 3",choices=c("All"),selected="All"),
               "accessories" = selectInput("group3","Group 3",choices =c(unlist(fg["accessories"],use.names=F)),selected="All",multiple=TRUE),
               "baby care" = selectInput("group3","Group 3",choices =c(unlist(fg["baby care"],use.names=F )),selected="All",multiple=TRUE),
               "clothing" = selectInput("group3","Group 3",choices=c(unlist(fg["clothing"],use.names=F )),selected="All",multiple=TRUE),
               "experiences" = selectInput("group3","Group 3",choices =c("experiences"),selected="experiences"),
               "food & drink" = selectInput("group3","Group 3",choices =c(unlist(fg["food & drink"],use.names=F )),selected="All",multiple=TRUE),
               "garden & outdoors" = selectInput("group3","Group 3",choices =c(unlist(fg["garden & outdoors"],use.names=F )),selected="All",multiple=TRUE),
               "haberdashery" = selectInput("group3","Group 3",choices =c(unlist(fg["haberdashery"],use.names=F )),selected="All",multiple=TRUE),
               "health & beauty" = selectInput("group3","Group 3",choices =c(unlist(fg["health & beauty"],use.names=F )),selected="All",multiple=TRUE),
               "home" = selectInput("group3","Group 3",choices =c(unlist(fg["home"],use.names=F )),selected="All",multiple=TRUE),
               "jewellery" = selectInput("group3","Group 3",choices =c(unlist(fg["jewellery"],use.names=F )),selected="All",multiple=TRUE),
               "pet accessories" = selectInput("group3","Group 3",choices =c(unlist(fg["pet accessories"],use.names=F )),selected="All",multiple=TRUE),
               "prints, pictures & art" = selectInput("group3","Group 3",choices =c(unlist(fg["prints, pictures & art"],use.names=F )),selected="All",multiple=TRUE),
               "stationery & parties" = selectInput("group3","Group 3",choices =c(unlist(fg["stationery & parties"],use.names=F )),selected="All",multiple=TRUE),
               "toys, games & sports" = selectInput("group3","Group 3",choices =c(unlist(fg["toys, games & sports"],use.names=F )),selected="All",multiple=TRUE),
               "unknown" = selectInput("group3","Group 3",choices =c("unknown"),selected="unknown")
        )
    })
    
    output$dateui <- renderUI({
        switch(input$date,
               "recently published" = radioButtons(inputId="publishedDate",label="time frame date options",choices=c("last week","last 2 weeks","last month")),
               "sales between dates" = dateRangeInput(inputId = "daterange", label = "sales window",start=Sys.Date()-15, end=Sys.Date()-1,min='2014-06-01',max=Sys.Date()-1)
        )
    })
    
    ### Create the reactive element. i.e. eberything here will go when the "go" button is selected.
    
    tab2 <- eventReactive(input$go, { 
        isolate(withProgress(message="Searching ...",{
            
            ### for some inputs, create the SQL query part for each selection.
            ### For sale
            if(input$OnSale == "yes"){
                sale <- "\'true\' "
            }else{if(input$OnSale == "no"){
                sale <- "\'false\' "
            }else{sale <- "\'true\',\'false\'"} }
            
            ### select from family
            if(input$InFamily2 =="No second family" && input$InFamily3 =="No third family") {
                if(input$InFamily == "All"){
                    fam_keyword <- paste0(paste0("'",names(fg),collapse="\',"),"\'")
                    group_keyword <- "IS NOT NULL"
                    
                }else{
                    fam_keyword <- paste0('\'',input$InFamily,'\'')
                    if(("All" %in% input$group)== TRUE & length(input$group) == 1){
                        groups <- unlist(fg[input$InFamily],use.names=F )[-grep("All", unlist(fg[input$InFamily],use.names=F))]
                        group_keyword <- paste0('IN (', paste0('\'', groups, '\'', collapse = ","), ')') 
                    }else{
                        group1 <- input$group[!grepl("All",input$group)]
                        group_keyword <- paste0('IN (', paste0('\'', group1, '\'', collapse = ","), ')') 
                    }
                }
            }else{
                if(input$InFamily2 !="No second family" && input$InFamily3 =="No third family"){
                    fam_keyword <- paste0('\'',input$InFamily,'\' , \'',input$InFamily2,'\'')
                    
                    if(("All" %in% input$group)== TRUE & length(input$group) == 1){
                        group1 <- unlist(fg[input$InFamily],use.names=F )[-grep("All", unlist(fg[input$InFamily],use.names=F))]
                        group_key1 <- paste0('\'', group1, '\'', collapse = ",")
                    }else{
                        group1 <- input$group[!grepl("All",input$group)]
                        group_key1 <- paste0('\'', group1, '\'', collapse = ",")
                    }
                    if(("All" %in% input$group2)== TRUE & length(input$group2) == 1){
                        group2 <- unlist(fg[input$InFamily2],use.names=F )[-grep("All", unlist(fg[input$InFamily2],use.names=F))]
                        group_key2 <- paste0('\'', group2, '\'', collapse = ",") 
                    }else{
                        group2 <- input$group2[!grepl("All",input$group2)]
                        group_key2 <- paste0('\'', group2, '\'', collapse = ",")
                    }
                    all_groups <- union(group_key1,group_key2)
                    group_keyword <- paste0("IN (", paste0(all_groups, collapse=','), ")" )  
                    
                }else{
                    if(input$InFamily2 !="No second family" && input$InFamily3 !="No third family"){
                        fam_keyword <- paste0('\'',input$InFamily,'\' , \'',input$InFamily2,'\' , \'',input$InFamily3,'\'')
                        
                        if(("All" %in% input$group)== TRUE & length(input$group) == 1){
                            group1 <- unlist(fg[input$InFamily],use.names=F )[-grep("All", unlist(fg[input$InFamily],use.names=F))]
                            group_key1 <- paste0('\'', group1, '\'', collapse = ",")
                        }else{
                            group <- input$group[!grepl("All",input$group)]
                            group_key1 <- paste0('\'', group, '\'', collapse = ",")
                        }
                        if(("All" %in% input$group2)== TRUE & length(input$group2) == 1){
                            group2 <- unlist(fg[input$InFamily2],use.names=F )[-grep("All", unlist(fg[input$InFamily2],use.names=F))]
                            group_key2 <- paste0('\'', group2, '\'', collapse = ",")
                        }else{
                            group2 <- input$group2[!grepl("All",input$group2)]
                            group_key2 <- paste0('\'', group2, '\'', collapse = ",")
                        }
                        if(("All" %in% input$group3)== TRUE & length(input$group3) == 1){
                            group3 <- unlist(fg[input$InFamily3],use.names=F )[-grep("All", unlist(fg[input$InFamily3],use.names=F))]
                            group_key3 <- paste0('\'', group3, '\'', collapse = ",")
                        }else{
                            group3 <- input$group3[!grepl("All",input$group3)]
                            group_key3 <- paste0('\'', group3, '\'', collapse = ",")
                        }
                        all_groups <- Reduce(union, list(group_key1,group_key2,group_key3))
                        
                        group_keyword <- paste0("IN (", paste0(all_groups, collapse=','), ")" )  
                    }
                }
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
                    pubDate <- Sys.Date() - 8
                    viewDate1 <- Sys.Date() - 8
                    viewDate2 <- Sys.Date() -1
                }else{
                    if(input$publishedDate == "last 2 weeks"){
                        pubDate <- Sys.Date() - 15
                        viewDate1 <- Sys.Date() - 15
                        viewDate2 <- Sys.Date() -1
                        
                    }else{ 
                        pubDate <- Sys.Date() - 29
                        viewDate1 <- Sys.Date() - 29
                        viewDate2 <- Sys.Date() - 1
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
                                              AND published_date BETWEEN date(\'', as.character(pubDate),  '\') 
                                              AND date(\'',as.character(viewDate2), '\'
                                              ) AND LOWER(product_name) LIKE (\'%', input$keyword ,'%\')
                                              AND currently_on_sale IN (',sale,')
                                              AND family IN (',fam_keyword,')
                                              AND \"group\" ', group_keyword,'
                                              AND number_of_options', personalised ,'
                                              AND delivery_class ', deliveryOption ,'
                                              AND has_express_delivery ', deliver_express, '
                                              AND lower(product_name) not like \'%bespoke%\' 
                                              AND lower(product_name) not like \'%upgrade%\'  
                                              AND lower(product_name) not like \'%special%\' 
                                              AND lower(product_name) not like \'%additional%\' 
                                              AND lower(product_name) not like \'%custom%\' 
                                              AND lower(product_name) not like \'%replacement%\'
                                              '
                                              ) )
            
         print(query_prod)
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
                    full_file$ttv <- round(replace(full_file$ttv,is.na(full_file$ttv),0),2)
                    full_file$num_checkouts <- replace(full_file$num_checkout,is.na(full_file$num_checkout),0)
                    
                }
                
                ### Add in conversion 
                conversion <- ifelse(full_file$page_views < full_file$num_checkouts, 0, round(full_file$num_checkouts / full_file$page_views,2))
                conversion <- round(replace(conversion,is.na(conversion),0),2)
                
                ### Put the file together, select only the needed columns
                full_file2 <- cbind(full_file,conversion)
                
                invalid <- which(full_file2$conversion > 0.15 & full_file2$num_checkouts == 1)
                if(length(invalid) == 0){
                    full_file2 <- full_file2
                }else{
                    full_fileInvalid <- full_file2[invalid, ]
                    full_fileValid <- full_file2[-invalid, ]
                    meanConversionByFamily <- sapply(split(full_fileValid, full_fileValid$family), function(x) mean(x$conversion))
                    
                    for (i in unique(full_fileValid$family)){
                        full_fileInvalid[full_fileInvalid$family == i, "conversion"] <- round(meanConversionByFamily[i],2)
                    }
                    full_file2 <- rbind(full_fileValid, full_fileInvalid)
                }
                
                
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
                url <- full_file2[,which(names(full_file2) =="url")]
                img <- full_file2[,which(names(full_file2) =="image_url")]
                productUrl <- full_file2[,which(names(full_file2) == "url")]
                image <- paste0("<a href = \"", productUrl, "\" target=\"_blank\"> <img src=\"", img,"\" height=\"150\"></a>")
                newtab <- data.frame(cbind(image, full_file2,img,url))
                
                toSelect <- c("impact", "image", "product_name","product_code","partner_name","family","group","current_availability", "current_stock_status", "partner_state", "current_gross_price","gross_price_on_sale",
                              "delivery_time","delivery_class","page_views","ttv","num_checkouts","conversion","img","url")
                outputTable <-newtab[,match(toSelect,names(newtab))]
                
                outputTableNames <- c("Relative Impact", "Image", "Product", "Product Code", "Partner","Family","Group","Availability", "Stock Status", "Partner State", "Price","Sale Price",
                                      "Delivery Time","Delivery Class","Page Views","TTV","Checkouts","Conversion","img","url")
                colnames(outputTable) <- outputTableNames
                
                outputTable
                
            }else{
                ### If there are no results from the search parameters, return a wanring to try different parameters 
                stop("There are no results for this query. Please try different search parameters.")}
        }))
    })  
    
    ### Outputs the result
    
    
    output$tab1 <- DT::renderDataTable({
        tab2()[,-c(19,20)]
    },escape=FALSE, rownames = FALSE,server=TRUE,
    options=list(pageLength=50, searchHighlight = TRUE)
    )
    
    output$gridimage <- renderUI({
        images <- list()
        for(i in 1:nrow(tab2())){
            src= tab2()$img[i]
            images[i] <- list(tags$a(href=tab2()$url[i], img(src=src),target="_blank"))
        }
        images
    })
    
    output$downloadAllData <- downloadHandler(
        filename = function() { paste("FullSearchResults_",Sys.Date(), '.csv', sep='') },
        content = function(file) {
            write.csv(tab2(), file,row.names = FALSE)
            
        }
    )
    
    output$downloadData <- downloadHandler( 
        filename = function() { paste("Selection_",Sys.Date(), '.csv', sep='') },
        content = function(file) {
            rows <- as.numeric(input$tab1_rows_selected)
            toRemove <- "Image"                  
            write.csv(tab2()[rows,-match(toRemove, colnames(tab2()))], file,row.names = FALSE)
        } 
    ) 
    
    output$FAQs <- renderUI({
        HTML('<h1>Frequently asked questions and information about metrics </h1>
             <h2> What is the relative impact?</h2>
             <p><font size=4> The relative impact using the TTV * conversion to give a score of the impact a product has.
             The values are taken over the time period you are looking at. So if the search looks at 
             recently published products over the last week, the TTV is a average of the TTV for that item
             per day for the number of days it has been live. And the conversion is the total nuber of checkouts 
             divided by the number of pages views for the same number of days. <br>
             If the time period is for example last Christmas, the TTV is averaged of the number of days you 
             have selected in that time period and conversion summed over that same period. Only products 
             which are published before the start of this time period are included and returned. <br>
             The products are then ordered with this metric (the highest TTV*conversion is returned first).
             </font></p>
             
             <h2> How do I get to the webpage? </h2>
             <p><font size=4> Click on the image! It will open a new tab showing the website for that product.
             This also works if the image link is broken (something we are working on still.)
             </font></p>
             
             
             <h2> How do I know the search is working? </h2>
             <p><font size=4> There is a search bar at the very top. When searching, a message will come up at the top right corner.
             If there are any problems, (such as no resulting are returned from your query), an error will appear 
             in red below the Go button.
             </font></p>
             
             <h2> How do I look up mulitple groups in a family? </h2>
             <p><font size=4> When a family is selected, the default is to search through all groups within that family.
             To selected groups, click on the family. For the first family selected the group box is visible. <br>
             For the second and third families this will cause the creation of the group drop down boxes. You can then 
             select multiple groups. <br>

             </font></p>
             
             <h2> How do I reset the search? </h2>
             <p><font size=4> The current best way is to refresh the page.
             </font></p>
             
             <h2> How do I download my data? <h2>
             <p><font size=4> There are two ways. If you want to download all of the search, click the \"DOWNLOAD FULL SEARCH\". This 
             will download a csv file to your downloads which can be opened with excel. It will contain the current date. (If you 
             save more than one seach, they will also be number 1,2,3 etc.) <br>
             To download a selection of items, select the rows you want. To unselect just click on the row again. Then click on the 
             \"DOWNLOAD YOUR SELECTIONS\" button. Again it downloads a csv. 
             </font></p>
             
             
             <h2> What has data has been removed form the search? </h2>
             <p><font size=4> We have removed any items containing custom and bospoke in the title. 
             
             </font></p>
             
             ')
        
    })
}

shinyApp(ui=ui,server=server)
