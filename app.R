#!/usr/bin/Rscript
library(shiny)
library(DT)
library(RPostgreSQL)
library(yaml)
ui <- fluidPage(
    titlePanel("Product Selection Tool"),
        fluidRow(
        column(3,
               
               sliderInput(inputId="priceRange", label="Price Range",min=0,max=100,value=c(10,20)),
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
                            choices=c("Everything","free","standard"),selected="Everything"),
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
        
        ### set up the SQL connection to redshift
        location <- paste0(getwd(),"/config.yml")
        configFile <- yaml.load_file(location)
        mycon <- dbConnect(PostgreSQL(), 
                           user=configFile$redshift$user, 
                           password=configFile$redshift$password, 
                           dbname=configFile$redshift$`db-name`, 
                           host=configFile$redshift$host,
                           port=configFile$redshift$port)
        
        ### for some inputs, create the SQL query part for each selection.
        if(input$OnSale == "yes"){
            sale <- 'true'
        }else{if(input$OnSale == "no"){
            sale <- 'false'
        }else{sale <- "\'True\',\'FALSE\'"} }
        
        
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
        
        if(input$group == "All"){
            group_keyword <- " IS NOT NULL"
        }else{
            group_key <- input$group
            group_keyword <- paste0('IN (\'',group_key,'\')')
        }
        
        
        if(input$personalise == "yes"){
            personalised <- " > 0"
        }else{
            if(input$personalise == "no"){
                personalised <- " = 0"
            } else {
                personalised <- " >= 0"}
        }
        
        
        if(input$delivery == "yes"){
            deliveryOption <- " IN (\'Free\') "
        }else{
            if(input$delivery == "no"){
                deliveryOption <- " NOT IN (\'Free\') "
            }else{
                deliveryOption <- " IS NOT NULL" }
        }        
        
        
        if(input$deliveryExpress == "yes"){
            deliver_express <-  " IN (\'TRUE\') "
        }else{ 
            if(input$deliveryExpress == "no"){
                deliver_express <- " IN (\'FALSE\') "
            }else{
                deliver_express <- " IS NOT NULL" }
        }     
        
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
        hols <- dbGetQuery(mycon,query_hols)    
        notonhols <- paste0(paste0(hols$partner_id,collapse=","))
        
        query_prod <- gsub("\n","",paste0('select product_name, product_code, partner_name,
                                          current_gross_price, gross_price_on_sale, delivery_time, delivery_class,
                                          url, family, partner_state, image_url, \"group\",
                                          has_express_delivery,target_age_range,number_of_options, partner_id,
                                          current_date-date(published_date) as days_live
                                          from product 
                                          WHERE current_availability = \'Available\' 
                                          AND current_stock_status IN (\'In Stock\',\'made to order\')
                                          AND partner_state = \'active\'
                                          AND current_gross_price BETWEEN ', input$priceRange[1], ' AND ', input$priceRange[2], '
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
        
        products <- dbGetQuery(mycon,query_prod)    
        
        if(dim(products)[1] > 0){
            
            product_codes <- paste0(paste0(products$product_code,collapse=","))
            
            query_views <- gsub("\n","",paste0('select product_code, count(*) as page_views 
                                               from noths.product_page_views_by_date  
                                               WHERE date BETWEEN date(\'',as.character(input$rangeDates[1]),'\'
                                               ) AND date(\'',as.character(input$rangeDates[2]),' \'
                                      )
                                               AND product_code IN (',product_codes,')
                                               group by product_code') )
            views <- dbGetQuery(mycon,query_views)  
            
            query_trans <- gsub("\n","",paste0('select product_code, sum(ttv) as TTV,
                                               count(distinct checkout_id) as num_checkouts
                                               from transaction_line  
                                               WHERE date BETWEEN date(\'',as.character(input$rangeDates[1]),'\'
                                               ) AND date(\'',as.character(input$rangeDates[2]),' \'
                                            ) 
                                               AND product_code IN (',product_codes,')
                                               group by product_code') )
            trans <- dbGetQuery(mycon,query_trans)  
            
            dbDisconnect(dbListConnections(PostgreSQL())[[1]]) 
            
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
            
            conversion <- full_file$num_checkouts / full_file$page_views
            conversion <- round(replace(conversion,is.na(conversion),0),2)
            
            full_file2 <- cbind(full_file,conversion)
            toSelect <- c("product_name","product_code","partner_name","family","group","current_gross_price","gross_price_on_sale",
                          "delivery_time","delivery_class","days_live","page_views","ttv","num_checkouts","conversion","url"  )
            
            small_file <-full_file2[,match(toSelect,names(full_file2))]
            
            img <- full_file[,which(names(full_file) =="image_url")]
            image <- paste0("<img src=\"", img," \" height=\"150\"></img>")
            data.frame(cbind(image,small_file))
     }else{
            stop("There are no results for this query. Please try different search parameters.")}
    })  
    
    output$tab1 <- DT::renderDataTable({
        tab2() 
    },escape=FALSE)
}

shinyApp(ui=ui,server=server)
