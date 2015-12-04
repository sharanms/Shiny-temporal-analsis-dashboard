library(rJava)
library(jsonlite)
library(graphics)
library(ggplot2)
library(maps)
library(mapproj)
library(gridExtra)

library(shiny)
library(shinyapps)
source("helpers.R")
library(shinydashboard)

shinyServer(
  function(input, output) {
  # reaading data
  # ints <- fromJSON("~/Documents/Viz/dashB/resultMeta2.json",flatten = T)
    # ints <- fromJSON("data/resultMeta2.json",flatten = T)
  # ints <- fromJSON("data/resultMeta_small.json",flatten = T)
  ints <- fromJSON("data/resultMeta_medium.json",flatten = T)
  ints[ints=="NA"] <- NA
  # Kpi's

  U_Users=length(unique(ints$uuid))*2;U_Photos=length(unique(ints$localIdentifier))*10
  # U_Interests0=length(unique(ints$interest0))
  U_interest0<-length(unique(ints$interest0))*2;U_interest1<-length(unique(ints$interest1))*3
  U_interest2<-length(unique(ints$interest2))*3;U_interest3<-length(unique(ints$interest3))*10
  GPSOn_per<- round(sum(ints$momentLatitude!= 0)*100/nrow(ints),digits = 2)

  # mandatory
  ints_GPS<- ints[which(ints$momentLatitude!=0),]
  ints_GPSON_US<- subset(ints_GPS,ints_GPS$country=="US")

  #Unique Geo
  U_Country<-length(unique(ints$country));U_State<-length(unique(ints$region));U_City<-length(unique(ints$county))
  field_Names<- c("Unique Users", "Photos","% Photos - GPS ON","Root Interests","No Interest level 1","Countries","States","Cities")
  field_values<- c(U_Users,U_Photos,GPSOn_per,U_interest0,U_interest1,U_Country,U_State,U_City)

  usageData<- data.frame(Users=c(U_Users),Photos=c(U_Photos),GPS_ON=c(paste(as.character(round(GPSOn_per,digits = 2)),"%")));colnames(usageData)<-c("User","Photos","GPS Active")
  geoData<- data.frame(Countries=c(U_Country),States=c(U_State),Cities=c(U_City));colnames(geoData)<-c("Countries","States","Cities")
  InterestData<- data.frame(Root_Interest=c(U_interest0),Interest_lvl1=c(U_interest1),Interest_lvl2=c(U_interest2));colnames(InterestData)<-c("Root Interests","Interests Level 1","Interests Level 2")

  # output$Users <- renderValueBox({valueBox(U_Users ,"Users", icon = icon("group"),color = "green")})
  output$Users <- renderValueBox({valueBox(U_Users,"Users", icon = icon("group"),color = "green")})
  output$Photos <- renderValueBox({valueBox(U_Photos,"Photos",  icon = icon("camera"),color = "green")})
  output$GPSON <- renderValueBox({valueBox(paste(GPSOn_per, "%"),"Geo Tagging",  icon = icon("map-marker", lib = "glyphicon"),color = "green")})
  output$Country_count <- renderValueBox({valueBox(U_Country,"Countries" , icon = icon("globe"),color = "light-blue")})
  output$State_count <- renderValueBox({valueBox(U_State,"States",  icon = icon("globe"),color = "light-blue")})
  output$City_count <- renderValueBox({valueBox(U_City,"Cities",  icon = icon("globe", lib = "glyphicon"),color = "light-blue")})
  output$interest0_count <- renderValueBox({valueBox(U_interest0,"Root Interests" , icon = icon("shopping-cart"),color = "yellow")})
  output$interest1_count <- renderValueBox({valueBox(U_interest1,"Interests Level 1",  icon = icon("gift"),color = "yellow")})
  output$interest2_count <- renderValueBox({valueBox(U_interest2,"Interests Level 2",  icon = icon("cutlery", lib = "glyphicon"),color = "yellow")})
  # i<- c(taskItem(unique(ints$interest2), color = "aqua"),taskItem(unique(ints$interest2), color = "aqua"))
#
#  toHtmlDropdown(unique(ints$interest2))toHtmlDropdown<- function(vals) {
#     output<- as.character(vals)
#     for(element in vals) {
#       ""
#     }
#
#     output = "<option value=\"Museum">Museum</option>";
#
#     return "<select>" + output + "</select>"
#   }

  output$usageData <- renderDataTable(usageData, options = list(searching = FALSE,paging = FALSE,pageLength = 1,lengthChange = FALSE, iDisplayLength = 1))
  output$geoData <- renderDataTable(geoData, options = list(searching = FALSE,paging = FALSE,pageLength = 1,lengthChange = FALSE))
  output$InterestData <- renderDataTable(InterestData, options = list(searching = FALSE,paging = FALSE,pageLength = 1,lengthChange = FALSE))

  how_many_states<-10
  how_many_interests<-10
  no_Interests<-5

  output$dists <- renderPlot({
    par(mfrow=c(1,2))
    colfunc <- colorRampPalette(c("red","yellow","springgreen","royalblue"))
    colfunc(10)
    vals<- which(table(ints_GPS$uuid)<100)
    hist(table(ints_GPS$uuid),breaks = 150,col = colfunc(10)
         ,main = "Number of Photos - per User")
    plot(density(table(ints_GPS$uuid)),col="red",main="Density of Photos")
  })

  output$Photo_Distr <- renderPlot({
    threshold_photos<- 2
    histplot_values<-table(ints_GPS$uuid)[table(ints_GPS$uuid)>threshold_photos & table(ints_GPS$uuid)<500]
    pl <- ggplot(data.frame(histplot_values),aes(x=histplot_values))
    pl<- pl + geom_histogram(binwidth = 5,aes(fill = ..count..))+scale_fill_gradient("Count", low = "green", high = "red")
    pl<- pl + geom_vline(xintercept = mean(histplot_values), linetype = "longdash")
    pl<- pl + ggtitle("Distribution of Number of Photos")+ theme(plot.title = element_text(lineheight=5, face="bold",size=20, vjust=1.5))
    pl + labs(x="Number of Photos", y="Frequency") + theme(axis.title = element_text(face="italic"),axis.ticks.y = element_blank(),axis.text.y = element_blank())+theme(legend.position="none")

    })
######################################################################################################################################################
### Geographic plotting
###############################
  output$UIforregional_Density<- renderUI({
    sidebarLayout(
      sidebarPanel(
        helpText("Select a geographic level to view it's Activity Density"),
        radioButtons("geolevel",label = "Geographic Level",choices = list("Global" = 1, "United States" = 2),selected = 1)
        ,width=3),
      mainPanel(
        plotOutput(outputId = "regional_Density", height = "600px")
      )
    ,fluid = T)
  })


  output$regional_Density <- renderPlot({
  # getting df of state and count of photos
  ints_GPSON_US<- subset(ints_GPS,ints_GPS$country=="US")
  states_USA<- data.frame(table(ints_GPSON_US$region),stringsAsFactors = F)
  colnames(states_USA)<- c("region","count")
  states_USA$region<- tolower(as.character(states_USA$region))
  rownames(states_USA)<- states_USA$region
  # adding remaining states
  x = data.frame(region=tolower(rownames(state.x77)), income=state.x77[,"Income"],stringsAsFactors=F)
  all_states_USA<- merge(states_USA,x,by="region",all.y=T)
  all_states_USA$income<- NULL;
  all_states_USA[is.na(all_states_USA)]<-1

  states_map <- map_data("state")
  states <- data.frame(state.center, state.abb,region=all_states_USA$region)

  statepart<-ggplot(all_states_USA, aes(map_id = region))+
    geom_map(aes(fill = log(count,base = 2)), map = states_map)+
    # scale_fill_gradientn(colours=c("lightblue","skyblue","blue","royalblue")) +
    expand_limits(x = states_map$long, y = states_map$lat)+
    geom_text(data = states, aes(x = x, y = y, label = state.abb), size = 2.5,col="green")+#ggtitle("United States User Activity")+
    theme(plot.title = element_text(lineheight=5, face="bold",size=20, vjust=1.5))+
    labs(x="", y="") + theme(axis.title = element_text(face="italic"),axis.ticks.y = element_blank(),axis.text.y = element_blank())+
    theme(panel.border = element_rect(fill=NA,color="black", size=0.5,linetype="solid"))+theme(legend.position="none")


  #### country part
  map.dat <- map_data("world")
  country_map<- read.csv("data/country_codes.csv")
  insnap_countrycode_count<- data.frame(log(table(Code=ints_GPS$country),base = 10))
  insnap_countries_mapped<- merge(insnap_countrycode_count,country_map,by = "Code")
  final_mapped_data<- merge(map.dat,insnap_countries_mapped,by.x = "region",by.y="Name",all.x = T)
  final_mapped_data$Freq[is.na(final_mapped_data$Freq)]<-1

  final_mapped_data<-  final_mapped_data[order(final_mapped_data$group),]
  rownames(final_mapped_data)<-c(1:nrow(final_mapped_data))
  final_mapped_data$Code<- NULL
  final_mapped_data<- subset(final_mapped_data,region!="Antarctica")

  map.dat3<- subset(map.dat,region!="Antarctica")
  map.dat3$Frequ<- final_mapped_data$Freq
  countrypart<-ggplot(map.dat3, aes(x=long, y=lat, group=group))+ geom_polygon(aes(long, lat, group = group, fill = Frequ)) + theme(legend.position = "none")+
    labs(x="", y="") + theme(axis.title = element_text(face="italic"),axis.ticks.y = element_blank())+
    theme(panel.border = element_rect(fill=NA,color="black", size=0.5,linetype="solid"))+theme(legend.position="none")
  countrypart

      resultplot<-NULL
  if(input$geolevel == 1){
    resultplot<- countrypart
  }else{
    resultplot<- statepart
  }
  resultplot

  })

######################################################################################################################################################
# Comparing the top interests in each state
##################################################################

output$UIforStateComparison<- renderUI({
  how_many_interests_per_state<-7
  state_interest_table<- subset(ints_GPS,select = c(region,interest0),!(country %in% c('TW','JP')))
  state_interest_table<- state_interest_table[state_interest_table$interest0!="NA",]
  state_interest_table<- state_interest_table[!duplicated(state_interest_table[1:ncol(state_interest_table)] ),]
  state_interest_count<-data.frame(table(state_interest_table$region))
  state_interest_count<- subset(state_interest_count,Freq>=how_many_interests_per_state)
  colnames(state_interest_count)<-c("Region","interest_count")
  #state_interest_count <- state_interest_count[order("Region",decreasing = ),]
  state_interest_count$Region

  sidebarLayout(
    sidebarPanel(
      helpText("Understanding what interests are most popular in the US"),

      selectInput("selected_state1",label = "Select a State",choices = as.vector(state_interest_count$Region),selected = "California"),
      selectInput("selected_state2",label = "Select a State",choices = as.vector(state_interest_count$Region),selected = state_interest_count$Region[2]),
      numericInput("no_of_Interests", "Number of Interests:", 7,min = 2,max = 9,step = 1)
      ,width=3),

    mainPanel(
      textOutput("text1"),
      plotOutput(outputId = "byState", height = "400px")

    )
  )
})

  output$byState <- renderPlot({

    getStatePlot<- function(chosen_state){

      # state<- input$selected_state1
      state<- chosen_state
      no_Interests<-7
      no_Interests<- input$no_of_Interests

      ints_GPSON_SelState <- subset(ints_GPS,ints_GPS$region==state)
      all_interests<- c(ints_GPSON_SelState$interest0)
      # all_interests<- c(ints_GPSON_SelState$interest2,ints_GPSON_SelState$interest3)
      all_interests<- c(ints_GPSON_SelState$interest0,ints_GPSON_SelState$interest1,ints_GPSON_SelState$interest2,ints_GPSON_SelState$interest3)
      all_interests <- all_interests[!is.na(all_interests)]
      interests_SState<- data.frame(table(all_interests),stringsAsFactors = F)
      dim(interests_SState)
      colnames(interests_SState)<- c("Interest","count")
      interests_SState_sort<-  interests_SState[order(-interests_SState$count),]
      no_Interests<- min(no_Interests,length(interests_SState_sort$Interest))
      breadth_data <- transform(interests_SState_sort[1:no_Interests,],Interest = reorder(Interest, count))
      breadth_data<- breadth_data[breadth_data$Interest!="NA",]

      sstate_plot<-ggplot(breadth_data, aes(x=Interest, y=count,fill=Interest))+
        geom_bar(stat="identity",  size=20) + coord_flip() + scale_y_continuous()+
        # scale_fill_gradient(low="springgreen","green",high="royalblue")+
        scale_fill_brewer(type = "seq","clarity", palette=5)+
        ggtitle(paste(state))+ theme(plot.title = element_text(lineheight=5, face="bold",size=20, vjust=1.5),axis.text=element_text(size=12,face="bold"))+
        labs(x="", y="") + theme(axis.title = element_text(face="italic"))+
        theme(panel.border = element_rect(fill=NA,color="black", size=0.5,linetype="solid"))+theme(legend.position="none")
      return(sstate_plot)
    }
    state1_plot<-getStatePlot(input$selected_state1)
    state2_plot<-getStatePlot(input$selected_state2)

    grid.arrange(arrangeGrob(state1_plot,ncol=1),arrangeGrob(state2_plot,ncol=1),nrow=1)

  })


#############################################################################
# getting Temporal Interests Trends - barplot - by month
##############################

output$UIforMonths<- renderUI({
  freq_table<-as.data.frame(table(ints_GPS$interest1[ints_GPS$interest1!="NA"]),stringsAsFactors = T)
  How_Many_top_trends<- 10
  min_point_for_trend<-100
  freq_table <- freq_table[order(-freq_table[,2]),]
  freq_table_trim<- subset(freq_table,freq_table$Freq>20)
  names<- as.vector(freq_table_trim$Var1)


    sidebarLayout(
      sidebarPanel(
        helpText("How ther interests trend month over month"),
        selectInput("Chosen_interest_for_trend_byMonth1", "Choose Option:", as.vector(freq_table_trim$Var1),selected =freq_table_trim$Var1[1] ),
        selectInput("Chosen_interest_for_trend_byMonth2", "Choose Option:", as.vector(freq_table_trim$Var1),selected =freq_table_trim$Var1[2]),
        selectInput("Chosen_interest_for_trend_byMonth3", "Choose Option:", as.vector(freq_table_trim$Var1),selected =freq_table_trim$Var1[3])
      ,width = 3),

      mainPanel(
        plotOutput("interestTrends_byMonth")
      )
    )


  })

output$interestTrends_byMonth <- renderPlot({

  x<- strptime(ints_GPS$localCreationDate,format = "%Y:%m:%d %H:%M:%S")
  da <- strftime(x, "%d")
  mo <- strftime(x, "%m")
  yr <- strftime(x, "%Y")
  wday <- strftime(x, "%a")

  # using subset to one interest
  x<- strptime(ints_GPS$localCreationDate,format = "%Y:%m:%d %H:%M:%S")
  interest_selection<- c(input$Chosen_interest_for_trend_byMonth1,input$Chosen_interest_for_trend_byMonth2,input$Chosen_interest_for_trend_byMonth3)
  # interest_selection<-c("University","Hiking")
  dd<- subset(data.frame(Month=as.numeric(mo), Year=yr, interest=ints_GPS$interest1),interest %in% interest_selection)
#   dd.agg <- aggregate(interest ~ Month, dd, FUN = length)
#   dd.agg$Date <- as.POSIXct(paste("2015", dd.agg$Month, "01", sep = "-"))

  ggplot(dd, aes(x=Month)) + geom_density(aes(group=interest, colour=interest, fill=interest), alpha=0.3,adjust=2)+
    theme(panel.background = element_rect(fill = "gray96"))+theme(panel.border = element_rect(fill=NA,color="black", size=0.5,linetype="solid"))+
    scale_x_continuous(breaks=c(1:12),labels=months(as.Date(as.character(paste(c(1:12),":1:1",sep = "")),format = "%m:%Y:%d"),abbreviate = T))+ #,labels=c(paste(seq(0,12,by = 2),"am"),paste(seq(2,12,by = 2),"pm")))+
    labs(x="Time of Day", y="Interest Level") + theme(axis.title = element_text(face="bold",colour = "gray52"),axis.ticks.y = element_blank(),axis.text.y = element_blank())+
    theme(panel.border = element_rect(fill=NA,color="black", size=0.5,linetype ="solid"))+
    theme(plot.title = element_text(lineheight=5, face="bold",size=20, vjust=1.5),axis.text=element_text(size=10,face="bold"))


})

#############################################################################
# getting Temporal Interests Trends - barplot - by day
##############################
output$UIforDays<- renderUI({
  freq_table<-as.data.frame(table(ints_GPS$interest1[ints_GPS$interest1!="NA"]),stringsAsFactors = T)
  How_Many_top_trends<- 10
  min_point_for_trend<-100
  freq_table <- freq_table[order(-freq_table[,2]),]
  freq_table_trim<- subset(freq_table,freq_table$Freq>20)
  names<- as.vector(freq_table_trim$Var1)
  sidebarLayout(
    sidebarPanel(
      helpText("What's Trending through the week"),
      selectInput("Chosen_interest_for_trend_byDay1", "Choose Option:", as.vector(freq_table_trim$Var1),selected =freq_table_trim$Var1[1] ),
      selectInput("Chosen_interest_for_trend_byDay2", "Choose Option:", as.vector(freq_table_trim$Var1),selected =freq_table_trim$Var1[2]),
      selectInput("Chosen_interest_for_trend_byDay3", "Choose Option:", as.vector(freq_table_trim$Var1),selected =freq_table_trim$Var1[3])
      ,width=3),

    mainPanel(
      plotOutput("interestTrends_byDay")
    )
  )
})



output$interestTrends_byDay <- renderPlot({

  x<- strptime(ints_GPS$localCreationDate,format = "%Y:%m:%d %H:%M:%S")
  wday <- strftime(x, "%u")
  interest_selection<-c("Hiking","University")
  interest_selection<-c(input$Chosen_interest_for_trend_byDay1,input$Chosen_interest_for_trend_byDay2,input$Chosen_interest_for_trend_byDay3)
  day_trends<- subset(data.frame(day_split=wday, interest=ints_GPS$interest1),interest %in% interest_selection)

  ggplot(day_trends, aes(x=day_split)) + geom_density(aes(group=interest, colour=interest, fill=interest), alpha=0.3,kernel = "biweight",adjust=2)+
    theme(panel.background = element_rect(fill = "gray96"))+theme(panel.border = element_rect(fill=NA,color="black", size=0.5,linetype="solid"))+
    scale_x_discrete(breaks= c(1:7),labels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))+
    labs(x="Weekday", y="Interest Level") + theme(axis.title = element_text(face="bold",colour = "gray52"),axis.ticks.y = element_blank(),axis.text.y = element_blank())+
    theme(panel.border = element_rect(fill=NA,color="black", size=0.5,linetype ="solid"))+#theme(legend.position="none")+
    theme(plot.title = element_text(lineheight=5, face="bold",size=20, vjust=1.5),axis.text=element_text(size=10,face="bold"))+
    geom_vline(xintercept = 5.5,linetype="dotted",col="blue")
  })


#############################################################################
# getting Temporal Interests Trends - barplot - by Hour
##############################

output$interests_List_forTrends_byHour <- renderUI({

  freq_table<-as.data.frame(table(ints_GPS$interest1[ints_GPS$interest1!="NA"]),stringsAsFactors = T)
  How_Many_top_trends<- 10
  min_point_for_trend<-100
  freq_table <- freq_table[order(-freq_table[,2]),]
  freq_table_trim<- subset(freq_table,freq_table$Freq>20)
  names<- as.vector(freq_table_trim$Var1)
  selectInput("Chosen_interest_for_trend_byDay", "Choose Option:", as.vector(freq_table_trim$Var1))

})

output$interestTrends_byHour <- renderPlot({

  x<- strptime(ints_GPS$localCreationDate,format = "%Y:%m:%d %H:%M:%S")
  time12<- round(as.integer(strftime(x, "%H")))
  # interest_selection<-c("Nature","Outdoors","Shopping")
  interest_selection<-c("Hiking","Islam","Golf")
  interest_selection<-c("Hiking")
  interest_selection<- c(input$Chosen_interest_for_trend_byHour1,input$Chosen_interest_for_trend_byHour2,input$Chosen_interest_for_trend_byHour3)
  hour_trends<-subset(data.frame(time_split=time12, interest=ints_GPS$interest1),interest %in% interest_selection)

  ggplot(hour_trends, aes(x=time_split)) + geom_density(aes(group=interest, colour=interest, fill=interest), alpha=0.3,adjust=2)+
    theme(panel.background = element_rect(fill = "gray96"))+theme(panel.border = element_rect(fill=NA,color="black", size=0.5,linetype="solid"))+
    scale_x_continuous(breaks= seq(0,24,by = 2),labels=c(paste(seq(0,12,by = 2),"am"),paste(seq(2,12,by = 2),"pm")))+
    labs(x="Time of Day", y="Interest Level") + theme(axis.title = element_text(face="bold",colour = "gray52"),axis.ticks.y = element_blank(),axis.text.y = element_blank())+
    theme(panel.border = element_rect(fill=NA,color="black", size=0.5,linetype ="solid"))+
    geom_vline(xintercept = 9,linetype="dotted",col="blue")+geom_vline(xintercept = 19,linetype="dotted",col="blue")

})

output$UIforhours<- renderUI({
    freq_table<-as.data.frame(table(ints_GPS$interest1[ints_GPS$interest1!="NA"]),stringsAsFactors = T)
    How_Many_top_trends<- 10
    min_point_for_trend<-100
    freq_table <- freq_table[order(-freq_table[,2]),]
    freq_table_trim<- subset(freq_table,freq_table$Freq>20)
    names<- as.vector(freq_table_trim$Var1)
    sidebarLayout(
      sidebarPanel(
        helpText("Understanding how daily inetrest trends vary"),
        selectInput("Chosen_interest_for_trend_byHour1", "Choose Option:", as.vector(freq_table_trim$Var1),selected ="Hiking"),
        selectInput("Chosen_interest_for_trend_byHour2", "Choose Option:", as.vector(freq_table_trim$Var1),selected ="Golf"),
        selectInput("Chosen_interest_for_trend_byHour3", "Choose Option:", as.vector(freq_table_trim$Var1),selected ="Picnic") #freq_table_trim$Var1[3]
        ,width=3),

      mainPanel(
        plotOutput("interestTrends_byHour")
      )
  )
})

})