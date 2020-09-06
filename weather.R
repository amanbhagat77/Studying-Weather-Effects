
df2 <- harmful.evtype %>% filter(Injuries.Total < 1000) #Only the events which caused below 1000 Injuries
df3 <- harmful.evtype %>% filter(Injuries.Total > 1000) #Only the events which caused above 1000 Injuries
df3$EVTYPE <- gsub("THUNDERSTORM WIND", "THUNDERSTORM",df3$EVTYPE)#Renamin the Event Type
df3$EVTYPE <- gsub("HURRICANE/TYPHOON", "TYPHOON",df3$EVTYPE)#Renamin the Event Type




plot1 <- ggplot(data = df3 , aes(y = Injuries.Total , x = EVTYPE, fill = EVTYPE,group = 1))+
    geom_bar(stat = "identity")+
    guides(fill = FALSE)+
    ggtitle("Injuries Over 1000")+
    xlab("Event Type")+
    ylab("Total number of Injuries")+
    theme(text = element_text(size=8),axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1))+
    scale_y_continuous(labels = comma)

    plot2 <- ggplot(data = df2 , aes(y = Injuries.Total , x = EVTYPE, color = "Blue",group = 1,lwd = 0.5))+
      geom_line()+
      guides(fill = FALSE)+
      ggtitle("Injuries below 1000")+
      xlab("Event Type")+
      ylab("Total number of Injuries")+
      theme(text = element_text(size=8),axis.text.x = element_blank())+
      guides(color = FALSE,lwd = FALSE)

    grid.arrange(plot1 , plot2 , nrow = 2,top = "Total number of people Injured by different Harmful Weather Events")
    
    data.after.2000 <- weather.copy %>% filter(weather.copy$BGN_DATE > "2000/01/01")
    data.after.1980 <- weather.copy %>% filter(weather.copy$BGN_DATE > "1980/01/01")
    data.after.1990 <- weather.copy %>% filter(weather.copy$BGN_DATE > "1990/01/01")
    harmful.evtype.2000 <- data.after.2000 %>% group_by(EVTYPE) %>% summarize(Injuries.Total = sum(INJURIES))
    harmful.evtype.1980 <- data.after.1980 %>% group_by(EVTYPE) %>% summarize(Injuries.Total = sum(INJURIES))
    harmful.evtype.1990 <- data.after.1990 %>% group_by(EVTYPE) %>% summarize(Injuries.Total = sum(INJURIES))
    harmful.evtype <- harmful.evtype[order(-harmful.evtype$Injuries.Total),]
    head(harmful.evtype)
    plot(harmful.evtype$EVTYPE,harmful.evtype$Injuries.Total,type = 'l')
    
    g1 <- ggplot(data = harmful.evtype.2000,aes(x = EVTYPE, y = Injuries.Total,color = "red",group = 1))+
      geom_line()+
      ggtitle("Events Recorded after 2000")+
      guides(fill = FALSE)+
      theme(axis.text.x = element_blank())+
      guides(color = FALSE)
    
    g2 <- ggplot(data = harmful.evtype.1990,aes(x = EVTYPE, y = Injuries.Total,color = "blue",group = 1))+
      geom_line()+
      guides(fill = FALSE)+
      ggtitle("Events Recorded after 1990")+
      theme(axis.text.x = element_blank())+
      guides(color = FALSE)
    
    g3 <- ggplot(data = harmful.evtype.1980,aes(x = EVTYPE, y = Injuries.Total,color = "green",group = 1))+
      geom_line()+
      ggtitle("Events Recorded after 1980")+
      guides(fill = FALSE)+
      theme(axis.text.x = element_blank())+
      guides(color = FALSE)
    
    g4 <- ggplot(data = harmful.evtype,aes(x = EVTYPE, y = Injuries.Total,color = "yellow",group = 1))+
      geom_line()+
      guides(fill = FALSE)+
      ggtitle("Events Recorded after 1950")+
      theme(axis.text.x = element_blank())+
      guides(color = FALSE)
    
    grid.arrange(g1 , g2 , g3, g4, nrow = 2,ncol = 2)