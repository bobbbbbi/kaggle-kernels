

#The script uses to show malaria estimated and reported deaths, treatments and prevention.
#These plots show years-by year results.



#Load libraries,
require(ggplot2)
require(animation) 
require(dplyr)


#Load data
edeaths <- read.csv('../input/estimated_deaths.csv')
mdeaths <- read.csv('../input/reported_deaths.csv')
treatments <- read.csv('../input/treatments_prevention.csv')


edeaths <- data.frame(edeaths)
mdeaths <- data.frame(mdeaths)

#Load the map data, set color value to -1
s = map_data('world')
s
s$colour = -1


#Malaria estimated  deaths.

#Create a list of years for use in both plots.
years <-c(2000, 2005, 2010, 2013)
years
saveGIF(
  for (yr in years ){
    print(yr)         # Show progress of script.
    
    #Reset values.
    #Disabling this results in old results staying on the chart until they are replaced. this may be useful, but
    #If the data is not being updated regularly, this could over-represent old data. Use caution.
    s$colour = -1     
    
    #Subset only the relevant years data
    yrsubset <- subset(edeaths, YEAR..DISPLAY. == yr) 
    
    # Set country color values to this year's numbers.
    i=1
    while (i <= length(yrsubset$COUNTRY..DISPLAY.)) {
      s$colour[s$region == yrsubset$COUNTRY..DISPLAY.[i]] = yrsubset[i,13]
      i=i+1
    }
    print(
      m <-  ggplot(s, 
                   aes(x=long, 
                       y=lat, 
                       group=group, 
                       fill=colour)) + 
        #Plot the Earth    
        geom_path(data = s,                                            
                  aes(x=long, y=lat, group=group), 
                  colour='black') + 
        scale_fill_gradientn(na.value = 'grey50',                           
                             colours=c('white','red','dark red'),
                             guide = 'colourbar',
                             limits=c(0,100))+ 
        geom_polygon(alpha=1, color = 'black')+                                          
        theme(plot.title = element_text(size = rel(2)),                  
              panel.background = element_rect(fill = 'lightskyblue2')) + 
        ggtitle('WHO estimates number of deaths from malaria by country from 2000-2013 ' )+
        theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=27, hjust=0))
    )
  }
  , movie.name = 'World-estimated-deaths.gif', interval = 0.5, convert = 'convert', ani.width = 1200, 
  ani.height = 700)

#Malaria reported deaths.

#Create a list of years for use in both plots.
years <-sort(c(min(mdeaths$YEAR..DISPLAY.):max(mdeaths$YEAR..DISPLAY.)))
years
saveGIF(
  for (yr in years ){
    print(yr)         # Show progress of script.
    
    #Reset values
    #Disabling this results in old results staying on the chart until they are replaced. this may be useful, but
    #If the data is not being updated regularly, this could over-represent old data. Use caution.
    s$colour = -1     
    
    #Subset only the relevant years data
    yrsubset <- subset(mdeaths, YEAR..DISPLAY. == yr) 
    
    #Set country color values to this year's numbers.
    i=1
    while (i <= length(yrsubset$COUNTRY..DISPLAY.)) {
      s$colour[s$region == yrsubset$COUNTRY..DISPLAY.[i]] = yrsubset[i,13]
      i=i+1
    }
    print(
      m <-  ggplot(s, 
                   aes(x=long, 
                       y=lat, 
                       group=group, 
                       fill=colour)) + 
        #Plot the Earth    
        geom_path(data = s,                                            
                  aes(x=long, y=lat, group=group), 
                  colour='black') + 
        scale_fill_gradientn(na.value = 'grey50',                           
                             colours=c('white','red','dark red'),
                             guide = 'colourbar',
                             limits=c(0,100))+ 
        geom_polygon(alpha=1, color = 'black')+                                          
        theme(plot.title = element_text(size = rel(2)),                  
              panel.background = element_rect(fill = 'lightskyblue2')) + 
        ggtitle(paste0('The reported deaths from malaria within each country/year',yr))+
        theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=27, hjust=0))
    )
  }
  , movie.name = 'World-reported-deaths.gif', interval = 0.5, convert = 'convert', ani.width = 1200, 
  ani.height = 700)


#Malaria treatments and prevention

#Year in treatments data set are in format of YYYY-YYYY
#Converting Year to YYYY formate. 

sub <- treatments %>% filter(treatments$YEAR..DISPLAY. == '2007-2013')
sub1 <- treatments %>% filter(!treatments$YEAR..DISPLAY. == '2007-2013')
sub$YEAR..DISPLAY. <- '2007-2008-2009-2010-2011-2012-2013'

temp<- rbind(sub, sub1)

library(splitstackshape)

treatment<- cSplit(temp, "YEAR..DISPLAY.", "-", direction="long")

treatment <- data.frame(treatment)

years <-sort(c(min(treatment$YEAR..DISPLAY.):max(treatment$YEAR..DISPLAY.)))
years
saveGIF(
  for (yr in years ){
    print(yr)         # Show progress of script.
    
    #Reset values.
    #Disabling this results in old results staying on the chart until they are replaced. this may be useful, but
    #If the data is not being updated regularly, this could over-represent old data. Use caution.
    s$colour = -1     
    
    #Subset only the relevant years data.
    yrsubset <- subset(treatment, YEAR..DISPLAY. == yr) 
    
    #Set country color values to this year's numbers.
    i=1
    while (i <= length(yrsubset$COUNTRY..DISPLAY.)) {
      s$colour[s$region == yrsubset$COUNTRY..DISPLAY.[i]] = yrsubset[i,13]
      i=i+1
    }
    print(
      m <-  ggplot(s, 
                   aes(x=long, 
                       y=lat, 
                       group=group, 
                       fill=colour)) + 
        #Plot the Earth    
        geom_path(data = s,                                            
                  aes(x=long, y=lat, group=group), 
                  colour='black') + 
        scale_fill_gradientn(na.value = 'grey50',                           
                             colours=c('red','green','black'),
                             guide = 'colourbar',
                             limits=c(0,200))+ 
        geom_polygon(alpha=1, color = 'black')+                                          
        theme(plot.title = element_text(size = rel(2)),                  
              panel.background = element_rect(fill = 'lightskyblue2')) + 
        ggtitle(paste0('Insecticide treated net and antimalarial drug use by country/year ',yr))+
        theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=27, hjust=0))
    )
  }
  , movie.name = 'World-treatments.gif', interval = 0.5, convert = 'convert', ani.width = 1200, 
  ani.height = 700)



