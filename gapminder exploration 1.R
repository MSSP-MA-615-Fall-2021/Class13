

library(tidyverse)
library(gapminder)



# library(printr) 
library(RColorBrewer) ## to chose different colors for the graph




## see https://www.rdocumentation.org/packages/gapminder/versions/0.3.0



data(gapminder)

str(gapminder)

dim(gapminder)

## note that the tibble tells you what kind of variables
## are in your table
gapminder

summary(gapminder)

## table gives out a contingency table of counts 
## for each combination of factor levels


## explorting the factors

table(gapminder$continent, gapminder$year)


## you can show the same data in a barchart 

ggplot(gapminder, aes(x=continent)) + geom_bar()


## this could be better with colors

ggplot(gapminder, aes(x=continent, fill=continent)) + geom_bar()


## Wait!!  What's being counted here?


ggplot(gapminder, aes(x=continent, fill=continent)) + 
  geom_bar(aes(y=..count../12)) +
  labs(y="Number of countries") +
  guides(fill=FALSE)


## What if I want to make other adjustments?

## 

barchart1 <- ggplot(gapminder, aes(x=continent, fill=continent)) + 
  geom_bar(aes(y=..count../12)) +
  labs(y="Number of countries") +
  guides(fill=FALSE)



## Now I can easily change the scale on the chare


barchart1 + coord_trans(y="sqrt")


## or flip it


barchart1 + coord_flip()


## or plot it on polar coordinates (what does this do?)


barchart1 + coord_polar()




### exploring continuous variales  -- distributions

gplot(data=gapminder, aes(x=lifeExp)) + 
  geom_density()




ggplot(data=gapminder, aes(x=lifeExp)) + 
  geom_density(size=1.5, fill="pink", alpha=0.3)




ggplot(data=gapminder, aes(x=lifeExp)) + 
  geom_density(size=1.5, fill="pink", alpha=0.5) +
  geom_histogram(aes(y=..density..), binwidth=4, color="black", fill="lightblue", alpha=0.5)






ggplot(data=gapminder, aes(x=lifeExp, fill=continent)) +
  geom_density(alpha=0.3)


####  Time series

gapminder %>%
  group_by(continent, year) %>%
  summarise(lifeExp=median(lifeExp)) %>%
  ggplot(aes(x=year, y=lifeExp, color=continent)) +
  geom_line(size=1) + 
  geom_point(size=1.5)


##########################  using layers

plt <- ggplot(data=gapminder,
              aes(x=gdpPercap, y=lifeExp))
plt

plt + geom_point()

plt + geom_point(aes(color=continent))

plt + geom_point(aes(color=continent)) +
  geom_smooth(method="loess") 

plt + geom_point(aes(color=continent)) +
  geom_smooth(method="loess") +
  scale_x_log10()



#############################################################

## #################   EDA  example 2


## trafford data labs
## https://www.trafforddatalab.io/



 ## https://www.trafforddatalab.io/graphics_companion/#Correlation

## ggtheme -- 

## source("https://raw.githubusercontent.com/traffordDataLab/assets/601e80334e0d78dfe913685561196b8b6fc278a7/theme/ggplot2/theme_lab.R")

df <- filter(gapminder, country == "Argentina") %>% 
  mutate(year = as.Date(paste(year, "-01-01", sep = "", format='%Y-%b-%d')))

source("https://raw.githubusercontent.com/traffordDataLab/assets/601e80334e0d78dfe913685561196b8b6fc278a7/theme/ggplot2/theme_lab.R")

library(ggthemes)
library(sf) 
library(rnaturalearth)

ggplot(df, aes(x = year, y = lifeExp)) +
  geom_line(colour = "#fc6721", size = 1) +
  geom_point(colour = "#fc6721", size = 2) +
  scale_x_date(breaks = df$year, date_labels = "%Y") +
  scale_y_continuous(limits = c(0, max(df$lifeExp)), labels = scales::comma) +
  labs(title = "",
       subtitle = "Life expectancy in Argentina, 1952-2007",
       caption = "Source: Gapminder.org  |  @traffordDataLab",
       x = "",
       y = "Age (years)") +
  theme_lab() +
  theme(panel.grid.major.x = element_blank())  

## + theme_gray()  
## + theme_economist()
## + theme_wsj()



###  World Map 1  ##################################


df <- gapminder %>%
  filter(year == 2007) %>%
  left_join(country_codes) %>% 
  rename("iso_a3" = "iso_alpha")



world <- ne_countries(type = "countries",  returnclass = 'sf')
sf <- ne_countries(type = "countries",  returnclass = 'sf') %>% 
  left_join(., df, by = "iso_a3", sort = FALSE) %>% 
  filter(!is.na(country)) %>% 
  select("country", "continent" = "continent.y", "year", "lifeExp", "pop", "gdpPercap", "geometry")



ggplot(sf, aes(fill = lifeExp)) +
  geom_sf(data = world, fill = "#f0f0f0", colour = "white") +
  geom_sf(alpha = 0.8, colour = "white", size = 0.1) +
  scale_fill_gradientn(colours = brewer.pal(5, "Oranges"),
                       name = "Age (Years)",
                       guide = guide_colourbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(50, units = "mm"),
                         title.position = 'top',
                         title.hjust = 0.5,
                         label.hjust = 0.5)) +
  labs(title = "",
       subtitle = "Life expectancy, 2007",
       caption = "Source: Gapminder.org  |  @traffordDataLab",
       x = NULL, 
       y = NULL) +
  theme_lab() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom") +
  coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000
           +datum=WGS84 +units=m +no_defs",
           datum = NA)







###################################################

####### World Map 2


library(rworldmap) ## plotting the data on World Map
library(countrycode) ## Converting the country name to Country code
library(Hmisc)


dim(gapminder_unfiltered)


colnames(gapminder_unfiltered)


sum(complete.cases(gapminder)) ## No missing values found

describe(gapminder_unfiltered)  ## see Hmisc



gapminder <-  gapminder_unfiltered
gapminder$countrycode <- countrycode(gapminder$country, 'country.name', 'iso3c')

sPDF <- joinCountryData2Map(gapminder %>% filter(year == 2007)
                            ,joinCode = "ISO3"
                            ,nameJoinColumn = "countrycode"
                            ,mapResolution = "coarse"
                            , verbose = T)




colourPalette <- brewer.pal(7,'GnBu')

mapParams <- mapCountryData(sPDF,
                            nameColumnToPlot="gdpPercap",
                            addLegend=FALSE,
                            colourPalette=colourPalette )

do.call(addMapLegend
        ,c(mapParams
           ,legendLabels="all"
           ,legendWidth=0.5
           ,legendIntervals="data"
           ,legendMar = 2))




