library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)

#read files
properties <- fread('properties_2016.csv')
transactions <- fread('train_2016_v2.csv')
sample_submission <- fread('sample_submission.csv')

#change names
properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)

transactions <- transactions %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

properties <- properties %>% 
  mutate(tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
         flag_fireplace = ifelse(flag_fireplace=="Y",1,0),
         flag_tub = ifelse(flag_tub=="Y",1,0))

#make date, group by date and count
tmp <- transactions %>% mutate(year_month = make_date(year=year(date),month=month(date)))
tmp %>% 
  group_by(year_month) %>% count() %>% 
  ggplot(aes(x=year_month,y=n)) +
  geom_bar(stat="identity", fill="red")+
  geom_vline(aes(xintercept=as.numeric(as.Date("2016-10-01"))),size=1) #black bar on october 2016

transactions %>% 
  ggplot(aes(x=logerror)) + 
  geom_histogram(bins=400, fill="red")+ 
  ylab("Count")+coord_cartesian(x=c(-0.5,0.5))

transactions = transactions %>% mutate(abs_logerror = abs(logerror))
transactions %>%
  ggplot(aes(x = abs_logerror)) +
  geom_histogram(bins = 400,fill = "red")+
  ylab("Count") + coord_cartesian(x = c(0,.5))

transactions %>%
  mutate(year_month = make_date(year = year(date),month = month(date))) %>%
  group_by(year_month) %>% summarize(mean_abs_logerror = mean(abs_logerror)) %>%
  ggplot(aes(x = year_month, y = mean_abs_logerror)) + geom_line() + geom_point()

transactions %>%
  mutate(year_month = make_date(year = year(date), month = month(date))) %>%
  group_by(year_month) %>% summarize(meanlogerror = mean(logerror)) %>%
  ggplot(aes(x = year_month,y = meanlogerror)) + geom_line() + geom_point()

#percent of each column that is missing a value
missing_values <- properties %>% summarize_each(funs(sum(is.na(.))/n()))
missing_values = gather(missing_values, key = "feature", value = "missing_pct")
missing_values %>% #reorder treats first argument as categorical and reorders it 
  #according to the second variable (usually numeric)
  ggplot(aes(x = reorder(feature,-missing_pct), y = missing_pct)) + 
  geom_bar(stat = "identity", fill = "red") +
  coord_flip()

good_features <- filter(missing_values, missing_pct<0.75)

#detects any string with 'num_'

#correlation with number features
vars = good_features$feature[str_detect(good_features$feature,'num_')]

cor_tmp = transactions %>% left_join(properties, by = "id_parcel")
tmp = cor_tmp %>% select(c(vars,"abs_logerror"))

#returns a correlation plot from all variables in tmp
corrplot(cor(tmp, use = "complete.obs"), type = "lower")
#find correlations between num of things in a house
#log error correlates negatively with bathrooms and bathroom calc
#useful to find which criteria correlates to absolute log error

#this is for area features
vars = good_features$feature[str_detect(good_features$feature,'area_')]

tmp = cor_tmp %>% select(c(vars, "abs_logerror"))
corrplot(cor(tmp, use = "complete.obs"), type = "lower")

#tax features
#set diff takes deletes the values with duplicates
#so we find everything with tax, then add tax_delinquency and tax_year
#this deletes those because they're not analytical values
vars = setdiff(good_features$feature[str_detect(good_features$feature,'tax_')],
               c("tax_delinquency","tax_year"))
tmp = cor_tmp %>% select(c(vars,"abs_logerror"))
corrplot(cor(tmp,use = "complete.obs"), type = "lower")


#***************************************
#with log error (instead of abs log error)

#num
vars = good_features$feature[str_detect(good_features$feature,'num_')]

cor_tmp = transactions %>% left_join(properties, by="id_parcel") 
tmp = cor_tmp %>% select(c(vars,"logerror"))

corrplot(cor(tmp, use="complete.obs"),type="lower")

#tax
vars <- setdiff(good_features$feature[str_detect(good_features$feature,'tax_')],c("tax_delinquency","tax_year"))

tmp <- cor_tmp %>%  select(one_of(c(vars,"logerror")))

corrplot(cor(tmp, use="complete.obs"), type="lower")
#no correlation between taxes and error
#means tax is likely not a factor in the error

#gg plot of build year into a density function
cor_tmp %>%
  ggplot(aes(x = build_year)) + geom_line(stat = "density", color = "red")


#graph shows that log error decreases over time generally
#last line of code here just decides the range of the y axis to make graph look better
cor_tmp %>%
  group_by(build_year) %>%
  summarize(mean_abs_logerror = mean(abs(logerror)),n()) %>%
  ggplot(aes(x = build_year, y = mean_abs_logerror)) +
  geom_smooth() + geom_point(color = "red") +
  coord_cartesian(ylim = c(0,.25))

tmp1 = cor_tmp %>%
  group_by(build_year) %>%
  summarize(mean_abs_logerror = mean(abs(logerror)),n())

tmp1 %>% ggplot(aes(x = build_year, y = mean_abs_logerror)) + geom_point(color = "red") + stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm',formula=y~x)

model1 = lm(tmp1$mean_abs_logerror~tmp1$build_year)

#geom_smooth makes a good regression line

#seems like a low correlation between logerror and year
cor_tmp %>%
  group_by(build_year) %>%
  summarize(mean_logerror = mean(logerror)) %>%
  ggplot(aes(x = build_year, y = mean_logerror)) + geom_point(col = "red") +
  geom_smooth() + coord_cartesian(ylim = c(-.05,.075))

#make a new column with all quantiles you define
transactions = transactions %>% mutate(percentile = cut(abs_logerror,quantile(abs_logerror, probs = c(0,.1,.25,.75,.9,1)), include.lowest = T,labels = F))

tmp1 = transactions %>%
  filter(percentile == 1) %>%
  sample_n(5000) %>%
  left_join(properties, by = "id_parcel")

tmp2 = transactions %>%
  filter(percentile == 5) %>%
  sample_n(5000) %>%
  left_join(properties, by = "id_parcel")
tmp3 = transactions %>%
  filter(percentile == 3) %>%
  sample_n(5000) %>%
  left_join(properties, by = "id_parcel")

tmp1 = tmp1 %>% mutate(type = "best_fit")
tmp2 = tmp2 %>% mutate(type = "worst_fit")
tmp3 = tmp3 %>% mutate(type = "typical_fit")

# joins into one big table, columns matched by name, missing columns have na
tmp = bind_rows(tmp1, tmp2, tmp3)

tmp = tmp %>% mutate(type = factor(type, levels = c("worst_fit","typical_fit", "best_fit")))

col_pal = "Set1"

#checking with latitude
tmp %>% ggplot(aes(x = latitude, fill = type, color = type)) + geom_line(stat = "density", size = 1.2)

tmptrans = transactions %>% left_join(properties, by = "id_parcel")

tmptrans %>% ggplot(aes(x = latitude, y = abs_logerror)) + geom_smooth(color = "red")

#where does zestimate over or underpredict

tmptrans = tmptrans %>% mutate(overunder = ifelse(logerror < 0, "under","over"))

tmptrans %>% group_by(overunder) %>%
  ggplot(aes(x = latitude, y = abs_logerror, fill = overunder, color = overunder)) + geom_smooth(color = "red")

tmptrans %>% group_by(overunder) %>%
  ggplot(aes(x = longitude, y = abs_logerror, fill = overunder, color = overunder)) + geom_smooth(color = "red")

leaflet() %>%
  addTiles() %>%
  fitBounds(-118.5,33.8,-118.25,34.15) %>% 
  addRectangles(-118.5,33.8,-118.25,34.15) %>% 
  addMiniMap()

tmptrans %>% group_by(overunder) %>%
  ggplot(aes(x = area_total_calc, y = abs_logerror, fill = overunder, color = overunder)) + geom_smooth(color = "red") + coord_cartesian(xlim = c(0,5000))

lat = range(properties$latitude/1e06,na.rm = T) 
lon = range(properties$longitude/1e06, na.rm = T)         
tmp = properties %>% sample_n(2000) %>%
  select(id_parcel, longitude, latitude) %>%
  mutate(lon = longitude/1e6, lat = latitude/1e6) %>%
  select(id_parcel,lat,lon) %>%
  left_join(transactions, by = "id_parcel")

leaflet(tmp) %>%
  addTiles() %>%
  fitBounds(lon[1], lat[1], lon[2], lat[2]) %>%
  addCircleMarkers(stroke = F) %>%
  addMiniMap()

tmp <- transactions %>% 
  sample_n(2000) %>% 
  left_join(properties,by="id_parcel") %>% 
  select(id_parcel,longitude,latitude, abs_logerror) %>% 
  mutate(lon=longitude/1e6,lat=latitude/1e6) %>% 
  select(id_parcel,lat,lon, abs_logerror)


qpal <- colorQuantile("YlOrRd", tmp$abs_logerror, n = 7)

leaflet(tmp) %>% 
  addTiles() %>% 
  fitBounds(lon[1],lat[1],lon[2],lat[2]) %>% 
  addCircleMarkers(stroke=FALSE, color=~qpal(abs_logerror),fillOpacity = 1) %>% 
  addLegend("bottomright", pal = qpal, values = ~abs_logerror,title = "Absolute logerror",opacity = 1) %>% 
  addMiniMap()
