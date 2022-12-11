
pacman::p_load(tidyverse, sf, tidycensus)

# Import a polygon shapefile
pluto = read_sf("Data/mappluto_22v2_shp/MapPLUTO.shp")
ct = read_sf('Data/CensusBlocksAvgValues/')
ct$BoroCT2020 <- as.numeric(ct$BoroCT2020)
EnergyPop = read.csv("Data/Yanchaos Energy and Population/totalEngeryAndDayNightPopTractLevel.csv")
EnergyPop$BoroCT2020 <- as.numeric(EnergyPop$BoroCT2020)

#replace 0 values in pluto for bldg year to NA
pluto["YearBuilt"][pluto["YearBuilt"] == 0] <- NA

pluto %>% write_sf('Data/mappluto_22v2_shp/MapPLUTO_RplcdYearBuilt.shp',overwrite=T)



p = pluto %>%
  # sample_n(1000) %>% 
  mutate(
   bldgGroup =  case_when(
      str_starts(BldgClass, 'A|B|C|D|L|N|S')  ~ 'Residential',
      str_starts(BldgClass, 'E|F|G|H|I|J|K|O|P|T|U|W|Y')  ~ 'Commercial',  
      
      str_starts(BldgClass, 'A|B|C|D|L|N|S|E|F|G|H|I|J|K|O|P|T|U|W|Y', negate = T)  ~ 'Others',  
    )
  ) %>%  select(BldgClass, bldgGroup) %>%  
  st_join(ct %>%  select(GEOID) %>%  st_transform(st_crs(pluto))  , join = st_intersects, largest = F) 
  



bdg_pct = p %>% 
  st_drop_geometry() %>% 
  group_by(GEOID, bldgGroup) %>% 
  summarise( count = n())  %>%
  pivot_wider(names_from = bldgGroup, values_from = count) %>%
  mutate_all(coalesce, 0) %>% 
  rename(oo = 'NA') %>% 
  mutate(Others = Others + oo) %>% 
  select(!oo)  %>% 
  mutate( sum = sum(Commercial, Residential, Others),
          Residential = Residential / sum,
          Commercial = Commercial /sum,
          Others = Others /sum,
          ) 



census_api_key('f3dfdb294bba61f585a2ddf74b11e030a1bca112')
income2020 <- get_acs(
  state = "NY",
  county = c('New York', 'Richmond', 'Bronx', 'Queens', 'Kings'),
  geography = "tract",
  variables = "B19013_001",
  # geometry = TRUE,
  year = 2020
) %>%  mutate(income2020 = estimate)

income2019<- get_acs(
  state = "NY",
  county = c('New York', 'Richmond', 'Bronx', 'Queens', 'Kings'),
  geography = "tract",
  variables = "B19013_001",
  # geometry = TRUE,
  year = 2019
) %>%  mutate(Income2019 = estimate)

PopCensus2019<- get_acs(
  state = "NY",
  county = c('New York', 'Richmond', 'Bronx', 'Queens', 'Kings'),
  geography = "tract",
  variables = "B01001_001",
  # geometry = TRUE,
  year = 2019
) %>%  mutate(PopCensus2019 = estimate)  %>%  mutate_at('estimate', coalesce, 0) %>% select('GEOID','PopCensus2019')


PopCensus2020 <- get_acs(
  state = "NY",
  county = c('New York', 'Richmond', 'Bronx', 'Queens', 'Kings'),
  geography = "tract",
  variables = "B01001_001",
  # geometry = TRUE,
  year = 2020
) %>%  mutate(PopCensus2020 = estimate)  %>%  mutate_at('estimate', coalesce, 0) %>% select('GEOID','PopCensus2020')




ct_blg = ct %>%  left_join(bdg_pct, by = 'GEOID') %>% 
  left_join(income2019 %>%  select(GEOID, income2019),  by = 'GEOID')  %>% 
  left_join(income2020 %>%  select(GEOID, income2020),  by = 'GEOID') %>%  
  left_join(PopCensus2019, by='GEOID')  %>% 
  left_join(PopCensus2020, by='GEOID') 
# %>%  
  # mutate_at('PopCensus2019', coalesce, 0)

ct_blg %>%
  ggplot()+
  geom_sf(aes(fill = PopCensus2019 ), size = 0.2, color = 'black')



ct_blg %>%  write_sf('Data/CensusBldgTypes/Census_BldgTypes.geojson',overwrite=T)




names(ct_blg)
