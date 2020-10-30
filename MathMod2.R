# #################################
# Все переменные имеют корректный тип данных
# Повторяющиеся переменные убраны
# Из имен переменных убраны размерности
# Всем переменам заданы их реальные размерности
# Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной
# Категориальные переменные должны быть факторами
# Категории переменной из имени должны быть убраны
# Коды категориальных переменных заменены их категориями
# Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84
# Виды должны быть переименованы на латыне
# #########################################
install.packages("tidyverse")
library(tidyverse)
#Все переменные имеют корректный тип данных

#Повторяющиеся переменные убраны

dbh mm
HR

citytrees = citytrees %>% select(-`dbh (mm)`, -HR)

# Из имен переменных убраны размерности

citytrees = citytrees %>% rename(dbh = `dbh (m)`)
citytrees = citytrees %>% rename(Ht = `Ht (m)`)
citytrees = citytrees %>% rename(Clearance_Ht = `Clearance Ht (m)`)
citytrees = citytrees %>% rename(Crown_Depth = `Crown Depth (m)`)
citytrees = citytrees %>% rename(Average_Radial_Crown_spread = `Average Radial Crown spread (m)`)
citytrees = citytrees %>% rename(Total_Mean_Radial_Crown_Spread = `Total Mean Radial Crown Spread (m)`)
citytrees = citytrees %>% rename(Crown_Diameter = `Crown Diameter (m)`)
citytrees = citytrees %>% rename(Stem_diameter_Jan_2017 = `Stem diameter Jan 2017 (mm)`)
citytrees = citytrees %>% rename(Annual_Girth_Increment = `Annual Girth Increment (mm)`)
citytrees = citytrees %>% rename(Two_yr_dia_gain = `2yr dia gain (mm)`)
citytrees = citytrees %>% rename(Total_NSEW_Radial_Crown_Spread = `Total N,S,E,W Radial Crown Spread (m)`)

# Всем переменам заданы их реальные размерности
install.packages("units")

library(units)
units(citytrees$dbh) = as_units("m")
units(citytrees$Ht) = as_units("m")
units(citytrees$Clearance_Ht) = as_units("m")
units(citytrees$Crown_Depth) = as_units("m")
units(citytrees$Average_Radial_Crown_spread) = as_units("m")
units(citytrees$Total_NSEW_Radial_Crown_Spread) = as_units("m")
units(citytrees$Total_Mean_Radial_Crown_Spread) = as_units("m")
units(citytrees$Crown_Diameter) = as_units("m")
units(citytrees$Stem_diameter_Jan_2017) = as_units("mm")
units(citytrees$Two_yr_dia_gain) = as_units("mm")
units(citytrees$Annual_Girth_Increment) = as_units("mm")
units(citytrees$`Predicted crown diamet using combined formulla`) = as_units("m")
units(citytrees$`Predicted Crown Diameter`) = as_units("m")

citytrees %>% as.data.frame()

# Если какая-то переменная является ошибкой другой переменной, она должна быть убрана и добавлена в виде ошибки к основной переменной

citytrees = citytrees %>% mutate(Crown_Diameter_Using_Combined_Formulla_Error` - Crown_Diameter)
citytrees = citytrees%>% mutate(Crown_Diameter_Using_Combined_Formulla_Error = `Predicted Crown Diameter` - Crown_Diameter)
citytrees = citytrees %>% select(-Difference, Diference)

# Категориальные переменные должны быть факторами

library(forcats)

names(citytrees)

citytrees$`Age Index 1=Y 2=SM 3=EM 4=M`
citytrees = citytrees %>% mutate(`Age Index 1=Y 2=SM 3=EM 4=M` = as.numeric(`Age Index 1=Y 2=SM 3=EM 4=M`))

citytrees = citytrees %>%
  mutate(AgeIndex = as_factor(`Age Index 1=Y 2=SM 3=EM 4=M`)) %>%
  mutate(AgeIndex = fct_recode(AgeIndex,Y = "1", SM = "2",EM = "3", M = "4"))

citytrees$AgeIndex[citytrees$AgeIndex == "<NA>"]

citytrees$AgeIndex

citytrees$`Data Set      1=Norwich                0= Peterborough`
citytrees = citytrees %>% 
  mutate(DataSet = as_factor(`Data Set      1=Norwich                0= Peterborough`)) %>%
  mutate(DataSet = fct_recode(DataSet, Norwich = "1", Peterborough = "0"))

citytrees$DataSet

citytrees$`Pruning Index 5 = pruned within 5yrs  10 pruned between 5 and 10yrs`
citytrees = citytrees %>%
  mutate(PruningIndex = as_factor(`Pruning Index 5 = pruned within 5yrs  10 pruned between 5 and 10yrs`))%>% 
  mutate(PruningIndex = fct_recode(PruningIndex,`pruned within 5yrs` = "5", `pruned between 5 and 10yrs` = "10"))

citytrees$PruningIndex
citytrees$`Type of Prunning None= 0 CR= 1 Other = 2 Both = 3`    
citytrees = citytrees %>%
mutate(TypeOfPruning = as_factor(`Type of Prunning None= 0 CR= 1 Other = 2 Both = 3`)) %>%
mutate(TypeOfPruning = fct_recode(TypeOfPruning,None = "0", CR = "1", Other = "2", Both = "3"))  
citytrees$TypeOfPruning

citytrees$`Soil Code 1=sand and gravel 2= Clay 3=silt`
citytrees = citytrees %>%
  mutate(SoilCode = as_factor(`Soil Code 1=sand and gravel 2= Clay 3=silt`))%>% 
  mutate(SoilCode = fct_recode(SoilCode,`Sand and Gravel` = "1", Clay = "2", Slit = "3"))
citytrees$SoilCode

# Виды должны быть переименованы на латыне

# maple - Acer platanoides, 
# Oak - Quercus robur,
# Silver birch - Betula pendula, 
# Sycamore - Platanus occidentali

citytrees$Species
citytrees$Species[citytrees$Species == "Oak"] = "Quercus robur"    
citytrees$Species[citytrees$Species == "Norway maple"] = "Acer platanoides"
citytrees$Species[citytrees$Species == "Norway Maple"] = "Acer platanoides"
citytrees$Species[citytrees$Species == "Silver Birch"] = "Betula pendula"
citytrees$Species[citytrees$Species == "Sycamore"] = "Platanus occidentalis"

# Должны быть созданы переменные координат(lat,lon) в британской системе координат(с учетом кодов квадратов) и в WGS84
library(stringr)

citytrees$`Grid Reference`
coord = str_replace_all(citytrees$`Grid Reference`,' ','')
coord_N = str_trunc(coord, 12, "right", ellipsis = "") %>% str_trunc(5,"left", ellipsis = "")
coord_E = str_trunc(coord, 7, "right", ellipsis = "") %>% str_trunc( 5, "left", ellipsis = "")
quadr = str_trunc(coord, 2, "right", ellipsis = "")
table_c = data.frame(as.integer(coord_E), as.integer(coord_N), quadr)

names(table_c)=c("E", "N", "quadr")
head(table_c)
table_c = na.exclude(table_c)

table_c = table_c %>% mutate("Easting_BC" = case_when(
  quadr == "TF" ~ E +600000,  
  quadr == "TG" ~ E +700000,  
  quadr == "TL" ~ E +600000,  
))

  table_c = table_c %>% mutate("Northing_BC" = case_when(
    quadr == "TF" ~ N +300000,  
    quadr == "TG" ~ N +300000,    
    quadr == "TL" ~ N +200000,    
))
  
  table_c = na.exclude(table_c)

  
install.packages("sf") 

library(sf)

table_WGS = 
  table_c %>%
  st_as_sf(coords = c("Easting_BC", "Northing_BC"), crs = 27700) %>%
  st_transform(4326) %>%
  st_coordinates() %>% as.data.frame()

table_WGS = data.frame(Lat = table_WGS$Y, Lon = table_WGS$X)
table_WGS %>% head

citytrees$`Grid Reference`[1]
table_c[1,]
table_WGS[1,]

coord = cbind(table_c,table_WGS)
head(coord)

citytrees
