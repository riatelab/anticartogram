#################################################################
# CARTOGRAMS & ANTI CARTOGRAMS
# V. JURY, N. LAMBERT, R. YSEBAERT, 2019
#################################################################

# Packages

library("sf")
library("cartogram")
library("cartography")

# Data import & formatting

# Cartogram

# Anti Cartogram

library(sf)
library(cartogram)

# Import géométries IGN 
fr_com <- st_read(dsn = "layers/2019/COMMUNE_CARTO.shp")
fr_com$CODE_EPCI <- as.character(fr_com$CODE_EPCI)


# Simplifier geometries
library(rmapshaper)
fr_com2 <- ms_simplify(fr_com, keep = 0.01)


# Agréger par EPCI 
fr_epci <- fr_com2 %>% 
  group_by(CODE_EPCI) %>% 
  summarise(POP = sum(POPULATION))


fr_epci2 <- ms_simplify(fr_epci, keep = 0.2)


# Anticartogramme calculation


# # Nettoyer les géométries
# library(lwgeom)
# fr_com <- st_make_valid(fr_com)

# 
# 
# # Nettoyer les géométries (2)
# fr_epci <- st_make_valid(fr_epci)
# plot(st_geometry(fr_epci))




head(fr_epci)
fr_epciplot(fr_ign4$geometry)

# Cartogram 
cartog <- cartogram_cont(fr_epci2, weight = "POP", itermax = 10, prepare = "none")
st_write(cartog, "layers/cartog_POP.shp", delete_layer = TRUE)


# Anti-cartogramme
# 1/N Calculation
fr_epci2$POP_INV <- 1/fr_epci2$POP
anticartog <- cartogram_cont(fr_epci2, weight = "POP_INV", itermax = 10, prepare = "none")
st_write(anticartog, "layers/anticartog_POP.shp", delete_layer = TRUE)

# Export des cartogrammes 
st_write(obj = cartog, dsn = "export/cartogram.gpkg", layer = "cartogram", 
         delete_layer = TRUE, quiet = TRUE)
st_write(obj = anticartog, dsn = "export/cartogram.gpkg", layer = "anticartogram", 
         delete_layer = TRUE, quiet = TRUE)


# La tronche des 2 cartogrammes
par(mfrow = c(1,2), mar = c(0,0,0,0))
plot(st_geometry(cartog))
plot(st_geometry(anticartog))


# Importer les données du numérique
library(readxl)

num1 <- read_excel(path = "data/open-data-deploiements-thd-2019-t2.xlsx", sheet = "EPCI", 
                   skip = 4)

num2 <- read_excel(path = "data/open-data-deploiements-thd-2019-t2.xlsx", sheet = "FttH par EPCI", 
                   skip = 4)


num2 <- num2[num2$Champ == "Locaux Raccordables",]

num <- cbind(num1,num2[,2:length(num2)])

head(num)

# Jointure avec les cartogrammes

cartog <- merge(x = cartog, y = num, by.x = "CODE_EPCI", by.y = "Siren EPCI",
                all.x = TRUE)

anticartog <- merge(x = anticartog, y = num, by.x = "CODE_EPCI", by.y = "Siren EPCI",
                    all.x = TRUE)


# Calcul ratios
cartog$log <- cartog$Logements + cartog$Établissements
anticartog$log <- anticartog$Logements + anticartog$Établissements
cartog$fibre <- cartog$`T2 2019`/cartog$log * 100
anticartog$fibre <- anticartog$`T2 2019`/cartog$log * 100




# Cartography
summary(anticartog$fibre)

library(cartography)

breaks <- c(0,0.5,1,5,10,25,50,120)


par(mar=c(0,0,0,0))
choroLayer(cartog, var = "fibre", breaks = breaks, nclass = 7, 
           col = carto.pal(pal1 = "blue.pal", n1 = 4, pal2 = "red.pal", n2 = 3),
           border = NA)

choroLayer(anticartog, var = "fibre", breaks = breaks, nclass = 7, 
           col = carto.pal(pal1 = "blue.pal", n1 = 4, pal2 = "red.pal", n2 = 3),
           border = "white")







####
# test comune

# Import géométries IGN 
fr_ign2019 <- st_read(dsn = "layers/2019/COMMUNE_CARTO.shp")

fr_ign4 <- ms_simplify(fr_ign2019, keep = 0.01)

# Cartogram  > trop lourd pour moi !!
test4 <- cartogram_cont(fr_ign4, weight = "POPULATION", itermax = 10, prepare = "none")
st_write(test4, "layers/test4.shp")

