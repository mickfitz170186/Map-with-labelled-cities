library(tidyverse)
library(rworldmap)
library(sf)
library(maps)








dir1 <- "C:/Users/kytl324/OneDrive - PHASTAR/Phastar/Phastar AsiaPac/"
# Get city label data
cities <- world.cities %>%
  filter(country.etc %in% c("Australia", "China") 
         & name %in% c("Sydney", "Melbourne", "Newcastle", "Chengdu", "Shanghai", "Brisbane")) %>%
  mutate(lat=ifelse(name=="Newcastle",lat+2,lat),
         long=long+6)

# Join map data and country colour data
status1 <- data.frame(region = c("Australia","Japan","India","China"),
                   status = c("Open","Open","Open","Open"))
map1 <- map_data("world") %>%
  full_join(status1,by="region") %>%
  mutate(status=ifelse(is.na(status),"One day",status))
map1$status <- factor(map1$status, levels = c("Open", "One day"))

# Make the map
jpeg(paste(dir1,"Phastar Asia map 2024-01-16.jpg",sep = ""), width = 1080, height = 1080)
ggplot(map1, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = status)) +
  scale_fill_manual(values=c("#a47b83","#c4c2c4")) +
  coord_sf(xlim = c(65,180), ylim = c(-60,55), expand = FALSE) +
  theme_void() +
  theme(legend.position = "none", legend.text=element_text(size=10)) +
  geom_text(data= cities,aes(x=long, y=lat, label=name), 
            color = "black", fontface = "bold", check_overlap = FALSE, size=8) +
  labs(fill=" ")
dev.off()

# Map with number of statisticians
jpeg(paste(dir1,"Phastar Asia map with N.jpg",sep = ""), width = 1080, height = 1080)
ggplot(map1, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = status)) +
  scale_fill_manual(values=c("#651226","#a47b83","#c4c2c4")) +
  coord_sf(xlim = c(65,180), ylim = c(-60,55), expand = FALSE) +
  theme_void() +
  theme(legend.position = c(0.8, 0.7), legend.text=element_text(size=30)) +
  labs(fill=" ") +
  annotate("text", x = 148, y = -30, label = "11", color="white", fontface = "bold", size=15) +
  annotate("text", x = 78, y = 20, label = "0", color="white", fontface = "bold", size=15) +
  annotate("text", x = 144, y = 38, label = "1", color="#651226", fontface = "bold", size=15) 
dev.off()




