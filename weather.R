library(here)
library(weathercan)
library(ggplot2)
library(leaflet)
library(lubridate)

strwb_flats <- c(49.067047, -120.894262)

stn <- stations_search(coords=strwb_flats, dist=35, interval="day" )

leaflet(stn) %>%
    addTiles() %>%
    addWMSTiles(baseUrl = "https://services.arcgisonline.com/arcgis/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}.png",
                layers = "1", options = WMSTileOptions(format = "image/png", transparent = TRUE)) %>%
    addMarkers(lng=stn$lon, lat=stn$lat, popup=paste0(stn$station_name," ",stn$elev,"m ", stn$start, "-", stn$end),
               popupOptions(closeButton = FALSE)) %>% 
    addCircleMarkers(lng=strwb_flats[2], lat=strwb_flats[1], label="Trailhead", color='red')

if(!dir.exists("data_out"))
  dir.create("data_out")
if(!file.exists(here("data_out/manning.csv"))) {
d <- weather_dl(station_ids = stn$station_id, interval = "day")
write.csv(d, here("data_out/manning.csv"), row.names = FALSE)
}
d <- read.csv(here("data_out/manning.csv"))

d$yday <- yday(d$date)
names(d)
str(d)
d$year<- as.numeric(d$year)

djj <- d[grep("06|07",d$month),] # june and july
tripdate <- yday(ymd("2021-07-10"))
# daily air temperature
f1<- ggplot(djj, aes(y=mean_temp, x=yday,colour=year, group=interaction(station_name,year))) +
  geom_path() +
  scale_colour_viridis_c() +
  facet_wrap(~paste0(station_name, " ", elev, " m")) +
  geom_hline(aes(yintercept=0), col="dodgerblue") +
  geom_vline(aes(xintercept=tripdate)) +
  theme_bw()
# Temps below freezing possible, especially up high
f2 <- ggplot(djj, aes(y=min_temp, x=yday,colour=year, group=interaction(station_name,year))) +
  geom_path() +
  scale_colour_viridis_c() +
  facet_wrap(~paste0(station_name, " ", elev, " m")) +
  geom_hline(aes(yintercept=0), col="dodgerblue") +
  geom_vline(aes(xintercept=tripdate)) +
  theme_bw()
# Warm temps up to and over 30C are possible
f3 <- ggplot(djj, aes(y=max_temp, x=yday,colour=year, group=interaction(station_name,year))) +
  geom_path() +
  scale_colour_viridis_c() +
  facet_wrap(~paste0(station_name, " ", elev, " m")) +
  geom_hline(aes(yintercept=0), col="dodgerblue") +
  geom_vline(aes(xintercept=tripdate)) +
  theme_bw()

# precip
# Rainfall is quite likely, big storms possible
f4 <- ggplot(djj, aes(y=total_rain, x=yday,colour=year, group=interaction(station_name,year))) +
  geom_path() +
  scale_colour_viridis_c() +
  facet_wrap(~paste0(station_name, " ", elev, " m")) +
  geom_hline(aes(yintercept=0), col="dodgerblue") +
  geom_vline(aes(xintercept=tripdate)) +
  theme_bw()
f5 <- ggplot(djj, aes(y=total_snow, x=yday,colour=year, group=interaction(station_name,year))) +
  geom_path() +
  scale_colour_viridis_c() +
  facet_wrap(~paste0(station_name, " ", elev, " m")) +
  geom_hline(aes(yintercept=0), col="dodgerblue") +
  geom_vline(aes(xintercept=tripdate)) +
  theme_bw()
f6 <- ggplot(djj, aes(y=snow_grnd, x=yday,colour=year, group=interaction(station_name,year))) +
  geom_path() +
  scale_colour_viridis_c() +
  facet_wrap(~paste0(station_name, " ", elev, " m")) +
  geom_hline(aes(yintercept=0), col="dodgerblue") +
  geom_vline(aes(xintercept=tripdate)) +
  theme_bw()


figs <- mget(ls(pattern="^f[[:digit:]]"))
if(!dir.exists("fig"))
  dir.create("fig")
purrr::iwalk(figs, function(o, name) {
  ggsave(o, filename=paste0(here("fig"),"/", name, ".png"), height=6, width=10, units="in", dpi=400)
  })

