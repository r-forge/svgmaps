#################################################################
#
# Exploring the structure of an osmar object
#
#################################################################
library(osmar)
library(SVGAnnotation)
library(reshape)

# load example data (eihornallee)
src <- osmsource_api(url = "http://api.openstreetmap.org/api/0.6/")
cbox <- center_bbox(11.503819,48.10425, 3000, 3000)
heidi <- center_bbox(11.549631,48.11481, 3000, 3000)
shuai <- center_bbox(11.548387, 48.157999,3000, 3000)
home2b <- center_bbox(11.472902, 48.10766,4000, 4000)
home <- get_osm(cbox, src)
home2 <- get_osm(home2b, src)
#home.sp <- as_sp(home)
heidi <- get_osm(heidi, src)
save(home, heidi, file="osm_object.rda")
load("../data/osm_object.rda")
