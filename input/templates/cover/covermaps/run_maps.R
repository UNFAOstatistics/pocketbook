## 
setwd("~/btsync/faosync/pocketbooks/pocketbook/input/templates/cover/covermaps")
rm(list=ls(all=TRUE));gc();.rs.restartR() 
rmarkdown::render("maps_for_covers.R")

file.remove(list.files(pattern = ".md"))

system("rsync -arv --exclude=run_maps.R  ~/btsync/faosync/pocketbooks/pocketbook/input/templates/cover/covermaps/ muuankarski@kapsi.fi:public_html/fao/RSPB15/misc/covermaps")
setwd("~/btsync/faosync/pocketbooks/pocketbook/")

