
#  ------------------------------------------------------------------------

# plotMap

# df   - data frame with lat long & value columns
# var.lower  - column name of the lower layer plot value vector
# var.upper  - column name of the upper layer plot value vector
# lat  - latitude column
# long - longitude column
# maps.dir - location of the map files
# map no. - which map to use
# main - chart title
# labs.lower - label for lower layer vector
# labs.upper - label for upper layer vector

# Example
# plotMap(df=df,var.lower="var1,var.upper="var2,maps.dir="D:/test",map.no = 6)


#  ------------------------------------------------------------------------




plotMap2Dim <- function(df,var.lower="value1",var.upper="value2",lat="lat",long="long",maps.dir="",map.no=1,main=paste("Geo-spatial analysis"),labs.lower=var.lower,labs.upper=var.upper)
{
  require(ggplot2)
  require(ggmap)
  require(scales)

  setwd(maps.dir)

  map.loc <-  ifelse(map.no == 1,"IndiaMap11.rds",
				ifelse(map.no == 2,"MumbaiMap11.rds",
					ifelse(map.no == 3,"MaharshatraMap7.rds",
						ifelse(map.no == 4,"JamNagarMap12.rds",
							ifelse(map.no == 5,"GujrathMap7.rds",
								ifelse(map.no == 6,"GujrathMaharashtraMap7.rds",
									ifelse(map.no == 7,"NaviMumbaiMap12.rds",NULL))))))
  map <- readRDS(file=paste(maps.dir,"/",map.loc,sep=""))

  assign("map",map,envir = .GlobalEnv)
  assign("var.lower",var.lower,envir = .GlobalEnv)
  assign("var.upper",var.upper,envir = .GlobalEnv)

  df[,"lat"] <- df[,lat]
  df[,"long"] <- df[,long]
  df[,"var1"] <- df[,var.lower]
  df[,"var2"] <- df[,var.upper]

  assign(x = "plot.df",df,envir = .GlobalEnv)

  p <- ggmap(map)
  p <- p +
    geom_point(data=subset(plot.df,!is.na(df[,"var1"])), aes(x=long, y=lat, color="value1"),size=10, alpha=1)+
    geom_point(data=subset(plot.df,!is.na(df[,"var2"])), aes(x=long, y=lat, color="value2"),size=8, alpha=1)+
  scale_colour_manual(name="",values = c("value1"="red","value2"="green"),labels=c("value1" = labs.lower,"value2" = labs.upper))


  # Plot chart on plots pane in GUI
  plot(p)

  # mention file location here
  ggsave(p, file = paste(var.lower,"-",var.upper,".png",sep=""), width = 10, height = 8, type = "cairo-png")

  # Remove datasets
  rm(list=c("map","plot.df","var.lower","var.upper"),envir = .GlobalEnv)

}

#load("D:/Projects/6. Reliance POC/R/RJIO data/Map/datadf.new.rda")
plotMap(df = subset(datadf.new,substr(datadf.new$PINcode,1,1)=="4"),var.lower =    "count",var.upper ="percentage",maps.dir = "D:/Projects/6. Reliance POC/R/Macros/",map.no = 6,labs.lower = "lab1",labs.upper = "lab2")
