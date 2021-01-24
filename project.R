readProjectFiles<-function() {
  NEI <<- readRDS("summarySCC_PM25.rds")
  SCC <<- readRDS("Source_Classification_Code.rds")
}

q1<-function()
{
  totalEmissions1999<-sum(subset(NEI, NEI$year==1999)$Emissions)
  totalEmissions2002<-sum(subset(NEI, NEI$year==2002)$Emissions)
  totalEmissions2005<-sum(subset(NEI, NEI$year==2005)$Emissions)
  totalEmissions2008<-sum(subset(NEI, NEI$year==2008)$Emissions)
  totalEmissions<-c(totalEmissions1999, 
                    totalEmissions2002, 
                    totalEmissions2005, 
                    totalEmissions2008)
  emissionsYear<-c(1999, 2002, 2005, 2008)
  
  png("q1.png")
  plot(emissionsYear, totalEmissions, ylim=c(0,8000000))
  dev.off()
}

q2<-function() {
  totalEmissionsBaltimore1999<-
      sum(subset(NEI, NEI$year==1999 & NEI$fips=="24510")$Emissions)
  totalEmissionsBaltimore2002<-
      sum(subset(NEI, NEI$year==2002 & NEI$fips=="24510")$Emissions)
  totalEmissionsBaltimore2005<-
      sum(subset(NEI, NEI$year==2005 & NEI$fips=="24510")$Emissions)
  totalEmissionsBaltimore2008<-
      sum(subset(NEI, NEI$year==2008 & NEI$fips=="24510")$Emissions)
  totalEmissionsBaltimore<-c(totalEmissionsBaltimore1999, 
                             totalEmissionsBaltimore2002, 
                             totalEmissionsBaltimore2005, 
                             totalEmissionsBaltimore2008)
  emissionsYear<-c(1999, 2002, 2005, 2008)
  png("q2.png")
  plot(emissionsYear, totalEmissionsBaltimore, ylim=c(0,3500))
  dev.off()
}

q3<-function() {
  typeFactors<-factor(c("NON-ROAD","NONPOINT","ON-ROAD","POINT"))
  
  baltimore1999<-subset(NEI, NEI$year==1999 & NEI$fips=="24510")
  baltimore1999$type<-as.factor(baltimore1999$type)
  baltimore1999ByType<-split(baltimore1999, baltimore1999$type)
  baltimore1999ByTypeSum<-c(with(baltimore1999ByType$"NON-ROAD", sum(Emissions)),
                            with(baltimore1999ByType$NONPOINT,   sum(Emissions)),
                            with(baltimore1999ByType$"ON-ROAD",  sum(Emissions)),
                            with(baltimore1999ByType$POINT,      sum(Emissions)))
  
  baltimore2002<-subset(NEI, NEI$year==2002 & NEI$fips=="24510")
  baltimore2002$type<-as.factor(baltimore2002$type)
  baltimore2002ByType<-split(baltimore2002, baltimore2002$type)
  baltimore2002ByTypeSum<-c(with(baltimore2002ByType$"NON-ROAD", sum(Emissions)),
                            with(baltimore2002ByType$NONPOINT,   sum(Emissions)),
                            with(baltimore2002ByType$"ON-ROAD",  sum(Emissions)),
                            with(baltimore2002ByType$POINT,      sum(Emissions)))
  
  baltimore2005<-subset(NEI, NEI$year==2005 & NEI$fips=="24510")
  baltimore2005$type<-as.factor(baltimore2005$type)
  baltimore2005ByType<-split(baltimore2005, baltimore2005$type)
  baltimore2005ByTypeSum<-c(with(baltimore2005ByType$"NON-ROAD", sum(Emissions)),
                            with(baltimore2005ByType$NONPOINT,   sum(Emissions)),
                            with(baltimore2005ByType$"ON-ROAD",  sum(Emissions)),
                            with(baltimore2005ByType$POINT,      sum(Emissions)))
  
  baltimore2008<-subset(NEI, NEI$year==2008 & NEI$fips=="24510")
  baltimore2008$type<-as.factor(baltimore2008$type)
  baltimore2008ByType<-split(baltimore2008, baltimore2008$type)
  baltimore2008ByTypeSum<-c(with(baltimore2008ByType$"NON-ROAD", sum(Emissions)),
                            with(baltimore2008ByType$NONPOINT,   sum(Emissions)),
                            with(baltimore2008ByType$"ON-ROAD",  sum(Emissions)),
                            with(baltimore2008ByType$POINT,      sum(Emissions)))
  
  data1999<-data.frame(typeFactors, rep(1999,4), baltimore1999ByTypeSum)
  data2002<-data.frame(typeFactors, rep(2002,4), baltimore2002ByTypeSum)
  data2005<-data.frame(typeFactors, rep(2005,4), baltimore2005ByTypeSum)
  data2008<-data.frame(typeFactors, rep(2008,4), baltimore2008ByTypeSum)
  
  colnames(data1999)<-c("Type", "Year", "Emissions")
  colnames(data2002)<-c("Type", "Year", "Emissions")
  colnames(data2005)<-c("Type", "Year", "Emissions")
  colnames(data2008)<-c("Type", "Year", "Emissions")
  
  t<-qplot(x=Year, y=Emissions, data=data1999, geom="point", color=Type) +
    coord_cartesian(xlim=c(1999,2008), ylim=c(0,2500)) +
    geom_segment(aes(x=Year,y=Emissions,xend=data2002$Year,yend=data2002$Emissions)) +
    geom_point(data=data2002) +
    geom_segment(aes(x=data2002$Year,y=data2002$Emissions,xend=data2005$Year,yend=data2005$Emissions)) +
    geom_point(data=data2005) +
    geom_segment(aes(x=data2005$Year,y=data2005$Emissions,xend=data2008$Year,yend=data2008$Emissions)) +
    geom_point(data=data2008)
  
  ggsave("q3.png", dpi=72)
}

q4<-function() {
  shortNameContainsCoal<-subset(SCC, grepl("Coal", Short.Name))
  coal1999<-subset(NEI, (year==1999) & (SCC %in% shortNameContainsCoal$SCC))
  coal2002<-subset(NEI, (year==2002) & (SCC %in% shortNameContainsCoal$SCC))
  coal2005<-subset(NEI, (year==2005) & (SCC %in% shortNameContainsCoal$SCC))
  coal2008<-subset(NEI, (year==2008) & (SCC %in% shortNameContainsCoal$SCC))
  
  totalCoalEmissions1999<-sum(coal1999$Emissions)
  totalCoalEmissions2002<-sum(coal2002$Emissions)
  totalCoalEmissions2005<-sum(coal2005$Emissions)
  totalCoalEmissions2008<-sum(coal2008$Emissions)
  
  totalCoalEmissions<-c(totalCoalEmissions1999, 
                        totalCoalEmissions2002, 
                        totalCoalEmissions2005, 
                        totalCoalEmissions2008)
  
  emissionsYear<-c(1999, 2002, 2005, 2008)
  
  png("q4.png")
  plot(emissionsYear, totalCoalEmissions)
  dev.off()
}

q5<-function(upperLimit=350, yaxisName="Baltimore Vehical Emissions", callPng=TRUE) {
  shortNameContainsVeh<-subset(SCC, grepl("Veh", Short.Name))
  
  baltimoreVeh1999<-subset(NEI, (fips=="24510") & (year==1999) & (SCC %in% shortNameContainsVeh$SCC))
  baltimoreVeh2002<-subset(NEI, (fips=="24510") & (year==2002) & (SCC %in% shortNameContainsVeh$SCC))
  baltimoreVeh2005<-subset(NEI, (fips=="24510") & (year==2005) & (SCC %in% shortNameContainsVeh$SCC))
  baltimoreVeh2008<-subset(NEI, (fips=="24510") & (year==2008) & (SCC %in% shortNameContainsVeh$SCC))
  
  totalBaltimoreVehEmissions1999<-sum(baltimoreVeh1999$Emissions)
  totalBaltimoreVehEmissions2002<-sum(baltimoreVeh2002$Emissions)
  totalBaltimoreVehEmissions2005<-sum(baltimoreVeh2005$Emissions)
  totalBaltimoreVehEmissions2008<-sum(baltimoreVeh2008$Emissions)
  
  totalBaltimoreVehEmissions<-c(totalBaltimoreVehEmissions1999, 
                                totalBaltimoreVehEmissions2002, 
                                totalBaltimoreVehEmissions2005, 
                                totalBaltimoreVehEmissions2008)
  
  emissionsYear<-c(1999, 2002, 2005, 2008)
  
  if (callPng) {png("q5.png")}
  plot(emissionsYear, totalBaltimoreVehEmissions, ylim=c(0,upperLimit), ylab=yaxisName)
  if (callPng) {dev.off()}
  }

q6<-function() {
  png("q6.png")
  q5(upperLimit=5000, yaxisName="Vehical Emissions", callPng=FALSE)
  shortNameContainsVeh<-subset(SCC, grepl("Veh", Short.Name))
  
  losAngelesVeh1999<-subset(NEI, (fips=="06037") & (year==1999) & (SCC %in% shortNameContainsVeh$SCC))
  losAngelesVeh2002<-subset(NEI, (fips=="06037") & (year==2002) & (SCC %in% shortNameContainsVeh$SCC))
  losAngelesVeh2005<-subset(NEI, (fips=="06037") & (year==2005) & (SCC %in% shortNameContainsVeh$SCC))
  losAngelesVeh2008<-subset(NEI, (fips=="06037") & (year==2008) & (SCC %in% shortNameContainsVeh$SCC))
  
  totalLosAngelesVehEmissions1999<-sum(losAngelesVeh1999$Emissions)
  totalLosAngelesVehEmissions2002<-sum(losAngelesVeh2002$Emissions)
  totalLosAngelesVehEmissions2005<-sum(losAngelesVeh2005$Emissions)
  totalLosAngelesVehEmissions2008<-sum(losAngelesVeh2008$Emissions)
  
  totalLosAngelesVehEmissions<-c(totalLosAngelesVehEmissions1999, 
                                 totalLosAngelesVehEmissions2002, 
                                 totalLosAngelesVehEmissions2005, 
                                 totalLosAngelesVehEmissions2008)
  
  emissionsYear<-c(1999, 2002, 2005, 2008)
  
  points(emissionsYear, totalLosAngelesVehEmissions, pch=2)
  legend(1999, y=3000, c("Baltimore","Los Angeles"), pch=c(1,2))
  dev.off()
}
