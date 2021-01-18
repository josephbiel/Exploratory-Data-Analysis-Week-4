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
  plot(emissionsYear, totalEmissions)
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
  plot(emissionsYear, totalEmissionsBaltimore)
}

q3<-function() {
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

  plot(rep(1999,4),   baltimore1999ByTypeSum, pch=c(1,2,3,4), xlim=c(1999, 2008), ylim=c(0, 2500))
  points(rep(2002,4), baltimore2002ByTypeSum, pch=c(1,2,3,4))
  points(rep(2005,4), baltimore2005ByTypeSum, pch=c(1,2,3,4))
  points(rep(2008,4), baltimore2008ByTypeSum, pch=c(1,2,3,4))
  
  segments(rep(1999, 4), baltimore1999ByTypeSum, 
           rep(2002, 4), baltimore2002ByTypeSum)
  segments(rep(2002, 4), baltimore2002ByTypeSum, 
           rep(2005, 4), baltimore2005ByTypeSum)
  segments(rep(2005, 4), baltimore2005ByTypeSum, 
           rep(2008, 4), baltimore2008ByTypeSum)
  View(c(baltimore1999ByTypeSum, baltimore2002ByTypeSum,
         baltimore2005ByTypeSum, baltimore2008ByTypeSum))
}