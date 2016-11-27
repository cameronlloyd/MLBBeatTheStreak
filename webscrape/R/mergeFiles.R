cols = c("NULL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)

data1 <-read.csv("baseballData_4-3_7-10.csv",colClasses=cols)
n1 = dim(data1)[1]

data2 <-read.csv("baseballData_7-15.csv",colClasses=cols)
n2 = dim(data2)[1]

data3 <-read.csv("baseballData_7-16.csv",colClasses=cols)
n3 = dim(data3)[1]

data4 <-read.csv("baseballData_7-17_8-07.csv",colClasses=cols)
n4 = dim(data4)[1]

data5 <-read.csv("baseballData_9-23.csv",colClasses=cols)
n5 = dim(data5)[1]

data6 <-read.csv("baseballData_10-2.csv",colClasses=cols)
n6 = dim(data6)[1]

result = data.frame(avgHandBatterCar = c(), 
                    avgPlaceBatterCar =c(),
                    avgHandPitcherCar = c(),
                    avgPlacePitcherCar = c(),
                    sampleHandBatterCar = c(),
                    samplePlaceBatterCar=c(),
                    sampleHandPitcherCar=c(),
                    samplePlacePitcherCar=c(),
                    avgHandBatter2015 = c(), 
                    avgPlaceBatter2015 =c(),
                    avgHandPitcher2015 = c(),
                    avgPlacePitcher2015 = c(),
                    sampleHandBatter2015 = c(),
                    samplePlaceBatter2015=c(),
                    sampleHandPitcher2015=c(),
                    samplePlacePitcher2015=c(),
                    avgHandBatter2014 = c(), 
                    avgPlaceBatter2014 =c(),
                    avgHandPitcher2014 = c(),
                    avgPlacePitcher2014 = c(),
                    sampleHandBatter2014 = c(),
                    samplePlaceBatter2014=c(),
                    sampleHandPitcher2014=c(),
                    samplePlacePitcher2014=c(),
                    hit=c(),
                    atBats=c(),
                    pitcherInMonth=c(),
                    batterInMonth=c(),
                    parkFactor=c(),
                    Date=c())

result<-rbind(result,data1)
result<-rbind(result,data2)
result<-rbind(result,data3)
result<-rbind(result,data4)
result<-rbind(result,data5)
result<-rbind(result,data6)

