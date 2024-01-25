install.packages("ggplot2")
install.packages("ggplot2movies")
library(ggplot2)
library(ggplot2movies)

# DATA & AESTHETICS

pl<- ggplot(movies,aes(x=rating))

# GEOMETRY

pl2<- (pl+geom_histogram(binwidth=0.1,color='green',fill='pink',alpha=0.4))

pl3<- pl2+xlab('Movie Rating')+ylab('Count')

print(pl3 + ggtitle("MYTITLE")

pl2<- (pl+geom_histogram(binwidth=0.1,aes(fill..=count..)))
      

install.packages('ggplot2')
library(ggplot2)
df<-mtcars

pl<-ggplot(df,aes(x=wt,y=mpg))

print(pl+geom_point())

library(ggplot2)
library(ggplot2movies)
library(ggthemes)
df <- movies <- movies[sample(nrow(movies), 1000), ]

qplot(rating,data=df,geom='histogram',binwidth=0.1,alpha=0.8)


pl<-ggplot(df,aes(x=rating))
print(pl+geom_histogram())


