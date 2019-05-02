library(xkcd)
library(ggplot2)

#data points of the thesis
thesis <- data.frame(article=1:3, length= c(637, 314, 120))

#plotting parameters
xrange <- c(1,5)
yrange <- c(0,650)

ratioxy <- diff(xrange)/diff(yrange)

mapping <- aes(x, y, scale, ratioxy, angleofspine,
               anglerighthumerus, anglelefthumerus,
               anglerightradius, angleleftradius,
               anglerightleg, angleleftleg, angleofneck)

#parameters to plot the little man
dataman <- data.frame( x= c(1.09,4.7), y=c(80, 550),
                       scale = c(35, 80),
                       ratioxy = ratioxy,
                       angleofspine = -pi/2 ,
                       anglerighthumerus = c(-pi/6, -pi/6),
                       anglelefthumerus = c(-pi/2 - pi/6, -pi/2 - pi/6),
                       anglerightradius = c(pi/5, -pi/5),
                       angleleftradius = c(pi/5, -pi/5),
                       angleleftleg = 3*pi/2 + pi / 12 ,
                       anglerightleg = 3*pi/2 - pi / 12,
                       angleofneck = runif(1, 3*pi/2-pi/10, 3*pi/2+pi/10))

#parameters to plot the different four lines in the plot
datalines <- data.frame(xbegin=c(1.19,4.5,3,3),ybegin=c(105,560,145,145),
                        xend=c(1.5,3.95,4.7,4.7), yend=c(130,590,40,360),
                        color=c("a", "a", "b", "c"), size=c("a", "a", "b", "c"))
#the rectangle
rectangle <- data.frame(x=3.5,y=1)

#and here it goes.
ggplot()+
  geom_point(data=thesis, aes(article, length),  size=4)+
  xkcdman(mapping, dataman)+
  xkcdline(data=datalines, aes(x=xbegin,y=ybegin,xend=xend,yend=yend, color=color, size=size),
           xjitteramount = c(0.12, 0.12, .5, .5))+
  scale_color_manual(values=c("black","chartreuse4","brown4"))+
  scale_size_manual(values=c(.5,2,2))+
  geom_smooth(data=thesis, aes(article, length), method="lm", 
              se = F, formula= (y ~ exp(-x)), color="gray10")+
  xkcdrect(aes(xmin = x,
               xmax= x +0.3,
               ymin=y,
               ymax = y + 500), rectangle)+
  annotate("point", x=4, y=60, shape=1, size=4)+
  annotate("text", x=1.6, y = 210,
           label = "Current me:\nHey, you writing\n one paper a week in\n the future or what?", family="xkcd" ) +
  annotate("text", x=3.7, y = 650,
           label = "Future me: No man, it looked brighter \nfrom the other side, writing perfomance\n back to an article a year, tops", family="xkcd" ) +
  annotate("text", x=3.62, y = 300,
           label = "DEFENSE", angle=90, family="xkcd", size=7 ) +
  annotate("text", x=4.3, y = 130,
           label = "Current \n trajectory", family="xkcd",color="chartreuse4", size=4 ) +
  annotate("text", x=4.25, y = 30,
           label = "Pending to submit", family="xkcd", size=3 ) +
  ylab("DAYS FROM START TO SUBMISSION")+
  xlab("ARTICLE NUMBER")+
  xkcdaxis(xrange, yrange)+
  theme_xkcd()+
  theme(legend.position = "none")

#Thank you XKCD for being a great source of distraction and procastination along my PhD.
