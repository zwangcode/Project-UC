p <- ggplot(data=mpg,
            aes(x=cty, y=hwy))
p + geom_point()

p <- ggplot(mpg,
              aes(x=cty, y=hwy,
                  colour=factor(year)))
p + geom_point()
p + geom_point() + stat_smooth()

p <- ggplot(mpg,
            aes(x=cty,y=hwy))
p + geom_point(aes(colour=factor(year))) + stat_smooth()

p + geom_point(aes(colour=factor(year))) + stat_smooth()+
    scale_color_manual(values =c('blue','red'))

p + geom_point(aes(colour=factor(year),size=displ)) + stat_smooth()+
    scale_color_manual(values =c('blue2','red4'))

