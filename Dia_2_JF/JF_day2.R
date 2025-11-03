library(ggplot2)

# scatter plot: ######
Fig1 <- ggplot(data = mpg) + geom_point(aes(x = displ, y = hwy))
Fig1
Fig1+theme_bw()


# Example scatter plot with regression line
Fig2 <- Fig1 +  geom_smooth(aes(x = displ, y = hwy),
                            method = 'lm', formula = y ~ x)
c_h <- round(cor(mpg$displ, mpg$hwy, method = 'pearson'),2)
Fig2 <- Fig2 + geom_label(aes(x = 3, y = 45),
                          label = paste0('Corr:',as.character(c_h)), 
                          show.legend = F, fill = 'white', color = 'blue')
Fig2

# Example scatter plot with color
Fig3 <- ggplot(data = mpg) + geom_point(aes(x = displ,
                                            y = hwy, color= class))

Fig3

# Example scatter plot by facets
Fig4 <- Fig3 + facet_wrap(~ class, nrow = 3)
Fig4

# Example scatter plot by facets – remove legend

Fig5 <- Fig4 + theme(legend.position = 'none')
Fig5

# Example scatter plot by facets – change the name of the axes and title
Fig6 <- Fig5+
  xlab('Engine displacement') + ylab('Highway miles per gallon') + 
  ggtitle ('Comsuption vs Displacement')

Fig6

# Example scatter plot by facets – change the margin of the text
Fig7 <- Fig6 + theme(plot.title = element_text(hjust = 0.5,
                                               margin=margin(0,0,10,0),
                                               size = 11),
                     axis.text.x = element_text(margin=margin(0,0,10,0)),
                     axis.text.y = element_text(margin=margin(0,0,0,10)))

Fig7

# Example scatter plot by facets – change style of the grid
Fig8 <- Fig7 + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())

Fig8

# Example scatter plot by facets – change the colors manually and save picture
Fig9 <- Fig8 + scale_color_manual(values = c('red', 'blue', 'orange',
                                             'black', 'yellow',
                                             'pink', 'green'))
Fig9

#para guardar la figura
# ggsave(filename = paste0(file.path('output','test.png')), height = 10, 
#        width = 10 , units = "cm", dpi = 500)

#boxplot ########

## Box-plot of engine-displacement by class of cars
Fig10 <- ggplot(data = mpg, aes(class,displ)) + 
  geom_boxplot(color="red", fill="orange",
               alpha=0.2, width = 0.8) +
  stat_boxplot(geom = 'errorbar', color="red", width = 0.4)+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,
                                  margin=margin(0,0,10,0),
                                  face = 'bold'),
        axis.ticks.x = element_blank()) +
  xlab('Class') + ylab('Engine displacement') +
  ggtitle('Boxplot- Engine displacement')

Fig10

## Box-plot of engine-displacement by class of cars
library(viridis)
Fig11 <- ggplot(data = mpg, aes(x=class,y=displ,fill=class)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) + theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, margin=margin(0,0,10,0),
                                  face = 'bold'), axis.ticks.x = element_blank(),   legend.position="none") + 
  xlab('Class') + ylab('Engine displacement') + ggtitle('Boxplot- Engine displacement')
Fig11


# violin plot ######

Fig12 <- ggplot(data = mpg, aes(class,displ)) + 
  geom_violin(color="red", fill="orange", alpha=0.2)+ theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, margin=margin(0,0,10,0), face = 'bold'),
        axis.ticks.x = element_blank()) +
  xlab('Class') + ylab('Engine displacement') + ggtitle('Violinplot- Engine displacement')

Fig12


# barplot #######

Fig14 <- ggplot(data = mpg) + geom_bar(mapping = aes(x = class), width = 0.5, 
                                       color = 'blue', fill='royalblue') +
  theme(plot.title = element_text(hjust = 0.5,margin=margin(0,0,10,0), size = 11),
        axis.text.x = element_text(margin=margin(0,0,10,0)), axis.text.y = element_text(margin=margin(0,0,0,10)), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = "black", size = 1)) +
  xlab('Class') + ylab('Count') + ggtitle('Frequency of each class')

Fig14


Fig15 <- ggplot(data = mpg) + geom_bar(mapping = aes(x = class, fill = trans), 
                                       position = 'dodge' ) +
  theme(plot.title = element_text(hjust = 0.5,margin=margin(0,0,10,0), size = 11),
        axis.text.x = element_text(margin=margin(0,0,10,0)), axis.text.y = element_text(margin=margin(0,0,0,10)), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = "black", size = 1)) +
  xlab('Class') + ylab('Count') + ggtitle('Frequency of each class and type of transmission')
Fig15


Fig16 <- ggplot(data = mpg) + 
  geom_bar(mapping = aes(x = class, fill = trans), position = 'stack' ) +
  theme(plot.title = element_text(hjust = 0.5,margin=margin(0,0,10,0), size = 11),
        axis.text.x = element_text(margin=margin(0,0,10,0)), axis.text.y = element_text(margin=margin(0,0,0,10)), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = "black", size = 1)) +
  xlab('Class') + ylab('Count') + ggtitle('Frequency of each class and type of transmission')
Fig16


Fig17 <- ggplot(data = mpg) + geom_bar(mapping = aes(x = class, fill = trans), 
                                       position = 'stack' ) + coord_flip() +
  theme(plot.title = element_text(hjust = 0.5,margin=margin(0,0,10,0), size = 11),
        axis.text.x = element_text(margin=margin(0,0,10,0)), axis.text.y = element_text(margin=margin(0,0,0,10)), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = "black", size = 1)) +
  xlab('Class') + ylab('Count') + ggtitle('Frequency of each class and type of transmission')
Fig17


mpg$year <- factor(mpg$year, levels= c('1999', '2008'))
Fig18 <- ggplot(data = mpg)+ geom_tile(aes(x = year, y = manufacturer, fill = hwy),colour='black') +  theme_minimal() +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
        axis.text.x = element_text(hjust = 0.9, vjust = 0.9, angle = 45)) +
  scale_fill_gradient(low="red", high="blue")
Fig18


