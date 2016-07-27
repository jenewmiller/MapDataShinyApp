library(plotly)
library(plotrix)
library(RColorBrewer)

group <- 1:6
angle <- 0:90
distance <- 20:100
elevation <- 5:15
n <- 100
dtaSample <-  data.frame( "Group"     = sample(group, n, replace =TRUE)
                        , "Angle"     = sample(angle, n, replace = TRUE)
                        , "Distance"  = sample(distance, n, replace = TRUE)
                        , "Elevation" = sample(elevation, n, replace = TRUE)
                        )
dtaSample$Radians <- dtaSample$Angle*pi/180
dtaSample$xCoords <- (dtaSample$Distance + 4.6) * cos(dtaSample$Angle*pi/180)
dtaSample$yCoords <- (dtaSample$Distance + 4.6) * sin(dtaSample$Angle*pi/180) 
dtaSample$Group <- as.factor(dtaSample$Group)

ggplot(dtaSample, aes( x = xCoords , y = yCoords))+
    geom_point( size = 4, aes(color=Group))



# A working polar plot
        polar.plot( lengths =  valuesClass$df[,3]
            , polar.pos =  valuesClass$df[,2]
            , rp.type = 's'
#            , labels = c(0,90,180,270)
#            , label.pos = c(0,pi/2,pi,3*pi/2)
            , point.symbols = 1 #c(21,22,23,24,25,8)
            , show.grid.labels = FALSE
            , radial.lim = c(0, 100)
            )
# ***********************************
        polar.plot( lengths =  dtaSample$Distance
            , polar.pos =  dtaSample$Angle
            , rp.type = 's'
#            , labels = c(0,90,180,270)
#            , label.pos = c(0,pi/2,pi,3*pi/2)
            , point.symbols = 15 #c(21,22,23,24,25,8)
            , point.col = 'red'
            , show.grid.labels = FALSE
            , radial.lim = c(0, 100)
            )
# ****************************
#' # A pie chart = stacked bar chart + polar coordinates
pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
 geom_bar(width = 1)
pie + coord_polar(theta = "y")






polar.plot( lengths =  dtaSample$Distance
            , polar.pos =  dtaSample$Angle #*pi/180
            , rp.type = 's'
#            , labels = c(0,90,180,270)
#            , label.pos = c(0,pi/2,pi,3*pi/2)
            , point.symbols = 16
            , show.grid.labels = FALSE
            , radial.lim = c(0, max(dtaSample$Distance))
            )


plot_ly( dtaSample
        , r = Distance
        , t
         Angle
#        , type = "scatter"
#        , symbols = 'dot'
#        , color = Elevation
#        , size = 1
        , mode = "markers"
        )


ggplot(dtaSample, aes(x = (Angle*pi/180), y = Distance))+
            geom_point()+
            coord_polar()


table <- "SurveyData201607test"

saveData <- function(data) {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Add the data as a new row
  gs_add_row(sheet, input = data)
}

loadData <- function() {
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Read the data
  gs_read_csv(sheet)
}


gs_gap() %>% 
  gs_copy(to = "Gapminder")

gap <- gs_title("Gapminder")
gap
sheet
sheet <- gs_key(table)
sheet <- gs_add_row(sheet, input = c(10, 20, 30))
