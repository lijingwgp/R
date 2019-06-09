###################
### First steps ###
###################
# creating a ggplot
# the dataset we will be using is call mpg enbedded in the ggplot2 package

require(ggplot2)
ggplot2::mpg

# ggplot() creates a coordinate system that you can add layers to
# The first argument of ggplot() is the dataset to use in the graph

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy))

# You complete your graph by adding one or more layers to ggplot()
# The function geom_point() adds a layer of points to your plot, 
# which creates a scatterplot
#
# Each geom function in ggplot2 takes a mapping argument
# This defines how variables in your dataset are mapped to visual properties
#
# The mapping argument is always paired with aes(), and the x and y arguments of aes()
# specify which variables to map to the x and y axes



##########################
### Aesthetic mappings ###
##########################
# You can add a third variable, like class, to a two dimensional scatterplot by 
# mapping it to an aesthetic. An aesthetic is a visual property of the objects in 
# your plot. Aesthetics include things like the size, the shape, or the color of 
# your points

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# You can also set the aesthetic properties of your geom manually

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")



##############
### Facets ###
##############
# One way to add additional variables is with aesthetics. Another way, 
# particularly useful for categorical variables, is to split your plot into facets, 
# subplots that each display one subset of the data

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

# To facet your plot on the combination of two variables, add facet_grid() 
# to your plot call. The first argument of facet_grid() is also a formula

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)



#########################
### Geometric objects ###
#########################
# A geom is the geometrical object that a plot uses to represent data
# For example, bar charts use bar geoms, line charts use line geoms, boxplots use boxplot geoms, and so on
#
# To change the geom in your plot, change the geom function that you add to ggplot()

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

# We can try a different geom object such as a smooth line
# Every geom function in ggplot2 takes a mapping argument
# However, not every aesthetic works with every geom
#
# Here geom_smooth() separates the cars into three lines based on their drv value, 
# which describes a car's drivetrain

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE)

# To display multiple geoms in the same plot, add multiple geom functions to ggplot():

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# or

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# Display different aesthetics in different layers

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

# We can use the same idea to specify different data for each layer
# Here, our smooth line displays just a subset of the mpg dataset, 
# the subcompact cars. The local data argument in geom_smooth() overrides the 
# global data argument in ggplot() for that layer only

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)



###################################
### Statistical transformations ###
###################################
# bar chart

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

# we can generally use geoms and stats interchangeably

ggplot(data = diamonds) +
  stat_count(mapping = aes(x = cut))

# or we might want to override the default stat methods 
# associated with each geom_() function
# here we will use a fake data called demo

demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)
ggplot(data = demo) +
  geom_bar(mapping = aes(x = cut, y = freq), stat = "identity")

# or we might want to display a bar chart of proportion, rather than count

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

# or we might want to draw attentions to the statistical transformation 

ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )



############################
### Position adjustments ###
############################
# we can colour a bar chart using either the colour aesthetic, 
# or, more usefully, fill

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

# Note what happens if you map the fill aesthetic to 
# another variable, like clarity: the bars are automatically 
# stacked. Each colored rectangle represents a combination 
# of cut and clarity

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

# The stacking is performed automatically by the 
# position adjustment specified by the position argument
# If you don't want a stacked bar chart, you can use one of 
# three other options: "identity", "dodge" or "fill"

# position = "identity" will place each object exactly where it falls 
# in the context of the graph. This is not very useful for bars, 
# because it overlaps them. To see that overlapping we either need to 
# make the bars slightly transparent by setting alpha to a small value, 
# or completely transparent by setting fill = NA

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 1/5, position = "identity")
ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

# position = "fill" works like stacking, but makes each set of stacked bars 
# the same height. This makes it easier to compare proportions across groups

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

# position = "dodge" places overlapping objects directly beside one another
# This makes it easier to compare individual values

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

# to avoid the problem of overplotting

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")



##########################
### Coordinate systems ###
##########################
# The default coordinate system is the Cartesian coordinate system where the 
# x and y positions act independently to determine the location of each point.
# There are a number of other coordinate systems that are occasionally helpful.

# coord_flip() switches the x and y axes. this is useful if we want horizontal boxplots

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

# coord_quickmap() sets the aspect ratio correctly for maps. this is important if plotting
# spatial data with ggplot2

nz <- map_data("nz")
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")
ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

# coord_polar() uses polar coordiantes. polar coordinates reveal an interesting 
# connection between a bar chart and a Coxcomb chart

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()

