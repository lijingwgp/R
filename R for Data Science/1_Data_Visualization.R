####################
### Introduction ###
####################

# the dataset we will be using is call mpg enbedded in the ggplot2 package

require(ggplot2)
ggplot2::mpg

# creating a ggplot
# ggplot() creates a coordinate system that you can add layers to. The first argument of ggplot() is the dataset 
# to use in the graph. So ggplot(data = mpg) creates an empty graph.

# You complete your graph by adding one or more layers to ggplot(). The function geom_point() adds a layer of 
# points to your plot, which creates a scatterplot. 

# Each geom function in ggplot2 takes a mapping argument. This defines how variables in your dataset are mapped to 
# visual properties. The mapping argument is always paired with aes(), and the x and y arguments of aes() 
# specify which variables to map to the x and y axes.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy))



##########################
### Aesthetic Mappings ###
##########################

# You can convey information about your data by mapping the aesthetics in your plot to the variables in your dataset.
# For example, you can map the colors of your points to the class variable to reveal the class of each car.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# size
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

# transparency
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# shape
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))



##############
### Facets ###
##############

# Another way, particularly useful for categorical variables, is to split your plot into facets, 
# subplots that each display one subset of the data.
# To facet your plot by a single variable, use facet_wrap(). The first argument of facet_wrap() should be a formula.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

# To facet your plot on the combination of two variables, add facet_grid() to your plot call. The first argument of 
# facet_grid() is also a formula.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)



##########################
### Geometrics Objects ###
##########################

# To change the geom in your plot, change the geom function that you add to ggplot(). 
# For instance, to make the plots above, you can use this code:

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = TRUE)

# To display multiple geoms in the same plot, add multiple geom functions to ggplot():

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

# This, however, introduces some duplication in our code. Imagine if you wanted to change the y-axis to 
# display cty instead of hwy. You'd need to change the variable in two places, and you might forget to update one.

# You can avoid this type of repetition by passing a set of mappings to ggplot(). ggplot2 will treat these mappings
# as global mappings that apply to each geom in the graph.

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# If you place mappings in a geom function, ggplot2 will treat them as local mappings for the layer.

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

# You can use the same idea to specify different data for each layer.

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)



###################################
### Statistical Transformations ###
###################################

# Next, let's take a look at a bar chart.

ggplot2::diamonds
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

# You can generally use geoms and stats interchangeably. For example, you can recreate the previous plot 
# using stat_count() instead of geom_bar():

ggplot(data = diamonds) + 
  stat_count(mapping = aes(x = cut))

# You can learn which stat a geom uses by inspecting the default value for the stat argument. For example, 
# ?geom_bar shows that the default value for stat is "count", which means that geom_bar() uses stat_count(). 
# stat_count() is documented on the same page as geom_bar()

# You can typically use geoms without worrying about the underlying statistical transformation. 
# There are three reasons you might need to use a stat explicitly:

# 1. You might want to override the default stat.

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

# 2. You might want to override the default mapping from transformed variables to aesthetics. 
# For example, you might want to display a bar chart of proportion, rather than count:

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = stat(prop), group=1))

# 3. You might want to draw greater attention to the statistical transformation in your code.

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )



############################
### Position Adjustments ###
############################

# There's one more piece of magic associated with bar charts. You can colour a bar chart using either the 
# colour aesthetic, or, more usefully, fill:

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

# Note what happens if you map the fill aesthetic to another variable, like clarity: 
# the bars are automatically stacked.

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

# If you don't want a stacked bar chart, you can use one of three other options: "identity", "dodge" or "fill".
# 1. position = "identity" will place each object exactly where it falls in the context of the graph. 
# This is not very useful for bars, because it overlaps them. 

ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 0.5, position = "identity")

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")

# 2. position = "fill" works like stacking, but makes each set of stacked bars the same height. 
# This makes it easier to compare proportions across groups.

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")

# position = "dodge" places overlapping objects directly beside one another. This makes it easier to 
# compare individual values.

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

# There's one other type of adjustment that's not useful for bar charts, but it can be very useful for scatterplots. 
# Add jittering

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), position = "jitter")



##########################
### Coordinate Systems ###
##########################

# coord_flip() switches the x and y axes. This is useful (for example), if you want horizontal boxplots.

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

# coord_quickmap() sets the aspect ratio correctly for maps. This is very important if you're plotting 
# spatial data with ggplot2

nz <- map_data("nz")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

ggplot(nz, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

# coord_polar() uses polar coordinates. Polar coordinates reveal an interesting connection 
# between a bar chart and a Coxcomb chart.

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
