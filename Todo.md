# To Do FINAL

Make all the text changes requested

Live database connection for deployed app (include some kind of visual to show how all tools, scrips, and other components interact)

Don't know what IDW scheme you're using, but we'll want to make that configurable. See example below, based on the variogram.

Change Graph time from 40 days back to 3

do variogram

make compare locations more intuitive

config file to my app

add a legend for the markers




On the Air Quality Map Tab
-instead of "Current Air Quality Index" as the title, refer to the time period you're graphing, it looks like the past 48 hours maybe? "Current" suggests only a right-now reading to my mind.
-for the AQI legend, flip the order so it reads the same way the graphs read (with good on the bottom)
-Add an axis label for the y-axis
-In that same vein, eventually clean up those to look less code-like, no underscores, switch (f) to Farenheit spelled out









Maybeboard:



Neighborhood focus for analyze/compare tabs

Structuring the analyze and compare tabs around neighborhoods, instead of individual sensors with their sometimes strange names, will be more intuitive for our audience. If we can make a new variable that categorizes each reading/sensor by neighborhood, we could then have a dropdown on these pages where users can select the neighborhood they are interested in.

If time allows, having the ability to compare “Neighborhood X” to “Greater Cincinnati” or even “Cincinnati” to “Newport” might be useful.

Newport neighborhoods and Cincnnati neighborhoods

Let's think about configuration, or options. Some things could be a toggle, or a menu, or a checkbox, or.... but whatever we can build in as an option, the better.

-For your Temperature legend/scale - did you source that from anywhere specific like the EPA scale? If not, it might be best to do gradient coloring as opposed to the more rigid blocking of 0-32 being cold, etc. Something like this could work.
