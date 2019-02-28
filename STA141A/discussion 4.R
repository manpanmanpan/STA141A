# 141A discussion 4

apt = readRDS("C:/Users/13720/Desktop/STA141A/data/cl_apartments.rds")

# Relation of sqft and price
# How does the relationship change depending on the major cities?

# Examine the sqft and price
hist(apt$sqft)
# How can I see outliers?
plot(sort(apt$sqft))
# Top 5 largest
tail(sort(apt$sqft), 5)
# 200000 is an error: set to NA
apt$sqft[apt$sqft >= 10000] = NA
hist(apt$sqft, breaks = 100)

# Do the same thong for price
hist(apt$price)
plot(sort(apt$price))
tail(sort(apt$price), 10)
apt[which(apt$price == max(apt$price, na.rm = TRUE)), ]
apt$price[apt$price >= 30000000] = 3408
apt[which(apt$price == max(apt$price, na.rm = TRUE)), ]
apt$price[apt$price >= 9900000] = 995
tail(sort(apt$price), 10)


# For personal plotting
plot(x = apt$sqft, apt$price)
# Generally as sqft increases, price increases

# Make a report-worthy plot
big_cities = c('San Francisco', 'Oakland', 'San Jose', 'Sacramento', 'Los Angeles', 'San Diego')
# Use the %in% operator
apt_big6 = apt[apt$city %in% big_cities, ]

apt_big6$city = factor(apt_big6$city)

library(ggplot2)
sp = ggplot(apt_big6, aes(x = sqft, y = price)) + geom_point()
sp = sp + facet_wrap(.~city, nrow = 2)

# We are interested in relationship for apartments up to 2500 sqft, price up to 10000

sp = sp + xlim(c(0,2500)) + ylim(c(0, 10000))

# label x and y, title
sp = sp + xlab('Size') + ylab('Monthly Rent') + ggtitle('Relation')

# ggsave('sqft_price.png', width = 7, height = 4)

# 1. Variance in rent across sqft are different for some cities
# Like SF, maybe because of luxury apartments

# 2. Range of data: SAC doesn't have many expensive houses

# 3. Slope of the linear relationship



# 4. PLot the apartments in SAC, CA by longitude and latitude
# Can we infer where downtown is?
# hint: There's a variable where used by color, that answered the question


# 5. examine the relationship of houses in each county




