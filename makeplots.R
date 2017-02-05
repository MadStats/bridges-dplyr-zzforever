library(tidyverse)
library(choroplethr)
library(data.table)

# First read the data into R.
dest = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt"
tmp = fread(dest) 
tmp = as.tbl(tmp)
classes = sapply(tmp, class)
states = read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states = states[-(1:12),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")
dat = list()
dest = rep("", 52)
for(i in 1:52) dest[i] = paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
x16 = ldply(dest, fread, colClasses = classes)  
M = x16

# Then select the variables that I am interested in.
keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008", "COUNTY_CODE_003", "LAT_016", "LONG_017", "TOLL_020", "YEAR_BUILT_027",
         "DECK_COND_058", "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060", "CHANNEL_COND_061", "CULVERT_COND_062")
M = as.tbl(M)
x = select(M, one_of(keep))

# Select the data from Wisconsin.
wi = filter(x, STATE_CODE_001 == 55)

# Plot1, the number of bridges built in each year.
build_year <- wi %>% group_by(YEAR_BUILT_027) %>% dplyr::summarise(count = n())
ggplot(build_year) + geom_line(mapping = aes(y = count, x = YEAR_BUILT_027), color = "limegreen")

# Plot2, relationship between the average condition of different parts and the built year.

condition <- wi %>% group_by(YEAR_BUILT_027) %>% dplyr::summarise(deck = mean(parse_integer(DECK_COND_058, na = "N"), na.rm = TRUE), 
                                                                  supstr = mean(parse_integer(SUPERSTRUCTURE_COND_059, na = "N"), na.rm = TRUE),
                                                                  substr = mean(parse_integer(SUBSTRUCTURE_COND_060, na = "N"), na.rm = TRUE),
                                                                  channel = mean(parse_integer(CHANNEL_COND_061, na = "N"), na.rm = TRUE),
                                                                  culvert = mean(parse_integer(CULVERT_COND_062, na = "N"), na.rm = TRUE))
numyears <- dim(condition)[1]
conditions <- data.frame(year_built = rep(condition$YEAR_BUILT_027, 5), type = rep(c("deck", "supstr", "substr", "channel", "culvert"), each = numyears), condition = c(condition$deck, condition$supstr, condition$substr, condition$channel, condition$culvert))
ggplot(data = conditions, aes(y = condition, x = year_built, colour = type)) + geom_line()

#Plot3, the relationship between the average condition and location of each bridge
min2dec = function(x) {
  as.numeric(substr(x, 1, 2)) + as.numeric(substr(x, 3, 8)) / 6e+05 %>% return
}
wi = mutate(wi, lat = min2dec(LAT_016), lon = min2dec(LONG_017))
wi = filter(wi, lon<100)
num_bridge_wi <- dim(wi)[1]
wi$avg_cond <- numeric(num_bridge_wi)
for(i in 1:num_bridge_wi) {
  wi$avg_cond[i] <- mean(c(as.integer(wi$DECK_COND_058[i]), as.integer(wi$SUPERSTRUCTURE_COND_059[i]), 
                           as.integer(wi$SUBSTRUCTURE_COND_060[i]), as.integer(wi$CHANNEL_COND_061[i])
                           ,as.integer(wi$CULVERT_COND_062[i])), na.rm = TRUE)
}

ggplot(data = wi) + geom_point(mapping = aes(y = lat, x = lon,col = avg_cond))
