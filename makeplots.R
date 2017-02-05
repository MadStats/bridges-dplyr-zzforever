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

