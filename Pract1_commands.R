# BIO8052 Introductory Quantitative Methods
# Commands used in Practical one with comments
# print and readline commands make the script easier to use

print("Before running these commands, save this file in the same folder 
      as your datafiles")
print("Make sure that you have set R to look in your correct folder")
readline("Hit Return to continue, else hit ESC to stop")

# Read and summarise the organic matter data
orgmatt.dat <- read.csv("orgmatt.csv")
print("Means of organic matter at each site")
print(summary(orgmatt.dat))
tapply(orgmatt.dat[,1], orgmatt.dat[,2], mean)
plot(orgmatt ~ site, data=orgmatt.dat, main="Summary of soil organic matter survey",
     ylab="Organic matter content (g)", xlab="Survey site")

# Read and stack the insect diversity dataset
readline("Hit return to import and stack the insect diversity data")
insect.dat <- read.csv("grassland.csv")
print(summary(insect.dat))
insect.stk = stack(insect.dat)
colnames(insect.stk) <- c("diversity", "cutting")
print(summary(insect.stk))
plot(diversity ~ cutting, data=insect.stk, main="Insect diversity in grasslands")

# Read, sort and plot the bluetit data
readline("Hit return to import, sort and plot the bluetit data")
bluetit.dat <- read.csv("bluetit.csv")
print(summary(bluetit.dat))
bluetit.dat2 = bluetit.dat[order(bluetit.dat$gardenfood),]
plot(condition ~ gardenfood, data=bluetit.dat2, type="b", main="Bluetit condition and garden feed")

# Simple summary statistics
print(mean(insect.stk$diversity))
print(tapply(insect.stk$diversity, insect.stk$cutting, mean))

# Function for standard error
se <- function(x) sqrt(var(x) / length(x) )

print(tapply(insect.stk$diversity, insect.stk$cutting, se))
