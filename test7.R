library(ggplot2)
library(scatterpie)
library(dplyr)


d <- data.frame(x=rnorm(5), y=rnorm(5))
d$A <- abs(rnorm(5, sd=1))
d$B <- abs(rnorm(5, sd=2))
d$C <- c(1, 2, 3, 5, 6)

m2 <- ggplot() + 
    geom_scatterpie(aes(x = x, y = y), data = d, cols = c("A", "B", "C"))
print(m2)
test <- ne_countries(scale=110, type="countries", continent="africa")
# d <- data.frame(x = rnorm(51))
# d$A <- abs(rnorm(51, sd=1))
# d$B <- abs(rnorm(51, sd=2))
# d$label_x <- rnorm(51)
# d$label_y <- rnorm(51)

test2 <- test#[, c("label_x", "label_y", "pop_rank")]
# test2 <- data.frame(test$label_x, test$label_y, test$pop_rank)
# test2$label_x <- abs(test2$label_x)
# test2$label_y <- abs(test2$label_y)
test2 <- test2 %>% as.data.frame
print(test2)
# transform(test2, char = as.numeric(char))
m <- ggplot() + 
    geom_scatterpie(aes(x=label_x, y=label_y), data=test2, cols=c("pop_rank", "name_len")) + coord_fixed()
# print(m)
# set.seed(123)
# long <- rnorm(50, sd=100)
# lat <- rnorm(50, sd=50)
# d <- data.frame(long=long, lat=lat)
# d <- with(d, d[abs(long) < 150 & abs(lat) < 70,])
# n <- nrow(d)
# d$region <- factor(1:n)
# d$A <- abs(rnorm(n, sd=1))
# d$B <- abs(rnorm(n, sd=2))
# d$C <- abs(rnorm(n, sd=3))
# d$D <- abs(rnorm(n, sd=4))
# d[1, 4:7] <- d[1, 4:7] * 3
# head(d)

# test <- ne_countries(scale=110, type="countries", continent="africa")
# t <- ggplot() + geom_scatterpie(aes(x=label_x, y=label_y, group=region), data=test,
#                            cols=c(test$name_len)) + coord_equal()

# print(t)