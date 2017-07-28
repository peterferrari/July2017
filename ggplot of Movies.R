# https://www.r-bloggers.com/r-a-quick-scrape-of-top-grossing-films-from-boxofficemojo-com/

library(XML)
library(ggplot2)

GetTable <- function(t) {
  table <- readHTMLTable(t)[[2]]
  names(table) <- c("Rank", "Title", "Studio", "Worldwide", "Domestic", "DomesticPct", "Overseas", "OverseasPct", "Year")
  boxdf <- as.data.frame(lapply(table[-1, ], as.character), stringsAsFactors=FALSE)
  boxdf <- as.data.frame(boxdf, stringsAsFactors=FALSE)
  boxdf <- transform(boxdf, Year = ifelse(Year==0, NA, Year))
  return(boxdf)
}

CleanDataFrame <- function(boxdf) {
  clean <- function(col) {
    col <- gsub("$", "", col, fixed = TRUE)
    col <- gsub("%", "", col, fixed = TRUE)
    col <- gsub(",", "", col, fixed = TRUE)
    col <- gsub("^", "", col, fixed = TRUE)
    return(col)
  }
  boxdf <- sapply(boxdf, clean)
  boxdf <- as.data.frame(boxdf, stringsAsFactors=FALSE)
  return(boxdf)
}

BoxOfficeMojoScraper <- function(npages) {
  # This line constructs the URLs
  urls <- paste("http://boxofficemojo.com/alltime/world/?pagenum=", 1:npages, "&p=.htm", sep = "")
  # The next line scrapes every table in the URLs formed
  boxdf <- do.call("rbind", lapply(urls, GetTable))
  # This does the janitor work
  boxdf <- CleanDataFrame(boxdf)
  # The next lines arrange the data to my needs
  cols <- c(1, 4:9)
  boxdf[, cols] <- sapply(boxdf[, cols], as.numeric)
  boxdf$Studio <- as.factor(boxdf$Studio)
  return(boxdf)
}

npages <- 7
box <- BoxOfficeMojoScraper(npages)
## Warning in lapply(X = X, FUN = FUN, ...): NAs introduced by coercion

box2 <- subset(box, Rank<=25)

ggplot(box2) +
  geom_bar(aes(x=reorder(Title, Worldwide), y=Worldwide, fill="Worldwide"), stat = "identity") +
  geom_bar(aes(x=Title, y=Domestic, fill="Domestic"),alpha=.5, stat = "identity") +
  scale_fill_manual(name="Grossing", values=c(Worldwide="#A6CEE3", Domestic="#386CB0")) +
  coord_flip() + 
  labs(x=NULL, y=NULL, title="Top 25 Films by Worldwide Grosses (US$ Millions)")