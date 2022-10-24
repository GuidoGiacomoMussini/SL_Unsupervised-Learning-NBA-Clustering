#Correlation Standing:
corrstanding <- function(dataframe, perc) {
corr <- round(cor(dataframe), 2) %>% data.frame()
ncol <- colnames(corr)
nrow <- rownames(corr)
corrnames <-c()
corrvalues <-c()

for(i in 1: length(ncol)) {
  for(j in 1: length(nrow)) {
    if(corr[i,j] > perc & corr[i,j] < 1 ) {
      corrnames <- append(corrnames, paste(ncol[i], nrow[j], sep = " ")) 
      corrvalues <- append(corrvalues, corr[i,j])
    }
    
  }
}
corrtab <- data.frame("names" = corrnames, "values" = corrvalues)

corrtab <- corrtab %>%
  summarise('names' = unique(corrnames), 'values' = corrvalues) %>% arrange(desc(corrvalues))

col <- c()
name <- c()
v = 0
while(v <= nrow(corrtab)) {
  col <- append(col, corrtab$values[v])
  name <- append(name, corrtab$names[v])
  v = v+2
} ##since each correlation is calculated twice (a b and b a), create two variables to delete the repetition
standing <- data.frame("variables" = name, "correlation" = col) ##save this variables in a dataframe
return(standing)
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

