DataColumnNotes <- rbind(SimpleExampleData %>% mutate_all(as.character),
                         c("This is an example of a note in a column", "", "", "", "", ""),
                         c("", "This is an example of a note in a column", "", "", "", ""),
                         c("", "", "This is an example of a note in a column", "", "", ""),
                         c("", "", "", "This is an example of a note in a column", "", ""),
                         c("", "", "", "", "", "This is an example of a note in a column"),
                         c("", "", "", "", "", ""),
                         c(NA, NA, NA, NA, NA, NA))
