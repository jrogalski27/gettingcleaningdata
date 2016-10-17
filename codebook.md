str(MyTidyDataset)
key(MyTidyDataset)
MyTidyDataset
summary(MyTidyDataset)
MyTidyDataset[, .N, by=c(names(MyTidyDataset)[grep("^feat", names(MyTidyDataset))])]
fileoutput <- file.path("DatasetHumanActivity.txt")
write.table(MyTidyDataset, fileoutput, quote=FALSE, sep="\t", row.names=FALSE)
