library(data.table)

t = fread("~/git/humanitarian-ssp-projections/Python/transformer_output.csv")
pred = data.table(table(t$conflict_actual, t$conflict_prediction))
names(pred) = c("actual", "prediction", "count")
pred$correct = pred$actual == pred$prediction

pred[,.(count=sum(count)),by=.(correct)]

subset(pred, actual!=0)[,.(count=sum(count)),by=.(correct)]