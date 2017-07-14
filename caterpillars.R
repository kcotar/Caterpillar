library(Stat2Data)
library(tsne)

# Measurements on a sample of Manduca Sexta caterpillars
data("Caterpillars") 
summary(Caterpillars)

# cols_use = c("LogMass", "LogIntake", "LogWetFrass", "LogDryFrass", "LogCassim", "LogNfrass", "LogNassim")
cols_use = c("Mass", "Intake", "WetFrass", "DryFrass", "Cassim", "Nfrass", "Nassim")
print(length(cols_use))

# data with valid rows
data_cat = Caterpillars[complete.cases(Caterpillars), ]
print(dim(data_cat))
life_Stage = as.vector(unlist(data_cat["Instar"]))
feeding = as.integer(factor(as.vector(unlist(data_cat["ActiveFeeding"]))))
fgp = as.integer(factor(as.vector(unlist(data_cat["Fgp"]))))
mgp = as.integer(factor(as.vector(unlist(data_cat["Mgp"]))))

# plot subset for anly first few life stages
data_cat_plot2 = data_cat[which(data_cat["Instar"]<5),]
life_Stage2 = as.vector(unlist(data_cat_plot2["Instar"]))
feeding2 = as.integer(factor(as.vector(unlist(data_cat_plot2["ActiveFeeding"]))))
data_cat_plot2 = data_cat_plot2[cols_use]

# subset of data
data_cat = data_cat[cols_use]

# Explore data
plot(data_cat, col=life_Stage)
plot(data_cat, col=feeding)
plot(data_cat_plot2, col=life_Stage2)
plot(data_cat_plot2, col=feeding2)

# correlation between variables
print(cor(data_cat))

# normalize data
data_cat_norm = scale(data_cat)
print(dim(data_cat_norm))

# decision tree classification
train = as.character(feeding)
to_classify = data.frame(cbind(data_cat, train))
tree = ctree(train ~ ., data=to_classify)
pred = predict(tree)
print(table(train, pred))
print(sum(pred==as.character(train))/length(pred))

# dimensinality reduction
tsne_result = tsne(data_cat_norm, k=2, perplexity=10, initial_dims=length(cols_use), max_iter=900, epoch=200)

# plot new projection
plot(tsne_result[,1], tsne_result[,2], xlab = "tSNE axis 1", ylab = "tSNE axis 1", main="Caterpillars database - life stage", col=life_Stage, pch=16)
plot(tsne_result[,1], tsne_result[,2], xlab = "tSNE axis 1", ylab = "tSNE axis 1", main="Caterpillars database - active feeding", col=feeding, pch=16)
plot(tsne_result[,1], tsne_result[,2], xlab = "tSNE axis 1", ylab = "tSNE axis 1", main="Caterpillars database - free growth period", col=fgp, pch=16)
plot(tsne_result[,1], tsne_result[,2], xlab = "tSNE axis 1", ylab = "tSNE axis 1", main="Caterpillars database - maximmum growth period", col=mgp, pch=16)