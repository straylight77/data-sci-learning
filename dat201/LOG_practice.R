## LOGISTIC REGRESSION

df = iris

df$Sepal.ratio = df$Sepal.Width / df$Sepal.Length
df$Petal.ratio = df$Petal.Width / df$Petal.Length

colors = c("tomato", "steelblue", "lightblue")
plot(df$Sepal.ratio, df$Petal.ratio, col=colors[df$Species], pch=16)
legend("bottomleft", legend=levels(df$Species), fill=colors)



# -----------------------------------------------------------
# https://wilkelab.org/classes/SDS348/2020_spring/worksheets/class11_solutions.html
df2 = df[df$Species %in% c("virginica", "versicolor"), ]
plot(df2$Sepal.ratio, df2$Petal.ratio, col=colors[df2$Species], pch=16)


m2 = glm(Species ~ Sepal.Width + Sepal.Length + Petal.Width + Petal.Length,
         data = df2,
         family = binomial)
summary(m2)

# drop Speal.Width since it has the highest p value
m2 = glm(Species ~ Sepal.Length + Petal.Width + Petal.Length,
         data = df2,
         family = binomial)
summary(m2)

plot(m2$linear.predictors, m2$fitted.values, col=colors[df2$Species], pch=16)
legend("bottomright", legend=levels(df$Species), fill=colors)
abline(h=0.5, col="red")

df2$prev_val = predict(m2, df2, type="response")
