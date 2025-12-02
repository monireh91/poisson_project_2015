#توزیع پواسون


set.seed(123)
calls <- rpois(n = 20, lambda = 5)  # lambda =میانگین تماس‌ها

df_calls <- tibble(
  Hour = 1:20,
  Calls = calls
)
print(df_calls)

summary(df_calls)

mean_calls <- mean(df_calls$Calls)
var_calls <- var(df_calls$Calls)


counts <- table(df_calls$Calls)

barplot(counts, 
        main="Distribution of Calls", 
        xlab="Number of Calls", 
        ylab="Number of Hours", 
        col="pink")

poisson_model <- glm(Calls ~ Hour, 
                     family = poisson(link = "log"), 
                     data = df_calls)
summary(poisson_model)


predicted_calls <- predict(poisson_model, type = "response") 
results <- cbind(df_calls, Predicted=round(predicted_calls,2))
print(results)

plot(df_calls$Hour, df_calls$Calls, type="b", pch=19, col="red",
     xlab="Hour", ylab="Number of Calls", main="Actual vs Predicted Calls")
lines(df_calls$Hour, predicted_calls, type="b", pch=17, col="green")
legend("topleft", legend=c("Actual","Predicted"), col=c("red","green"), pch=c(19,17), cex = 0.5)



