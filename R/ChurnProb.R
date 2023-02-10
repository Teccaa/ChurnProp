ChurnProb <- function(Data, Cust_id){
  #Check if CustomerId exists
  if (Cust_id %in% final_data$CustomerId ==FALSE){
    stop("The CustomerId you provided is not in the dataset")
  }
  #GLM
  glm_churn <- glm(Exited ~ CreditScore + Gender + Age + Tenure + Balance +
                     NumOfProducts + HasCrCard + IsActiveMember + EstimatedSalary,
                   data= Data, family="binomial")


  Data$Churn <- predict(glm_churn, Data, type="response")

  prob <- predict(glm_churn, Data[CustomerId==Cust_id], type="response")

  results <- data.frame("CustomerId"=Cust_id, "Churn_Prob"=prob)
  return(results)

}
