library(tidyverse)
library(here)

#' Cargar datos crudos de entrenamiento
#' 
load_raw_train_data <- function(){
  dataset <- read_csv(here("data","Entrenamieto_ECI_2020.csv"), 
                      col_types = cols(Account_Created_Date = col_date(format = "%m/%d/%Y"), 
                                       Last_Modified_Date = col_date(format = "%m/%d/%Y"), 
                                       Opportunity_Created_Date = col_date(format = "%m/%d/%Y"), 
                                       Planned_Delivery_End_Date = col_date(format = "%m/%d/%Y"), 
                                       Planned_Delivery_Start_Date = col_date(format = "%m/%d/%Y"), 
                                       Quote_Expiry_Date = col_date(format = "%m/%d/%Y")))
  return(dataset)
}

load_raw_test_data <- function(){
  dataset <- read_csv(here("data","Validacion_ECI_2020.csv"), 
                      col_types = cols(Account_Created_Date = col_date(format = "%m/%d/%Y"), 
                                       Last_Modified_Date = col_date(format = "%m/%d/%Y"), 
                                       Opportunity_Created_Date = col_date(format = "%m/%d/%Y"), 
                                       Planned_Delivery_End_Date = col_date(format = "%m/%d/%Y"), 
                                       Planned_Delivery_Start_Date = col_date(format = "%m/%d/%Y"), 
                                       Quote_Expiry_Date = col_date(format = "%m/%d/%Y")))
  return(dataset)
}

#' De datos crudos a armar el dataset para entrenamiento
clean_dataset <- function(is_test_dataset=FALSE){
  
  if(is_test_dataset == FALSE) dataset <- load_raw_train_data()
  else  dataset <- load_raw_test_data()
  
   
  drop <- c("ID", "Account_Name", "Opportunity_Name", "Opportunity_ID","Sales_Contract_No","Submitted_for_Approval")
  
  
  drop <- c(drop,"Actual_Delivery_Date","Prod_Category_A")
  
  var_fechas <- c("Opportunity_Created_Date","Last_Activity","Quote_Expiry_Date","Last_Modified_Date",
                  "Last_Modified_By","Actual_Delivery_Date","Planned_Delivery_Start_Date","Planned_Delivery_End_Date",
                  "Month","Delivery_Quarter","Delivery_Year","Actual_Delivery_Date")
  
  precios <- c("ASP_Currency","ASP_(converted)","ASP_(converted)_Currency",
               "Total_Amount_Currency","Total_Taxable_Amount_Currency","Total_Taxable_Amount")
  # sacar los precios 
  dataset <- select(dataset, -one_of(drop,var_fechas,precios))
  
  # limpio columna Price
  dataset[dataset$Price == "None","Price"] <- NA
  dataset$Price <- as.double(dataset$Price)
  
  # VARIABLE de clase u objetivo pasarla a factor
  if(is_test_dataset!=TRUE) {
    dataset <- dataset %>% filter(!(Stage %in% c("Negotiation","Proposal","Qualification")))
    dataset$Stage <- as.factor(dataset$Stage)
  }
    
  # Guardo este dataset!!!
  # write_csv(dataset,here("data","dataset-limpio-1.csv"))
  return(dataset)
}

#' puntajes de prediccion de testeo, armar tabla y archivo para enviar al sitio
#' 
save_scores_to_send <- function(){}

#' Genero datasets de entrenamiento y testeo
#' train

write.csv(clean_dataset(FALSE),here("data","train-set-limpio-1.csv"))
#' testeo
write.csv(clean_dataset(TRUE),here("data","test-set-limpio-1.csv"))