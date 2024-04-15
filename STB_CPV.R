## Stability Analysis - statistical outputs 12Dec23
# packages from citric
library("tidyverse")
library("readxl") # function read_excel()
library("plyr") # need for recipes
library("dplyr")#, lib="//amrsomw557/Informa-reports/R Studio Reports/Graph Test/")
library("data.table")#rbindlist()
library("ggplot2")
library("rstatix")#, lib="//amrsomw557/Informa-reports/R Studio Reports/Graph Test/") function: anova_test
library("BBmisc") # ffunction chunk()
###########################################################################################
### CPV
###########################################################################################
df <- read_excel("New CPV LT Data.xlsx") # ActiveDataSet #Holds the active dataset(s)~ActiveDataSet #Holds the active dataset(s)
# Replace spaces in variable names with "."
names(df) <- gsub(" ", ".", names(df)) # working 04Jul23
### Calculate missing filed shelf lives: Expiry date - Stability base date: may not use this, potentially Filed Shelf Life MDE will be compulsary
###
# Calc_Filed_Shelf_Life <- function(df) {
#   df %>%
#     filter(is.na(`Filed.Shelf.Life`)) %>%
#     filter(
#       !is.na(`Stability.Base.Date`) |
#         !is.na(`Expiry.Date`) |
#         !is.na(`Testing.Target.Date`)
#     ) %>% #NOT WORKING
#     distinct(
#       Folder,
#       `Manufacture.Site`,
#       `Product.Name`,
#       `Sub.Product.Name`,
#       `Batch.Number`,
#       `Project.Name`,
#       `Stability.Base.Date`,
#       `Expiry.Date`,
#       `Testing.Target.Date`
#     ) %>%
#     group_by(Folder,
#              `Manufacture.Site`,
#              `Product.Name`,
#              `Sub.Product.Name`) %>%
#     slice(which.max(Testing.Target.Date)) %>%
#     mutate(`Filed.Shelf.Life.Days` = difftime(Expiry.Date, Stability.Base.Date, units =
#                                                 'days')) %>%
#     mutate(`Filed.Shelf.Life.Months` = as.numeric(round(`Filed.Shelf.Life.Days` /
#                                                           30.14, 0))) %>%
#     distinct(
#       Folder,
#       `Manufacture.Site`,
#       `Product.Name`,
#       `Sub.Product.Name`,
#       `Filed.Shelf.Life.Months`
#     ) %>%
#     left_join(df, .) %>%
#     mutate(Filed.Shelf.Life = ifelse(
#       is.na(Filed.Shelf.Life),
#       Filed.Shelf.Life.Months,
#       Filed.Shelf.Life
#     )) %>%
#     select(-Filed.Shelf.Life.Months)
# }
# df1 <- Calc_Filed_Shelf_Life(df)
### Rename Batch Number, when there is >1 Project Name per Batch Number within a Grouping Parameter
#
Batch_Rename <- function(df) {
  df %>%
    distinct(Folder, Grouping.Parameter, Batch.Number, Project.Name) %>%
    select(-Project.Name) %>%
    count() %>%
    left_join(., df) %>%
    mutate(Batch.Number = ifelse(
      freq > 1,
      paste(Batch.Number, Project.Name, sep = ":"),
      Batch.Number
    )) %>%
    select(-freq)
}
df2 <- Batch_Rename(df)
###
#df2_check <-  df2 %>% select("Grouping.Parameter","Batch.Number") %>% unique()
#### Create variable called Spec Grouping, Grouping Parameter without Condition
#
Spec_Group <- function(df) {
  test <- df %>%
    mutate(
      Spec.Grouping = paste(
        Product.Name,
        Sub.Product.Name,
        Assay.Name,
        Parameter.Field.Name,
        sep = "_"
      )
    )
}
df3 <- Spec_Group(df2)
#df3_check <- df3 %>% select("Spec.Grouping") %>% unique()
### Identify Current Spec
#
Current_Spec <- function(df) {
  df %>%
    filter(!is.na(Spec.Rule)) %>%
    group_by(Spec.Grouping) %>%
    slice(which.max(Testing.Target.Date)) %>%
    distinct(
      Spec.Grouping,
      Spec.Rule,
      Spec.Min.Value,
      Spec.Max.Value,
      Spec.Rounding.Places,
      .keep_all = TRUE
    ) %>%
    select(Spec.Grouping,
           Spec.Rule,
           Spec.Min.Value,
           Spec.Max.Value,
           Spec.Rounding.Places) %>%
    left_join(df, ., by = "Spec.Grouping", keep = TRUE)
}
df_3_1 <- Current_Spec(df3)
### Identify XMax
#
XMax_Axis <- function(df) {
  df %>%
    distinct(Grouping.Parameter, Protocol.Months.On.Stability) %>%
    group_by(Grouping.Parameter) %>%
    slice(which.max(Protocol.Months.On.Stability)) %>%
    mutate(`XMax` = round(Protocol.Months.On.Stability * 1, digits = 0)) %>%
    select(Grouping.Parameter, `XMax`) %>%
    left_join(df, ., by = "Grouping.Parameter", keep = TRUE)
}
df_3_2 <- XMax_Axis(df_3_1)
### Overwrite existing Spec & xMax
#
Overwrite_Spec <- function(df){
  df %>%
    mutate(Spec.Rule = Spec.Rule.y) %>%
    mutate(Spec.Min.Value = Spec.Min.Value.y) %>%
    mutate(Spec.Max.Value = Spec.Max.Value.y) %>%
    mutate(Spec.Rounding.Places = Spec.Rounding.Places.y) %>%
    mutate(YAxis.Significant.Figures = ifelse( is.na(YAxis.Significant.Figures),Spec.Rounding.Places.y,YAxis.Significant.Figures)) %>%
    mutate(XMin= ifelse(is.na(XMin),-0.25,XMin)) %>%
    mutate(XMax= ifelse(is.na(XMax.x),XMax.y,XMax.x)) %>%
    mutate(XStepSize= ifelse(is.na(XStepSize),3,XStepSize)) %>%
    mutate(YAxis.Significant.Figures= as.numeric(YAxis.Significant.Figures)) %>%
    mutate(Numeric.Parameter.Value.Chart = ifelse(is.na(YAxis.Significant.Figures), Numeric.Parameter.Value,round(Numeric.Parameter.Value, digits = YAxis.Significant.Figures))) %>%
    mutate(Grouping.Parameter = Grouping.Parameter.x) %>%
    select(.,-c(Spec.Rounding.Places,Spec.Min.Value.x,Spec.Max.Value.x,Spec.Rule.x,Spec.Max.Value.y,Spec.Min.Value.y,Spec.Rule.y,Spec.Rounding.Places.x,Spec.Rounding.Places.y,XMax.x,XMax.y,Grouping.Parameter.y,Spec.Grouping.y,Spec.Grouping.x,Grouping.Parameter.x)) %>%
    as.data.frame()
  }
df_3_3 <- Overwrite_Spec(df_3_2)
### Identify how many TPs within a study
###
Project_TP_Count <- function(df){
 df %>%
    distinct(Folder, Grouping.Parameter, Batch.Number, Project.Name, Testing.Time.Point, Protocol.Months.On.Stability) %>% # 26Mar24 : added Testing Time Point, incase there are multiple Protocol Months On Stability dups 
    select(-c(Protocol.Months.On.Stability,Testing.Time.Point)) %>%
    count() %>%
    mutate(Project.TP.Count = freq) %>%
    left_join(df,.) %>%
    select(-freq)
}
df4 <- Project_TP_Count(df_3_3)
###df4_check <- df4 %>% select(Grouping.Parameter, Batch.Number, Project.Name, Protocol.Months.On.Stability,Project.TP.Count) %>% unique()#### 26Mar24: WORKING
### Identify Projects which Protocol Months have gone beyond Filed Shelf Life
#
Mature_Batch <- function(df) {
  df %>%
    distinct(#Folder,`Manufacture.Site`,`Product.Name`,`Sub.Product.Name`,
      `Grouping.Parameter`,
      `Batch.Number`,
      `Project.Name`,
      `Protocol.Months.On.Stability`,
      `Filed.Shelf.Life`
    ) %>%
    group_by(Grouping.Parameter, Batch.Number, Project.Name) %>%
    slice(which.max(Protocol.Months.On.Stability)) %>%
    mutate(
      `Mature.Study` = ifelse(
        `Protocol.Months.On.Stability` >= `Filed.Shelf.Life`,
        "Yes",
        "No"
      )
    ) %>% # 26Mar24: Changed to >=
    select(-c(Filed.Shelf.Life,Protocol.Months.On.Stability)) %>%
    left_join(df, .)
}
df5 <- Mature_Batch(df4)
###df5_check <- df5 %>% select(Grouping.Parameter,Batch.Number,Protocol.Months.On.Stability,Mature.Study) #### 26Mar24: WORKING
### Function to count the number of mature studies (identified by Mature=1)
###
Mature_Project_Count <- function(df){
df %>%
    distinct(Grouping.Parameter, Batch.Number, Project.Name, Mature.Study) %>%
    group_by(Grouping.Parameter) %>%
    tally(Mature.Study == "Yes") %>%
    mutate(`Group.Mature.Count`= n) %>%
    select(-n) %>%
    mutate(Mature.Group = ifelse(Group.Mature.Count >=5,"Yes","No")) %>% #26Mar24: changed to >=5
    left_join(df,.)
}
df6 <- Mature_Project_Count(df5)
###df6_check<- df6 %>% select(Grouping.Parameter,Batch.Number,Mature.Study,Group.Mature.Count,Mature.Group) %>% unique()#### 26Mar24: WORKING
### Function to sort the stability base dates decreasing, then slice the first 5
###
Sys.setenv(TZ = "GMT") # setting time zone to GMT, to get rid of warning. NOT WORKING
Most_Recent_Mature <- function(df){
df %>%
    filter(Mature.Group == "Yes") %>%
    filter(Mature.Study == "Yes") %>% ### 26Mar24: added Mature.Study Filter
    distinct(Grouping.Parameter, Batch.Number, Project.Name, Stability.Base.Date) %>%
    mutate(Date = as.Date(Stability.Base.Date, '%m/%d/%Y')) %>%
    group_by(Grouping.Parameter) %>%
    arrange(desc(Date)) %>%
    slice(1:5) %>% # selecting the top 5 ascending stability base dates per grouping parameter 5 most recent mature batches per grouping
    mutate(Most.Recent.Mature = "Yes") %>%
    select(-Date) %>%
    left_join(df,.)%>%
  mutate(Most.Recent.Mature = ifelse(is.na(Most.Recent.Mature),"No", Most.Recent.Mature))
}
df7 <- Most_Recent_Mature(df6) # includes all data
# Warning message: There was 1 warning in `mutate()`.i In argument: `Date = as.Date(Stability.Base.Date, "%m/%d/%Y")`.Caused by warning in `as.POSIXlt.POSIXct()`:  ! unknown timezone '%m/%d/%Y'
###df7_check <- df7 %>% select(Grouping.Parameter,Batch.Number,Mature.Study,Group.Mature.Count,Mature.Group,Most.Recent.Mature,Stability.Base.Date) %>% unique() ### 26Mar24: WORKING
### Check the Spec Type, Upper/Lower/Both/None
#
Spec_Type <- function(df){
  df %>%
    mutate(Spec.Type = ifelse(is.na(Spec.Min.Value) & is.na(Spec.Max.Value),"None", ifelse(is.na(Spec.Min.Value) & !is.na(Spec.Max.Value),"Upper", ifelse(!is.na(Spec.Min.Value) & is.na(Spec.Max.Value),"Lower","Both"))))
}
df8 <- Spec_Type(df7)
df_most_recent <- df8 %>% filter(Most.Recent.Mature == "Yes")
df_immature_group <- df8 %>% filter(Mature.Group == "No")

## at this point the mature groupings should drop anything that's not a 5 most recent
df_ANOVA_first_go <- rbind(df_most_recent,df_immature_group) # doesn't have mature group/non 5 most recent batches

###df8_check <- df8 %>% select(Grouping.Parameter,Spec.Min.Value,Spec.Max.Value,Spec.Type) %>% unique() ### 26Mar24: WORKING
### Push list through ANOVA with try catch
#
out <- list()
Run_Anova <- function(df_list){
  lapply(names(df_list), function(i){
    df <- data.frame(df_list[[i]])
    tryCatch(
      {expr =
        grouping <- df$Grouping.Parameter[1]
      names(grouping) <- "Grouping.Parameter"
      anova_output <- anova_test(data = df, Numeric.Parameter.Value ~ Batch.Number * Protocol.Months.On.Stability)
      out <- c(out,grouping,anova_output)
      return(out)
      },
      error=function(e) {NULL})
  })
} # if there isn't enough data, the anova splits out NULL against the grouping parameter

df_list <- df_ANOVA_first_go %>% split(.$Grouping.Parameter) # 26Mar24: Changed this from df8 to df_ANOVA_first_go
df9 <- Run_Anova(df_list)
df10 <- rbindlist(df9)

### Function to Unstack ANOVA output
###
Unstack_Anova <- function(df){
  tryCatch(
    {expr =
  df %>%
    as.data.frame() %>%
    filter(.$Effect != "Protocol.Months.On.Stability") %>%
    mutate(Type = ifelse(Effect == "Batch.Number", "Intercept", "Slope")) %>%
    select(Grouping.Parameter, Type, p) %>%
    pivot_wider(names_from = Type, values_from = p) %>%
    mutate(`Remove` = ifelse(is.na(.$Slope) |
                               is.na(.$Intercept), "Yes", "No")) %>%
    filter(Remove == "No") %>%
    select(-Remove) %>%
    mutate(Model = ifelse(Slope <= 0.05, "Unequal", "Pooled"))
    },
  error=function(e) {NULL})
}
df11 <-
  Unstack_Anova(df10) %>% left_join(df8, .) %>% 
  mutate(Model = ifelse(is.na(.$Model), "No Model", .$Model)) 
#Joins back to full dataset, including mature groups with non 5 most recent
# if the ANOVA output is NULL, the model type is = No Model
########################################################################################################################
#df_11_check <- df11 %>% select(Grouping.Parameter,Model) %>% unique()
#df_all <- df11

df_all_list <-
  df11 %>%
  split(., list(.$Grouping.Parameter), drop = TRUE)
out <- list()
df100 <- Run_Anova(df_all_list)
df101 <- rbindlist(df100)
df102 <- Unstack_Anova(df101) %>% left_join(df11, ., by  ="Grouping.Parameter") %>% 
  mutate(Model.y = ifelse(is.na(.$Model.y), "No Model", .$Model.y)) 
df_all_list <- df102 %>%
  split(., list(.$Grouping.Parameter), drop = TRUE)
#create list object called All_Shelf_Lives
All_Shelf_lifes_for_outputfile <- list()
### Testing
calc_SL <- function(SL_data_input, df_to_test) {
  #Below variables must be assigned a value in order to include them in the return statement
  lower_SL_TP = "120"
  names(lower_SL_TP) = "Lower_SL"
  Upper_SL_TP = "120"
  names(Upper_SL_TP) = "Upper_SL"
  Spec_Min <- df_to_test$Spec.Min.Value[[1]]
  Spec_Max <- df_to_test$Spec.Max.Value[[1]]
  #input order is 'fit lwr upr timepoint Batch'
  tryCatch(
    expr = {
      cross_lower_spec_index <-
        which(as.data.frame(SL_data_input)$lwr <= Spec_Min)[[1]]
      #the index of the match is assigned to cross_lower_spec_index  #Use the index of the match to pull out the time from the 4th column in SL_data_input
      lower_SL_TP <- SL_data_input[cross_lower_spec_index, 4]
      print(lower_SL_TP)
      lower_SL_value <- SL_data_input[cross_lower_spec_index, 2]
      print(lower_SL_value)
    },
    error = function(e)
    {
      if (is.na(Spec_Min)) {
        message(paste("There is no Lower Spec"))
      }
      else{
        message(paste("There are no lower shelf life violtations for 10 years"))
      }
      print(e)
    }
  )
  tryCatch(
    expr = {
      cross_upper_spec_index <-
        which(as.data.frame(SL_data_input)$upr >= Spec_Max)[[1]]
      Upper_SL_TP <- SL_data_input[cross_upper_spec_index, 4]
      print(Upper_SL_TP)
      Upper_SL_value <- SL_data_input[cross_upper_spec_index, 3]
      print(Upper_SL_value)
    },
    error = function(e)
    {
      if (is.na(Spec_Max)) {
        message(paste("There is no Upper Spec"))
      }
      else{
        message(paste("There are no upper shelf life violtations for 10 years"))
      }
      print(e)
    }
  )
  return(c(lower_SL_TP, Upper_SL_TP))
}

#########################
# round to 2 decimal places
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <- round(x[numeric_columns], digits)
  x
}

### testing 08Dec23
All_Shelf_lifes <- list()

y <- lapply(names(df_all_list), function(i){
  Grouping = df_all_list[[i]]$`Grouping.Parameter`[[1]]
  names(Grouping) = "Grouping.Parameter"
  Spec_Max = df_all_list[[i]]$`Spec.Max`[[1]]
  names(Spec_Max) = "Spec.Max.Value"
  Spec_Min = df_all_list[[i]]$`Spec.Min`[[1]]
  names(Spec_Min) = "Spec.Min.Value"
 # Batch = df_all_list[[i]]$Batch.Number[[1]]
 # names(Batch) = "Batch.Number"
  Project_TP_Count = df_all_list[[i]]$Project.TP.Count[[1]]
  names(Project_TP_Count) = "Project.TP.Count"
  Mature_Group = df_all_list[[i]]$Mature.Group[[1]]
  names(Mature_Group) = "Mature.Group"
  Model = df_all_list[[i]]$Model.y[[1]] # Changed to Model.y
  names(Model) = "Model"
  Spec_Type = df_all_list[[i]]$Spec.Type[[1]]
  names(Spec_Type) = "Spec.Type"
  Grouping = df_all_list[[i]]$`Grouping.Parameter`[[1]]
  names(Grouping) = "Grouping.Parameter"
  #Spec_Max=df_all_list[[i]]$`Spec.Max`[[1]]
  #Spec_Min=df_all_list[[i]]$`Spec.Min`[[1]]
  ### New x-axis, TP seq 0 to 120 at intervals specified [months]
  newx <<- seq(min(df_all_list[[i]]$`Protocol.Months.On.Stability`),120, by = 0.1)
  if(df_all_list[[i]]$Spec.Type[[1]]=="Upper" | 
     df_all_list[[i]]$Spec.Type[[1]]=="Lower"){
    CI_level = 0.90
  }else{
    CI_level = 0.95
  }
  if(df_all_list[[i]]$Model.y[[1]]=="Pooled") { # Changed to Model.y
    Batch = NA
    names(Batch) = "Batch.Number"
    #print(df_all_list[[i]]$Model[[1]])
    #print(df_all_list[[i]]$Spec.Type[[1]])
    #print(CI_level)
    ### run the lm on the pooled data above
    pooled_lm <- lm(formula =`Numeric.Parameter.Value` ~ `Protocol.Months.On.Stability`,data=df_all_list[[i]]);
    # Get slope
   # coeff <- summary(pooled_lm)$coefficients %>% as.data.frame() %>% filter(row.names(.) %like% "Protocol") %>% distinct(.$Estimate) %>% round_df(.,4)
    ### if one sided or two sided specs, confidence interval will be different
      ### predict confidence interval to 90% when only one sided spec
    #print("pooled_lm")
    #print(pooled_lm)
      CI<-predict(pooled_lm,newdata=data.frame(Protocol.Months.On.Stability=newx),interval="confidence",level=CI_level)%>% round_df(.,2)
      #print("CI")
      #print(CI)
      ### bind TP
      CI_time<-cbind(CI,newx)
      #print("CI_time")
      #print(CI_time)
      ### get intercept of prediction line
      Intercept <- CI_time %>% as.data.frame() %>% filter(.$newx == "0") %>% select(fit)%>% round_df(.,2)
      #print("Intercept")
      #print(Intercept)
      ### calculate shelf life
      Shelf_lifes <- calc_SL(as.data.frame(CI_time),df_all_list[[i]])#%>% round_df(.,0)
      names(Shelf_lifes) <- c("Lower.SL","Upper.SL")
      All_Shelf_lifes <<-
                c(
                  All_Shelf_lifes,
                  Batch,
                  Model,
                  Grouping,
                  Mature_Group,
                  Project_TP_Count,
                  Shelf_lifes,
                  Spec_Min,
                  Spec_Max,
                  Spec_Type,
                  Intercept
                )
  }
  else if(df_all_list[[i]]$Model.y[[1]]=="Unequal") { # Changed to Model.y
    Batch = NA
    names(Batch) = "Batch.Number"
    Batches_list <- df_all_list[[i]] %>% distinct(Batch.Number)
    #print(Batches_list)
    vector_batches <- Batches_list[['Batch.Number']]
    #print(vector_batches)
    new_data <- expand.grid(Protocol.Months.On.Stability = newx,Batch.Number = vector_batches)
    #print(new_data)
    sep_s_sep_i_lm <- lm(formula = `Numeric.Parameter.Value` ~ `Protocol.Months.On.Stability`*`Batch.Number`, data = df_all_list[[i]]);
    #print(sep_s_sep_i_lm)
      ### predict confidence interval to 90% when only one sided spec
      CI_unequal_i <- predict(sep_s_sep_i_lm, newdata = data.frame(new_data), interval="confidence", level= CI_level)%>% round_df(.,2)
      #print(CI_unequal_i)
      ### bind TP & batch info
      CI_unequal_i_time <- cbind(CI_unequal_i,new_data)
      #print(CI_unequal_i_time)
      ### split by batch so can determine for each batch what is the SL
      CI_unequal_i_time_splitBatch <- split(CI_unequal_i_time,CI_unequal_i_time$Batch.Number)
      ### lapply
      lapply(names(CI_unequal_i_time_splitBatch),function(j){
        names(j) <- "Batch.Number"
        Intercept <- CI_unequal_i_time_splitBatch[[j]] %>% as.data.frame() %>% filter(.$Protocol.Months.On.Stability == "0") %>% select(fit)%>% round_df(.,2)
        Shelf_lifes <- calc_SL(CI_unequal_i_time_splitBatch[[j]],df_all_list[[i]])#%>% round_df(.,0)
        names(Shelf_lifes) <- c("Lower.SL","Upper.SL")
        All_Shelf_lifes <<-
          c(
            All_Shelf_lifes,
            j,
            Model,
            Grouping,
            Mature_Group,
            Project_TP_Count,
            Shelf_lifes,
            Spec_Min,
            Spec_Max,
            Spec_Type,
            Intercept
          )
      })
  }
  else{
    Batch = NA
    names(Batch) = "Batch.Number"
    Shelf_lifes <- c(NA,NA) 
    names(Shelf_lifes) <- c("Lower.SL","Upper.SL")
    Intercept <- NA
    names(Intercept) <- "fit"
    All_Shelf_lifes <<-
      c(
        All_Shelf_lifes,
        Batch,
        Model,
        Grouping,
        Mature_Group,
        Project_TP_Count,
        Shelf_lifes,
        Spec_Min,
        Spec_Max,
        Spec_Type,
        Intercept
      )
  }
})

splited_SL <-
  chunk(All_Shelf_lifes,11) # #10) #bind results together and write to file
final_SL <- rbindlist(splited_SL, use.names = FALSE, fill = TRUE)
final_SL_1 <- final_SL %>% mutate_at(c('Upper.SL', 'Lower.SL'), as.numeric) %>%
  select("Grouping.Parameter","Batch.Number","Lower.SL","Upper.SL")
final_SL_group <- final_SL_1 %>% filter(is.na(Batch.Number)) %>% select(-Batch.Number) %>% inner_join(.,df11)
final_SL_batch <- final_SL_1 %>% filter(!is.na(Batch.Number)) %>% inner_join(.,df11)
final_SL_all <- rbind(final_SL_group,final_SL_batch) %>% ### DEFO missing groupings which don't have models or SL etc
#df_all_SL <- df11 %>%
 # left_join(., final_SL_1, copy = TRUE) %>%
  mutate(Alarm.Shelf.Life = Filed.Shelf.Life + 1) %>% ### 26Mar24: Added Alarm.Shelf.Life
  mutate("SL.Breach" = ifelse(.$Lower.SL > .$Alarm.Shelf.Life & .$Upper.SL > .$Alarm.Shelf.Life,"No","Yes"))%>%
  unique()

#df_all_SL_check <- final_SL_all %>% filter(Batch.Number == "FW4714") %>% select("Grouping.Parameter","Batch.Number","Spec.Type","Lower.SL","Upper.SL","SL.Breach","Spec.Min.Value","Spec.Max.Value")%>%
#  unique()

df_Pooled <- final_SL_all %>%
  filter(Model == "Pooled") %>% # which model should be chosen at this point .x or .y?
  mutate(
    PI = ifelse(
      (Mature.Group == "Yes" &
        Most.Recent.Mature == "Yes") | Mature.Group == "No", ### 26Mar24: Added brackets around Mature.Group == Yes etc
      "Yes",
      "No"
    )
  )

#final_SL_num <-
#  final_SL %>% mutate_at(c('Upper_SL', 'Lower_SL'), as.numeric)
final_SL_list <- final_SL_all  %>% split(.$Grouping.Parameter)

pick_best_batch <- function(df_list) {
  lapply(names(df_list), function(i) {
    df <- df_list[[i]] %>% filter(.$Mature.Study == "Yes" & "Project.TP.Count"> 3) %>% 
      select("Grouping.Parameter","Batch.Number","Stability.Base.Date","Spec.Type","Upper.SL","Lower.SL") %>%
      distinct() %>%  as.data.frame() #df_list[[i]] %>% filter(.$Project.TP.Count > 3) %>% as.data.frame(.)
    #print(df)
    tryCatch({
      expr =
        if (df$Spec.Type[[1]] == "Upper") {
          output <- df %>% mutate(Max.SL.Batch = Batch.Number[which.max(Upper.SL)]) %>% 
            mutate(PI="Yes") %>%
            distinct(Grouping.Parameter,Max.SL.Batch,PI)#slice_max(df$Upper_SL, order_by = Stability.Base.Date) %>% distinct(df$Grouping.Parameter)
        } #slice(which.max(Protocol.Months.On.Stability))
      if (df$Spec.Type[[1]] == "Lower") {
        output <- df %>% mutate(Max.SL.Batch = Batch.Number[which.max(Upper.SL)]) %>% 
          mutate(PI="Yes") %>%
          distinct(Grouping.Parameter,Max.SL.Batch,PI)
      }
      else{
        output <- df %>% mutate(Max.SL.Batch = Batch.Number[which.max(Upper.SL & Upper.SL)]) %>% 
          mutate(PI="Yes") %>%
          distinct(Grouping.Parameter,Max.SL.Batch,PI)
      }
      return(output)
    },
    error = function(e) {
      NULL
    })
  })
}


SL_best_batch <-
  pick_best_batch(final_SL_list) %>%
  rbindlist()# gives you grouping and the best batch for SL - all data

df_Unequal <- final_SL_all %>%
  filter(Model == "Unequal") %>% 
  left_join(.,SL_best_batch, by= c("Grouping.Parameter" = "Grouping.Parameter", "Batch.Number"="Max.SL.Batch")) %>% #by=c("name1" = "name3", "name2" = "name4")
  mutate(PI = ifelse(is.na(PI),"No",PI))
df_NoModel <- final_SL_all %>%
  filter(Model == "No Model") %>% 
  left_join(.,SL_best_batch, by= c("Grouping.Parameter" = "Grouping.Parameter", "Batch.Number"="Max.SL.Batch")) %>% #by=c("name1" = "name3", "name2" = "name4")
  mutate(PI = "No")

All_data_for_PI_list <- rbind(df_Unequal,df_Pooled) %>% 
 # rbind(.,df_NoModel) %>%
  split(.$Grouping.Parameter)

Calc_PI <- function(df_list) {
  lapply(names(df_list), function(i) {
    tryCatch({
      expr =
    df <- df_list[[i]]
    df_PI <- df_list[[i]] %>% filter(df_list[[i]]$PI == "Yes")
    newx <- seq(0, 120, by = 1)
    model <-
      lm(Numeric.Parameter.Value ~ Protocol.Months.On.Stability,
         data = df_PI)
    if (df_PI$Spec.Type[[1]] == "Upper" | df_PI$Spec.Type[[1]] == "Lower") {
      CI_9 <-
        predict(
          model,
          newdata =  data.frame(Protocol.Months.On.Stability = newx),
          interval = "prediction",
          level = 0.90
        )
    }
    else{
      CI_9 <-
        predict(
          model,
          newdata =  data.frame(Protocol.Months.On.Stability = newx),
          interval = "prediction",
          level = 0.95
        )
    }
    CI_9_time <- cbind(CI_9, newx)
    colnames(CI_9_time) <- c("fit", "lwr", "upr", "Protocol.Months.On.Stability")
    all_data <- left_join(df, CI_9_time, copy = T) },
    error = function(e) {
      NULL
    })
  })
} ### Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
###0 (non-NA) cases
## need to add trycatch

df_all_PI <- Calc_PI(All_data_for_PI_list) %>% rbindlist() %>% 
  rbind(.,df_NoModel, fill=TRUE)

### Add star to batches which were used in the PI calculations, so this is visible on chart legend
###
Mark_PI_Batch_Chart <- function(df){
  df %>%
    mutate(`Batch.Number` = ifelse(`PI` == "Yes", paste0(`Batch.Number`,"*"), paste0(`Batch.Number`)))
}
df_Pooled_Astrix <- Mark_PI_Batch_Chart(df_all_PI)
Chart_df_list <- df_Pooled_Astrix %>% split(.$Grouping.Parameter) # missing No Model

########################################################################################################################
### Charting
N <- 1000
good.shapes <- c(1:25,33:127)
good_cols = rep(seq(2, 8, by = 1),20)
plot_CPV <- lapply(names(Chart_df_list), function(i) {
  output_plot_name <<-
    paste(Chart_df_list[[i]]$`Folder`[[1]],
          Chart_df_list[[i]]$`Grouping.Parameter`[[1]],
          sep = "$")
  
  p <- ggplot(
    Chart_df_list[[i]],
    aes(
      x = Chart_df_list[[i]]$Protocol.Months.On.Stability,
      y = Chart_df_list[[i]]$Numeric.Parameter.Value,
      color = Chart_df_list[[i]]$Batch.Number,
      shape = Chart_df_list[[i]]$Batch.Number
    )
  ) +
    scale_shape_manual(values = good.shapes[1:N]) +
    scale_color_manual(values = good_cols[1:N]) +
    geom_point() +
    geom_line(aes(y = Spec.Min.Value[[1]]), color = "black") +
    geom_line(aes(y = Spec.Max.Value[[1]]), color = "black") + #geom_line(aes(y = lwr), col = "red") + #lwr pred interval #geom_line(aes(y = upr), col = "red") + #upr
    ggtitle(Chart_df_list[[i]]$`Grouping.Parameter`[[1]],
            subtitle = paste0(
                "Mature Group: ",
                Chart_df_list[[i]]$`Mature.Group`[[1]],", ",
                "Model: ",
                Chart_df_list[[i]]$`Model`[[1]],", ",
                "Spec Type: ",
                Chart_df_list[[i]]$`Spec.Type`[[1]]
              )
            ) +
    ylab(paste(Chart_df_list[[i]]$`Parameter.[w/Units].Field.Name`[[1]])) +
    labs(caption = c(
      "Data Verification Required for GMP use as per SOP-00680",
      format(Sys.time(), "%d-%b-%Y %H:%M:%S")
    )) +
    labs(caption = c("",format(Sys.time(),"%d-%b-%Y %H:%M:%S"))) + 
    theme(plot.caption = element_text(hjust=c(0,1)))+ 
    scale_x_continuous(limits = c(0, Chart_df_list[[i]]$`XMax`[1]), breaks = seq(0, Chart_df_list[[i]]$`XMax`[1], 3))+
    theme(plot.caption = element_text(hjust = c(0, 1))) +#scale_x_continuous(limits = c(0, df_chart[[i]]$`XMax`[1]), breaks = seq(0, df_chart[[i]]$`XMax`[1], 3))+#scale_x_continuous(n.breaks=10) + # no. of breaks
    theme(
      axis.ticks = element_line(size = 0.2),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      #angle = 90,
      axis.title.x = element_text(),
      text = element_text(size = 6),
      panel.background = element_rect(
        fill = "white",
        colour = "black",
        size = 0.2,
        linetype = "solid"
      ),
      panel.grid.major = element_line(
        size = 0.2,
        linetype = 'solid',
        colour = "#D8F8F7"
      ),
      panel.grid.minor = element_line(
        size = 0.2,
        linetype = 'solid',
        colour = "#D8F8F7"
      ),
      plot.background = element_rect(fill = "#F8F8ED"),
      legend.background = element_rect(fill = "#F8F8ED"),
      legend.key.height = unit(0.05, "cm"),
      legend.key.width = unit(0.05, "cm"),
      legend.title = element_text(size = 6),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )#,
  if (Chart_df_list[[i]]$Spec.Type[[1]] == "Lower") {
    p <- p + geom_line(aes(y = lwr), col = "red") #upr
  } 
  else if(Chart_df_list[[i]]$Spec.Type[[1]] == "Upper") {
    p <- p + geom_line(aes(y = upr), col = "red") #upr
  } 
  else {
    p <- p + geom_line(aes(y = upr), col = "red") + geom_line(aes(y = lwr), col = "red")
  }
  ggsave(
    filename = paste0(
      sub("\\$.*", "", output_plot_name),
      "/",
      make_clean_names(sub(".*\\$", "", output_plot_name)),
      ".png"
    ),
    width = 8,
    height = 4,
    plot = p
  ) #print(p)
})

#####
# Check if lwr or upr breach SL
check <- df_Pooled_Astrix %>%
  distinct(Grouping.Parameter, Model)
CL_Breach <- df_Pooled_Astrix %>%
  #ifelse((.$lwr <=.$Spec.Min.Value | .$upr >=.$Spec.Max.Value) & Protocol.Months.On.Stability <= Filed.Shelf.Life)
  filter(.$lwr <=.$Spec.Min.Value | .$upr >=.$Spec.Max.Value) %>%
  mutate(CL.Breach = ifelse(Protocol.Months.On.Stability <= Filed.Shelf.Life, "Yes","No")) %>%
  filter(CL.Breach == "Yes") %>%
  distinct(Grouping.Parameter,CL.Breach) %>%
  left_join(df_Pooled_Astrix,.)

CL_Breach <- function(df){
  tryCatch{ 
    df %>%
      mutate(CL.Breach <= ifelse(
        (.$lwr <=.$Spec.Min.Value | .$upr >=.$Spec.Max.Value) &
          Protocol.Months.On.Stability <= Filed.Shelf.Life, "Yes","No"))} 
  else{
    NULL
  }
}



### Risk Analysis 

chart_alarms <- df_Pooled_Astrix %>%
  filter(Spec.Type != "None") %>% ### 26Mar24: added as we don't care about alarms when there are no specs
  mutate(upr = ifelse(Spec.Type == "Lower",NA,upr)) %>%
  mutate(lwr = ifelse(Spec.Type == "Upper",NA,lwr)) %>%
  mutate(
    Risk = ifelse(
      Numeric.Parameter.Value <= Spec.Min.Value | ### 26Mar24: Added = sign to both, in case spec is >= <=
        Numeric.Parameter.Value >= Spec.Max.Value,
      1,
      NA
    )
  ) %>%
  select(Folder,
    Grouping.Parameter,
    Product.Name,
    Spec.Type,
    Batch.Number,
    Testing.Time.Point,
    Protocol.Months.On.Stability,
    Project.TP.Count,
    Numeric.Parameter.Value,
    lwr,
    upr,
    Spec.Min.Value,
    Spec.Max.Value,
    Filed.Shelf.Life,
    Lower.SL,
    Upper.SL,
    SL.Breach,
    Risk
  )  %>%
  mutate(Risk = ifelse(
    is.na(Risk) &
      Protocol.Months.On.Stability > 3 &
      Project.TP.Count >= 3 &
      (
        Numeric.Parameter.Value > upr |
          Numeric.Parameter.Value < lwr
      ) & SL.Breach == "Yes", # already need to add if <3 TP
    2,
    Risk
  )) %>%
  mutate(Risk = ifelse(
    is.na(Risk) &
      Protocol.Months.On.Stability > 3 &
      Project.TP.Count >= 3 &
      (
        Numeric.Parameter.Value > upr |
          Numeric.Parameter.Value < lwr
      ) & SL.Breach == "No",
    3,
    Risk
  )) %>%
  mutate(Risk = ifelse(
    is.na(Risk) &
      Protocol.Months.On.Stability <= 3 &
      (
        Numeric.Parameter.Value > upr |
          Numeric.Parameter.Value < lwr
      ) & SL.Breach == "No",
    4,
    Risk
  )) %>%
  filter(.$Risk <= 4)

chart_alarms_list <- chart_alarms %>% split(.$Folder)

r <- lapply(names(chart_alarms_list), function(i){
  #output file name
  output_excel_name <- paste(chart_alarms_list[[i]]$Folder[1],chart_alarms_list[[i]]$Product.Name[1],"_","Charting_Alarms_Report.xlsx",sep="");
  #contents of file: Removing Var 1 = Folder
  output_excel <- chart_alarms_list[[i]] %>% select(-Folder);
  # write to excel file
  openxlsx::write.xlsx(x=output_excel,file=output_excel_name)
})
################### END FOR NOW 2024


#
#
#
#
#
#
#
#
#
#
#
##
######################### OLD #############################################
#################### new 05Dec23 ############################################################
##############################################################################################################################
#############################################################################################################################
# y <- lapply(names(df_all_list), function(i) {
# Grouping = df_all_list[[i]]$`Grouping.Parameter`[[1]]
# names(Grouping) = "Grouping.Parameter"
# Spec_Max = df_all_list[[i]]$`Spec.Max`[[1]]
# names(Spec_Max) = "Spec.Max.Value"
# Spec_Min = df_all_list[[i]]$`Spec.Min`[[1]]
# names(Spec_Min) = "Spec.Min.Value"
# # Batch = df_all_list[[i]]$Batch.Number[[1]]
# # names(Batch) = "Batch.Number"
# # Project_TP_Count = df_all_list[[i]]$Project.TP.Count[[1]]
# # names(Project_TP_Count) = "Project.TP.Count"
# Mature_Group = df_all_list[[i]]$Mature.Group[[1]]
# names(Mature_Group) = "Mature.Group"
# Model = df_all_list[[i]]$Model[[1]]
# names(Model) = "Model"
# Spec_Type = df_all_list[[i]]$Spec.Type[[1]]
# names(Spec_Type) = "Spec.Type"
#   #newx <-
#   #  seq(min(df_all_list[[i]]$`Protocol.Months.On.Stability`),
#   #      120,
#   #      by = 1) ### changed to 1 from 0.1
#   ### New x-axis, TP seq 0 to 120 at intervals specified [months]
#   ### run the lm on the pooled data above
#   if(df_all_list[[i]]$Model[[1]] == "Upper" | 
#      df_all_list[[i]]$Model[[1]] == "Lower"){
#     CI_level = 0.90
#   } else{
#     CI_level = 0.95
#   }
#   if(df_all_list[[i]]$Model[[1]] != "No Model"){
#     if(df_all_list[[i]]$Model[[1]] == "Unequal"){
#       Batches_list <- df_all_list[[i]] %>% distinct(Batch.Number)
#       #print(Batches_list)
#       vector_batches <- Batches_list[['Batch.Number']]
#       newx <-
#         expand.grid(Protocol.Months.On.Stability = newx,
#                     Batch.Number = vector_batches)
#       #print(new_data)
#       #lm_model <- lm(formula = `Numeric.Parameter.Value` ~ `Protocol.Months.On.Stability` * `Batch.Number` ,
#       #               data = df_all_list[[i]]);
#       lm_model <-
#         lm(
#           formula = `Numeric.Parameter.Value` ~ `Protocol.Months.On.Stability` * `Batch.Number`,
#           data = df_all_list[[i]]
#         )
#       CI <- # breaking here when the model is Unequal
#         predict(
#           lm_model,
#           newdata = data.frame(newx),
#           interval = "confidence",
#           level = CI_level
#         )%>% round_df(.,2)
#       CI_time <- cbind(CI, newx)
#       CI_time_splitBatch <-
#         split(CI_time, CI_time$Batch.Number)
#       #print(CI_time_splitBatch)
#       ### lapply
#       lapply(names(CI_time_splitBatch), function(j) {
#         Intercept <-
#           CI_time_splitBatch[[j]] %>% as.data.frame() %>% filter(.$Protocol.Months.On.Stability == "0") %>% select(fit) %>% round_df(., 2)
#         Shelf_lifes <-
#           calc_SL(CI_time_splitBatch[[j]], df_all_list[[i]]) #%>% round_df(., 0)
#         All_Shelf_lifes <<-
#           c(
#             All_Shelf_lifes,
#             Model,
#             Grouping,
#             Mature_Group,
#             #Batch,
#             #Project_TP_Count,
#             Shelf_lifes,
#             Spec_Min,
#             Spec_Max,
#             Spec_Type,
#             Intercept
#           )
#         #print(All_Shelf_lifes)
#       })
#     }
#     else{
#       lm_model <- lm(formula = `Numeric.Parameter.Value` ~ `Protocol.Months.On.Stability`,
#                      data = df_all_list[[i]]);
#       newx <-
#         seq(min(df_all_list[[i]]$`Protocol.Months.On.Stability`),
#             120,
#             by = 1) ### changed to 1 from 0.1
#       CI <- # breaking here when the model is Unequal
#         predict(
#           lm_model,
#           newdata = data.frame(Protocol.Months.On.Stability = newx),
#           interval = "confidence",
#           level = CI_level
#         )%>% round_df(.,2)
#     }
#       ### bind TP
#       CI_time <- cbind(CI, newx)
#       #print(CI_time)
#       ### get intercept of prediction line
#         Intercept <-
#           CI_time %>% as.data.frame() %>% filter(.$newx == "0") %>% select(fit) # changed from 0 to 1 due to error 29Mar24
#         names(Intercept) = "Intercept"
#         print(df_all_list[[i]]$Grouping.Parameter[[1]])
#         print(df_all_list[[i]]$Batch.Number[[1]])
#         print(Intercept)
#       # print("Intercept CI 90")
#       #  print(Intercept)
#       ### calculate shelf life
#       Shelf_lifes <-
#         calc_SL(as.data.frame(CI_time), df_all_list[[i]])
#       names(Shelf_lifes) = c("Lower_SL", "Upper_SL")
#       All_Shelf_lifes <<-
#         c(
#           All_Shelf_lifes,
#           Model,
#           Grouping,
#           Mature_Group,
#           #Batch,
#           #Project_TP_Count,
#           Shelf_lifes,
#           Spec_Min,
#           Spec_Max,
#           Spec_Type,
#           Intercept
#         )
#     }
#   else{
#     print("It's me")
#   }
# }
# )
### most recent error
# Error in `filter()`:
#   i In argument: `.$Protocol.Months.On.Stability == 0`.
# Caused by error:
#   ! `..1` must be of size 121 or 1, not size 0.

# y <- lapply(names(df_all_list), function(i) {
#   Grouping = df_all_list[[i]]$`Grouping.Parameter`[[1]]
#   names(Grouping) = "Grouping.Parameter"
#   Spec_Max = df_all_list[[i]]$`Spec.Max`[[1]]
#   names(Spec_Max) = "Spec.Max.Value"
#   Spec_Min = df_all_list[[i]]$`Spec.Min`[[1]]
#   names(Spec_Min) = "Spec.Min.Value"
#   Batch = df_all_list[[i]]$Batch.Number[[1]]
#   names(Batch) = "Batch.Number"
#   Project_TP_Count = df_all_list[[i]]$Project.TP.Count[[1]]
#   names(Project_TP_Count) = "Project.TP.Count"
#   Mature_Group = df_all_list[[i]]$Mature.Group[[1]]
#   names(Mature_Group) = "Mature.Group"
#   Model = df_all_list[[i]]$Model[[1]]
#   names(Model) = "Model"
#   Spec_Type = df_all_list[[i]]$Spec.Type[[1]]
#   names(Spec_Type) = "Spec.Type"
#   ### New x-axis, TP seq 0 to 120 at intervals specified [months]
#   newx <-
#     seq(min(df_all_list[[i]]$`Protocol.Months.On.Stability`),
#         120,
#         by = 1) ### changed to 1 from 0.1
#   ### run the lm on the pooled data above
#   if(df_all_list[[i]]$Model[[1]] != "No Model"){
#     if(df_all_list[[i]]$Model[[1]] == "Unequal"){
#       lm_model <- lm(formula = `Numeric.Parameter.Value` ~ `Protocol.Months.On.Stability` * `Batch.Number` ,
#                      data = df_all_list[[i]]);
#     }
#     else{
#       lm_model <- lm(formula = `Numeric.Parameter.Value` ~ `Protocol.Months.On.Stability`,
#                      data = df_all_list[[i]]);
#     }
#     # pooled_lm <-
#     #   lm(formula = `Numeric.Parameter.Value` ~ `Protocol.Months.On.Stability`,
#     #      data = df_all_list[[i]])
#     #print(df_all_list)
#     ### if one sided or two sided specs, confidence interval will be different
#     if (df_all_list[[i]]$Spec.Type[[1]] == "Upper" |
#         df_all_list[[i]]$Spec.Type[[1]] == "Lower") {
#       ### predict confidence interval to 90% when only one sided spec
#       CI <- # breaking here when the model is Unequal
#         predict(
#           lm_model,
#           newdata = data.frame(Protocol.Months.On.Stability = newx),
#           interval = "confidence",
#           level = 0.90
#         )%>% round_df(.,2)}
#     else{
#       CI <-
#         predict(
#           lm_model,
#           newdata = data.frame(Protocol.Months.On.Stability = newx),
#           interval = "confidence",
#           level = 0.95
#         )%>% round_df(.,2)
#     }
#       ### bind TP
#       CI_time <- cbind(CI, newx)
#       # print("CI 90 time")
#       # print(CI_90_time)
#       ### get intercept of prediction line
#       Intercept <-
#         CI_time %>% as.data.frame() %>% filter(.$newx == "0") %>% select(fit)
#       names(Intercept) = "Intercept"
#       # print("Intercept CI 90")
#       #  print(Intercept)
#       ### calculate shelf life
#       Shelf_lifes <-
#         calc_SL(as.data.frame(CI_time), df_all_list[[i]])
#       names(Shelf_lifes) = c("Lower_SL", "Upper_SL")
#       All_Shelf_lifes <<-
#         c(
#           All_Shelf_lifes,
#           Model,
#           Grouping,
#           Mature_Group,
#           Batch,
#           Project_TP_Count,
#           Shelf_lifes,
#           Spec_Min,
#           Spec_Max,
#           Spec_Type,
#           Intercept
#         )
#     }
#   else{ 
#     print("It's me")
#   }
# }
# )

# mutate(
#   "SL_Breach" = ifelse(
#       .$Lower_SL > .$Alarm.Shelf.Life &
#       .$Upper_SL > .$Alarm.Shelf.Life,
#     "No",
#     NA
#   )
# ) %>%
# mutate(
#   "SL_Breach" = ifelse(
#     is.na(SL_Breach) &
#     .$Lower_SL <= .$Alarm.Shelf.Life &
#       .$Upper_SL <= .$Alarm.Shelf.Life,
#     "Both Specs Breached",
#     NA
#   )
# ) %>%
# mutate(
#   "SL_Breach" = ifelse(
#     is.na(SL_Breach) &
#       .$Lower_SL <= .$Alarm.Shelf.Life &
#       .$Upper_SL > .$Alarm.Shelf.Life,
#     "Lower Spec Breached",
#     "Upper Spec Breached"
#   )
# )
###
#
# Model = Pooled, Mature Group = Yes ... Most Recent Mature Only
# Model = Pooled, Mature Group = No ... All data
# Model = Unequal, Mature Group = Yes ...
# Model = Unequal, Mature Group = No ...
#
### filtering for pooled data only (for now)
# df_all_SL_1 <- df_all_SL %>%
#   filter(.$Grouping.Parameter == "Infliximab:Drug Substance:-40C:LABO_0425:Reported Result {CFU/30ml}")

# slice <- df_all_SL_1 %>% mutate(
#   max_SL = Batch.Number[which.max(Upper_SL & Lower_SL)]) %>%
#     distinct(Grouping.Parameter, max_SL)

### Calculating PI: prediction intervals for PI = Yes
#
# Calc_PI <- function(df_list) {
#   lapply(names(df_list), function(i) {
#     df <- df_list[[i]]
#     df_PI <- df_list[[i]] %>% filter(df_list[[i]]$PI == "Yes")
#     newx <- seq(0, 120, by = 1)
#     model <-
#       lm(Numeric.Parameter.Value ~ Protocol.Months.On.Stability,
#          data = df_PI)
#     CI_90 <-
#       predict(
#         model,
#         newdata =  data.frame(Protocol.Months.On.Stability = newx),
#         interval = "prediction",
#         level = 0.90
#       )
#     CI_90_time <- cbind(CI_90, newx)
#     colnames(CI_90_time) <-
#       c("fit", "lwr", "upr", "Protocol.Months.On.Stability")
#     all_data <- left_join(df, CI_90_time, copy = T)
#   })
# }
######################### ANGELA

# create list
df_Unequal_list <- df_Unequal %>% split(.$Grouping.Parameter)
### Function which creates confidence intervals of each batch in the grouping against the upper/lower specs
#
calc_unequals_PI <- function(df_list) {
  lapply(names(df_list)) {
    # Grouping Parameter of i
    Grouping <- df_list[[i]]$Grouping.Parameter[[1]]
    names(Grouping) <- "Grouping.Parameter"
    # Spec Min/Max of i
    Spec_Min <- df_list[[i]]$Spec.Min.Value[[1]]
    Spec_Max <- df_list[[i]]$Spec.Max.Value[[1]]
    names(Spec_Min) <- "Spec.Min,Value"
    names(Spec_Max) <- "Spec.Max.Value"
    # 0,120 in 0.1 intervals
    newx <- seq(min(Protocol.Months.On.Stability), 120, by = 0.1)
    names(newx) <- "New.Protocol.Months.On.Stability"
    # batch list
    batch_list <-
      df_list[[i]]$Batch.Number %>% distinct(.$Batch.Number)
    names(batch_list) <- "Batch.Number"
    # unequal model
    unequal_lm <-
      lm(
        formula = `Numeric.Parameter.Value` ~ `Protocol.Months.On.Stability` * `Batch.Number`,
        data = df_list[[i]]
      )
    
    # get confidence intervals
    CI90  <-
      predict(
        unequal_lm,
        newdata = data.frame(newx),
        interval = "confidence",
        level = 0.90
      )
    # add time to CI
    CI_90_time <- cbind(CI90, newx)
    ### split by batch so can determine for each batch what is the SL
    CI_90_time_splitBatch <-
      split(CI_90_unequal_i_time, CI_90_unequal_i_time$Batch.Number)
  }
}



Derive_DF_To_Pick_BestSL <- function(GP_for_SL_calc) {
  lapply(names(GP_for_SL_calc), function(i) {
    #Set up a blank DF to hold the upper and lower SL details
    DF_look_for_best_SL <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(DF_look_for_best_SL) < -c("Grouping.Parameter",
                                       "Batch.Number",
                                       "Lower_SL",
                                       "Upper_SL")
    # newx ensures we run the CI on the predicted model out to 120 months
    newx <<-
      seq(min(GP_for_SL_calc[[i]]$`Protocol.Months.On.Stability`),
          120,
          by = 0.1)
    Spec_Max = GP_for_SL_calc[[i]]$`Spec.Max`[[1]]
    Spec_Min = GP_for_SL_calc[[i]]$`Spec.Min`[[1]]
    #Get a list of all batches in the dataset
    Batches_list <- GP_for_SL_calc[[i]] %>% distinct(Batch.Number)
    #Convert to vector
    vector_batches <- Batches_list[['Batch.Number']]
    #Ensure you get newx (i.e. 0.1 ..120) applied to each batch so we can have CIs across each batch and know which timepoint the CI line crosses
    new_data <<-
      expand.grid(Protocol.Months.On.Stability = newx,
                  Batch.Number = vector_batches)
    #Run anova
    sep_s_sep_i_lm <-
      lm(
        formula = `Numeric.Parameter.Value` ~ `Protocol.Months.On.Stability` * `Batch.Number`,
        data = GP_for_SL_calc[[i]]
      )
    
    ###########XXXXXXXXXxxNEED TO DO THIS PART STILL FOR NUMBER OF SPECS as it determine what level we run the CI to
    #if(GP_for_SL_calc[[i]]$Spec.Type[[1]] == "One"){
    ### predict confidence interval to 90% when only one sided spec
    CI_90_unequal_i <-
      predict(
        sep_s_sep_i_lm,
        newdata = data.frame(new_data),
        interval = "confidence",
        level = 0.90
      )
    CI_90_unequal_i_time <- cbind(CI_90_unequal_i, new_data)
    ### split by batch so can determine for each batch what is the SL
    CI_90_unequal_i_time_splitBatch <-
      split(CI_90_unequal_i_time, CI_90_unequal_i_time$Batch.Number)
    #Break up the DF with the CI i.e. fit, upr CI, lower Ci, Month, Intercept by Batch and then pass each split out CI (by batch) into the Calc_SL method
    lapply(names(CI_90_unequal_i_time_splitBatch), function(j) {
      print(paste("batch", j))
      Intercept <-
        CI_90_unequal_i_time_splitBatch[[j]] %>% as.data.frame() %>% filter(.$Protocol.Months.On.Stability == "0") %>% select(fit)
      #Calc_SL returns upper and lower CI
      Shelf_lifes <-
        calc_SL(CI_90_unequal_i_time_splitBatch[[j]], GP_for_SL_calc[[i]])# CI_90_unequal_i_time_splitBatch[[j]])#
      #Dont need below yet - havent set up the SL output file and we may not need
      All_Shelf_lifes_for_outputfile <<-
        c(
          All_Shelf_lifes_for_outputfile,
          j,
          GP_for_SL_calc[[i]]$Grouping.Parameter[[1]],
          Shelf_lifes,
          Spec_Min,
          Spec_Max,
          Intercept,
          GP_for_SL_calc[[i]]$Model[[1]]
        )
      #Wanted data stored in data frame so append on the data to the no of rows+1 so starts at position 1 (as declared blank DF above) and appends on to end
      DF_look_for_best_SL[nrow(DF_look_for_best_SL) + 1, ] <<-
        c(i, j, Shelf_lifes[1], Shelf_lifes[2])
    })
    return(DF_look_for_best_SL)
  })
}
PI_Unequal_Output <- Derive_DF_To_Pick_BestSL(df_Unequal_list)
### Testing
### Identifies single Project for PI generation of unequal models
#
Unequal_PI_Function <- function(df_list) {
  lapply(names(df_list), function(i) {
    # creates new x-axis sequencing data from 0 to 120 in 0.1 intervals
    tryCatch({
      expr =
        newx <-
        seq(min(df_list[[i]]$`Protocol.Months.On.Stability`), 120, by = 0.1)
      # Spec
      Grouping <- df_list[[i]]$Grouping.Parameter[[1]]
      Spec_Max <- df_list[[i]]$Spec.Max.Value[[1]]
      Spec_Min <- df_list[[i]]$Spec.Min.Value[[1]]
      # Get list of batches per grouping
      Batch_List <- df_list[[i]] %>% distinct(Batch.Number)
      # Convert to vector
      Batch_Vector <- Batch_List[["Batch.Number"]]
      # Create Batch & Newx
      New_Data <<-
        expand.grid(Protocol.Months.On.Stability = newx,
                    Batch.Number = Batch_Vector)
      # Run ANOVA
      Unequal_Model <-
        lm(
          formula = `Numeric.Parameter.Value` ~ `Protocol.Months.On.Stability` * `Batch.Number`,
          data = df_list[[i]]
        )
      
      #
      CI_90_Unequal <-
        predict(
          Unequal_Model,
          New_Data = data.frame(New_Data),
          interval = "confidence",
          level = 0.90
        )
      # Add x-axis TP
      CI_90_Unequal_Time <-
        cbind(Grouping, CI_90_Unequal, New_Data, Spec_Min, Spec_Max)
      # Split by Batch
      CI_90_Unequal_Time_Batch <-
        split(CI_90_Unequal_Time, CI_90_Unequal_Time$Batch.Number)
    },
    error = function(e) {
      NULL
    })
  })
}
#
PI_Unequal_Output <- Unequal_PI_Function(df_Unequal_list)
#
Derive_DF_To_Pick_BestSL <- function(GP_for_SL_calc) {
  lapply(names(GP_for_SL_calc), function(i) {
    #Set up a blank DF to hold the upper and lower SL details
    DF_look_for_best_SL <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(DF_look_for_best_SL) <-
      c("Grouping.Parameter",
        "Batch.Number",
        "Lower_SL",
        "Upper_SL")
    #newx ensures we run the CI on the predicted model out to 120 months
    newx <<-
      seq(min(GP_for_SL_calc[[i]]$`Protocol.Months.On.Stability`),
          120,
          by = 0.1)
    Spec_Max = GP_for_SL_calc[[i]]$`Spec.Max`[[1]]
    Spec_Min = GP_for_SL_calc[[i]]$`Spec.Min`[[1]]
    #Get a list of all batches in the dataset
    Batches_list <- GP_for_SL_calc[[i]] %>% distinct(Batch.Number)
    #Convert to vector
    vector_batches <- Batches_list[['Batch.Number']]
    #Ensure you get newx (i.e. 0.1 ..120) applied to each batch so we can have CIs across each batch and know which timepoint the CI line crosses
    new_data <<-
      expand.grid(Protocol.Months.On.Stability = newx,
                  Batch.Number = vector_batches)
    #Run anova
    sep_s_sep_i_lm <-
      lm(
        formula = `Numeric.Parameter.Value` ~ `Protocol.Months.On.Stability` * `Batch.Number`,
        data = GP_for_SL_calc[[i]]
      )
    
    ###########XXXXXXXXXxxNEED TO DO THIS PART STILL FOR NUMBER OF SPECS as it determine what level we run the CI to
    #if(GP_for_SL_calc[[i]]$Spec.Type[[1]] == "One"){
    ### predict confidence interval to 90% when only one sided spec
    CI_90_unequal_i <-
      predict(
        sep_s_sep_i_lm,
        newdata = data.frame(new_data),
        interval = "confidence",
        level = 0.90
      )
    CI_90_unequal_i_time <- cbind(CI_90_unequal_i, new_data)
    ### split by batch so can determine for each batch what is the SL
    CI_90_unequal_i_time_splitBatch <-
      split(CI_90_unequal_i_time, CI_90_unequal_i_time$Batch.Number)
    #Break up the DF with the CI i.e. fit, upr CI, lower Ci, Month, Intercept by Batch and then pass each split out CI (by batch) into the Calc_SL method
    lapply(names(CI_90_unequal_i_time_splitBatch), function(j) {
      print(paste("batch", j))
      Intercept <-
        CI_90_unequal_i_time_splitBatch[[j]] %>% as.data.frame() %>% filter(.$Protocol.Months.On.Stability == "0") %>% select(fit)
      #Calc_SL returns upper and lower CI
      Shelf_lifes <-
        calc_SL(CI_90_unequal_i_time_splitBatch[[j]], GP_for_SL_calc[[i]])# CI_90_unequal_i_time_splitBatch[[j]])#
      #Dont need below yet - havent set up the SL output file and we may not need
      All_Shelf_lifes_for_outputfile <<-
        c(
          All_Shelf_lifes_for_outputfile,
          j,
          GP_for_SL_calc[[i]]$Grouping.Parameter[[1]],
          Shelf_lifes,
          Spec_Min,
          Spec_Max,
          Intercept,
          GP_for_SL_calc[[i]]$Model[[1]]
        )
      #Wanted data stored in data frame so append on the data to the no of rows+1 so starts at position 1 (as declared blank DF above) and appends on to end
      DF_look_for_best_SL[nrow(DF_look_for_best_SL) + 1, ] <<-
        c(i, j, Shelf_lifes[1], Shelf_lifes[2])
    })
    return(DF_look_for_best_SL)
  })
}
###########################################################################################################################


##########################################################################################################################
#FUNCTION TO CALCULATE SHELF LIFE and RETURN UPPER and LOWER SL
#######Input is df with fit,lwr,upr, timepoint, batch
calc_SL <- function(SL_data_input, df_to_test) {
  #Below variables must be assigned a value in order to include them in the return statement
  lower_SL_TP = "120"
  Upper_SL_TP = "120"
  # Spec_Min <- df_to_test$Spec.Min.Value[[1]]
  #Spec_Max <- df_to_test$Spec.Max.Value[[1]]
  Spec_Min <- 10.0
  Spec_Max <- 18.1
  #input order is 'fit lwr upr timepoint Batch'
  tryCatch(
    expr = {
      cross_lower_spec_index <-
        which(as.data.frame(SL_data_input)$lwr <= Spec_Min)[[1]]
      #the index of the match is assigned to cross_lower_spec_index  #Use the index of the match to pull out the time from the 4th column in SL_data_input
      lower_SL_TP <- SL_data_input[cross_lower_spec_index, 4]
      lower_SL_value <- SL_data_input[cross_lower_spec_index, 2]
    },
    error = function(e)
    {
      if (is.na(Spec_Min)) {
        message(paste("There is no Lower Spec"))
      }
      else{
        message(paste("There are no lower shelf life violtations for 10 years"))
      }
      print(e)
    }
  )
  tryCatch(
    expr = {
      cross_upper_spec_index <-
        which(as.data.frame(SL_data_input)$upr >= Spec_Max)[[1]]
      Upper_SL_TP <- SL_data_input[cross_upper_spec_index, 4]
      Upper_SL_value <- SL_data_input[cross_upper_spec_index, 3]
    },
    error = function(e)
    {
      if (is.na(Spec_Max)) {
        message(paste("There is no Upper Spec"))
      }
      else{
        message(paste("There are no upper shelf life violtations for 10 years"))
      }
      print(e)
    }
  )
  return(c(lower_SL_TP, Upper_SL_TP))
}
################################################################################################################################

################################################################################################################################
# FUNCTION TO REMOVE calcualted SL that falls before the Filed SL as these are violations
RemoveSLViolators <- function(DF_look_for_best_SL_withfiled_SL) {
  #indexes the DF to only pull out rows where $upper and $lower are >Filed_SL
  NoLowerViolations <<-
    DF_look_for_best_SL_withfiled_SL[as.numeric(DF_look_for_best_SL_withfiled_SL$Lower_SL) >
                                       DF_look_for_best_SL_withfiled_SL$Filed.Shelf.Life.Months, ]
  NoUpper_LowerViolations <<-
    NoLowerViolations[as.numeric(NoLowerViolations$Upper_SL) > NoLowerViolations$Filed.Shelf.Life.Months, ]
  #returns the DF without the violators
}
############################## CPV ##########################################################
############################################################################################
# Read in data
df <-
  read_excel("GC CPV Data.xlsx") #ActiveDataSet #Holds the active dataset(s)~ActiveDataSet #Holds the active dataset(s)
# Replace spaces in variable names with "."
names(df) <- gsub(" ", ".", names(df)) # working 04Jul23
# Change Batch Number to Batch Number:Project Name for Grouping Parameters with more than 1 study per batch number
df_clean_batch <- Identify_Batch_MoreThanOne_Study(df)
# Calculate Shelf life, get max testing target date (most current) difference between stability base date & expiry date
df_shelf_life <- Calculate_RegShelf_Life(df_clean_batch)
# Check if a study is "mature" = tested at/beyond filed shelf life, 1 = Yes, 0 = No
df_MatureStudy <- Identify_mature_or_not(df_shelf_life)
# Merge Mature Study information to the dataset
df_Mature_Study_df <- df_MatureStudy %>%
  select(`Grouping.Parameter`,
         `Batch.Number`,
         `Project.Name`,
         `Mature.Study`) %>%
  left_join(df_1, .)
# Count how many mature studies are in a grouping parameter
df_Mature_Study_count <- Mature_Study_Count(df_Mature_Study_df)
# Add mature study count per grouping back to dataset
df_2 <- df_Mature_Study_df %>%
  left_join(., df_Mature_Study_count)
# Filter the dataset for grouping which have >= 5 mature studies
df_mature_group <- df_2 %>%
  filter(Mature.Count >= 5)
# Filter the dataset for grouping which have < 5 matures studies
df_immature_group <- df_2 %>%
  filter(Mature.Count < 5)
# From the groupings with at least 5 mature studies, get the 5 most recent studies
df_mature_5recent <-
  Identify_5mostrecent_mature(df_mature_group) # This gives a WARNING MESSAGE
#######################
#Set up anova variables
#######################
# Merge the Immature Groups with the 5 most recent Mature Group
anova_df <-
  rbind(df_mature_5recent, df_immature_group) # !! need to add try catch for when either list is blank
# Convert anova_df to list for lapply ANOVA
anova_df_list <- anova_df %>%
  split(.$Grouping.Parameter)
# Create an list object called out
out <- list()
# Perform ANOVA
anova_results <- RunAnova(anova_df_list)
# Convert ANOVA output list into a dataframe
anova_results_df <- rbindlist(anova_results)
# Get a list of the folders by grouping parameter
df_folders <- df %>%
  distinct(Folder, Grouping.Parameter)
# Add folder variable
anova_results_folder <- df_folders %>%
  left_join(., anova_results_df, "Grouping.Parameter")
# unstack the ANOVA output
anova_res_unstack <- anova_results_folder %>%
  as.data.frame() %>%
  filter(.$Effect != "Protocol.Months.On.Stability") %>%
  mutate(Type = ifelse(Effect == "Batch.Number", "Intercept", "Slope")) %>%
  select(Folder, Grouping.Parameter, Type, p) %>%
  pivot_wider(names_from = Type, values_from = p) %>%
  mutate(`Remove` = ifelse(is.na(.$Slope) |
                             is.na(.$Intercept), "Yes", "No")) %>%
  filter(Remove == "No") %>%
  select(-Remove) %>%
  mutate(Model = ifelse(Slope <= 0.05, "Unequal", "Pooled"))
# Join the ANONVA unstack to the dataset, adds Model
df_add_model <- df_2 %>%
  left_join(., anova_res_unstack) %>% mutate(Model = ifelse(is.na(Model), "Too early for CPV", Model))
# split up data sets based on Model from 1st ANOVA
df_pooled <- df_add_model %>% filter(Model == "Pooled")
df_unequal <- df_add_model %>% filter(Model == "Unequal")
df_too_early_CPV <-
  df_add_model %>% filter(Model == "Too early for CPV")
#
# of the pooled, we need to remove the older mature batches which aren't used to calc PI
df_pooled_1 <- df_pooled %>%
  anova_df_PI <- Identify_Pooled_from_Mature_Immature(anova_df)
#
df_add_PI_pooled <- df_add_model %>%
  left_join(., anova_df_PI) %>%
  filter(Model == "Pooled") %>%
  filter(PI == "Yes" | Mature.Study == 0)
#
#Identify_Pooled_from_Mature_Immature
#
df_pool_chart_list <-
  df_add_PI_pooled %>% split(.$Grouping.Parameter)
Chart_df <- Charting_df_PI(df_pool_chart_list) %>% rbindlist(.)
#
Chart_df_test <- Chart_df %>%
  mutate(PI = ifelse(is.na(PI), "No", PI)) %>%
  mutate(Batch.Number = ifelse(PI == "Yes", paste0(Batch.Number, "*"), paste0(Batch.Number)))
Chart_df_list <- Chart_df_test %>% split(.$Grouping.Parameter)
########################################################################################################################
### Charting
N <- 100
good.shapes <- c(1:25, 33:127)
plot_CPV <- lapply(names(Chart_df_list), function(i) {
  output_plot_name <<-
    paste(Chart_df_list[[i]]$`Folder`[[1]],
          Chart_df_list[[i]]$`Grouping.Parameter`[[1]],
          sep = "$")
  
  p <- ggplot(
    Chart_df_list[[i]],
    aes(
      x = Chart_df_list[[i]]$Protocol.Months.On.Stability,
      y = Chart_df_list[[i]]$Numeric.Parameter.Value,
      color = Chart_df_list[[i]]$Batch.Number,
      shape = Chart_df_list[[i]]$Batch.Number
    )
  ) +
    scale_shape_manual(values = good.shapes[1:N]) +
    scale_color_manual(values = good.shapes[1:N]) +
    geom_point() +
    geom_line(aes(y = Spec.Min.Value[[1]]), color = "black") +
    geom_line(aes(y = Spec.Max.Value[[1]]), color = "black") +
    geom_line(aes(y = lwr), col = "red") + #lwr pred interval
    geom_line(aes(y = upr), col = "red") + #upr pred interval
    ggtitle(Chart_df_list[[i]]$`Grouping.Parameter`[[1]]) + #,subtitle = "Equal Intercepts, Equal Slopes") +
    ylab(paste(Chart_df_list[[i]]$`Parameter.[w/Units].Field.Name`[[1]])) +
    labs(caption = c(
      "Data Verification Required for GMP use as per SOP-00680",
      format(Sys.time(), "%d-%b-%Y %H:%M:%S")
    )) +
    theme(plot.caption = element_text(hjust = c(0, 1))) +
    #scale_x_continuous(limits = c(0, df_chart[[i]]$`XMax`[1]), breaks = seq(0, df_chart[[i]]$`XMax`[1], 3))+
    #scale_x_continuous(n.breaks=10) + # no. of breaks
    theme(
      axis.ticks = element_line(size = 0.2),
      axis.text.x = element_text(vjust = 0.5, hjust = 0.5),
      #angle = 90,
      axis.title.x = element_text(),
      text = element_text(size = 6),
      panel.background = element_rect(
        fill = "white",
        colour = "black",
        size = 0.2,
        linetype = "solid"
      ),
      panel.grid.major = element_line(
        size = 0.2,
        linetype = 'solid',
        colour = "#D8F8F7"
      ),
      panel.grid.minor = element_line(
        size = 0.2,
        linetype = 'solid',
        colour = "#D8F8F7"
      ),
      plot.background = element_rect(fill = "#F8F8ED"),
      legend.background = element_rect(fill = "#F8F8ED"),
      legend.key.height = unit(0.05, "cm"),
      legend.key.width = unit(0.05, "cm"),
      legend.title = element_text(size = 6),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )#,
  ggsave(
    filename = paste0(
      sub("\\$.*", "", output_plot_name),
      "/",
      make_clean_names(sub(".*\\$", "", output_plot_name)),
      ".png"
    ),
    width = 8,
    height = 4,
    plot = p
  )
  
  #print(p)
})
####################################Functions######################################################################
# FUNCTION
#Method to identify batches with more than one study and attach it to batch name
Identify_Batch_MoreThanOne_Study <- function(df) {
  df %>%
    distinct(Grouping.Parameter, Batch.Number, Project.Name) %>%
    select(-Project.Name) %>%
    count() %>%
    left_join(., df) %>%
    mutate(Batch.Number = ifelse(
      freq > 1,
      paste(Batch.Number, Project.Name, sep = ":"),
      Batch.Number
    )) %>%
    mutate(
      Spec.Grouping = paste(
        Product.Name,
        Sub.Product.Name,
        Assay.Name,
        Parameter.Field.Name,
        sep = "_"
      )
    ) %>%
    select(-freq)
}
##################################################################################################################
# FUNCTION
#Method to calculate the registered shelf life using the Expiry date - Stability base date.
Calculate_RegShelf_Life <- function(df_clean_batch) {
  df_clean_batch %>%
    filter(
      !is.na(`Stability.Base.Date`) |
        !is.na(`Expiry.Date`) |
        !is.na(`Testing.Target.Date`)
    ) %>% #NOT WORKING
    distinct(
      Folder,
      `Manufacture.Site`,
      `Product.Name`,
      `Sub.Product.Name`,
      `Batch.Number`,
      `Project.Name`,
      `Stability.Base.Date`,
      `Expiry.Date`,
      `Testing.Target.Date`
    ) %>%
    group_by(Folder,
             `Manufacture.Site`,
             `Product.Name`,
             `Sub.Product.Name`) %>%
    slice(which.max(Testing.Target.Date)) %>%
    mutate(`Filed.Shelf.Life.Days` = difftime(Expiry.Date, Stability.Base.Date, units =
                                                'days')) %>%
    mutate(`Filed.Shelf.Life.Months` = as.numeric(round(`Filed.Shelf.Life.Days` /
                                                          30.14, 0))) %>%
    distinct(
      Folder,
      `Manufacture.Site`,
      `Product.Name`,
      `Sub.Product.Name`,
      `Filed.Shelf.Life.Months`
    )
}
###################################################################################################################
# FUNCTION
# Function to identify by grouping, if the study is mature or not
Identify_mature_or_not <- function(df_1) {
  df_1 %>%
    distinct(
      Folder,
      `Manufacture.Site`,
      `Product.Name`,
      `Sub.Product.Name`,
      `Grouping.Parameter`,
      `Batch.Number`,
      `Project.Name`,
      `Protocol.Months.On.Stability`,
      `Filed.Shelf.Life.Months`
    ) %>%
    group_by(Grouping.Parameter, Batch.Number, Project.Name) %>%
    slice(which.max(Protocol.Months.On.Stability)) %>%
    mutate(
      `Mature.Study` = ifelse(
        `Protocol.Months.On.Stability` > `Filed.Shelf.Life.Months`,
        1,
        0
      )
    )
}
###################################################################################################################
# FUNCTION
# Function to count the number of mature studies (identified by Mature=1)
Mature_Study_Count <- function(df_Mature_Study_df) {
  df_Mature_Study_df %>%
    distinct(Grouping.Parameter,
             Batch.Number,
             Project.Name,
             Mature.Study) %>%
    group_by(Grouping.Parameter) %>%
    tally(Mature.Study == 1) %>%
    mutate(`Mature.Count` = n)
}
#####################################################################################################################
# FUNCTION
## Function to sort the stability base dates decreasing, then slice the first 5
Identify_5mostrecent_mature <- function(df_mature_group) {
  df_mature_group %>% # 5 most recent studies from df_mature per grouping only
    filter(Mature.Study == 1) %>%
    distinct(Grouping.Parameter,
             Batch.Number,
             Project.Name,
             Stability.Base.Date) %>%
    mutate(Date = as.Date(Stability.Base.Date, '%m/%d/%Y')) %>%
    group_by(Grouping.Parameter) %>%
    arrange(desc(Date)) %>%
    slice(1:5) %>%
    mutate(Most.Recent = "Yes") %>%
    left_join(., df_mature_group)
}
######################################################################################################################
# FUNCTION
#Function to run anova
RunAnova <- function(anova_df_list) {
  #anova_results <-
  lapply(names(anova_df_list), function(i) {
    df <- data.frame(anova_df_list[[i]])
    #grouping <- df$Grouping.Parameter[1]
    tryCatch({
      expr =
        grouping <- df$Grouping.Parameter[1]
      names(grouping) <- "Grouping.Parameter"
      anova_output <-
        anova_test(data = df,
                   Numeric.Parameter.Value ~ Batch.Number * Protocol.Months.On.Stability)
      out <- c(out, grouping, anova_output)
      return(out)
    },
    error = function(e) {
      NULL
    }) # !! could we spit out grouping parameter & NULL so we know what ones are the issue
  })
}
#######################################################################################################################
# FUNCTION
# Function to Unstack ANOVA output
Unstack_Anova <- function(anova_results_folder)
  anova_res_unstack <- anova_results_folder %>%
  as.data.frame() %>%
  filter(.$Effect != "Protocol.Months.On.Stability") %>%
  mutate(Type = ifelse(Effect == "Batch.Number", "Intercept", "Slope")) %>%
  select(Folder, Grouping.Parameter, Type, p) %>%
  pivot_wider(names_from = Type, values_from = p) %>%
  mutate(`Remove` = ifelse(is.na(.$Slope) |
                             is.na(.$Intercept), "Yes", "No")) %>%
  filter(Remove == "No") %>%
  select(-Remove) %>%
  mutate(Model = ifelse(Slope <= 0.05, "Unequal", "Pooled"))
########################################################################################################################
# FUNCTION
Identify_Pooled_from_Mature_Immature <- function(anova_df) {
  anova_df %>%
    left_join(., anova_res_unstack) %>% # gives you your dataset (5recent & immature) the model assigned, missing your not recent studies of mature grouping
    mutate(`PI` = ifelse(`Model` == "Pooled", "Yes", "No")) %>%
    filter(`PI` == "Yes") %>%
    distinct(`Grouping.Parameter`, `Batch.Number`, `Project.Name`, `PI`)
}
#########################################################################################################################
### FUNCTION to calculate PI with newx (0:120,step=1), merge back to dataset
Charting_df_PI <- function(df_pool_chart_list) {
  lapply(names(df_pool_chart_list), function(i) {
    data <- df_pool_chart_list[[i]]
    data_PI <-
      df_pool_chart_list[[i]] %>% filter(df_pool_chart_list[[i]]$PI == "Yes")
    newx <- seq(0, 120, by = 1)
    model <-
      lm(Numeric.Parameter.Value ~ Protocol.Months.On.Stability,
         data = data_PI)
    CI_90 <-
      predict(
        model,
        newdata =  data.frame(Protocol.Months.On.Stability = newx),
        interval = "prediction",
        level = 0.90
      )
    CI_90_time <- cbind(CI_90, newx)
    colnames(CI_90_time) <-
      c("fit", "lwr", "upr", "Protocol.Months.On.Stability")
    all_data <- left_join(data, CI_90_time, copy = T)
  })
}
##########################################################################################################################
# FUNCTION
Add_star <- function(Data, ID, Variable) {
  Data %>%
    mutate(`ID` = ifelse(`Variable` == "Yes", paste0(`ID`, "*"), paste0(`ID`)))
}
##############################################################################################################################
## END    20Nov23


calc_SL <- function(SL_data_input, df_to_test) {
  #Below variables must be assigned a value in order to include them in the return statement
  lower_SL_TP = "120"
  names(lower_SL_TP) = "Lower_SL"
  Upper_SL_TP = "120"
  names(Upper_SL_TP) = "Upper_SL"
  Spec_Min <- df_to_test$Spec.Min.Value[[1]]
  Spec_Max <- df_to_test$Spec.Max.Value[[1]]
  # Spec_Min <- 10.0
  # Spec_Max <- 18.1
  #input order is 'fit lwr upr timepoint Batch'
  tryCatch(
    expr = {
      cross_lower_spec_index <-
        which(as.data.frame(SL_data_input)$lwr <= Spec_Min)[[1]]
      #the index of the match is assigned to cross_lower_spec_index  #Use the index of the match to pull out the time from the 4th column in SL_data_input
      lower_SL_TP <- SL_data_input[cross_lower_spec_index, 4]
      lower_SL_value <- SL_data_input[cross_lower_spec_index, 2]
    },
    error = function(e)
    {
      if (is.na(Spec_Min)) {
        message(paste("There is no Lower Spec"))
      }
      else{
        message(paste("There are no lower shelf life violtations for 10 years"))
      }
      print(e)
    }
  )
  tryCatch(
    expr = {
      cross_upper_spec_index <-
        which(as.data.frame(SL_data_input)$upr >= Spec_Max)[[1]]
      Upper_SL_TP <- SL_data_input[cross_upper_spec_index, 4]
      Upper_SL_value <- SL_data_input[cross_upper_spec_index, 3]
    },
    error = function(e)
    {
      if (is.na(Spec_Max)) {
        message(paste("There is no Upper Spec"))
      }
      else{
        message(paste("There are no upper shelf life violtations for 10 years"))
      }
      print(e)
    }
  )
  return(c(lower_SL_TP, Upper_SL_TP))
}


### testing 08Dec23-> commented out 26Mar24
# All_Shelf_lifes <- list()
# y <- lapply(names(df_all_list), function(i) {
#   Grouping = df_all_list[[i]]$`Grouping.Parameter`[[1]]
#   names(Grouping) = "Grouping.Parameter"
#   Spec_Max = df_all_list[[i]]$`Spec.Max`[[1]]
#   names(Spec_Max) = "Spec.Max.Value"
#   Spec_Min = df_all_list[[i]]$`Spec.Min`[[1]]
#   names(Spec_Min) = "Spec.Min.Value"
#   Batch = df_all_list[[i]]$Batch.Number[[1]]
#   names(Batch) = "Batch.Number"
#   Project_TP_Count = df_all_list[[i]]$Project.TP.Count[[1]]
#   names(Project_TP_Count) = "Project.TP.Count"
#   Mature_Group = df_all_list[[i]]$Mature.Group[[1]]
#   names(Mature_Group) = "Mature.Group"
#   Model = df_all_list[[i]]$Model[[1]]
#   names(Model) = "Model"
#   Spec_Type = df_all_list[[i]]$Spec.Type[[1]]
#   names(Spec_Type) = "Spec.Type"
#   ### New x-axis, TP seq 0 to 120 at intervals specified [months]
#   newx <-
#     seq(min(df_all_list[[i]]$`Protocol.Months.On.Stability`),
#         120,
#         by = 0.1)
#   ### run the lm on the pooled data above
#   pooled_lm <-
#     lm(formula = `Numeric.Parameter.Value` ~ `Protocol.Months.On.Stability`,
#        data = df_all_list[[i]])
#   
#   ### if one sided or two sided specs, confidence interval will be different
#   if (df_all_list[[i]]$Spec.Type[[1]] == "Upper" |
#       df_all_list[[i]]$Spec.Type[[1]] == "Lower") {
#     ### predict confidence interval to 90% when only one sided spec
#     CI_90 <-
#       predict(
#         pooled_lm,
#         newdata = data.frame(Protocol.Months.On.Stability = newx),
#         interval = "confidence",
#         level = 0.90
#       )
#     ### bind TP
#     CI_90_time <- cbind(CI_90, newx)
#     ### get intercept of prediction line
#     Intercept <-
#       CI_90_time %>% as.data.frame() %>% filter(.$newx == "0") %>% select(fit)
#     names(Intercept) = "Intercept"
#     ### calculate shelf life
#     Shelf_lifes <-
#       calc_SL(as.data.frame(CI_90_time), df_all_list[[i]])
#     
#     names(Shelf_lifes) = c("Lower_SL", "Upper_SL")
#     All_Shelf_lifes <<-
#       c(
#         All_Shelf_lifes,
#         Model,
#         Grouping,
#         Mature_Group,
#         Batch,
#         Project_TP_Count,
#         Shelf_lifes,
#         Spec_Min,
#         Spec_Max,
#         Spec_Type,
#         Intercept
#       )
#   }
#   else if (df_all_list[[i]]$Spec.Type[[1]] == "Both" |
#            df_all_list[[i]]$Spec.Type[[1]] == "None") {
#     ### predict confidence interval to 95% when two-sided spec
#     CI_95 <-
#       predict(
#         pooled_lm,
#         newdata = data.frame(Protocol.Months.On.Stability = newx),
#         interval = "confidence",
#         level = 0.95
#       )
#     ### bind TP
#     CI_95_time <- cbind(CI_95, newx)
#     ### get intercept of prediction line
#     Intercept <-
#       CI_95_time %>% as.data.frame() %>% filter(.$newx == "0") %>% select(fit) # works on getting intercept
#     names(Intercept) = "Intercept"
#     ### calculate shelf life
#     Shelf_lifes <-
#       calc_SL(as.data.frame(CI_95_time), df_all_list[[i]])
#     
#     names(Shelf_lifes) = c("Lower_SL", "Upper_SL")
#     All_Shelf_lifes <<-
#       c(
#         All_Shelf_lifes,
#         Model,
#         Grouping,
#         Mature_Group,
#         Batch,
#         Project_TP_Count,
#         Shelf_lifes,
#         Spec_Min,
#         Spec_Max,
#         Spec_Type,
#         Intercept
#       )
#   } else {
#     NULL #enter this else when you have no spec min or max so we set the SLs to be NA
#     Intercept = "NA"
#     names(Intercept) = "Intercept"
#     Shelf_lifes = c("NA", "NA")
#     names(Shelf_lifes) = c("Lower_SL", "Upper_SL")
#     All_Shelf_lifes <<-
#       c(
#         All_Shelf_lifes,
#         Model,
#         Grouping,
#         Mature_Grouping,
#         Batch,
#         Project_TP_Count,
#         Shelf_lifes,
#         120,
#         120,
#         Intercept
#       )
#   }
# })
#All_Shelf_lifes