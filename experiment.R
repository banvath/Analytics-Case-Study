
#correct the paysum column
final$"Pay Amount Sum"= final$`Employee Pay Summary Net Hours Sum`*final$`Employee Pay Summary Rate Sum`

#Export the data file
write.xlsx(final, 'final.xlsx')

#check if they are identical
identical(final$"Pay Amount Sum", data$`Pay Amount Sum`) #False


#Equating with regular payrate
nrow(data_final[data_final$`Pay Category Name`=="Unpaid" , ])
unpaid_rate = data_final[data_final$`Pay Category Name`=="Unpaid" , ]
unpaid_rate$`Job Name` = impute_pay
regular_rate = data_final[data_final$`Pay Category Name`== "Regular" , ]
regular_rate = unique(data_final)
temp = as.data.frame(duplicated(regular_rate))
temp = as.data.frame(table(data_final$`Pay Category Name` ,data_final$`Job Name`) )
temp = temp[temp$Var1 =="Regular",]
regular_rate[1:3]

temp = impute_pay[impute_pay$`Pay Category Name`== "Regular" , c("Job Name" ,"Pay Category Name", "mean_total")]






nrow(data[(data$`Job Name`%in% temp$`Job Name`) & (data$`Pay Category Name`%in% temp$`Pay Category Name`),])

102625+119773

data$`Employee Pay Summary Rate Sum`
temp$`Pay Category Name`
colnames(impute_pay)

temp = impute_pay %>% filter(is.na(impute_pay$mean_total))







#Correct the pay column
impute_pay = data %>%
  group_by(`Job Name`, `Pay Category Name`)%>%
  summarise(mean_total = mean(`Employee Pay Summary Rate Sum`))


data = merge(x = data, y = impute_pay,all.x = TRUE)

data$`Employee Pay Summary Rate Sum`[is.na(data$`Employee Pay Summary Rate Sum`)] <- "k"


for(i in 1:nrow(data)){
  if(data$`Employee Pay Summary Rate Sum`[i] == "k"){ 
    data$`Employee Pay Summary Rate Sum`[i] = data$mean_total[i]
  }
  else{
    data$`Employee Pay Summary Rate Sum`[i]=data$`Employee Pay Summary Rate Sum`[i]
  }
}   

sum(is.na(data$`Employee Pay Summary Rate Sum`))

table(data$`Employee Pay Summary Rate Sum`)


copy = data
for(i in 1:nrow(copy)){
  if(copy$`Employee Pay Summary Rate Sum`[i] == "k"){ 
    copy$`Employee Pay Summary Rate Sum`[i] = copy$mean_total[i]
  }
  else{
    copy$`Employee Pay Summary Rate Sum`[i] = copy$`Employee Pay Summary Rate Sum`[i]
  }
}   
sum(is.na(ultimate$`Work Schedule`))

table(data_w_direct_indirect[data_w_direct_indirect$"Employee ID" %in% aws$`Employee ID`,"Direct/Indirect"])

copy = ultimate
copy = copy[-14]
colnames(data_w_direct_indirect)
colnames(aws)
sum(is.na(copy$`Work Schedule`))
sum(is.na(copy$`Pay Rate Type`))
temp = as.data.frame(unique(copy$`Job Name`,copy$`Direct/Indirect`))

#merge all datas to get all the columns
pre_final = merge(data_final, jobs, all.x = TRUE)
final = merge(pre_final , aws, all.x =TRUE)
sum(is.na(final$`Direct/Indirect`))
comp = data.frame(unique(data$`Job Name`))

aws[(aws[aws$`Pay Rate Type` , "Employee ID" ]!=NA) , "Employee_ID"]
copy = as.data.frame(aws[aws$`Pay Rate Type` == "Salary", "Employee ID" ])

copy[copy$`Employee Pay Summary Rate Sum`=="NA" , copy$`Employee Pay Summary Rate Sum`]= copy[copy$`Employee Pay Summary Rate Sum`=="NA" , copy$mean_total]

sum(is.na(copy$`Employee Pay Summary Rate Sum`))


table(data$`Employee Pay Summary Rate Sum`)

copy[copy$`Employee Pay Summary Rate Sum`=="NA" , "Employee Pay Summary Rate Sum"]= copy[copy$`Employee Pay Summary Rate Sum`=="NA" , "mean_total"]

sum(is.na(data[data$mean_total =="NA" , "mean_total"]))

length(copy[copy$`Employee Pay Summary Rate Sum`=="NA" , "mean_total"])
