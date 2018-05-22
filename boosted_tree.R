packages <- c('useful', 'coefplot', 'xgboost', 'here', 'magrittr', 'dygraphs', 'dplyr', 'RMySQL', 'caret')
purrr::walk(packages, library, character.only = TRUE)

lapply( dbListConnections( dbDriver( drv = "MySQL")), dbDisconnect)
mychannel <- dbConnect(MySQL(), user="bentley", pass="dave41", host="127.0.0.1")
query <- function(...) dbGetQuery(mychannel, ...)

source('../../RMySQL_Update.R')
var_whse <- 3
var_build <- 2

#Query to pull all lines
sqlquery <- paste("SELECT 
                  hist_equip as EQUIP,
                  CASE
                  WHEN predicted_availhour < 6 THEN 6
                  WHEN predicted_availhour > 16 THEN 16
                  ELSE predicted_availhour
                  END AS HOUR,
                  workday_workday AS WORKDAY,
                  workday_weekofmon AS MONTHWEEK,
                  workday_weekday AS WEEKDAY,
                  workday_dayofmon AS MONTHDAY,
                  workday_month AS MONTH,
                  YEAR(predicted_availdate) as YEAR,
                  workday_befvac AS BEFVAC,
                  workday_aftvac AS AFTVAC,
                  workday_befchrist AS BEFCHRIST,
                  workday_aftchrist AS AFTCHRIST,
                  SUM(CASE
                  WHEN
                  hist_twoday = 1
                  AND predicted_availhour < 6
                  THEN
                  1
                  ELSE 1
                  END) AS BOXES,
                  SUM(CASE
                  WHEN
                  hist_twoday = 1
                  AND predicted_availhour < 6
                  THEN
                  hist_cubeinch
                  ELSE hist_cubeinch
                  END) AS CUBE
                  FROM
                  slotting.hist_casevol
                  JOIN
                  slotting.workdayofweek ON predicted_availdate = workday_date
                  WHERE 
                  hist_whse = ",var_whse," and hist_build = ",var_build," 
                  AND cutoff_group NOT IN ('TRUCK' , 'COLGATE')
                  AND predicted_availhour BETWEEN 1 AND 16
                  GROUP BY predicted_availdate , hist_equip, 
                  CASE
                  WHEN predicted_availhour < 6 THEN 6
                  WHEN predicted_availhour > 16 THEN 16
                  ELSE predicted_availhour
                  END
                  ORDER BY predicted_availdate , CASE
                  WHEN predicted_availhour < 6 THEN 6
                  WHEN predicted_availhour > 16 THEN 16
                  ELSE predicted_availhour
                  END", sep = "")
data <- query(sqlquery)

set.seed(222)
trainIndex <- createDataPartition(data$BOXES, 
                                  p = .75, 
                                  list = FALSE, 
                                  times = 1)

dataTrain <- data[ trainIndex,]
dataTest  <- data[-trainIndex,]

data_formula_boxes <- BOXES ~ EQUIP + HOUR + WORKDAY + MONTHWEEK + WEEKDAY + MONTHDAY + MONTH + YEAR + BEFVAC + AFTVAC + BEFCHRIST + AFTCHRIST
data_formula_cube <- CUBE ~ EQUIP + HOUR + WORKDAY + MONTHWEEK + WEEKDAY + MONTHDAY + MONTH + YEAR + BEFVAC + AFTVAC + BEFCHRIST + AFTCHRIST

#boxes data training
dataX_Train_box <- build.x(data_formula_boxes, data=dataTrain,
                       contrasts=FALSE,
                       sparse=TRUE)
dataY_Train_box <- build.y(data_formula_boxes,data=dataTrain) 

dataX_Test_box <- build.x(data_formula_boxes, data=dataTest,
                      contrasts=FALSE,
                      sparse=TRUE)
dataY_Test_box <- build.y(data_formula_boxes,data=dataTest)

xgTrain_box <- xgb.DMatrix(data=dataX_Train_box,
                       label=dataY_Train_box)

xgVal_box <- xgb.DMatrix(data=dataX_Test_box,
                     label=dataY_Test_box)


#cube data training
dataX_Train_cube <- build.x(data_formula_cube, data=dataTrain,
                       contrasts=FALSE,
                       sparse=TRUE)
dataY_Train_cube <- build.y(data_formula_cube,data=dataTrain) 

dataX_Test_cube <- build.x(data_formula_cube, data=dataTest,
                      contrasts=FALSE,
                      sparse=TRUE)
dataY_Test_cube <- build.y(data_formula_cube,data=dataTest)

xgTrain_cube <- xgb.DMatrix(data=dataX_Train_cube,
                       label=dataY_Train_cube)

xgVal_cube <- xgb.DMatrix(data=dataX_Test_cube,
                     label=dataY_Test_cube)


# LM model
#value1 <- lm(data_formula, data=dataTrain)
#mse <- mean(residuals(value1)^2)
#rmse <- sqrt(mse)
#rmse

#validate-rmse:127.167473
#xg6 <- xgb.train(data=xgTrain, objective='reg:linear', booster='gbtree', eval_metric='rmse', nrounds=10000, 
#                 print_every_n = 20, watchlist = list(train=xgTrain, validate=xgVal), early_stopping_rounds=250,eta = .01, max_depth = 10)

#box model
xg14_box <- xgb.train(data=xgTrain_box, objective='reg:linear', eval_metric='rmse', booster='gbtree', watchlist = list(train=xgTrain_box, validate=xgVal_box), 
                  early_stopping_rounds=250, nrounds = 10000, num_parallel_tree=20, print_every_n = 20, nthread=4,eta = .01, max_depth = 7)
#cube model
xg14_cube <- xgb.train(data=xgTrain_cube, objective='reg:linear', eval_metric='rmse', booster='gbtree', watchlist = list(train=xgTrain_box, validate=xgVal_cube), 
                  early_stopping_rounds=250, nrounds = 10000, num_parallel_tree=20, print_every_n = 20, nthread=4,eta = .01, max_depth = 7)

#xgb.plot.importance(xgb.importance(xg14, feature_names=colnames(xgTrain)))
#importance <- xgb.importance(feature_names = colnames(xgTrain), model = xg14)
#head(importance)


sqlquery <- paste("SELECT 
                  equip_type as EQUIP,
                  hour_hour AS HOUR,
                  workday_workday AS WORKDAY,
                  workday_weekofmon AS MONTHWEEK,
                  workday_weekday AS WEEKDAY,
                  workday_dayofmon AS MONTHDAY,
                  workday_month AS MONTH,
                  YEAR(workday_date) AS YEAR,
                  workday_befvac AS BEFVAC,
                  workday_aftvac AS AFTVAC,
                  workday_befchrist AS BEFCHRIST,
                  workday_aftchrist AS AFTCHRIST,
                  0 AS BOXES,
                  0 as CUBE
                  FROM
                  slotting.workdayofweek
                  JOIN
                  slotting.forecasthours
                  JOIN 
                  slotting.caseforecast_equip
                  WHERE
<<<<<<< HEAD
                  workday_date between '2018-05-18' and '2018-05-18'
=======
                  workday_date between '2018-05-15' and '2018-05-15'
>>>>>>> 9bf68be8ce07951c4b8dbc6149bfa9562d0d8f36
                  AND hour_hour BETWEEN 6 AND 16
                  ORDER BY workday_date, equip_type, hour_hour", sep = "")
preddata <- query(sqlquery)
#need to build preddata as build.x
data_new_boxes <- build.x(data_formula_boxes, data=preddata, contrasts = FALSE, sparse = TRUE)
data_new_cube <- build.x(data_formula_cube, data=preddata, contrasts = FALSE, sparse = TRUE)


#build data frame to insert into printvis.forecast case
sqlquery <- paste("SELECT 
                    ",var_whse,", ",var_build,", workday_date, hour_hour, equip_type
                  FROM
                    slotting.workdayofweek
                  JOIN
                    slotting.forecasthours
                  JOIN
                    slotting.caseforecast_equip
                  WHERE
<<<<<<< HEAD
                    workday_date BETWEEN '2018-05-18' AND '2018-05-18'
=======
                    workday_date BETWEEN '2018-05-15' AND '2018-05-15'
>>>>>>> 9bf68be8ce07951c4b8dbc6149bfa9562d0d8f36
                    AND hour_hour BETWEEN 6 AND 16
                  ORDER BY workday_date , equip_type , hour_hour", sep = "")
forecast_insert <- query(sqlquery)


forecast_insert$boxes <- predict(xg14_box,newdata = data_new_boxes)
forecast_insert$cube <- predict(xg14_cube,newdata = data_new_cube)

#insert 0 for time estimate which we be populated by php update file during night stream process.
forecast_insert$fcase_minuteforecast <- 0

#update mysql table forecast_case
rmysql_update(mychannel, forecast_insert, 'slotting.forecast_case', verbose = FALSE)





