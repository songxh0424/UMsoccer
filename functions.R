# training models
# train_wrap = function(train, method, metric = "ROC", sampling = NULL, cv = FALSE, numcore = 30, seed = 1234, ...) {
#   registerDoMC(cores = numcore)
#   set.seed(seed)
#   k = ncol(train)
#   
#   ctrl = trainControl(sampling = sampling)
#   if(cv) {ctrl$method = "repeatedcv"; ctrl$number = 10; ctrl$repeats = 5}
#   if(metric == "ROC") {ctrl$savePredictions = TRUE; ctrl$classProbs = TRUE; ctrl$summaryFunction = twoClassSummary}
#   
#   model = train(injured ~ ., data = train, method = method, trControl = ctrl, metric = metric, ...)
#   trainpred = predict(model, newdata = train[, -k])
#   traintb = table(train$injured, trainpred)
#   df = data.frame(injured = train$injured, pred = predict(model, newdata = train[, -k], type = "prob")[, 1])
#   roc = calculate_roc(df, 1, sum(train$injured == "No") / sum(train$injured == "Yes"))
#   plots = plot_roc(roc, 0.5, 1, sum(train$injured == "No") / sum(train$injured == "Yes"))
#   plot.train = plot_pred_type_distribution(df, 0.5)
#   
#   return(list(model = model, roc = roc, traintable = traintb, plot.roc = plots[[1]], plot.cost = plots[[2]], plot.train = plot.train))
# }

# total load within a recent time window
recent_load = function(record, period = 7) {
  name = record$`Student Name`
  dat = session[[name]]
  injury.date = record$`Injury Date`
  dat.period = dplyr::filter(dat, (Date > injury.date - period) & (Date <= injury.date))
  num_session = nrow(dat.period)
  length = sum(dat.period$Activity.Length) / 60
  ima = sum(dat.period$Total.IMA)
  ima.high = sum(dat.period$High.IMA)
  load = sum(dat.period$Total.Player.Load)
  return(data.frame(Player = name, Date = injury.date, Period = period, Num.Session = num_session, 
                    Total.Length = length, IMA = ima, IMA.High = ima.high, Total.Load = load))
}

# the daily load trend of a player
load_plot = function(name) {
  record = filter(injury, `Student Name` == name)
  dat = daily.load[[name]]
  injury.date = record$`Injury Date`
  datL = gather(dat, key = legend, value = value, -Date)
  ggplot(datL, aes(Date, value)) + geom_line(color = "blue") + 
    geom_vline(xintercept = as.numeric(injury.date), linetype = 4, color = "red") + 
    facet_grid(legend ~ ., scales = "free_y") + ggtitle(name)
}

# acute-chronic ratio of a player on a specific date
acute_load = function(name, date, acute = 7) {
  dat = daily.load[[name]]
  acute.stats = dat %>% dplyr::filter((Date > date - acute) & (Date <= date))
  acute.stats = colSums(acute.stats[, -1]) / acute
  acute.stats = data.frame(t(acute.stats))
  names(acute.stats) = paste("acute.", names(acute.stats), sep = "")
  return(acute.stats)
}

chron_load = function(name, date, chron = 28) {
  dat = daily.load[[name]]
  chron.stats = dat %>% dplyr::filter((Date > date - chron) & (Date <= date))
  chron.stats = colSums(chron.stats[, -1]) / chron
  chron.stats = data.frame(t(chron.stats))
  names(chron.stats) = paste("chron.", names(chron.stats), sep = "")
  return(chron.stats)
}

acute_chron = function(name, date, acute = 7, chron = 28) {
  dat = daily.load[[name]]
  acute.stats = dat %>% dplyr::filter((Date > date - acute) & (Date <= date))
  acute.stats = colSums(acute.stats[, -1]) / acute
  chron.stats = dat %>% dplyr::filter((Date > date - chron) & (Date <= date))
  chron.stats = colSums(chron.stats[, -1]) / chron
  ratio = acute.stats / chron.stats
  ratio = data.frame(t(ratio))
  names(ratio) = paste("ratio.", names(ratio), sep = "")
  return(ratio)
}

# training dataset
prep_data = function(acute = 7, chron = 28) {
  injury.dates = unique(injury$`Injury Date`)
  dat.class = list()
  cnt = 0
  for (date in injury.dates) for (name in names) {
    cnt = cnt + 1
    dat.class[[cnt]] = bind_cols(acute_chron(name, date, acute = acute, chron = chron), 
                                 acute_load(name, date, acute = acute), chron_load(name, date, chron = chron))
    dat.class[[cnt]]$injured = ifelse(nrow(filter(injury, `Injury Date` == date & `Student Name` == name)) > 0, "Yes", "No")
    dat.class[[cnt]]$injured = factor(dat.class[[cnt]]$injured, levels = c("Yes", "No"))
    if(is.nan(dat.class[[cnt]][1, 1])) dat.class[[cnt]] = dat.class[[cnt]][-1, ]
  }
  dat.class = bind_rows(dat.class)
  return(dat.class)
}

# # codes from joyofdata
# calculate_roc <- function(df, cost_of_fp, cost_of_fn, n=100) {
#   tpr <- function(df, threshold) {
#     sum(df$pred >= threshold & df$injured == "Yes") / sum(df$injured == "Yes")
#   }
#   
#   fpr <- function(df, threshold) {
#     sum(df$pred >= threshold & df$injured == "No") / sum(df$injured == "No")
#   }
#   
#   cost <- function(df, threshold, cost_of_fp, cost_of_fn) {
#     sum(df$pred >= threshold & df$injured == "No") * cost_of_fp +
#       sum(df$pred < threshold & df$injured == "Yes") * cost_of_fn
#   }
#   
#   roc <- data.frame(threshold = seq(0,1,length.out=n), tpr=NA, fpr=NA)
#   roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th))
#   roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th))
#   roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn))
#   
#   return(roc)
# }
# 
# 
# plot_roc <- function(roc, threshold, cost_of_fp, cost_of_fn) {
#   norm_vec <- function(v) (v - min(v))/diff(range(v))
#   
#   idx_threshold = which.min(abs(roc$threshold-threshold))
#   
#   col_ramp <- colorRampPalette(c("green","orange","red","black"))(100)
#   col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1]
#   p_roc <- ggplot(roc, aes(fpr,tpr)) +
#     geom_line(color=rgb(0,0,1,alpha=0.3)) +
#     geom_point(color=col_by_cost, size=4, alpha=0.5) +
#     coord_fixed() +
#     geom_line(aes(threshold,threshold), color=rgb(0,0,1,alpha=0.5)) +
#     labs(title = sprintf("ROC")) + xlab("FPR") + ylab("TPR") +
#     geom_hline(yintercept=roc[idx_threshold,"tpr"], alpha=0.5, linetype="dashed") +
#     geom_vline(xintercept=roc[idx_threshold,"fpr"], alpha=0.5, linetype="dashed")
#   
#   p_cost <- ggplot(roc, aes(threshold, cost)) +
#     geom_line(color=rgb(0,0,1,alpha=0.3)) +
#     geom_point(color=col_by_cost, size=4, alpha=0.5) +
#     labs(title = sprintf("cost function")) +
#     geom_vline(xintercept=threshold, alpha=0.5, linetype="dashed")
#   
#   return(list(p_roc, p_cost))
# }
# 
# # visualize distribution of predictions
# plot_pred_type_distribution <- function(df, threshold) {
#   v <- rep(NA, nrow(df))
#   v <- ifelse(df$pred >= threshold & df$injured == "Yes", "TP", v)
#   v <- ifelse(df$pred >= threshold & df$injured == "No", "FP", v)
#   v <- ifelse(df$pred < threshold & df$injured == "Yes", "FN", v)
#   v <- ifelse(df$pred < threshold & df$injured == "No", "TN", v)
#   
#   df$pred_type <- v
#   
#   ggplot(data=df, aes(x=injured, y=pred)) +
#     geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) +
#     geom_jitter(aes(color=pred_type), alpha=0.6) +
#     geom_hline(yintercept=threshold, color="red", alpha=0.6) +
#     scale_color_discrete(name = "type")
# }
# 
# cv_thres_ada = function(thres) {
#   temp = filter(model.na.ada.smote$model$pred, nu == 0.1 & maxdepth == 5 & iter == 250) %>%
#     mutate(pred = factor(ifelse(Yes > thres, "Yes", "No"), levels = c("Yes", "No"))) %>%
#     group_by(Resample) %>%
#     summarise(sens = sum(pred == "Yes" & obs == "Yes") / sum(obs == "Yes"), spec = sum(pred == "No" & obs == "No") / sum(obs == "No"))
#   return(data.frame(threshold = thres, cv.sens = mean(temp$sens), cv.spec = mean(temp$spec),
#                     test.sens = mean(model.na.ada.smote$probs[test.na$injured == "Yes"] > thres), test.spec = mean(model.na.ada.smote$probs[test.na$injured == "No"] < thres)))
# }
# 
# cv_thres_gbm = function(thres) {
#   temp = filter(model.na.gbm.smote$model$pred, n.trees == 300 & interaction.depth == 7 & shrinkage == 0.025 & n.minobsinnode == 10) %>%
#     mutate(pred = factor(ifelse(Yes > thres, "Yes", "No"), levels = c("Yes", "No"))) %>%
#     group_by(Resample) %>%
#     summarise(sens = sum(pred == "Yes" & obs == "Yes") / sum(obs == "Yes"), spec = sum(pred == "No" & obs == "No") / sum(obs == "No"))
#   return(data.frame(threshold = thres, cv.sens = mean(temp$sens), cv.spec = mean(temp$spec),
#                     test.sens = mean(model.na.gbm.smote$probs[test.na$injured == "Yes"] > thres), test.spec = mean(model.na.gbm.smote$probs[test.na$injured == "No"] < thres)))
# }
