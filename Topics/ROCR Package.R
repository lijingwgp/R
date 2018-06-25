# The prediction() and performance() functions are the workhorses of most of the analyses 
# in ROCR.
#
# prediction(predictions, labels, label.ordering = NULL)
# where predictions are some predicted measure for the "truth", which are labels. In many cases,
# predictions are estimated probabilities (or log odds) and the labels are binary values. 
#
# dim(predictions) must equal dim(labels)


library(ROCR)
data("ROCR.simple")
head(cbind(ROCR.simple$predictions, ROCR.simple$labels))
# Now, let's make the prediction object and show its contents:
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
class(pred)
slotNames(pred)
# We see the returned result of prediction is an object of class prediction, which an S4 object 
# with a series of slots. 
#
# Multiple set of prediction and labels
# Let's use the ROCR.hiv dataset to show how this works if more than one set of predictions
# and labels are supplied. Here we pass a list of 10 predictions and a list of labels to the 
# prediction function:
data("ROCR.hiv")
head(ROCR.hiv)
manypred = prediction(ROCR.hiv$hiv.nn$predictions, ROCR.hiv$hiv.nn$labels)
sapply(slotNames(manypred), function(x) length(slot(manypred, x)))
#
# Performance objects
# From the help file of performance(), the syntax for this function is:
# performance(prediction.obj, measure, x.measure="cutoff", ...)
# we see that the first argument is a prediction object, and the second is a measure. 
# For the reference of all performance measures, go to ?performance document.
# The third argument, x.measure, is the unit in direction of the x axis.
#
# ROC Curve
# Simple example: one set of prediction and labels
# An ROC curve has false positive rate on the x-axis and true positive rate on the y-axis
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
plot(roc.perf)
abline(a=0, b=1)
# At every cutoff, the TPR and FPR are calculated and plotted. The smoother the graph, the more cutoffs
# the predictions have. We also plotted a 45-degree line, which represents, on average, the performance of a Uniform(0,1)
# random variable. The further away from the diagonal line, the better.
#
# Complex example: multiple sets of prediction and labels
many.perf = performance(manypred, measure = "tpr", x.measure = "fpr")
plot(many.perf, col=1:10)
abline(a=0, b=1)
#
# Getting an "optimal" cut point
# This cut point is "oprimal" in the sense it weights both sensitivity and specificitgy equally
# To determine this cutoff, you can use the code blow.
# The code takes in both the performance object and prediction object and gives the optimal cusoff value
# of you predictions:
opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))
# Now, there is a cost measure in the ROCR package that you can use to create a performance 
# object. If you use it to find the minimum cost, then it will give you the same cutoff 
# as opt.cut, but not give you the sensitivity and specificity.
cost.perf = performance(pred, "cost")
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
# 
# Different costs for FP and FN
# The output from opt.cut and a performance object with measure cost are NOT equivalent 
# if false positives and false negatives are not weighted equally. 
# The cost.fn and cost.fp arguments can be passed to performance, corresponding to the 
# cost of a false negative and false positive, respectively. 
# Let's say false positives are twice as costly as false negatives, and let's get a 
# cut point:
cost.perf = performance(pred, "cost", cost.fp = 2, cost.fn = 1)
pred@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
# In many real-life applications of biomarkers, the cost of a false positive and false 
# negative are not the same. For example, missing someone with a disease based on a test 
# may cost a hospital $1,000,000 in lawsuits, but treating someone who did not have the 
# disease may cost $100,000 in treatments. In that case, the cost of a false negative is 
# 10 times that of a false positive, strictly in monetary measures. 
#
# Accuracy
# Simple example: one set of prediction and labels
acc.perf = performance(pred, measure = "acc")
plot(acc.perf)
# What if we actually want to extract the maximum accuracy and the cutoff corresponding to that?
# In the performance object, we have the slot x.values, which corresonds to the cutoff in this case,
# and y.values, which corresonds to the accuracy of each cutoff. 
# We'll grab the index for maximum accuracy and then grab the corresponding cutoff:
ind = which.max(slot(acc.perf, "y.values")[[1]])
acc = slot(acc.perf, "y.values")[[1]][ind]
cutoff = slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))
# Area under the curve (AUC) 
# Simple example: one set of prediction and labels
# The area under curve summarizes the ROC curve just by taking the area between the 
# curve and the x-axis.
auc.perf = performance(pred, measure = "auc")
auc.perf@y.values

