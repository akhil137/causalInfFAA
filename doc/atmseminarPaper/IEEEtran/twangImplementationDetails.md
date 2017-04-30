*notes on twang code*

#use R to view source code

Below are the functions we looked at and the package; package:function

twang:mnps
calls ps()

twang:ps
calls gbm()

gbm:gbm

``  gbm1 <- gbm(formula(form), data = data, weights = sampW, 
              distribution = "bernoulli", n.trees = n.trees, interaction.depth = interaction.depth, 
              n.minobsinnode = 10, shrinkage = shrinkage, bag.fraction = bag.fraction, 
              train.fraction = 1, verbose = verbose, keep.data = FALSE)``
              
                  opt <- optimize(MetricI, interval = iters[interval], 
                    maximum = FALSE, tol = 1, fun = match.fun(stop.method[[i.tp]]$metric), 
                    vars = var.names, treat.var = treat.var, data = data, 
                    sampw = sampW, rule.summary = match.fun(stop.method[[i.tp]]$rule.summary), 
                    na.action = stop.method[[i.tp]]$na.action, gbm1 = gbm1, 
                    estimand = estimand, multinom = multinom)