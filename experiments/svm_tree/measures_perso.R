
gini_score <- function(pred, target) {
  
  lclass <- target[pred == 0]
  rclass <- target[pred == 1]
  pl <- length(lclass[lclass==0]) / length(lclass)
  pr <- length(rclass[rclass==0]) / length(rclass)
  (length(lclass) * (1 - pl*pl - (1-pl)*(1-pl)) + length(rclass) * (1- pr*pr -(1-pr)*(1-pr)) ) /length(pred)
}

entropy <- function(data) {
  p <- length(data[data == 0]) / length(data)
  - p * log2(p) - (1-p) * log2(1-p)
}


information_gain <- function(pred, target) {
  
  root.entropy <- entropy(target)
  
  lclass <- target[pred == 0]
  rclass <- target[pred == 1]
  
  l.entropy <- entropy(lclass)
  r.entropy <- entropy(rclass)
  
  root.entropy - (length(lclass) * l.entropy + length(rclass) * r.entropy) / length(pred)
}