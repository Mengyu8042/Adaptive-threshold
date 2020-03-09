library(R.matlab)
volume <- readMat('sample.mat')$vol
label  <- readMat('true_label.mat')$md
label[label!=0] = 1

w = length(volume[,1,1])
l = length(volume[1,,1])
h = length(volume[1,1,])
out = matrix(NA, w*h, l)
dim(out)=c(w,l,h)
intImg = matrix(NA, w*h, l)
dim(intImg)=c(w,l,h)
s = w/10
t = 20

#calculate the integral image
for(k in 1:h) {
  for (i in 1:w) {
    sum = 0
    for (j in 1:l) {
      sum = sum + volume[i,j,k]
      if (i == 1) 
        intImg[i,j,k] = sum
      else 
        intImg[i,j,k] = intImg[i-1,j,k] + sum
    }
  }
}

#calculate the average and compare
for (i in 1:w) {
  for (j in 1:l) {
    for (k in 1:h) {
      x1 = max(i-round(s/2), 2)
      x2 = min(i+round(s/2), w)
      y1 = max(j-round(s/2), 2)
      y2 = min(j+round(s/2), l)
      z1 = max(k-round(s/2), 2)
      z2 = min(k+round(s/2), h)
      count = (x2-x1) * (y2-y1) *(z2-z1)
      sum = intImg[x2,y2,z2] - intImg[x1-1,y2,z2] - intImg[x2,y1-1,z2] - intImg[x2,y2,z1-1] + intImg[x1-1,y1-1,z2] + intImg[x1-1,y2,z1-1] + intImg[x2,y1-1,z1-1] - intImg[x1-1,y1-1,z1-1]
      if ((volume[i,j,k]*count) <= (sum*(100-t)/100))
        out[i,j,k] = 0  #black=0
      else
        out[i,j,k] = 1  #white=255
    }
  }
}

TPN = 0  #true positive number
FPN = 0  #false positive number
TNN = 0  #ture negative number
FNN = 0  #false negative number
N = w * l * h
for (i in 1:w) {
  for (j in 1:l) {
    for (k in 1:h) {
      if(label[i,j,k]==1 & out[i,j,k]==0) FPN = FPN + 1
      if(label[i,j,k]==1 & out[i,j,k]==1) TNN = TNN + 1
      if(label[i,j,k]==0 & out[i,j,k]==0) TPN = TPN + 1
      if(label[i,j,k]==0 & out[i,j,k]==1) FNN = FNN + 1
    }
  }
}
(TPR = TPN / N)  #true positive rate
(FPR = FPN / N)  #false positive rate
(TNR = TNN / N)  #ture negative rate
(FNR = FNN / N)  #false negative rate