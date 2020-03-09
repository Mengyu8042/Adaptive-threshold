library(jpeg)
input = readJPEG("example2.jpg")[,,2]
w = length(input[1,])
h = length(input[,1])
out = matrix(NA, h, w)
intImg = matrix(NA, h, w)
s = h/10
t = 20

for (i in 1:w) {
  sum = 0
  for (j in 1:h){
    sum = sum + input[j,i]
    if (i == 1) 
      intImg[j,i] = sum
    else 
      intImg[j,i] = intImg[j,i-1] + sum
  }
}
for (i in 1:w) {
  for (j in 1:h) {
    x1 = max(i-round(s/2), 2)
    x2 = min(i+round(s/2), w)
    y1 = max(j-round(s/2), 2)
    y2 = min(j+round(s/2), h)
    count = (x2-x1) * (y2-y1)
    sum = intImg[y2,x2] - intImg[(y1-1),x2] - intImg[y2,(x1-1)] + intImg[(y1-1),(x1-1)]
    if ((input[j,i]*count) <= (sum*(100-t)/100))
      out[j,i] = -9223372036854775808  #black0
    else
      out[j,i] = 9223372036854775807 #white255
  }
}
writeJPEG(out, 'adaptive2.jpg', quality = 0.95)
