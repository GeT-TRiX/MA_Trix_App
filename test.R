test = c("blue","red",'Darkcyan')
mycol = lapply(test, FUN= function(x)return(col2rgb(x)))
mycol
test = col2rgb(c(mycol = 1:3)) %>% as.data.frame() %>% lapply(.,function(x)return(x)) 
(test[[1]])

grDevices::colorRamp("red")
rgb("red")

rgb(0, 1, 0)

rgb((0:15)/15, green = 0, blue = 0, names = paste("red", 0:15, sep = "."))

rgb(0, 0:12, 0, max = 255) # integer input

ramp <- colorRamp(c("red", "white"))
rgb( ramp(seq(0, 1, length = 5)), max = 255)
col2hex(test)
