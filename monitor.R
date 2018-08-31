# Source https://www.r-bloggers.com/a-shiny-app-serves-as-shiny-server-load-balancer/



lapply(1:60, function(x) {
  tops <- system("top -n 1 -b -u shiny", intern = TRUE)
  if(length(tops) > 0) {
    ids <- grep("R *$", tops)
    header <- grep("%CPU", tops)
    names <- strsplit(gsub("^ +|%|\\+", "", tops[header]), " +")[[1]]
    
    if(length(ids) > 0) {
      dat <- as.data.frame(do.call(rbind, strsplit(gsub("^ *", "", tops[ids]), " +")))
      names(dat) <- names
      info <- as.data.frame(do.call(rbind, lapply(dat$PID, function(pid) {
        netstat <- system(paste("sudo netstat -p | grep", pid), intern = TRUE)
        lsof <- system(paste("sudo lsof -p", pid, "| grep /home/fsoubes"), intern = TRUE)
        users <- length(grep("ESTABLISHED", netstat) & grep("tcp", netstat))
        app <- regmatches(lsof, regexec("home/fsoubes/(.*)", lsof))[[1]][2]
        c(app = app, users = users)
      })))
    } else {
      info <- data.frame(app = "app", users = 0)
    }
    write.table(info, file = "/home/fsoubes/server/monitor.log")
  }  
})

