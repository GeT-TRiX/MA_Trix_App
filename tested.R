(a("*[id^="+N+"-resultC]").mouseover(function(){a(this).addClass("number-over")}),a("*[id^="+N+"-resultC]")
  .mouseout(function(){a(this).removeClass("number-over")}),
  a("*[id^="+N+"-resultC]").click(d.fnClickCallback))



wrongcol <- function(y)
  if (any(grepl("col2rgb", y)))
    invokeRestart("muffleWarning")

tryCatch({
col2rgb(c("raid"))%>% withCallingHandlers(warning = wrongcol)
}, error = function(e) {print(paste("non-numeric argument"))})
        
