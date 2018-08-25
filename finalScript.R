##########################################
## Pull a set of data and condition it for modeling to predict price movement.
## 2018-08-16
##########################################
# reference info
# https://wikitech.wikimedia.org/wiki/Analytics/AQS/Pageviews
#
# Put-call ratio from CBOE
# http://r.789695.n4.nabble.com/as-xts-convert-all-my-numeric-data-to-character-td975564.html
# http://www.cboe.com/publish/ScheduledTask/MktData/datahouse/totalpc.csv
#
##########################################
agg_stock_data <- function(ticker=NULL, term=NULL, save_pulls = NULL) {
  
  # Libraries required
  library(quantmod)
  library(xts)
  library(zoo)
  library(pageviews)
  library(bizdays)
  library(gtrendsR)
  library(devtools)
  library(TTR)
  
  
  
  thePath = "C:\\Users\\Michael\\Downloads"
  today = gsub( pattern = "-", replacement="", as.character(Sys.Date()))
  today = paste(today, "00", sep = "")
  
  # TESTING VALUES Symbols to generate data on
  ticker = "AAPL"  #, "AMZN", "T")
  term = "aapl stock"
  
  df = getSymbols(Symbols = ticker, return.class='xts', env=NULL)
  
  # subset to include december 2013 onward, which allows for calculating moving averages and stdev
  strt = as.Date(first(grep(pattern="2013-12-0+",index(df), value = TRUE)), format = "%Y-%m-%d")
  end = nrow(df)
  strt = which(index(df)==strt)
  # df = index(df)[strt:nrow(df)]  # here's how to grab the index only
  # df = df[index(df)[strt:end],]
  df = df[strt:end,]
  # Fix the names of the standard object returned
  colnames(df) = c("open","high","low","close","volume","adj.")
  # head(df)
  
  ##### ______________________ Compute  Stock Technicals _____________________________#######
  # __________________________________________________________________________#
  
  df$EMA3 = EMA(df$adj, n=3)   # thesis
  df$EMA5 = EMA(df$adj, n=5)   # thesis
  df$EMA10 = EMA(df$adj, n=10) # thesis
  
  df$WPR = WPR(df[,c("high", "low", "close")])
  df$RSI.10 = RSI(df$adj, n=10, maType = "EMA")
  df$RSI.5 = RSI(df$adj, n=5, maType = "EMA")
  df$MACD.12.26.EMA = MACD(df$adj, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA")
  
  df$stoch.3 = stoch(df[,c('high','low','close')],
                     n.fastK=14,
                     ma.fastD=list("SMA", n=3),
                     ma.slowD=list("SMA", n=3))
  
  df$stoch.def = stoch(df[,c('high','low','close')])
  
  df$ROC.3 = ROC(df$adj, n=3)
  df$ROC.5 = ROC(df$adj, n=5)
  df$ROC.10 = ROC(df$adj, n=10)
  
  # Volumetric calculations
  df$Vol.ROC.1 = ROC(df$volume, n=1)
  df$Vol.ROC.3 = ROC(df$volume, n=3)
  df$Vol.ROC.5 = ROC(df$volume, n=5)
  df$Vol.ROC.10 = ROC(df$volume, n=10)
  
  df$Vol.RSI.3 = RSI(df$volume, n=3, maType = "EMA")
  df$Vol.RSI.5 = RSI(df$volume, n=5, maType = "EMA")
  df$Vol.RSI.10 = RSI(df$volume, n=10, maType = "EMA")
  
  
  ##### ______________________ Wikipedia Counts __________________________________#####
  # __________________________________________________________________________#
  
  wikicnts = article_pageviews(project = "en.wikipedia",
                               article = "AAPL",
                               platform = "all",
                               user_type = "all",
                               start = "2015100100",
                               end = today,
                               reformat = TRUE)[,7:8] #col 7 & 8 have date and count
  
  wikicnts = xts(as.numeric(wikicnts[,-1]),
                 order.by = as.Date(wikicnts[,'date'],
                                    format="%Y-%m-%d") )
  
  colnames(wikicnts) = "wikicnts"
  
  # get ROC for wiki.views
  wikicnts$wiki.ROC.1 = ROC(wikicnts$wikicnts, n=1, na.pad = TRUE)
  wikicnts$wiki.ROC.3 = ROC(wikicnts$wikicnts, n=3, na.pad = TRUE)
  # get RSI for wiki views
  wikicnts$wiki.RSI.3 = RSI(wikicnts$wikicnts, n=3, maType = "EMA")
  wikicnts$wiki.RSI.5 = RSI(wikicnts$wikicnts, n=5, maType = "EMA")
  
  # tail(wikicnts,25)
  # str(wikicnts)
  # if(save_pulls == TRUE){
  #   wikiname = paste(thePath, Sys.Date(),"_WikiCnts.csv", sep = "")
  #   write.zoo(wikicnts, wikiname, sep=",", index.name = "Date", col.names = TRUE)
  # }
  
  ##### Merge the wikicnts with the df dataframe of stock data ____________________#####
  
  df = merge(df, wikicnts)  # join='left')  THIS SHOULD BE GOOD NOW!
  # tail(df,40)
  
  ##### Pull Google Trends Data for Ticker #################################################
  
  # https://github.com/PMassicotte/gtrendsR/issues/252
  
  # The library to pull from google trends is a dev library that you install from here
  # devtools::install_github("PMassicotte/gtrendsR")
  
  library(gtrendsR)
#######################################################################  
  trendHelper <- function(keyword = NULL, geo = NULL, timeStart = NULL, timeEnd = NULL, gprop = NULL) {
    dateStart <- as.Date(timeStart)
    dateEnd <- as.Date(timeEnd)
    curDate <- dateStart
    dftemp = data.frame(date= character(), hits = numeric(), stringsAsFactors = FALSE)
    while(curDate <= dateEnd) {
      trend = gtrends(keyword = term,
                      geo = geo,
                      time = paste(curDate, curDate+90, sep=" "),       # "today+5-y", #"today 1-m"
                      gprop = "web", #c("web","news"),
                      # category = 0,
                      hl = "en-US",
                      low_search_volume = FALSE)
      curDate <- curDate + 90
      dfNew = data.frame(trend$interest_over_time$date, trend$interest_over_time$hits, stringsAsFactors = FALSE)
      dftemp <- rbind(dftemp, dfNew)
      print(curDate)
    }
    
    trend <- gtrends(keyword = term,
                     geo = geo,
                     time = paste(curDate, Sys.Date(), sep=" "),       # "today+5-y", #"today 1-m"
                     gprop = "web", #c("web","news"),
                     # category = 0,
                     hl = "en-US",
                     low_search_volume = FALSE)
    dfNew = data.frame(trend$interest_over_time$date, trend$interest_over_time$hits, stringsAsFactors = FALSE)
    dftemp <- rbind(dftemp, dfNew)
  }
################################################################  
  
  
  trend = trendHelper(term, "US", "2015-01-01", "2015-10-01", "web")
  
  
  # trend = gtrends(keyword = term,
  #                 geo = "US",
  #                 time = paste("2015-01-01", Sys.Date(), sep=" "),       # "today+5-y", #"today 1-m"
  #                 gprop = "web", #c("web","news"),
  #                 # category = 0,
  #                 hl = "en-US",
  #                 low_search_volume = FALSE)
  # head(trend$interest_over_time$hits)
  # This works.  Appears that the gprop cannot take more than one arg (e.g. it cannot
  # be c("news", "web"))
  # gtrend = gtrends("insomnia",
  #                 gprop = 'web', #c("news"), #test case used 'news'
  #                 geo = "US",
  #                 time = "today 1-m")
  
  ##### WRITE THE GTREND DATA TO FILE
  if(save_pulls=TRUE){
    gtrendfile = trend$interest_over_time
    gtrendname = paste(thePath, Sys.Date(),"_GTREND_Data.csv", sep = "")
    write.table(gtrendfile, gtrendname, sep=",", col.names = TRUE, row.names = FALSE)
  }
  
  ##### create a sequence to capture the time series for all dates in the query #####
  trend$interest_over_time$hits = as.numeric(trend$interest_over_time$hits)
  trnd.hits = xts(as.numeric(trend$interest_over_time$hits), order.by = as.Date(trend$interest_over_time$date, format="%Y-%m-%d"))
  colnames(trnd.hits) = "gtrnd"
  trnd.hits$gtrnd = trnd.hits$gtrnd/100
  trnd.hits$gtrnd_ROC = Delt(trnd.hits$gtrnd, k=1)
  # str(trnd.hits)
  
  # tail(trnd.hits)
  # write a time series that covers all dates from the beginning of the trend data
  tr.series = seq.Date(from=(as.Date(trend$interest_over_time$date[1], format="%Y-%m-%d"))-7, to=Sys.Date(),by="day")
  tr.series = data.frame(date=tr.series, gtrnd=0)
  tr.series = xts(as.numeric(tr.series$gtrnd), order.by = as.Date(tr.series$date, format="%Y-%m-%d"))
  colnames(tr.series) = "gtrnd"
  tail(tr.series)
  
  # tr.series$gtrnd = tr.series$gtrnd + trnd.hits$gtrnd
  trend.series = merge(tr.series$gtrnd, trnd.hits$gtrnd)[,2]
  trend.series = merge(trend.series, trnd.hits$gtrnd_ROC)
  trend.series[is.na(trend.series)] = 0
  colnames(trend.series) = c("g_hits", "g_hits_ROC")
  # tr.series[ind,] = trnd.hits[ind,]
  
  # str(tr.series)
  # get summary info for gtrend
  
  ###### get related query summary data ###########################################
  rel_query = unlist(trend$related_queries$value)
  print(paste("Number of related queries for symbol: ", length(rel_query), sep = ""))
  print(rel_query)
  # sumdf = summary(trend)
  
  
  
  
  ##### DO NOT USE RIGHT NOW ################
  # get the data for scaled hits for the keyword in gtrend
  # time.trend = trend$interest_over_time[,c(1,2)]
  
  # turn the trend data into an extensible time series (xts)
  # time.trend = xts(time.trend[,2], order.by = as.Date(time.trend$date, format="%Y-%m-%d"))
  
  # call the new column
  # colnames(time.trend) = "g_hits"
  # class(time.trend)
  # head(time.trend)
  
  # ifelse(summary(trend$interest_by_country)==0, trend$interest_by_country=NULL)
  
  # summary(time.trend)
  # class(time.trend)
  
  # scale the hits value to be a percentage
  # time.trend$g_hits = time.trend$g_hits/100
  
  # calculate the rate of change for k=1 period lookback (because google trend data is provided
  # only weekly for queries of a year or more k=1 is the best resolution you can get).  I could not
  # find a way to get data at a daily rate for queries spanning more than a few days/a week.
  
  # time.trend$goog_wk_ROC = Delt(time.trend$g_hits, k=1, type="log")
  # head(time.trend)
  
  ####_______________________MERGE Google Trend Data to Dataframe _________________________#####
  # this does a left join which maps the weekly google trend data into the xts df
  # and creates a bunch of NAs in between the actual values
  # Merge the raw hits data into df first (this will have it in the data file if there's more to do
  # with it later without having to run this function).
  df = merge.xts(df, trend.series) # join = "left") DO NOT LEFT JOIN AS IT REMOVES THE SUNDAYS!!!
  tail(df,40)
  # Merge the ROC data next
  # df = merge.xts(df, time.trend$goog_wk_ROC) #, join = "left")
  
  # parse thru the zero values and fill in delta value for the previous week
  # na.idx = which(is.na(df$goog_wk_ROC))    # get values which are NA in the merged data frame
  val.idx = which(!df$g_hits_ROC==0)  #get values that are not ZEROS
  
  for (i in 1:(length(val.idx)-1)){
    loc.0 = val.idx[i] + 1
    loc.1 = val.idx[i+1] - 1
    prev.wk.delta = loc.1 + 1
    # print(loc.1-loc.0)
    # check to make sure the range between values is a week or less
    if((loc.1+1 - loc.0-1 <= 7)){        # check to make sure the range in dates is not greater than a week
      # print(paste("good to go",i,sep=""))
      df$g_hits_ROC[loc.0:loc.1] = df$g_hits_ROC[prev.wk.delta]
    } else {}
    
  } #end for
  
  
  ##### Pull Google Trend Data for Mortgage Data ######################################
  mrtg.trend = gtrends(keyword = "mortgage rates",
                       geo = "US",
                       time = paste("2015-01-01", Sys.Date(), sep=" "),  #"today+5-y",
                       gprop = "web",
                       hl = "en-US",
                       low_search_volume = FALSE)
  
  ##### create a sequence to capture the time series for all dates in the query #####
  
  mrtg.trnd.hits = xts(as.numeric(mrtg.trend$interest_over_time$hits),
                       order.by = as.Date(mrtg.trend$interest_over_time$date,
                                          format="%Y-%m-%d"))
  colnames(mrtg.trnd.hits) = "gtrnd.mrtg"
  mrtg.trnd.hits$gtrnd.mrtg = mrtg.trnd.hits$gtrnd.mrtg/100
  
  # Calculate Rate of Change for the trend
  mrtg.trnd.hits$gtrnd.mrtg_ROC = Delt(mrtg.trnd.hits$gtrnd.mrtg, k=1)
  tail(mrtg.trnd.hits,20)
  
  # ##### Merrge Mortgage Trend Data into df ###############################
  
  df = merge.xts(df, mrtg.trnd.hits) # join = "left") DO NOT LEFT JOIN AS IT REMOVES THE SUNDAYS!!!
  df$gtrnd.mrtg[is.na(tdf$gtrnd.mrtg)] = 0
  df$gtrnd.mrtg_ROC[is.na(tdf$gtrnd.mrtg_ROC)] = 0
  tail(df,40)
  
  # Clean up the zeroes
  val.idx = which(!df$gtrnd.mrtg_ROC==0)  #get values that are not ZEROS
  
  for (i in 1:(length(val.idx)-1)){
    loc.0 = val.idx[i] + 1
    loc.1 = val.idx[i+1] - 1
    prev.wk.delta = loc.1 + 1
    # check to make sure the range between values is a week or less
    if((loc.1+1 - loc.0-1 <= 7)){        # check to make sure the range in dates is not greater than a week
      # print(paste("good to go",i,sep=""))
      df$gtrnd.mrtg_ROC[loc.0:loc.1] = df$gtrnd.mrtg_ROC[prev.wk.delta]
      df$gtrnd.mrtg[loc.0:loc.1] = df$gtrnd.mrtg[prev.wk.delta]
    } else {}
    
  } #end for
  
  ##### __________________________ CALCULATE TARGETS ______________________________________ #####
  
  # Calculate the rolling standard deviation
  
  # tdf = df
  
  df$Delt.1 = Delt(df$adj., k=1, type = "log") # % daily returns
  # df$Delt.5 = Delt(tdf$adj., k=5, type = "log") # % return on five day lag
  
  df$sd.month.d1 = NA # create column for month standard deviation
  df$sd.qrtr.d1 = NA # create column for quarter standard deviation
  df$sd.annual.d1 = NA # create column for annual standard deviation
  
  # biz day calculations, so i don't forget
  252/12
  252/4 # one quarter in business days
  
  # USE k=1 DELTS OF RETURNS FOR CALCULATIONS -------------------------------------------------
  
  # Calculate the rolling std. dev. for the previous MONTH's time
  for (i in 22:length(df$adj.)){
    lookback = i-21
    df$sd.month.d1[i] = sd(df$Delt.1[lookback:i-1], na.rm = TRUE)
  }
  
  # Calculate the rolling std. dev. for the previous QUARTER's time
  for (i in 64:length(df$adj.)){
    lookback = i-63
    df$sd.qrtr.d1[i] = sd(df$Delt.1[lookback:i-1], na.rm = TRUE)
  }
  
  # Calculate the rolling std. dev. for the previous YEAR's time
  for (i in 253:length(df$adj.)){
    lookback = i-252
    df$sd.annual.d1[i] = sd(df$Delt.1[lookback:i-1], na.rm = TRUE)
  }
  
  # take a look at results for comparison
  tail(df[,c('sd.month.d1','sd.qrtr.d1','sd.annual.d1')])
  # comparing these numbers, there might be some room to develop an attribute using the ratio
  # between them as a momentum indicator.  It may be redundant to other momentum indicators.
  
  # USE k=5 DELTS OF RETURNS FOR CALCULATIONS (NOT USED CURRENTLY) -------------------------------------------------
  # THIS WAS LEFT OUT DELIBERATELY AS IT ADDS COMPLEXITY IN MODELING RIGHT NOW AND SEEMS TO
  # BE ATYPICAL WITH COMMON METHODS. IT CAN UNCOMMENTED AND APPLIED IF NECESSARY
  #_____________________________________________________________________________________
  # df$sd.month.d5 = NA # create column for month standard deviation
  # df$sd.qrtr.d5 = NA # create column for quarter standard deviation
  # df$sd.annual.d5 = NA # create column for annual standard deviation
  #
  # # Calculate the rolling std. dev. for the previous month's time
  # for (i in 22:length(df$close)){
  #   lookback = i-21
  #   df$sd.month.d5[i] = sd(df$Delt.5[lookback:i-1], na.rm = TRUE)
  # }
  #
  # # Calculate the rolling std. dev. for the previous quarter's time
  # for (i in 64:length(df$close)){
  #   lookback = i-63
  #   df$sd.qrtr.d5[i] = sd(df$Delt.5[lookback:i-1], na.rm = TRUE)
  # }
  #
  # # Calculate the rolling std. dev. for the previous year's time
  # for (i in 253:length(df$close)){
  #   lookback = i-252
  #   df$sd.annual.d5[i] = sd(df$Delt.5[lookback:i-1], na.rm = TRUE)
  # }
  #
  # tail(df[,c('Delt.1', 'sd.month.d5','sd.qrtr.d5','sd.annual.d5')])
  
  ####______________________GENERATE RESPONSE FOR 1,2 STDEVs____________________________####
  # identify the price movements as crossing the threshold for ONE std. dev.
  df$Resp.1.std = 0
  df$Resp.2.std = 0
  
  df$Resp.1.std = ifelse(df$Delt.1 >= df$sd.month.d1, 1, 0)
  df$Resp.2.std = ifelse(df$Delt.1 >= 2*(df$sd.month.d1), 1, 0)
  
  # df$Resp.1.std.ann = ifelse(df$Delt.1 >= df$sd.qrtr.d1, 1, 0)
  
  # calculate the forward movement by stdev forward 1, 3, 5, 10 days
  
  ##### calculate the binary variables for price change >= 2 std.dev in FIVE days forward #####
  
  df$Resp.5d.2sd=rep(0, nrow(df)) # fill the column with zeros
  # tr = c(0, 1,NA,0,2)
  
  for(i in 1:(nrow(df)-5)){
    # tval = df$Delt.1[i]
    bdays = df$Resp.2.std[(i+1):(i+5)]
    if (sum(bdays, na.rm=TRUE)>0){
      df$Resp.5d.2sd[i] = 1
      # ifelse(i%%5==0,print(i),"")
    }
    else{df$Resp.5d.2sd[i] = 0}
    
  } # endfor
  
  ##### calculate the binary variables for price change >= 2 std.dev TEN days forward #####
  
  df$Resp.10d.2sd = rep(0, nrow(df)) #fill the column with zeros
  
  for(i in 1:(nrow(df)-10)){
    # tval = df$Delt.1[i]
    bdays = df$Resp.2.std[(i+6):(i+10)]
    if (sum(bdays, na.rm=TRUE) > 0){
      df$Resp.10d.2sd[i]=1
      # ifelse(i%%5==0,print(i),"")
    }
    else{df$Resp.10d.2sd[i] = 0}
    
  } # endfor
  
  # sd2ind = which(df$Resp.2.std==1)
  # df[70:85,c('adj.', 'Delt.1', 'Resp.2.std', 'sd.month.d1','Resp.5d.2sd')]
  
  ##### REMOVE NAs FROM THE DATA TABLE ####################################################
  nrow(df)
  df = na.omit(df)
  
  ####_____________________________ Write the file ____________________________________#####
  
  write.zoo(df,
            paste(thePath, Sys.Date(), "_AGGREGATE_", ticker, ".csv", sep=""),
            sep=",",
            row.names=FALSE)
  
  
} #END FUNCTION