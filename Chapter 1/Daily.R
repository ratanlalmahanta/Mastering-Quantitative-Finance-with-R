
library(IBrokers)
setwd("/home/ubuntu/Dropbox/Algo-Desk/IB/src/Stock/daily")
#setwd("D:/QuantIb/Data/1h")

#IBConn <- twsConnect(clientId = 2, host = '192.168.1.13', port = 7497, verbose = TRUE, timeout = 5,filename = NULL, blocking=.Platform$OS.type=="windows")
#IBConn <- twsConnect(clientId = 2, host = '13.234.141.211', port = 7497, verbose = TRUE, timeout = 5,filename = NULL, blocking=.Platform$OS.type=="windows")

IBConn <- twsConnect(port = 7497)
#IBConn <- twsConnect(port = 4002)

time_frame = '1 day' 
#Underlying = "AMZN"
Exchange_1 = "NSE"
Exchange_2 = ""

Exchange_3 = "CBOE"
Exchange_4 = ""

Currency = "INR"
Currency2 = "USD"
time_duration = "5 Y"
#index

contract = "nifty50"
contract = twsContract(0,"NIFTY50","IND",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='TRADES', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"NIFTY50.csv")

contract = "invix"
contract = twsContract(0,"invix","IND",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='TRADES', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"invix.csv")



contract = "banknifty"
contract = twsContract(0,"BANKNIFTY","IND",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='TRADES', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"BANKNIFTY.csv")


contract = "niftyit"
contract = twsContract(0,"NIFTYIT","IND",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='TRADES', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"NIFTYIT.csv")

contract = "cnxphrm"
contract = twsContract(0,"cnxphrm","IND",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='TRADES', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"CNXPHRM.csv")


contract = "cnxauto"
contract = twsContract(0,"CNXAUTO","IND",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='TRADES', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"CNXAUTO.csv")


contract = "CNXFMCG"
contract = twsContract(0,"CNXFMCG","IND",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='TRADES', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"NIFTYFMCG.csv")

contract = "cnxpsbk"
contract = twsContract(0,"CNXPSBK","IND",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='TRADES', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"CNXPSBK.csv")


contract = "cnxmet"
contract = twsContract(0,"CNXMET","IND",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='TRADES', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"CNXMET.csv")


#cement Sector 

contract = "acc"
contract = twsContract(0,"ACC","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"ACC.csv")


contract = "ambujacem"
contract = twsContract(0,"ambujacem","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"ambujacem.csv")

contract = "grasim"
contract = twsContract(0,"grasim","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"grasim.csv")


contract = "ultracemc"
contract = twsContract(0,"ultracemc","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"ultracemc.csv")

contract = "axisbank"
contract = twsContract(0,"axisbank","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"axisbank.csv")


contract = "icicibank"
contract = twsContract(0,"icicibank","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"icicibank.csv")

contract = "hdfcbank"
contract = twsContract(0,"hdfcbank","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"hdfcbank.csv")


contract = "kotakbank"
contract = twsContract(0,"kotakbank","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"kotakbank.csv")

#FMCG
contract = "hindunilv"
contract = twsContract(0,"hindunilv","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"hindunilv.csv")

contract = "dabur"
contract = twsContract(0,"dabur","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"dabur.csv")

#power Sector
contract = "ntpc"
contract = twsContract(0,"ntpc","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"ntpc.csv")

contract = "powergrid"
contract = twsContract(0,"powergrid","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"powergrid.csv")

#tech Sector
contract = "infy"
contract = twsContract(0,"infy","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"infy.csv")

contract = "tcs"
contract = twsContract(0,"tcs","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"tcs.csv")

contract = "wipro"
contract = twsContract(0,"wipro","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"wipro.csv")

contract = "hcltech"
contract = twsContract(0,"hcltech","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"hcltech.csv")


#Psu bank
contract = "sbin"
contract = twsContract(0,"sbin","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"sbin.csv")

contract = "pnb"
contract = twsContract(0,"pnb","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"pnb.csv")


contract = "bankbarod"
contract = twsContract(0,"bankbarod","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"bankbarod.csv")

#AUTO SECTOR

contract = "MARUTI"
contract = twsContract(0,"MARUTI","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"MARUTI.csv")

contract = "tatamotor"
contract = twsContract(0,"tatamotor","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"tatamotor.csv")

contract = "bajaj-aut"
contract = twsContract(0,"bajaj-aut","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"bajaj-auto.csv")


contract = "heromotoco"
contract = twsContract(0,"heromotoc","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"heromotoco.csv")

#pharma Sector


contract = "DRREDDY"
contract = twsContract(0,"DRREDDY","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"DRREDDY.csv")

contract = "DIVISLAB"
contract = twsContract(0,"DIVISLAB","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"DIVISLAB.csv")

contract = "SUNPHARMA"
contract = twsContract(0,"SUNPHARMA","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"SUNPHARAMA.csv")


contract = "CIPLA"
contract = twsContract(0,"CIPLA","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"CIPLA.csv")


contract = "LUPIN"
contract = twsContract(0,"LUPIN","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"LUPIN.csv")

##STEEL
contract = "TATASTEEL"
contract = twsContract(0,"TATASTEEL","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"TATASTEEL.csv")

contract = "JSWSTEEL"
contract = twsContract(0,"JSWSTEEL","STK",Exchange_1,Exchange_2, "","0.0",Currency,"","","",NULL,NULL,"0")
contract = reqHistoricalData(IBConn, contract, whatToShow ='BID', useRTH = "0", barSize = time_frame, duration=time_duration, endDateTime = paste0(gsub("-","", reqCurrentTime(IBConn))))
write.zoo(contract,"JSWSTEEL.csv")

twsDisconnect(IBConn)

