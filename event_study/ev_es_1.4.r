# 1. User defined functions and package (library) imports #########################
print("Loading libraries and user-defined functions.")
start_time <- Sys.time()

library(estudy2)

fun_insert <- function(x, pos, insert) {
  # Create own function
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", insert, "\\2"),
       x)
}

fix_digit_names <- function(x, insertion, pos_idx = 0) {
  # NOTE: Need to assign it to the x variable
  # e.g. x <- tf_fixer(x, insertion)
  fun_insert <- function(x, pos, insert) {
    # Function inserts 'insertion' argument at the 0 position
    gsub(paste0("^(.{", pos, "})(.*)$"),
         paste0("\\1", insert, "\\2"),
         x)
  }
  
  test_vec <- grepl("^[[:digit:]+]", x)
  for (i in 1:length(x)) {
    if (test_vec[i] == TRUE) {
      x[i] <- fun_insert(x[i], pos_idx, as.character(insertion))
    }
  }
  return(x)
}

name_as_string <- function(x) {
  # Returns the name of whatever you pass as a character string
  # Primary purpose is to get string representation of function and variable names
  # Added benefit is anything passed as x will come out as 'x'
  deparse(substitute(x))
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")
# 2. Data Loading and basic wrangling #############################################
# Get directories of files
print("Fetching data...")
start_time <- Sys.time()

cd <-
  'C:/Users/Keegan/iCloudDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/daily/cleaned/es_format/'
files <-
  c('stock_data_column-wise.csv',
    'market-index_data_column-wise.csv')

# Join the strings together (static = simpler in R, but wanted to practice looping over txt)
cds <- list()

for (i in files) {
  string <- paste(cd, i, sep = "")
  # print(string)
  cds <- append(cds, string,)
}

# Read in data
# No df1 because that would be the 3rd file containing everything in df2 and df3
df2 <- read.csv(as.character(cds[1]))
df3 <- read.csv(as.character(cds[2]))

# Remove duplicate index
df2 = subset(df2, select = -c(X))
df3 = subset(df3, select = -c(X))

# Rename 'Date' var into 'date'
names(df2)[names(df2) == 'Date'] <- 'date'
names(df3)[names(df3) == 'Date'] <- 'date'

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")

print("Reformatting data.shape...")
start_time <- Sys.time()

# Reformatting 'date's into Date-class
df2$date <- as.Date(df2$date, format = "%Y-%m-%d")
df3$date <- as.Date(df3$date, format = "%Y-%m-%d")

# Index by dates
rownames(df2) <- as.Date(df2$date, format = "%Y-%m-%d")
rownames(df3) <- as.Date(df3$date, format = "%Y-%m-%d")

# Subset data.frames to get rid of the bulk of NaNs
df2 <- subset(df2, rownames(df2) > as.Date("2017-01-04"))
df3 <- subset(df3, rownames(df3) > as.Date("2017-01-04"))

# Store 'date' column
dates2 <- c(as.Date(df2$date))
dates3 <- c(as.Date(df3$date))

# Create new data.frames without 'date' cols since index by 'date'
df2 <- subset(df2, select = -date)
df3 <- subset(df3, select = -date)

# Remove remaining rows where all data points = NaN
df2 <- df2[rowSums(is.na(df2)) != ncol(df2), ]
df3 <- df3[rowSums(is.na(df3)) != ncol(df3), ]
# NOTE: Does not remove all remaining NaNs

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")

# 3. I.D. data wrangling (strings) ################################################
cat(
  "Fetching variable identification data.",
  "\n",
  "Creating variables. Reformatting for r syntax..."
)
start_time <- Sys.time()

cd_dict = "C:/Users/Keegan/iCloudDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/id/index_member_dict_fixed.xlsx"
name_dict = readxl::read_xlsx(cd_dict)

# copy df3 (market data) into list of market's and their data
# NOTE: SEEMS TO BE REDUNDANT COPYING
df_3 <- as.list(df3)

# Get colnames
keys <- colnames(name_dict)

# Replace spaces with '.' to be able to select columns from data.frame
keys2 <- as.vector(gsub(" ", ".", keys))

# Initialise empty lists with required length
# indice_list <- vector(mode = "list", length = n)
stock_list <- vector(mode = "list", length = length(df_3))
market_list <- vector(mode = "list", length = length(df_3))

# Swap numerical index for named index --> provides similarity to a python dictionary
names(market_list) <- keys2
names(stock_list) <- keys2

# Specify patterns 
pattern_list <- c(" ", "/", "-", "\\*", "&")
trouble <-
  c(
    "X009900.KS.Equity",
    "ADMR.IJ.Equity",
    "AEP.UW.Equity",
    "X300866.CH.Equity",
    "CTS.CT.Equity",
    "ACWA.AB.Equity",
    "DRR.AT.Equity",
    "BHG.SJ.Equity",
    "AYDEM.TI.Equity",
    "STLA.FP.Equity",
    "X9618.HK.Equity",
    "IVG.IM.Equity",
    "STLA.FP.Equity", #NOTE: this name is a double-up, but does not break code
    "ENR.GY.Equity",
    "HON.UW.Equity",
    "X360.AT.Equity",
    "X1810.HK.Equity",
    "X1876.HK.Equity",
    "X2269.HK.Equity",
    "X3690.HK.Equity",
    "X6098.HK.Equity",
    "X6862.HK.Equity",
    "X9988.HK.Equity",
    "X9999.HK.Equity",
    "AIL.SJ.Equity",
    "BYI.SJ.Equity",
    "DGH.SJ.Equity",
    "KRO.SJ.Equity",
    "L4L.SJ.Equity",
    "LBR.SJ.Equity",
    "MCG.SJ.Equity",
    "MKR.SJ.Equity",
    "N91.SJ.Equity",
    "NRP.SJ.Equity",
    "NY1.SJ.Equity",
    "PPH.SJ.Equity",
    "PRX.SJ.Equity",
    "QLT.SJ.Equity",
    "RNI.SJ.Equity",
    "TGA.SJ.Equity",
    "TGO.SJ.Equity",
    "TXT.SJ.Equity",
    "ABDA.IJ.Equity",
    "ABMM.IJ.Equity",
    "AGAR.IJ.Equity",
    "AHAP.IJ.Equity",
    "AKKU.IJ.Equity",
    "AKPI.IJ.Equity",
    "ALKA.IJ.Equity",
    "ALMI.IJ.Equity",
    "ALTO.IJ.Equity",
    "AMFG.IJ.Equity",
    "ANJT.IJ.Equity",
    "APII.IJ.Equity",
    "ARCI.IJ.Equity",
    "ARGO.IJ.Equity",
    "ARKA.IJ.Equity",
    "ARMY.IJ.Equity",
    "ARTA.IJ.Equity",
    "ARTI.IJ.Equity",
    "ASBI.IJ.Equity",
    "ASDM.IJ.Equity",
    "ASLC.IJ.Equity",
    "ASRM.IJ.Equity",
    "ATAP.IJ.Equity",
    "ATIC.IJ.Equity",
    "AVIA.IJ.Equity",
    "AYLS.IJ.Equity",
    "BACA.IJ.Equity",
    "BALI.IJ.Equity",
    BANK.IJ.Equity
    BAPA.IJ.Equity
    BAUT.IJ.Equity
    BAYU.IJ.Equity
    BBLD.IJ.Equity
    BBMD.IJ.Equity
    BBRM.IJ.Equity
    BBSI.IJ.Equity
    BCIC.IJ.Equity
    BEBS.IJ.Equity
    BIKA.IJ.Equity
    BIMA.IJ.Equity
    BINO.IJ.Equity
    BKDP.IJ.Equity
    BLTA.IJ.Equity
    BLTZ.IJ.Equity
    BMAS.IJ.Equity
    BMHS.IJ.Equity
    BOBA.IJ.Equity
    BOLT.IJ.Equity
    BPFI.IJ.Equity
    BPII.IJ.Equity
    BRAM.IJ.Equity
    BRNA.IJ.Equity
    BSML.IJ.Equity
    BSWD.IJ.Equity
    BTEK.IJ.Equity
    BTEL.IJ.Equity
    BUKA.IJ.Equity
    BYAN.IJ.Equity
    CANI.IJ.Equity
    CASA.IJ.Equity
    CITA.IJ.Equity
    CITY.IJ.Equity
    CLAY.IJ.Equity
    CMNP.IJ.Equity
    CMNT.IJ.Equity
    CMPP.IJ.Equity
    CMRY.IJ.Equity
    CNKO.IJ.Equity
    CNTX.IJ.Equity
    COCO.IJ.Equity
    COWL.IJ.Equity
    CPRI.IJ.Equity
    CPRO.IJ.Equity
    CSAP.IJ.Equity
    CSMI.IJ.Equity
    CTBN.IJ.Equity
    CTTH.IJ.Equity
    DART.IJ.Equity	DAYA.IJ.Equity	DCII.IJ.Equity
    DEPO.IJ.Equity	DEWA.IJ.Equity	DFAM.IJ.Equity	DGIK.IJ.Equity	DGNS.IJ.Equity	DIGI.IJ.Equity
    DNAR.IJ.Equity
    DPNS.IJ.Equity
    DRMA.IJ.Equity
    DSSA.IJ.Equity
    DUTI.IJ.Equity
    ECII.IJ.Equity	EDGE.IJ.Equity
    ELTY.IJ.Equity	EMDE.IJ.Equity	EMTK.IJ.Equity	ENAK.IJ.Equity
    ENZO.IJ.Equity	EPAC.IJ.Equity
    ETWA.IJ.Equity
    FAPA.IJ.Equity
    FASW.IJ.Equity
    FISH.IJ.Equity
    FMII.IJ.Equity
    FORU.IJ.Equity
    GAMA.IJ.Equity
    GDYR.IJ.Equity
    GEMA.IJ.Equity	GEMS.IJ.Equity
    GHON.IJ.Equity
    GMTD.IJ.Equity
    GOLL.IJ.Equity
    GPSO.IJ.Equity
    GSMF.IJ.Equity	GTBO.IJ.Equity	GTSI.IJ.Equity
    GZCO.IJ.Equity	HADE.IJ.Equity	HAIS.IJ.Equity	HDFA.IJ.Equity
    HDTX.IJ.Equity
    HOME.IJ.Equity	HOMI.IJ.Equity	HOPE.IJ.Equity
    IATA.IJ.Equity	IBFN.IJ.Equity	IBST.IJ.Equity
    IFSH.IJ.Equity
    IIKP.IJ.Equity
    IKAI.IJ.Equity
    INCF.IJ.Equity
    INDX.IJ.Equity
    INPP.IJ.Equity
    INRU.IJ.Equity	INTA.IJ.Equity
    IPPE.IJ.Equity
    ITMA.IJ.Equity
    JECC.IJ.Equity	JGLE.IJ.Equity	JIHD.IJ.Equity
    JKSW.IJ.Equity	JMAS.IJ.Equity
    JSPT.IJ.Equity
    KBRI.IJ.Equity
    KIAS.IJ.Equity
    KIOS.IJ.Equity	KJEN.IJ.Equity
    KMDS.IJ.Equity	KMTR.IJ.Equity
    KOIN.IJ.Equity	KONI.IJ.Equity
    KRAH.IJ.Equity
    KUAS.IJ.Equity	LABA.IJ.Equity
    LAPD.IJ.Equity	LCGP.IJ.Equity	LCKM.IJ.Equity	LEAD.IJ.Equity	LIFE.IJ.Equity
    LION.IJ.Equity	LMAS.IJ.Equity	LMPI.IJ.Equity	LMSH.IJ.Equity
    LPGI.IJ.Equity
    LPLI.IJ.Equity
    LRNA.IJ.Equity
    MABA.IJ.Equity	MAGP.IJ.Equity
    MAPB.IJ.Equity
    MASB.IJ.Equity	MAYA.IJ.Equity
    MCOL.IJ.Equity
    MDLN.IJ.Equity	MDRN.IJ.Equity
    MEGA.IJ.Equity
    MGNA.IJ.Equity
    MIDI.IJ.Equity
    MINA.IJ.Equity	MIRA.IJ.Equity	MITI.IJ.Equity	MKNT.IJ.Equity	MKPI.IJ.Equity
    MOLI.IJ.Equity
    MPRO.IJ.Equity
    MREI.IJ.Equity
    MSKY.IJ.Equity
    MTEL.IJ.Equity	MTFN.IJ.Equity	MTLA.IJ.Equity
    MTRA.IJ.Equity	MTSM.IJ.Equity
    MYRX.IJ.Equity	MYRXP.IJ.Equity	MYTX.IJ.Equity
    NASI.IJ.Equity
    NETV.IJ.Equity
    NICK.IJ.Equity	NICL.IJ.Equity
    NIPS.IJ.Equity
    NIRO.IJ.Equity
    NOBU.IJ.Equity	NPGF.IJ.Equity
    NTBK.IJ.Equity	NUSA.IJ.Equity	NZIA.IJ.Equity	OASA.IJ.Equity	OBMD.IJ.Equity	OCAP.IJ.Equity	OILS.IJ.Equity
    OMRE.IJ.Equity	OPMS.IJ.Equity	PADI.IJ.Equity
    PBSA.IJ.Equity	PCAR.IJ.Equity	PDES.IJ.Equity
    PGLI.IJ.Equity	PGUN.IJ.Equity
    PKPK.IJ.Equity	PLAS.IJ.Equity	PLIN.IJ.Equity	PMJS.IJ.Equity	PMMP.IJ.Equity
    PNGO.IJ.Equity
    PNSE.IJ.Equity
    POLL.IJ.Equity
    POLU.IJ.Equity	POLY.IJ.Equity	POOL.IJ.Equity	PORT.IJ.Equity
    PSDN.IJ.Equity
    PSKT.IJ.Equity
    PTDU.IJ.Equity	PTIS.IJ.Equity
    PTSP.IJ.Equity	PUDP.IJ.Equity
    PURI.IJ.Equity
    RBMS.IJ.Equity	RDTX.IJ.Equity
    RELI.IJ.Equity
    RIMO.IJ.Equity
    RMKE.IJ.Equity	ROCK.IJ.Equity
    RSGK.IJ.Equity	RUIS.IJ.Equity
    SAPX.IJ.Equity	SATU.IJ.Equity
    SBMA.IJ.Equity
    SCNP.IJ.Equity	SCPI.IJ.Equity	SDMU.IJ.Equity
    SDPC.IJ.Equity	SDRA.IJ.Equity	SEMA.IJ.Equity	SFAN.IJ.Equity	SGER.IJ.Equity
    SHID.IJ.Equity
    SIMA.IJ.Equity
    SINI.IJ.Equity	SIPD.IJ.Equity	SKBM.IJ.Equity	SKLT.IJ.Equity
    SKYB.IJ.Equity
    SMRU.IJ.Equity
    SNLK.IJ.Equity
    SOHO.IJ.Equity	SONA.IJ.Equity
    SSTM.IJ.Equity	STAR.IJ.Equity	STTP.IJ.Equity	SUGI.IJ.Equity	SULI.IJ.Equity	SUPR.IJ.Equity	SURE.IJ.Equity
    TALF.IJ.Equity
    TAPG.IJ.Equity	TARA.IJ.Equity	TAXI.IJ.Equity	TAYS.IJ.Equity
    TEBE.IJ.Equity
    TELE.IJ.Equity
    TFCO.IJ.Equity	TGKA.IJ.Equity
    TIRA.IJ.Equity	TIRT.IJ.Equity
    TOYS.IJ.Equity
    TRAM.IJ.Equity	TRIL.IJ.Equity
    TRIO.IJ.Equity	TRIS.IJ.Equity	TRJA.IJ.Equity	TRST.IJ.Equity	TRUE.IJ.Equity
    TRUS.IJ.Equity
    UANG.IJ.Equity
    UFOE.IJ.Equity
    UNIC.IJ.Equity	UNIQ.IJ.Equity	UNIT.IJ.Equity
    VICI.IJ.Equity
    WAPO.IJ.Equity
    WICO.IJ.Equity	WIFI.IJ.Equity
    WMPP.IJ.Equity	WMUU.IJ.Equity
    YPAS.IJ.Equity	YULE.IJ.Equity	ZBRA.IJ.Equity
    ZYRX.IJ.Equity
    
    
    
  )
trouble_selector <- unique(trouble)
trouble_selector <- paste("^",trouble_selector,"$", sep="")
trouble_selector <- paste0(trouble_selector, collapse = "|")

# Break ID data.frame into list to remove NANs
name_list <- as.list(name_dict)
for (i in 1:3) {
  name_list <-
    lapply(
      name_list,
      stringr::str_replace_all,
      pattern = paste0(pattern_list, collapse = "|"),
      replacement = "."
    )
  name_list <- lapply(
    name_list,
    stringr::str_replace_all,
    pattern = trouble_selector,
    replacement = NA_character_
  )
  name_list <- lapply(name_list, na.omit)
  name_list <- lapply(name_list, fix_digit_names, "X")
  
  # Make syntactically compatible
  names(name_list) <-
    lapply(names(name_list),
           gsub,
           pattern = " ",
           replacement = ".")
  
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")

# 4. Removal of empty columns #######################################################
print("Dropping problematic share names.")
start_time <- Sys.time()

# Allocate colnames
names_to_keep <- names(df2)[!(names(df2) %in% trouble)]

# Drop from data
df2_trouble <- subset(df2, select = trouble)
df2 <- subset(df2, select = names_to_keep)

# Check carve happened correctly
if ((any(trouble %in% names(df2)))==TRUE) {
  message("WARNING: Removal of problem columns failed")
} 

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")

# 5. Creation of list of market-index data.frames ##############################

print("Making list of market data.frame")
start_time <- Sys.time()

for (i in 1:length(keys2)) {
  # print(i)
  # print(keys2[[i]])
  # Select as.data.frame the market index
  temp_df <- as.data.frame(df3[, keys2[[i]], drop = FALSE])
  temp_df <- cbind(date = dates2, temp_df)
  # print(length(rownames(temp_df)))
  # rownames(temp_df) <- dates3
  
  market_list[[keys2[[i]]]] <- temp_df  
  #as.data.frame(temp_df, row.names = dates3_char) dates3_char <- as.character(dates3)
  
  rm(temp_df)
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")
# 6. data.frame2 Slicing process###################################################
print("Begining 'data.frame 2' (df2) slicing process.")
start_time <- Sys.time()

for (i in 1:length(keys2)) {
  tryCatch({
    print(i)
    print(keys2[[i]])
    # gets names corresponding to key: gets constituents of index
    str_vec <- unlist(name_list[[keys2[[i]]]], use.names = FALSE)
    
    # selects index by their names
    stock_series <- df2[str_vec]
    stock_series <- cbind(date = dates2, stock_series)
    # stock_series <- subset(df2, select=str_vec) #df2 = subset(df2, select = -c(X))
    
    # remove NA rows from data.frame
    stock_series <- stock_series[rowSums(is.na(stock_series)) != ncol(stock_series), ]
    
    # stores data in dictionary of index constituent pd.DataFrame --> Index name is key
    stock_list[[keys2[[i]]]] <- stock_series
  }, error = function(e) {
    message(cat("ERROR: ", conditionMessage(e), "\n"))
  })
}
# NOTE: THERE ARE STILL NANs PRESENT IN THE ORGANISED DATASET

# write problem data to csv to view in excel
problem_list <-
  c(
    "KOSPI.Index",
    "JCI.Index",
    "SPX.Index",
    "SHSZ300.Index",
    "SPTSX.Index",
    "SASEIDX.Index",
    "AS51.Index",
    "JALSH.Index",
    "XU100.Index",
    "N100.Index",
    "HSI.Index"
  )
# write filenames
cd_prob <- "C:/Users/Keegan/Desktop/staging/problem_stocks/"
problem_filenames <- vector(mode = "character",length=length(problem_list))
for (i in 1:length(problem_list)) {
  fname <- paste0(problem_list[[i]],".csv")
  directory <- paste0(cd_prob,fname)
  problem_filenames[[i]] = directory
  rm(fname)
  rm(directory)
}

problem_stocks <- stock_list[problem_list]
# lapply(problem_stocks, write.csv, file = "C:/Users/Keegan/Desktop/staging/problem_stocks/")

for (i in 1:length(problem_list)) {
  temp_selector <- problem_stocks[[i]]
  write.csv(temp_selector,
            file = problem_filenames[[i]]
            )
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")
# 7. Removal of remaining NAs ################################################################
cat(
  "Removing NANs for data.frames in lists:", "\n",
  "1. ", name_as_string(stock_list),"\n",
  "2. ", name_as_string(market_list),"\n"
)
start_time <- Sys.time()

# Make copies to check if results behaved as expected
market_list_copy <- market_list
stock_list_copy <- stock_list

# Drop NAs from respective list
for (i in 1:length(market_list)) {
  market_list[[i]] <- na.omit(market_list[[i]])
}
for (i in 1:length(stock_list)) {
  stock_list[[i]] <- na.omit(stock_list[[i]])
}

if ((identical(market_list,market_list_copy) == TRUE)&(identical(stock_list,stock_list_copy) == TRUE)) {
  message(cat("WARNING: Attempted NAN removal has resulted in identical lists.", "\n",
              "1.", name_as_string(market_list)), "\n",
              "2. ", name_as_string(stock_list)
          )
} else if (identical(market_list,market_list_copy) == TRUE) {
  message(cat("WARNING: Attempted NAN removal has resulted in identical lists.", "\n",
              "1.", name_as_string(market_list))
          )
} else if (identical(stock_list,stock_list_copy) == TRUE) {
  message(cat("WARNING: Attempted NAN removal has resulted in identical lists.", "\n",
              "1.", name_as_string(stock_list)))
} else {
  message(cat("NANs have successfully been removed from the following lists", "\n",
              "1. ", name_as_string(market_list), "\n",
              "2. ", name_as_string(stock_list))
          )
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")

# 8. Application of Estudy2 ########################################################
start_time <- Sys.time()
# Create data storage lists
reg_results_list <-
  vector(mode = "list", length = length(market_list))
test_results_list <-
  vector(mode = "list", length = length(market_list))

# da big loop you've all been waiting for yessir
for (i in 1:length(market_list)) {
  tryCatch({
    # print("Getting rates from prices...")
    rates <- get_rates_from_prices(
      stock_list[[i]],
      quote = "Close",
      multi_day = TRUE,
      compounding = "continuous"
    )
    
    rates_indx <- get_rates_from_prices(
      market_list[[i]],
      quote = "Close",
      multi_day = TRUE,
      compounding = "continuous"
    )
    # print("Removing NANs from rates...")
    
    # rates <- na.omit(rates)
    # rates_indx <- na.omit(rates_index)
    
    # print("Done. Applying single-index market model...")
    # apply single-index market model to get ARs
    securities_returns <- apply_market_model(
      rates = rates,
      regressor = rates_indx,
      same_regressor_for_all = TRUE,
      market_model = "sim",
      estimation_method = "ols",
      estimation_start = as.Date("2019-04-01"),
      estimation_end = as.Date("2020-03-13")
    )
    # print("Done. Applying parametric and non-parametric tests to abnormal returns...")
    # TESTS
    # Parametric tests
    para <- data.frame(
      parametric_tests(
        list_of_returns = securities_returns,
        event_start = as.Date("2020-03-16"),
        event_end = as.Date("2020-03-20")
      )
    )
    # Non-parametric tests
    non_para <- data.frame(
      nonparametric_tests(
        list_of_returns = securities_returns,
        event_start = as.Date("2020-03-16"),
        event_end = as.Date("2020-03-20")
      )
    )
    # print("Merging results.")
    # all_tests <- merge(para, non_para, by = "date")
    df_results <- data.frame(merge(para, non_para, by = "date"))
    
    test_results_list[[keys2[[i]]]] <- df_results
    reg_results_list[[keys2[[i]]]] <- securities_returns
    
  }, error = function(e) {
    message(cat("ERROR: ", conditionMessage(e), "i = ", i , "\n"))
  })
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")
# 9. Recording of results in new datafiles #####################################
start_time <- Sys.time()
# Specify directories

# Write results
# write.csv()
# write.csv()

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")
print("Terminating script...")