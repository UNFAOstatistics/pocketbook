###########################################################################
## This script is used to manually construct some variables
###########################################################################

# MORTWEIGHT --------------------------------------------------------------

preConstr.df[, "MORTWEIGHT"] <- 
  preConstr.df[, "SP.DYN.CBRT.IN"] * preConstr.df[, "OA.TPBS.POP.PPL.NO"] / 1000

# # LABPARTFEWEIGHT ---------------------------------------------------------
# 
# preConstr.df[, "LABPARTFEWEIGHT"] <- 
#   preConstr.df[, "SP.POP.1564.FE.IN"] + preConstr.df[, "SP.POP.65UP.FE.IN"]
# 
# # LABPARTMAWEIGHT ---------------------------------------------------------
# 
# preConstr.df[, "LABPARTMAWEIGHT"] <- 
#   preConstr.df[, "SP.POP.1564.MA.IN"] + preConstr.df[, "SP.POP.65UP.MA.IN"]

# RF.FERT.NIPH.TN.NO ------------------------------------------------------

preConstr.df[, "RF.FERT.NIPH.TN.NO"] <- 
  preConstr.df[, "RF.FERT.NI.TN.NO"] + preConstr.df[, "RF.FERT.PH.TN.NO"]

# RP.PEST.TOT.TN.NO -------------------------------------------------------
## NOTE (FILIPPO): we cannot do a simple sum of vector with the operator "+"
##                 because "+" as the function "sum" doesn't treat NA properly
RP.PEST.TOT.TN.NO.df <- 
  apply(preConstr.df[, c("RP.PEST.INS.TN.NO", "RP.PEST.MO.TN.NO",
                         "RP.PEST.HE.TN.NO", "RP.PEST.FB.TN.NO",
                         "RP.PEST.STF.TN.NO", "RP.PEST.STI.TN.NO")],
        MARGIN = 1, FUN = sum2)
preConstr.df[, "RP.PEST.TOT.TN.NO"] <- RP.PEST.TOT.TN.NO.df
rm(RP.PEST.TOT.TN.NO.df)

# QC.PRD.FRUNOGR.TN.NO ----------------------------------------------------

preConstr.df[, "QC.PRD.FRUNOGR.TN.NO"] <- 
  preConstr.df[, "QC.PRD.FRU.TN.NO"] - preConstr.df[, "QD.PRD.WINE.TN.NO"]

# QC.RHRV.FRUNOGR.HA.NO ---------------------------------------------------

preConstr.df[, "QC.RHRV.FRUNOGR.HA.NO"] <- 
  preConstr.df[, "QC.RHRV.FRU.HA.NO"] - preConstr.df[, "QD.RHRV.VINE.HA.NO"]

# POP.TOT.BASE.0406 -------------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "OA.TPBS.POP.PPL.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "POP.TOT.BASE.0406"
base = base[, c("FAOST_CODE", "POP.TOT.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by = "FAOST_CODE", all.x = TRUE)

# QV.GPV.FOOD.BASE.0406 ---------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.GPV.FOOD.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.GPV.FOOD.BASE.0406"
base = base[, c("FAOST_CODE", "QV.GPV.FOOD.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# QV.NPV.FOOD.BASE.0406 ---------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.NPV.FOOD.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.NPV.FOOD.BASE.0406"
base = base[, c("FAOST_CODE", "QV.NPV.FOOD.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# QV.NPV.CRPS.BASE.0406 ---------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.NPV.CRPS.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.NPV.CRPS.BASE.0406"
base = base[, c("FAOST_CODE", "QV.NPV.CRPS.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# QV.GPV.AGR.BASE.0406 ----------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.GPV.AGR.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.GPV.AGR.BASE.0406"
base = base[, c("FAOST_CODE", "QV.GPV.AGR.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# QV.NPV.AGR.BASE.0406 ----------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.NPV.AGR.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.NPV.AGR.BASE.0406"
base = base[, c("FAOST_CODE", "QV.NPV.AGR.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# QV.NPV.CRLS.BASE.0406 ---------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.NPV.CRLS.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.NPV.CRLS.BASE.0406"
base = base[, c("FAOST_CODE", "QV.NPV.CRLS.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# QV.NPV.LVSTCK.BASE.0406 -------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.NPV.LVSTCK.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.NPV.LVSTCK.BASE.0406"
base = base[, c("FAOST_CODE", "QV.NPV.LVSTCK.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# QV.NPV.NNFOOD.BASE.0406 -------------------------------------------------

base = ddply(subset(preConstr.df, 
                    subset = Year %in% c(2004,2005,2006),
                    select = c("FAOST_CODE", "Year", "QV.NPV.NNFOOD.ID.NO")),
             .(FAOST_CODE), numcolwise(mean), na.rm = TRUE)
colnames(base)[3] = "QV.NPV.NNFOOD.BASE.0406"
base = base[, c("FAOST_CODE", "QV.NPV.NNFOOD.BASE.0406")]
preConstr.df = merge(preConstr.df, base, by="FAOST_CODE", all.x=TRUE)

# TP.NETVAL.CRLSPREP.USD.NO -----------------------------------------------

preConstr.df$TP.NETVAL.CRLSPREP.USD.NO = preConstr.df$TP.EXVAL.CRLSPREP.USD.NO - 
  preConstr.df$TP.IMVAL.CRLSPREP.USD.NO

# TP.NETVAL.MEATPREP.USD.NO -----------------------------------------------

preConstr.df$TP.NETVAL.MEATPREP.USD.NO = preConstr.df$TP.EXVAL.MEATPREP.USD.NO - 
  preConstr.df$TP.IMVAL.MEATPREP.USD.NO

# TP.NETVAL.FV.USD.NO -----------------------------------------------------

preConstr.df$TP.NETVAL.FV.USD.NO = preConstr.df$TP.EXVAL.FV.USD.NO - 
  preConstr.df$TP.IMVAL.FV.USD.NO

# TP.NETVAL.MILKEQ.USD.NO -------------------------------------------------

preConstr.df$TP.NETVAL.MILKEQ.USD.NO = preConstr.df$TP.EXVAL.MILKEQ.USD.NO - 
  preConstr.df$TP.IMVAL.MILKEQ.USD.NO

# TP.NETVAL.AFOVO.USD.NO --------------------------------------------------

preConstr.df$TP.NETVAL.AFOVO.USD.NO = preConstr.df$TP.EXVAL.ANFATS.USD.NO + 
  preConstr.df$TP.EXVAL.OILSEEDS.USD.NO + preConstr.df$TP.EXVAL.VEGOIL.USD.NO -
  preConstr.df$TP.IMVAL.ANFATS.USD.NO + preConstr.df$TP.IMVAL.OILSEEDS.USD.NO -
  preConstr.df$TP.IMVAL.VEGOIL.USD.NO

# TP.NETVAL.BEV.USD.NO ----------------------------------------------------

preConstr.df$TP.NETVAL.BEV.USD.NO = preConstr.df$TP.EXVAL.BEV.USD.NO - 
  preConstr.df$TP.IMVAL.BEV.USD.NO

# TP.NETVAL.CTCS.USD.NO ---------------------------------------------------

preConstr.df$TP.NETVAL.CTCS.USD.NO = preConstr.df$TP.EXVAL.CTCS.USD.NO - 
  preConstr.df$TP.IMVAL.CTCS.USD.NO

# TP.NETVAL.SUGHON.USD.NO -------------------------------------------------

preConstr.df$TP.NETVAL.SUGHON.USD.NO = preConstr.df$TP.EXVAL.SUGHON.USD.NO - 
  preConstr.df$TP.IMVAL.SUGHON.USD.NO

# GLI.CHPF.TOT.ECO2EQ.NO --------------------------------------------------

preConstr.df$GLI.CHPF.TOT.ECO2EQ.NO = preConstr.df$GL.CL.TOT.NERCO2EQ.NO + 
  preConstr.df$GL.GL.TOT.NERCO2EQ.NO

# GHG.AFOLU.TOT.ECO2EQ.NO -------------------------------------------------

preConstr.df$GHG.AFOLU.TOT.ECO2EQ.NO = preConstr.df$GHG.TOT.ALL.GG.NO + 
  preConstr.df$GL.LU.TOT.NERCO2EQ.NO

# GN.UI.EA.TJPIN.NO -------------------------------------------------------

preConstr.df[, "GN.UI.EA.TJPIN.NO"] <- 
  preConstr.df[, "GN.TE.CIA.TJ.NO"]/preConstr.df[, "QV.GPV.AGR.ID.NO"]*1000000

# FI.PRD.TOT.TN.NO --------------------------------------------------------

preConstr.df$FI.PRD.TOT.TN.NO = preConstr.df$FI.PRD.AQ.TN.NO + 
  preConstr.df$FI.PRD.CAPT.TN.NO

# FI.NETVAL.FISH.USD.NO ---------------------------------------------------

preConstr.df$FI.NETVAL.FISH.USD.NO = preConstr.df$FI.EXVAL.FISH.USD.NO - 
  preConstr.df$FI.IMVAL.FISH.USD.NO

# TP.EXVAL.FOODWF.USD.NO --------------------------------------------------

preConstr.df$TP.EXVAL.FOODWF.USD.NO = preConstr.df$TP.EXVAL.FOOD.USD.NO + 
  preConstr.df$FI.EXVAL.FISH.USD.NO

# TP.IMVAL.FOODWF.USD.NO --------------------------------------------------

preConstr.df$TP.IMVAL.FOODWF.USD.NO = preConstr.df$TP.IMVAL.FOOD.USD.NO + 
  preConstr.df$FI.IMVAL.FISH.USD.NO

# Clean the environment ---------------------------------------------------

rm(list = "base")
