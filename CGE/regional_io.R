#Sys.setenv(MYSQL_HOME = 'C:/Program Files/R/R-3.0.2/library/RMySQL')
library(RMySQL)
library(xlsx)
library(plyr)
library(data.table)
library(gdxrrw)


####################################################################################################
# Constants used in this file


gdx.file.out <- "shock_out_prod"
value.added.per.worker <- .75
employee.comp.per.worker <- .01


####################################################################################################


asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                   asNumeric))


empl_change <- function(gdx.file)
{
  setwd("C:/Users/rjrow.ASURITE/Desktop/model production/results/run")
  usmexpat.con <- dbConnect(MySQL(),
                            user = "usmexpat_master",
                            password = "usmexpat",
                            dbname = "usmexpat", 
                            host = "usmexpat-instance2.cotzely14ram.us-west-2.rds.amazonaws.com")
  
  
  main <- rgdx.param(gdx.file.out, toString(list(name = "p_results")))
  names(main) <- c("r","PQV","s","i","oris","value")
  
  main.base <- dbReadTable(usmexpat.con, "cge_baseline_in")
  
  current.frame <- merge(main.base, main, by = c("r","PQV","s","i","oris"))
  current.frame$delta <- (current.frame$value.y-current.frame$value.x)/current.frame$value.x
  
  ####################################################################################################
  # Pull out CGE-inputs to I/O model, need outputs for both mexico and united states
  
  ####################### Employment
  
  factors.name <- c("unSkLab","Land","NatlRes","Capital","SkLab")
  factors <- subset(main, s %in% factors.name)
  
  
  mex.factors <- subset(current.frame, PQV == "V" & s == "totf" & oris == "tot" & r == "MEX",
                        select = -c(r,s,PQV,oris))
  
  us.factors <- subset(current.frame, PQV == "V" & s == "totf" & oris == "tot" & r == "US",
                       select = -c(r,s,PQV,oris,value.x,value.y))
  
  cge.io.in <- merge(mex.factors, sectors.merge, by  = "i")
  cge.io.in$V4 <- revalue(as.factor(cge.io.in$sectors), gtap.mapping.agg)
  cge.io.in <- data.table(cge.io.in)

  cge.io.in$values <- cge.io.in$delta*cge.io.in$value.x

  cge.io.in.v1 <- cge.io.in[,list(values = sum(values),
                                  value.x = sum(value.x)),
                            by = V4]

  cge.io.in.v1$delta.emp <- cge.io.in.v1$values/cge.io.in.v1$value.x

  
  #dbWriteTable(usmexpat.con, "io_regional_delta_in", )
  
  ##################### GDP # find GDP change for io mdoels
  
  ##################### Labor Income # Labor Income is simply factor income for Employment
  return(cge.io.in.v1)
  dbDisconnect(usmexpat.con)
  
}






# regional_main_run <- function()
# { 
  # Load in mappings
  source('C:/Users/rjrow.ASURITE/Desktop/model production/R/sectors.R')
  
  
  # MySQL Connection
  usmexpat.con <- dbConnect(MySQL(),
                            user = "usmexpat_master",
                            password = "usmexpat",
                            dbname = "usmexpat", 
                            host = "usmexpat-instance2.cotzely14ram.us-west-2.rds.amazonaws.com")

####################################################################################################
# Input CGE changes here to be ran through regional IO model

   
    wd <- "C:/Users/rjrow.ASURITE/Desktop/model production/R/region-IO/Mexico"
    setwd(wd)
    files <- list.files()
    lst <- vector("list", length(files))
    names(lst) <- files
    mainframe <- data.frame()



    # call in the percentage change in employment
    cge.io.in <- empl_change()
    
    for(i in 1:length(files)) 
    {
      
      file.name <- as.character(files[i])
      State.name <- gsub(" |_|.xlsx|model","",file.name)
      print(State.name)  
      
    ####################################################################################################
    # cge.input.dummy is the % change from the cge model corresponding to changes in basic industries
    # Basic industries here require an amalgamation of percentage changes of multiple sectors.
    # How to combine these?    
    
    # This needs to be updated to amalgamate percentage changes
      cge.input.dummy <- c(.15,
                           8.4,
                           .69,
                           -1.55,
                           .82,
                           .82,
                           -2.25,
                           -2.34,
                           -3.84,
                           -3.84,
                           -1.70)
      
    cge.input.dummy <- cge.input.dummy/100

    
    ####################################################################################################
    # Read in datasets from database 
    
    empl.shares <- dbReadTable(usmexpat.con, "io_employment_shares_in", row.names = "row_names")
    empl.shares <- subset(empl.shares, State == State.name)
    
    empl.impacts.basic <- dbReadTable(usmexpat.con, "io_employment_impacts_in", row.names = "row_names")
    empl.impacts.basic <- subset(empl.impacts.basic, State == State.name)
    
    empl.impacts.tot <- dbReadTable(usmexpat.con, "io_employment_impacts_total_in", row.names = "row_names")
    empl.impacts.tot <- subset(empl.impacts.tot, State == State.name)
    
    value.added <- dbReadTable(usmexpat.con, "io_value_added_in", row.names = "row_names")
    value.added <- subset(value.added, State == State.name)
    
    # This is a value that can be updated, it is used during the value.added computations

    
    ####################################################################################################
    # Part A
    
    tot.econ <- subset(empl.impacts.basic, industry_names == "Total economy", select = -c(basic_industries,
                                                                                    industry_names, State,
                                                                                    row_names))
    
    tecon.empl.shares <- t(empl.shares$lvl_empl[1:11])
    tot.empl <- data.matrix(subset(empl.shares, industry_names == "Total", select = c(lvl_empl)))
    mex.empl.shares <- (empl.shares$emp_Mexico[1:11])
    tot.econ.1000 <- (data.matrix(tot.econ))
    sweep.A <- mex.empl.shares %*% (tot.econ.1000/1000)
    str.mat.dat <- data.matrix(empl.impacts.basic[1:11,4:14]/1000)
    str.mat.A <- sweep.A-str.mat.dat
    inv.A <- solve(str.mat.A)
    empl.shares.mex <- (mex.empl.shares) %*% tot.empl
    rhs.vec.A <- data.matrix(t(tecon.empl.shares)-empl.shares.mex)
    req.empl.A <-  inv.A %*% rhs.vec.A 
    
    #####################################
    
    empl.impacts.tot.mat <- data.matrix(empl.impacts.tot[1:79,4:14])/1000
    direct.basic.empl.A <-  data.frame(sweep(empl.impacts.tot.mat, MARGIN = 2, req.empl.A, FUN = `*`))
    direct.basic.empl.A <- cbind(direct.basic.empl.A, empl.impacts.tot$Employment[1:79])
    direct.empl.A <- rowSums(direct.basic.empl.A)
    direct.empl.B <- empl.impacts.tot$Employment[1:79] - direct.empl.A 
    
    ####################################################################################################
    # Part B
    
    init.empl <- c(direct.empl.B[1],
                   direct.empl.B[6],
                   direct.empl.B[7],
                   direct.empl.B[16] + direct.empl.B[17] + direct.empl.B[18],
                   direct.empl.B[24],
                   direct.empl.B[25],
                   direct.empl.B[27] + direct.empl.B[28],
                   direct.empl.B[29],
                   direct.empl.B[30],
                   direct.empl.B[31],
                   direct.empl.B[32])
    
    
    inv.str.mat.dat <- solve(str.mat.dat)
    rhs.vec.B <- init.empl*cge.input.dummy
    req.empl.B <- inv.str.mat.dat %*% rhs.vec.B
    
    #######################################
    
    direct.basic.empl.B <-  data.frame(sweep(empl.impacts.tot.mat, MARGIN = 2, req.empl.B, FUN = `*`))
    change.B <- rowSums(direct.basic.empl.B)
    new.empl <- direct.empl.B + change.B
    
    ################################################################################################
    # here we do some mapping of values and revaluing factors
    
    direct.empl.tot.A <- data.frame(cbind(direct.empl.A, direct.empl.B,
                                          empl.impacts.tot$Employment[1:79],
                                          as.character(scian_codes)))
    
    direct.empl.tot.B <- data.frame(cbind(change.B, direct.empl.B, new.empl, 
                                          as.character(scian_codes)))
    
    
    direct.empl.tot.A[,4] <- revalue(direct.empl.tot.A[,4], scian_mapping)
    direct.empl.tot.B[,4] <- revalue(direct.empl.tot.B[,4], scian_mapping)
    
    

    direct.empl.tot.B[,4] <- as.character(direct.empl.tot.B[,4])
    direct.empl.tot.A[,4] <- as.character(direct.empl.tot.A[,4])
    
    direct.empl.tot.A <- data.table(factorsNumeric(direct.empl.tot.A))
    direct.empl.tot.B <- data.table(factorsNumeric(direct.empl.tot.B))
    
    
    direct.empl.tot.A2 <- direct.empl.tot.A[,list(direct.empl.A = sum(direct.empl.A),
                                                  direct.empl.B = sum(direct.empl.B),
                                                  total = sum(V3)),
                                            by = V4]

    direct.empl.tot.B2 <- direct.empl.tot.B[,list(change.B = sum(change.B),
                                                  direct.empl.B = sum(direct.empl.B),
                                                  new.empl = sum(new.empl)),
                                            by = V4]
    
    cge.io.in.m <- merge(direct.empl.tot.A2, cge.io.in, by = "V4")

    direct.empl.tot.A2$new.empl <- direct.empl.tot.A2$direct.empl.A * (1 + cge.io.in.m$delta.emp)
    direct.empl.tot.A2$change.A <- direct.empl.tot.A2$new.empl - direct.empl.tot.A2$direct.empl.A
    direct.empl.tot.A2$change.B <- direct.empl.tot.B2$change
    direct.empl.tot.A2$change.Tot <- direct.empl.tot.A2$change.A + direct.empl.tot.A2$change.B
    direct.empl.tot.A2$new.total <- direct.empl.tot.A2$change.Tot + direct.empl.tot.A2$total
    
    io.model.out <- direct.empl.tot.A2
    io.model.out$State <- State.name
    print(head(io.model.out,50))
    mainframe <- rbind(mainframe, io.model.out)
    
    # mainframe is the dataframe that outs to database. contains regional info
    # dbWriteTable(usmexpat.con, "io_empl_impacts_out", mainframe, row.names = FALSE, overwrite = TRUE)
    
    ################################################################################################
    # value added portion
    
    value.added.v1 <- value.added
    value.added.v1$SCIAN_code <- revalue(value.added.v1$SCIAN_code, scian_mapping)
    
    value.added.v1 <- data.table(value.added.v1)
    
    value.added.v2 <- value.added.v1[,list(employment = sum(Employment),
                                        value.added = sum(Value_added),
                                        compensation = sum(compensation)),
                                  by = SCIAN_code]
       
    value.added.v2$va.per.worker <- value.added.v2$value.added/value.added.v2$employment
    value.added.v2$comp.per.worker <- value.added.v2$compensation/value.added.v2$employment
    value.added.v2$new.va.per.worker <- value.added.v2$va.per.worker*(1+value.added.per.worker)
    value.added.v2$new.comp.per.worker <- value.added.v2$comp.per.worker*(1+employee.comp.per.worker)
    # value.added.v2 needs to go out as well, need to check the contants provided
    
    ################################################################################################
    # Data out section
    # mainframe & value.added.v3 are out-bound data sets
    
    }

dbDisconnect(usmexpat.con)

# }





