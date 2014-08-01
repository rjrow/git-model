library(reshape2)
library(gdxrrw)

## Purpose of this file is to support the efforts of Synapse in deconstructing the data
## for visualizations

# current.frame is our beasty dataset in this case, a completely flattened dataset 
# which can be cast into several matrices and then based on the topic, sheets can be created
# Tables will eventually be created. This upcoming block will display the products as final demands
# By the constiuents and then intermediate demand, exports imports etc.

# Meta information needs to go into this function for the database
# Who ran, what time, etc etc



cgerun_to_db <- function(gdx.file)
{
  
# Initialize connection object, a few constants, set working directory real fast
# my use of setwd is not correct. what is the correct way to operate filepaths?

setwd("C:/Users/rjrow.ASURITE/Desktop/model production/git-model/CGE/data")
usmexpat.con <- dbConnect(MySQL(),
                          user = "usmexpat_master",
                          password = "usmexpat",
                          dbname = "usmexpat", 
                          host = "usmexpat-instance2.cotzely14ram.us-west-2.rds.amazonaws.com")

####################################################################################################

current.frame <- rgdx.param(gdx.file, toString(list(name = "p_results")))
names(current.frame) <- c("r","PQV","s","i","oris","value")

####################################################################################################
## Products This upcoming block will display the products as final demands
## By the constituents and then intermediate demand 


######################################
# households
fd.hh <- subset(current.frame, PQV %in% QPT & s %in% sectors & i == "hou" & oris 
                %in% c("dom","imp"))
fd.hh.out <- fd.hh

######################################
# Govt
fd.gov <- subset(current.frame, PQV %in% QPT & s %in% sectors & i == "gov" & oris %in% 
                   c("dom","imp"))
fd.gov.out <- fd.gov

#####################################
# intermediate Demand
int <- subset(current.frame, PQV %in% QPT & s %in% sectors & i %in% sectors & 
                oris %in% c("dom","imp"))
int.out <- dcast(int, r + PQV + i + oris ~ s, value.var = "value")

#####################################
# international transport

intTrans <- subset(current.frame, PQV %in% QPT & s %in% sectors & i == "intTrs" & oris == "dom")
intTrans.out <- dcast(intTrans, r + PQV + i + oris ~ s, value.var = "value")

#####################################
# investment demand

inv.totals <- subset(current.frame, (s == "totp") & i == "inv" & oris == "dom")
inv.totals.out <- inv.totals

inv <- subset(current.frame, i == "inv" & PQV %in% QPT & oris == "dom" & s %in% sectors)
inv.out <- dcast(inv, r + PQV + oris ~ s, value.var = "value")

#####################################
# Exports

xs <- subset(current.frame, PQV %in% QPT & s %in% sectors & i %in% ctrs & oris == "dom")
xs.out <- dcast(xs, r + PQV + i ~ s, value.var = "value")

#####################################
# Imports

ms <- subset(current.frame, PQV %in% QPT & s %in% ctrs & i %in% sectors & oris == "imp")
ms.out <- dcast(ms, r + PQV + s ~ i, value.var = "value")

#####################################
# Factors

factors <- subset(current.frame, PQV %in% QPT & s %in% f & i %in% sectors & oris == "dom")
factors.out <- dcast(factors, r + PQV + s ~ i, value.var = "value")

#####################################
# sectors

output <- subset(current.frame, PQV %in% QPT & s == "out" & i %in% sectors & oris == "dom")
output.out <- dcast(output, r + PQV + s ~ i, value.var = "value")

####################################################################################################
# Write tables to database/excel, depending on who is asking

dbWriteTable(usmexpat.con, "cge_household_out", fd.hh.out, row.names = FALSE, overwrite = TRUE, 
             field.types=generate_field_list(fd.hh.out, matrix.field.list))

dbWriteTable(usmexpat.con, "cge_government_out", fd.gov.out, row.names = FALSE, overwrite = TRUE,
             field.types=generate_field_list(fd.gov.out, matrix.field.list))

dbWriteTable(usmexpat.con, "cge_intermediates_out", int.out, row.names = FALSE, overwrite = TRUE,
             field.types=generate_field_list(int.out, matrix.field.list))

dbWriteTable(usmexpat.con, "cge_int_transports_out", intTrans.out, row.names = FALSE, overwrite = TRUE,
             field.types=generate_field_list(intTrans.out, matrix.field.list))

dbWriteTable(usmexpat.con, "cge_investment_out", inv.out, row.names = FALSE, overwrite = TRUE,
             field.types=generate_field_list(inv.out, matrix.field.list))

dbWriteTable(usmexpat.con, "cge_investment_totals_out", inv.totals.out, row.names = FALSE, overwrite = TRUE,
             field.types=generate_field_list(inv.totals.out, matrix.field.list))

dbWriteTable(usmexpat.con, "cge_exports_out", xs.out, row.names = FALSE, overwrite = TRUE,
             field.types=generate_field_list(xs.out, matrix.field.list))

dbWriteTable(usmexpat.con, "cge_imports_out", ms.out, row.names = FALSE, overwrite = TRUE,
             field.type=generate_field_list(ms.out, matrix.field.list))

dbWriteTable(usmexpat.con, "cge_factors_out", factors.out, row.names = FALSE, overwrite = TRUE,
             field.type=generate_field_list(factors.out, matrix.field.list))

dbWriteTable(usmexpat.con, "cge_output_out", output.out, row.names = FALSE, overwrite = TRUE,
             field.type=generate_field_list(output.out, matrix.field.list))

dbDisconnect(usmexpat.con)


}

