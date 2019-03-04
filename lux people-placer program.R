# load libraries
library(dplyr) # unused so far

# ** CREATING MEMBERS DF **
# creating fake info to put into the dataframe for testing
name <-           c("Member 1",     "Member 2",     "Member 3")
pref_prod_1 <-    c("production_A", "production_B", "production_C")
pref_prod_2 <-    c("production_B", "production_C", "production_A")
pref_prod_3 <-    c("production_C", "production_A", "production_B")
pref_role_1 <-    c("director",     "editor",       "camera")
pref_role_2 <-    c("pa",           "director",     "assistant camera")
pref_role_3 <-    c("art dept",     "sound",        "pa")
years_in_lux <-   c(2,              2,              3)
years_at_uw <-    c(2,              3,              4)
importance <-     c("role",         "production",   "role")
notes <-          c("",             "busy 3/5/19",  "here's a link to my channel \"youtube.com\"")

# create data frame from fake info
members <- data.frame(name, pref_prod_1, pref_prod_2, pref_prod_3, pref_role_1, pref_role_2, pref_role_3,
                      years_in_lux, years_at_uw, importance, notes, stringsAsFactors = FALSE)

# name columns
column_names <- c("name", "pref_prod_1", "pref_prod_2", "pref_prod_3", "pref_role_1", "pref_role_2",
                  "pref_role_3", "years_in_lux", "years_at_uw", "importance", "notes")
colnames(members) <- column_names





# ** CREATING PRODUCTION DFS **
# list of roles, ordered by experience required
roles <- c("director", "producer", "special effects", "editor", "dp", "ad", "soundtrack", "camera", 
           "assistant camera", "sound", "makeup", "art dept", "costumes", "scripty", "pa")

# create 1 empty production dataframe
production <- data.frame(roles)
production[,column_names] <- NA
# turn factors into strings
production <- data.frame(lapply(production, as.character), stringsAsFactors=FALSE)

# copy/paste 3 dataframes
production_A <- production
production_B <- production
production_C <- production





# ** SORTING PEOPLE **

# sort members df by experience
members <- members[order(-members$years_in_lux, -members$years_at_uw),]

# for testing purposes - just sort 1st members in new "members" df (should be Member 3)
  # first...find importance (should be production)
#isTRUE(members[1,10] == "role") # returns FALSE ("production") - as test
if (members[1,10] == "role") {
  role_to_check <- as.character(members[1,5]) #"[1,5]" will change depending on # iteration, set to prod_A for testing
  # filtering production_A to specified role to be used in the boolean for the following if statement
  filtered <- filter(eval(as.name(as.character(members[1,2]))), roles == role_to_check)
  # ** CHRIS -- I CHANGED OUR IDEA TO GET PRODUCTION DF'S BELOW:
  # if the row is empty in the prod_df,
  if (is.na(filtered$name)) {
    # THEN place member's name & info in the "filtered" df,
    for (i in 1:11) {
      filtered[1, i+1] <- members[1, i]
    }
    # THEN combine with prod_A df, matching row by role name,
    combined <- left_join(eval(as.name(as.character(members[1,2]))), 
                          filtered, by = "roles", suffix = c("_to_remove", ""))
      # ...and FINALLY, remove NA columns remaining after left_join.
    combined <- combined[,-2:-12]
    
    # rename new dataframe as the production's title. ***** WORK-IN-PROGRESS BY RACHEL 3/4
    assign(get(as.character(members[1,2])), combined)
    # finished...prod_A df is updated with new placement!
  }
} else { # importance = production
  
}

# Hello CHRIS, this is a test commit! 

# View(as.name(production_list[1])) # why doesnt this work? Why cant i view df "production_C"?
