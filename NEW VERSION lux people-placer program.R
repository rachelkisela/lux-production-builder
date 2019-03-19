# load libraries
library(dplyr)
library(devtools)
# devtools::install_github("dokato/todor") - Finds "#TODO"s in the document.
  # However it doesnt work because this is an RScript file and not an R Project?
  # Not quite sure how that works.


# ** FAKE INFO: CREATING MEMBERS DF **
# creating fake info to put into the dataframe for testing
# real-world, would take in a Google Forms spreadsheet
name <-           c("Member 1",     "Member 2",     "Member 3")
pref_prod_1 <-    c("production_A", "production_C", "production_C")
pref_prod_2 <-    c("production_B", "production_B", "production_A")
pref_prod_3 <-    c("production_C", "production_A", "production_B")
pref_role_1 <-    c("director",     "camera",       "camera")
pref_role_2 <-    c("pa",           "director",     "assistant camera")
pref_role_3 <-    c("art dept",     "sound",        "pa")
years_in_lux <-   c(2,              2,              3)
years_at_uw <-    c(2,              3,              4)
importance <-     c("production",   "role",         "role")
notes <-          c("",             "busy 3/5/19",  "here's a link to my channel \"youtube.com\"")

# create data frame from fake info
members <- data.frame(name, pref_prod_1, pref_prod_2, pref_prod_3, pref_role_1, pref_role_2, pref_role_3,
                      years_in_lux, years_at_uw, importance, notes, stringsAsFactors = FALSE)

# name columns
column_names <- c("name", "pref_prod_1", "pref_prod_2", "pref_prod_3", "pref_role_1", "pref_role_2",
                  "pref_role_3", "years_in_lux", "years_at_uw", "importance", "notes")
colnames(members) <- column_names



# ** ACTUAL GOOGLE FORMS DATA: CREATING MEMBERS DF -- VERY INCOMPLETE **
members_real = read.csv("EXAMPLE_ LUX Role Survey AU19 (Responses) - Form Responses 1.csv")
# I had to make a new line (pressed ENTER once) at the bottom of the actual CSV file in TextEdit 
# to get R to read it correctly -- huh, weird.
# The names of the columns are gonna present a big problem. Lots of code will need to be changed since specific
# column names are referenced.



# ** CREATING PRODUCTION DF **
# list of roles, ordered by experience required
roles <- c("director", "producer", "special effects", "editor", "dp", "ad", "soundtrack", "camera", 
           "assistant camera", "sound", "makeup", "art dept", "costumes", "scripty", "pa")

# create 1 empty production dataframe
production <- data.frame(roles)
# add roles row names
row.names(production) <- roles
# delete inital row of role names
production$roles <- NULL

# create column titles corresponding to each production
prod_1_column_names <- sapply(column_names, function(x) paste0("production_A_", x))
prod_2_column_names <- sapply(column_names, function(x) paste0("production_B_", x))
prod_3_column_names <- sapply(column_names, function(x) paste0("production_C_", x))
# combine all column titles into one giant list of column titles
all_column_names <- c(prod_1_column_names, prod_2_column_names, prod_3_column_names)

# create 1 big "production" dataframe, name the columns and make every cell "NA"
production[,all_column_names] <- NA


# ** CREATING PA DF **
# create a "pa" dataframe with 1 cell
pa <- data.frame("pa")
# delete inital row
pa$X.pa. <- NULL

# create 1 "pa" dataframe, name the columns and make every cell "NA"
pa[,all_column_names] <- NA







# ** SORTING PEOPLE **

# sort members df by experience, so more experienced people get placed first
members <- members[order(-members$years_in_lux, -members$years_at_uw),]


# ** ALGORITHM **

# define 1st production choice, 1st role choice, and if a member was just placed
production_choice <- 1
role_choice <- 1
member_just_placed <- FALSE

# run this loop while there are still members to sort
while (!is.na(members[1,1])) {
  
  # reset to 1st production choice, 1st role choice, and if a member was just placed
  production_choice <- 1
  role_choice <- 1
  member_just_placed <- FALSE
  
  repeat {
   
    # store the role to check
    role_to_check <- members[1, role_choice + 4]
    # store the row # matching the role_to_check string for future use
    starting_row_number <- which(rownames(production) == role_to_check)
    
    # paste together the correct column name to check in the "production" DF
    column_to_check <- paste0(members[1, production_choice + 1], "_name") # bracket #2 will change depending on production_choice
    # store the column # matching the column_to_check string for future use
    starting_column_number <- which(colnames(production) == column_to_check)
    
    
# ********* IMPORTANCE = ROLE *********
    if (members[1,10] == "role") {
  
      if (is.na(production[role_to_check, column_to_check])) {   # if the cell is empty in the "production" df,
        # THEN place member's name & info into the "production" df.
        for (i in 1:11) {
          production[starting_row_number, starting_column_number - 1 + i] <- members[1, i]
        }
        
        # "production" df is now updated with the new placement!
        # delete the top row off of the "members" df 
        members <- members[-1,]
        member_just_placed <- TRUE
        
      } else {    # however, if the row is NOT empty in the prod_df,
        
        # increment production_choice and role_choice according to their preference (role more important)
        production_choice <- production_choice + 1
        
        if (production_choice == 4) {
          
          production_choice <- 1
          role_choice <- role_choice + 1
          
          # if all choices are exhausted, add this member to the PA df
          if (role_choice == 4 && production_choice == 1) {
            
            for (i in 1:11) {
              # copy name into PA df
              pa[1, i] <- members[1, i]
              
              # add blank row to top of pa df
              x <- rep(NA, ncol(pa))
              pa <- rbind(x, pa)
            }
            
          }
          
        }
      }
      
      
# ********* IMPORTANCE = PRODUCTION *********
    } else {
    
        if (is.na(production[role_to_check, column_to_check])) {   # if the cell is empty in the "production" df,
          # THEN place member's name & info into the "production" df.
          for (i in 1:11) {
          production[starting_row_number, starting_column_number - 1 + i] <- members[1, i]
         }
        
         # "production" df is now updated with the new placement!
         # delete the top row off of the "members" df 
         members <- members[-1,]
         member_just_placed <- TRUE
        
        } else {    # however, if the row is NOT empty in the prod_df,
          
          # increment production_choice and role_choice according to their preference (production more important)
          role_choice <- role_choice + 1
          
          if (role_choice == 4) {
            
            production_choice <- production_choice + 1
            role_choice <- 1
            
            # if all choices are exhausted, add this member to the PA df
            if (role_choice == 1 && production_choice == 4) {
              for (i in 1:11) {
                # copy name into PA df
                pa[1, i] <- members[1, i]
                
                # add blank row to top of pa df
                x <- rep(NA, ncol(pa))
                pa <- rbind(x, pa)
              }
              
            }
            
          }
        }
      
    }

    if (member_just_placed) {break} # keep running this "repeat" loop while on the same member,
                                     # allows loop to remember the # of iteration
  }
}
# FINALLY - sort PA df by member order again

# Hi Rachel!

