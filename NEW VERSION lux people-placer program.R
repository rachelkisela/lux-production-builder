# load libraries
library(dplyr)
library(devtools)
# devtools::install_github("dokato/todor") - Finds "#TODO"s in the document.
  # However it doesnt work because this is an RScript file and not an R Project?
  # Not quite sure how that works.


# ** FAKE INFO: CREATING MEMBERS DF **
# creating fake info to put into the dataframe for testing
# real-world, would take in a Google Forms spreadsheet
name <-           c("Member 1",     "Member 2",     "Member 3",         "pa1", "pa2", "pa3") # added these 3 pa members to test pa df at end of program
pref_prod_1 <-    c("production_A", "production_C", "production_C",     "production_A", "production_B", "production_C")
pref_prod_2 <-    c("production_B", "production_B", "production_A",     "production_A", "production_B", "production_C")
pref_prod_3 <-    c("production_C", "production_A", "production_B",     "production_A", "production_B", "production_C")
pref_role_1 <-    c("pa",           "camera",       "camera",           "pa", "pa", "pa")
pref_role_2 <-    c("director",     "director",     "assistant camera", "pa", "pa", "pa")
pref_role_3 <-    c("art dept",     "sound",        "pa",               "pa", "pa", "pa")
years_in_lux <-   c(2,              2,              3,                  5, 5, 5)
years_at_uw <-    c(2,              3,              4,                  5, 5, 5)
importance <-     c("production",   "role",         "role",             "role", "role", "role")
notes <-          c("",             "busy 3/5/19",  "here's a link to my channel \"youtube.com\"", "", "", "")

# create data frame from fake info
members <- data.frame(name, pref_prod_1, pref_prod_2, pref_prod_3, pref_role_1, pref_role_2, pref_role_3,
                      years_in_lux, years_at_uw, importance, notes, stringsAsFactors = FALSE)

# name columns
column_names <- c("name", "pref_prod_1", "pref_prod_2", "pref_prod_3", "pref_role_1", "pref_role_2",
                  "pref_role_3", "years_in_lux", "years_at_uw", "importance", "notes")
colnames(members) <- column_names



# ** ACTUAL GOOGLE FORMS DATA: CREATING MEMBERS DF -- VERY INCOMPLETE **
members_real = read.csv("EXAMPLE_ LUX Role Survey AU19 (Responses) - Form Responses 1-2.csv")
# delete timestamped row
members_real <- members_real[,-1]
# rename column names so the program can reference them
colnames(members_real) <- column_names

# I had to make a new line (pressed ENTER once) at the bottom of the actual CSV file in TextEdit 
# to get R to read it correctly -- huh, weird.



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
pa[,column_names] <- NA







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
      
      # if the current "role to check" is a pa, copy & paste the member onto the PA df,
      # remove the top row of the members df, and begin placing the next member.
      if (role_to_check == "pa") {
        
        for (i in 1:11) {
          # copy name into PA df
          pa[1, i] <- members[1, i]
        }
          
         # add blank row to top of pa df
        x <- rep(NA, ncol(pa))
        pa <- rbind(x, pa)
        
        # delete the top row off of the "members" df 
        members <- members[-1,]
        member_just_placed <- TRUE
        break
      }
      
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
      
      # if the current "role to check" is a pa, copy & paste the member onto the PA df,
      # remove the top row of the members df, and begin placing the next member.
      if (role_to_check == "pa") {
        
        for (i in 1:11) {
          # copy name into PA df
          pa[1, i] <- members[1, i]
        }
        
        # add blank row to top of pa df
        x <- rep(NA, ncol(pa))
        pa <- rbind(x, pa)
        
        # delete the top row off of the "members" df 
        members <- members[-1,]
        member_just_placed <- TRUE
        break
      }
    
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
# THEN, split the 1 big production df into 3 separate productions dfs
production_A_df <- production[1:11]
production_B_df <- production[12:22]
production_C_df <- production[23:33]


# ** PLACING PAs ON PRODUCTION DFs **
# group PAs by #1 preferred production
# (grouping prevents having to use string as a variable name)
pa <- pa[order(pref_prod_1),] # TODO RK 4/14: why does this order it prod_B THEN prod_A? shouldnt it be alphabetical?

# this creates 2 blank rows on top and bottom. Delete these rows
pa <- pa[-1,] # deletes top row
pa_df_length <- nrow(pa) # finds total # rows, uses that variable to...
pa <- pa[-pa_df_length,] #...delete bottom row.

# TODO RK 4/14:
# for the length of the pa df, check if "pref_prod_1" matches the next row.
# if it does, split the data frame into each production's pa df.
for (i in 1:pa_df_length - 1) {
  
}

# finally, place into appropriate separate production DF








