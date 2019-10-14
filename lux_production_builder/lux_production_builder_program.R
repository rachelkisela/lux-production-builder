library(devtools)
library(data.table)
library(dplyr)
library(tibble)


people_placer <- function(production_titles, googleform, num_productions) {
  # ***** COMMONLY USED VARIABLES - entered by user in app *****
  
  # replace spaces with underscores
  production_titles_u <- c()
  for (i in 1:num_productions) {
    next_item <- gsub(" ", "_", production_titles[i])
    production_titles_u <- c(production_titles_u, next_item)
  }

  
  # ** ACTUAL GOOGLE FORMS DATA: CREATING MEMBERS DF **
  # delete timestamped column
  members <- googleform[,-1]
  # define column names
  column_names <- c("email", "name", "years_in_lux", "years_at_uw", "writer/director?",
                    "writer/director film", "pref_prod_1", "pref_prod_2", "pref_prod_3",
                    "pref_role_1", "pref_role_2", "pref_role_3", "importance", "notes")
  # rename column names so the program can reference them
  colnames(members) <- column_names
  
  
  
  # ** CREATING PRODUCTION DF **
  # list of roles, ordered by experience required
  roles <- c("Director", "Producer", "Special Effects", "Editor", "Director of Photography", "Assistant Director",
             "Soundtrack", "Camera Operator", "Assistant Camera Operator", "Sound Recordist", "Boom Operator", "Makeup",
             "Art Department", "Costume", "Script Supervisor")
  
  # create 1 empty production dataframe
  production <- data.frame(roles)
  # add roles row names
  row.names(production) <- roles
  # delete inital row of role names
  production$roles <- NULL
  
  # create column titles corresponding to each production
  all_column_names <- c()
  for (i in 1:num_productions) {
    next_item <- sapply(column_names, function(x) paste0(production_titles_u[i], "_", x))
    all_column_names <- c(all_column_names, next_item)
  }
  
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
  View(members)
  
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
      
      # ********* WRITERS/DIRECTORS *********
      if (members[1, "writer/director?"] == "Yes") {
        # paste together the correct column name to check in the "production" DF
        column_to_check <- paste0(members[1, 6], "_email")
        column_to_check <- gsub(" ", "_", column_to_check)
        # store the column # matching the column_to_check string for future use
        starting_column_number <- which(colnames(production) == column_to_check)
        # place member on production df
        for (i in 1:14) {
          production["Director", starting_column_number - 1 + i] <- members[1, i]
          # "production" df is now updated with the new placement!
        }
        # delete the top row off of the "members" df 
        members <- members[-1,]
        member_just_placed <- TRUE
        break
      }
      # **************************************
      
      
      
      
      # ********* SETTING UP VARIABLES *********
      # store the role to check
      role_to_check <- members[1, paste0("pref_role_", role_choice)]
      # store the row # matching the role_to_check string for future use
      starting_row_number <- which(rownames(production) == role_to_check)
      # paste together the correct column name to check in the "production" DF
      column_to_check <- paste0(members[1, production_choice + 6], "_email")
      column_to_check <- gsub(" ", "_", column_to_check)
      # store the column # matching the column_to_check string for future use
      starting_column_number <- which(colnames(production) == column_to_check)
      # **************************************  
      
      
      # ********* IMPORTANCE = ROLE *********
      if (members[1, "importance"] == "Role") {
        # if the current "role to check" is a pa, copy & paste the member onto the PA df,
        # remove the top row of the members df, and begin placing the next member.
        if (role_to_check == "Production Assistant") {
          
          for (i in 1:14) {
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
        
        if (is.na(production[role_to_check, column_to_check])) { # if the cell is empty in the "production" df,
          
          # THEN place member's name & info into the "production" df.
          for (i in 1:14) {
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
              for (i in 1:14) {
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
            
          }
        }
        
        
        # ********* IMPORTANCE = PRODUCTION *********
      } else {
        # if the current "role to check" is a pa, copy & paste the member onto the PA df,
        # remove the top row of the members df, and begin placing the next member.
        if (role_to_check == "Production Assistant") {
          
          for (i in 1:14) {
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
          for (i in 1:14) {
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
              for (i in 1:14) {
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
            
          }
        }
        
      }
      
      if (member_just_placed) {break} # keep running this "repeat" loop while on the same member,
      # allows loop to remember the # of iteration
    }
  }
  
  # THEN, split the 1 big production df into 3 separate productions dfs and
  # create a list of all production dataframes for variable-free referencing
  prod_df_list <- vector('list', num_productions)
  for (i in 1:num_productions) {
    prod_df_list[[i]] <- production[(1 + ((i - 1) * 14)):(i * 14)]
  }
  
  
  # ********* PLACING PA's ON PRODUCTION DATAFRAMES *********
  if (nrow(pa) > 1) { # ** ADDED THIS CHECK 10/14
    # delete the top blank row
    pa <- pa[-1,]
    # sort PAs by #1 preferred production
    pa <- pa[order(pa$pref_prod_1),]
  
    # store indeces of unique values in pref_prod_1 column to find out where to split pa df
    unique_indexes <- tapply(seq_along(pa$pref_prod_1), pa$pref_prod_1, identity)[unique(pa$pref_prod_1)]
    unique_indexes <- lapply(unique_indexes, `[[`, 1) # only save first element from each list value
    unique_indexes <- unlist(unique_indexes, use.names = FALSE) # turn list into a vector
  
  
    for (i in 1:length(unique_indexes)) {
    
      if (i == length(unique_indexes)) {
        split <- pa[unique_indexes[i]:nrow(pa),]
      } else {
        split <- pa[unique_indexes[i]:(unique_indexes[i + 1] - 1),] # ** ARGUMENT OF LENGHT 0 ON THIS LINE
      }
    
      names(split) <- names(prod_df_list[[i]]) # change pa_df colnames to prod_df colnames to match for rbind fxn
      # moving row(s) matching production to that production's dataframe
      prod_df_list[[i]] <- rbind(prod_df_list[[i]], "Production Assistant" = split)
    }
  }
  
  #8/8 TESTING MAKING MASTER DF
  #prod_df_list[[num_productions + 1]] <- rbindlist(prod_df_list, use.names = TRUE, fill = TRUE)
  #prod_df_list[[num_productions + 1]] <- bind_cols(lapply(prod_df_list, add_rownames)) ## THIS ONE TOO MANY ROWS
  #prod_df_list[[num_productions + 1]] <- merge(prod_df_list, by = "row.names", all.x = T)
  
  # ********* CREATING USABLE FILENAMES *********
  
filenames <- c(paste0(production_titles_u, ".csv")) #8/8 TESTING MAKING MASTER DF->  , "MASTER.csv")
  
  # ********* EXPORTING RESULTS *********

# returns prod. dataframes and underscored filenames
return(c(prod_df_list, filenames))
  
}

# Local Test Information:
#luxtitles <- c("Pumpkin Spice", "Home", "Life on Earth", "Pseudonym")
#file <- read.csv("LUX Role Survey AU19 (Responses) EDITED BY RACHEL - Form Responses 1.csv", stringsAsFactors = FALSE)
#testlist <- people_placer(luxtitles, file, 4)