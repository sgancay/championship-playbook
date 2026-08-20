library(tidyverse)
library(cfbfastR)
library(dplyr)
library(gt)
library(pagedown)   
library(qpdf)
options(scipen = 9999)

setwd("~/Desktop/CFB_OffSumStats")

# Generates a table for college coaches to use to get info on teams
# @param team name is the name of the team
# @param sType is the regular or post season
# @param year must be a valid year
# @return the table with summary information on play type and success on different downs
# @pre the college football team name must be an existing team and the season must be a valid type
# @post tableData = [a gt table that offensive run/pass summary statistics the selected college football team and season type, broken down by down and distance]
#AND summary_df = #summary_df
#AND team = #team
#AND sType = #sType
#Builds the raw down-and-distance summary data frame for a team, without any gt styling.
#Shared by tableData() (for the styled PDF table) and exportTeamJSON() (for the web tool).
#@param team name is the name of the team
#@param sType is the regular or post season
#@param year must be a valid year
#@return a tibble with one row per down & distance bucket plus a Total row
buildSummaryDF <- function(team, sType, year) {
  
  pbpData <- load_cfb_pbp(year)
  
  # find exact team and season_type strings 
  # Get the exact pos_team value that matches the input team (case-insensitive, partial)
  exact_team <- unique(pbpData$pos_team)[grepl(team, unique(pbpData$pos_team), ignore.case = TRUE)]
  if (length(exact_team) == 0) {
    stop(paste("Team", team, "not found in pos_team column. Available teams:", 
               paste(unique(pbpData$pos_team)[1:20], collapse = ", ")))
  }
  if (length(exact_team) > 1) {
    warning(paste("Multiple matches for", team, "using first:", exact_team[1]))
    exact_team <- exact_team[1]
  }
  
  # Get the exact season_type value ("regular" vs "Regular")
  exact_season <- unique(pbpData$season_type)[grepl(sType, unique(pbpData$season_type), ignore.case = TRUE)]
  if (length(exact_season) == 0) {
    stop(paste("Season type", sType, "not found. Available:", 
               paste(unique(pbpData$season_type), collapse = ", ")))
  }
  exact_season <- exact_season[1]
  
  #define scenarios
  scenarios <- list(
    list(label = "1st & 10",  down_val = 1, yds_min = 10, yds_max = 10),
    list(label = "2nd & 1-2", down_val = 2, yds_min = 1,  yds_max = 2),
    list(label = "2nd & 3-6", down_val = 2, yds_min = 3,  yds_max = 6),
    list(label = "2nd & 7+",  down_val = 2, yds_min = 7,  yds_max = 99),
    list(label = "3rd & 1-2", down_val = 3, yds_min = 1,  yds_max = 2),
    list(label = "3rd & 3-5", down_val = 3, yds_min = 3,  yds_max = 5),
    list(label = "3rd & 6-9", down_val = 3, yds_min = 6,  yds_max = 9),
    list(label = "3rd & 10+", down_val = 3, yds_min = 10, yds_max = 99),
    list(label = "4th & 1-2", down_val = 4, yds_min = 1,  yds_max = 2),
    list(label = "4th & 3+",  down_val = 4, yds_min = 3,  yds_max = 99)
  )
  
  #For loop to build summary rows
  rows_list <- list()
  
  for (s in scenarios) {
    
    #pass attempts
    pass_attempts <- pbpData %>%
      filter(
        pos_team      == exact_team,
        season_type   == exact_season,
        yards_to_goal >  20 & yards_to_goal < 90,
        !penalty_flag,                # same as penalty_flag == FALSE
        down          == s$down_val,
        distance      >= s$yds_min,
        distance      <= s$yds_max
      ) %>%
      summarize(total_passes = sum(pass_attempt, na.rm = TRUE))
    
    #rush attempts
    rush_attempts <- pbpData %>%
      filter(
        pos_team      == exact_team,
        season_type   == exact_season,
        yards_to_goal >  20 & yards_to_goal < 90,
        !penalty_flag,
        down          == s$down_val,
        distance      >= s$yds_min,
        distance      <= s$yds_max
      ) %>%
      summarize(total_rushes = sum(rush, na.rm = TRUE))
    
    #pass yards
    pass_yards <- pbpData %>%
      filter(
        pos_team      == exact_team,
        season_type   == exact_season,
        yards_to_goal >  20 & yards_to_goal < 90,
        !penalty_flag,
        down          == s$down_val,
        distance      >= s$yds_min,
        distance      <= s$yds_max,
        pass_attempt  == 1
      ) %>%
      summarize(total_pass_yards = sum(yards_gained, na.rm = TRUE))
    
    #rush yards
    rush_yards <- pbpData %>%
      filter(
        pos_team      == exact_team,
        season_type   == exact_season,
        yards_to_goal >  20 & yards_to_goal < 90,
        !penalty_flag,
        down          == s$down_val,
        distance      >= s$yds_min,
        distance      <= s$yds_max,
        rush          == 1
      ) %>%
      summarize(total_rush_yards = sum(yards_gained, na.rm = TRUE))
    
    #extract values
    pass_att_val  <- pass_attempts$total_passes
    rush_att_val  <- rush_attempts$total_rushes
    total_att_val <- pass_att_val + rush_att_val
    pass_yds_val  <- pass_yards$total_pass_yards
    rush_yds_val  <- rush_yards$total_rush_yards
    
    rows_list[[length(rows_list) + 1]] <- tibble(
      `Down & Distance` = s$label,
      `Run %`           = if (total_att_val > 0) as.numeric(round(rush_att_val / total_att_val * 100)) else NA_real_,
      `Run Plays`       = rush_att_val,
      `Run Yards/Play`  = if (rush_att_val > 0) round(rush_yds_val / rush_att_val, 1) else NA_real_,
      `Pass Plays`      = pass_att_val,
      `Pass Yards/Play` = if (pass_att_val > 0) round(pass_yds_val / pass_att_val, 1) else NA_real_,
      `All Plays`       = total_att_val,
      `All Yards/Play`  = if (total_att_val > 0) round((rush_yds_val + pass_yds_val) / total_att_val, 1) else NA_real_
    )
  }
  
  # Combine rows and add totals row
  summary_df <- bind_rows(rows_list)
  
  # Totals row
  total_rush_plays <- sum(summary_df$`Run Plays`)
  total_pass_plays <- sum(summary_df$`Pass Plays`)
  total_all_plays  <- total_rush_plays + total_pass_plays
  
  total_rush_yds <- sum(summary_df$`Run Plays` * summary_df$`Run Yards/Play`, na.rm = TRUE)
  total_pass_yds <- sum(summary_df$`Pass Plays` * summary_df$`Pass Yards/Play`, na.rm = TRUE)
  
  totals_row <- tibble(
    `Down & Distance` = "Total",
    `Run %`           = as.numeric(round(total_rush_plays / total_all_plays * 100)),
    `Run Plays`       = total_rush_plays,
    `Run Yards/Play`  = round(total_rush_yds / total_rush_plays, 1),
    `Pass Plays`      = total_pass_plays,
    `Pass Yards/Play` = round(total_pass_yds / total_pass_plays, 1),
    `All Plays`       = total_all_plays,
    `All Yards/Play`  = round((total_rush_yds + total_pass_yds) / total_all_plays, 1)
  )
  
  summary_df <- bind_rows(summary_df, totals_row)
  
  summary_df
}

#Generates a table for college coaches to use to get info on teams
#@param team name is the name of the team
#@param sType is the regular or post season
#@param year must be a valid year
#@return the table with summary information on play type and success on different downs
#@pre the college football team name must be an existing team and the season must be a valid type
#@post tableData = [a gt table that offensive run/pass summary statistics the selected college football team and season type, broken down by down and distance]
#AND summary_df = #summary_df
#AND team = #team
#AND sType = #sType
tableData <- function(team, sType, year) {
  
  #get team colors
  colors <- get_team_colors(team)
  primary   <- colors[["primary"]]
  secondary <- colors[["secondary"]]
  
  summary_df <- buildSummaryDF(team, sType, year)
  
  # GT Table Creation
  summary_df %>%
    gt() %>%
    tab_header(
      title    = "Offensive Run / Pass Summary",
      subtitle = getSubtitle(team, sType, year)
    ) %>%
    tab_spanner(label = "Run Plays",  id = "run_spanner",  columns = c(`Run Plays`,  `Run Yards/Play`)) %>%
    tab_spanner(label = "Pass Plays", id = "pass_spanner", columns = c(`Pass Plays`, `Pass Yards/Play`)) %>%
    tab_spanner(label = "All Plays",  id = "all_spanner",  columns = c(`All Plays`,  `All Yards/Play`)) %>%
    fmt(
      columns = c(`Run Plays`, `Pass Plays`, `All Plays`),
      fns = function(x) formatC(x, format = "d", big.mark = ",")
    ) %>%
    fmt(
      columns = c(`Run Yards/Play`, `Pass Yards/Play`, `All Yards/Play`),
      fns = function(x) formatC(round(x, 1), format = "f", digits = 1)
    ) %>%
    fmt(
      columns = `Run %`,
      fns = function(x) paste0(round(x, 0), "%")
    ) %>%
    tab_source_note(
      source_note = "Excludes red zone (+1 to +20), backed up (-1 to -10), plays with penalties, and non-standard down and distances."
    ) %>%
    tab_options(
      heading.background.color       = primary,
      heading.title.font.size        = px(20),
      heading.title.font.weight      = "bold",
      heading.subtitle.font.size     = px(13),
      column_labels.background.color = secondary,
      column_labels.font.weight      = "bold",
      column_labels.font.size        = px(13),
      column_labels.border.top.color    = secondary,
      column_labels.border.bottom.color = "black",
      source_notes.background.color  = "white",
      source_notes.font.size         = px(11),
      table.border.top.color         = secondary,
      table.border.bottom.color      = secondary,
      table.font.size                = px(13),
      data_row.padding               = px(6)
    ) %>%
    #white text on column labels and spanners
    tab_style(
      style     = cell_text(color = "white", weight = "bold"),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style     = cell_text(color = "white", weight = "bold"),
      locations = cells_column_spanners()
    ) %>%
    #white text on navy header
    tab_style(
      style     = cell_text(color = "white", weight = "bold"),
      locations = cells_title(groups = c("title", "subtitle"))
    ) %>%
    #red border around source note
    tab_style(
      style     = cell_borders(sides = c("top", "bottom", "left", "right"), color = "#C8102E", weight = px(2)),
      locations = cells_source_notes()
    ) %>%
    #white background, text on Down & Distance column
    tab_style(
      style     = list(cell_fill(color = "white"), cell_text(color = "black")),
      locations = cells_body(columns = `Down & Distance`)
    ) %>%
    #red background, white bold text on Totals row
    tab_style(
      style     = list(cell_fill(color = secondary), cell_text(color = "white", weight = "bold")),
      locations = cells_body(rows = `Down & Distance` == "Total")
    ) %>%
    #borders on all body cells
    tab_style(
      style     = cell_borders(sides = c("left", "right"), color = "black", weight = px(2)),
      locations = cells_body()
    ) %>%
    #borders on top and bottom
    tab_style(
      style     = cell_borders(sides = c("top", "bottom"), color = "lightgrey", weight = px(2)),
      locations = cells_body()
    ) %>%
    #borders on column label row
    tab_style(
      style     = cell_borders(sides = "all", color = "black", weight = px(2)),
      locations = cells_column_labels()
    ) %>%
    #borders on spanner row
    tab_style(
      style     = cell_borders(sides = "all", color = "black", weight = px(2)),
      locations = cells_column_spanners()
    )
  
}

#tableData("LSU", "Regular", 2025)

#Exports a team's down-and-distance summary as JSON matching the `rows()` schema used by
#the Sideline Card web tool (index.html). Paste the printed array into that file's
#teamData.<teamkey>.rows field to light up a new opponent.
#@param team name of the college football team (same value passed to tableData)
#@param sType is the regular or post season
#@param year must be a valid year
#@param outFile optional path to also write the JSON array to disk
#@return invisibly returns the JSON string; also cats it to the console
#@pre the college football team name must be an existing team and the season must be a valid type
#@post a JSON array of [label, down, distMin, distMax, runPct, runPlays, runYpp, passPlays, passYpp, allPlays, allYpp]
# rows, in the same order/shape as the rows() calls already in the web tool's <script> block
exportTeamJSON <- function(team, sType, year, outFile = NULL) {
  
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Install with install.packages('jsonlite').")
  }
  
  summary_df <- buildSummaryDF(team, sType, year)
  
  # convert each row into the flat array shape the JS rows() helper expects
  row_arrays <- purrr::pmap(summary_df, function(`Down & Distance`, `Run %`, `Run Plays`,
                                                  `Run Yards/Play`, `Pass Plays`, `Pass Yards/Play`,
                                                  `All Plays`, `All Yards/Play`) {
    label <- `Down & Distance`
    down  <- dplyr::case_when(
      grepl("^1st", label) ~ 1,
      grepl("^2nd", label) ~ 2,
      grepl("^3rd", label) ~ 3,
      grepl("^4th", label) ~ 4,
      TRUE ~ 0
    )
    dist_min <- dplyr::case_when(
      label == "Total"                ~ 0,
      grepl("10$", label) & down == 1 ~ 10,
      grepl("1-2$", label)            ~ 1,
      grepl("3-6$", label)            ~ 3,
      grepl("3-5$", label)            ~ 3,
      grepl("6-9$", label)            ~ 6,
      grepl("7\\+$", label)           ~ 7,
      grepl("10\\+$", label)          ~ 10,
      grepl("3\\+$", label)           ~ 3,
      TRUE                            ~ NA_real_
    )
    dist_max <- dplyr::case_when(
      label == "Total"                ~ 99,
      grepl("10$", label) & down == 1 ~ 10,
      grepl("1-2$", label)            ~ 2,
      grepl("3-6$", label)            ~ 6,
      grepl("3-5$", label)            ~ 5,
      grepl("6-9$", label)            ~ 9,
      grepl("7\\+$", label)           ~ 99,
      grepl("10\\+$", label)          ~ 99,
      grepl("3\\+$", label)           ~ 99,
      TRUE                            ~ NA_real_
    )
    list(label, down, dist_min, dist_max, `Run %`, `Run Plays`,
         `Run Yards/Play`, `Pass Plays`, `Pass Yards/Play`, `All Plays`, `All Yards/Play`)
  })
  
  json_txt <- jsonlite::toJSON(row_arrays, auto_unbox = TRUE, na = "null")
  cat("// paste below into teamData.<teamkey>.rows in index.html, wrapped in rows([ ... ])\n")
  cat(json_txt, "\n")
  
  if (!is.null(outFile)) {
    writeLines(as.character(json_txt), outFile)
    cat("Saved to", outFile, "\n")
  }
  
  invisible(json_txt)
}

# example: exportTeamJSON("Georgia Southern", "regular", 2026, "georgia_southern_2026.json")


#Team Name Lookup helper function, gets the name of the team abbreviation
#@param team name of the college football team
#@return full name of abbrev. team
#@pre team must be a valid college football team abbrev. name
#@post getTeamName = [a string containing full name of given team abbrev.]
# AND getTeamName = #team_names[[team]]
getTeamName <- function(team) {
  display_names <- list(
    "Clemson"        = "Clemson Tigers",
    "LSU"            = "LSU Tigers",
    "Troy"           = "Troy Trojans",
    "Georgia Tech"   = "Georgia Tech Yellow Jackets",
    "Syracuse"       = "Syracuse Orange",
    "North Carolina" = "North Carolina Tar Heels",
    "Boston College" = "Boston College Eagles",
    "SMU"            = "SMU Mustangs",
    "Duke"           = "Duke Blue Devils",
    "Florida State"  = "Florida State Seminoles",
    "Louisville"     = "Louisville Cardinals",
    "Furman"         = "Furman Paladins",
    "South Carolina" = "South Carolina Gamecocks",
    "Penn State"     = "Penn State Nittany Lions"
  )
  name <- display_names[[team]]
  if (is.null(name)) team else name
}

#create subtitle helper function
#@param College football Team name is the name of the team
#@param season_type is the regular or post season
#@param year the year the table data is from
#@return gives a description subtitle of team, year and season type in table
#@pre valid college football Team abbrev., valid season type("REG" or "POST"), and valid season year
#@post [a formatted subtitle string for the summary table header]
# AND getSubtitle = paste0(getTeamName(team), " - ", year, " ", season_label)
getSubtitle <- function(team, sType, year){
  season_label <- if (tolower(sType) == "regular") "Regular Season" else "Post Season"
  paste0(getTeamName(team), " - ", year, " ", season_label)
}



#gets the colors of the team for later table use
#@param college football Team name is the name of the team
#@return the primary and secondary colors of the selected team
#@pre team must be a valid college football team abbrev. name
#@post get_team_colors = [list of primary and secondary colors for slected team]
# AND get_team_colors$primary = [hex color string for the team's primary color]
# AND get_team_colors$secondary = [hex color string for the team's secondary color]
get_team_colors <- function(team) {
  colors <- list(
    "Clemson"             = c(primary = "#522D80", secondary = "#F66733"),
    "LSU"                 = c(primary = "#461D7C", secondary = "#FDD023"),
    "Georgia Tech"        = c(primary = "#003057", secondary = "#B3A369"),
    "Georgia Southern"    = c(primary = "#001C43", secondary = "#A1A2A3"),
    "North Carolina"      = c(primary = "#4B9CD3", secondary = "#13294B"),
    "California"          = c(primary = "#003262", secondary = "#FDB515"),
    "Miami"               = c(primary = "#F47321", secondary = "#005030"),
    "Charleston Southern" = c(primary = "#002855", secondary = "#B29D6C"),
    "Virginia Tech"       = c(primary = "#630031", secondary = "#CF4420"),
    "Florida State"       = c(primary = "#782F40", secondary = "#CEB888"),
    "Syracuse"            = c(primary = "#F76900", secondary = "#001E62"),
    "Duke"                = c(primary = "#003087", secondary = "#FFFFFF"),
    "South Carolina"      = c(primary = "#73000A", secondary = "#000000")
  )
  
  result <- colors[[team]]
  if (is.null(result)) stop(paste("No colors found for team:", team))
  result
}
#Generates a PDF containing a summary table for each team
#@param team_list as list of user input containg team abbreviation, season type, and year
#@param filename name of output file
#@return a PDF file saved to the working directory with one table per sheet
#@pre each input in team list must contain valid college football Team abbrev., valid season type("REG" or "POST"), and valid season year
#@post createTablePDF = [a PDF file saved to the working directory containing one per page]
# AND filename = #filename
# AND team_list = #team_list
createTablePDF <- function(team_list, filename = "CollegeFBOffSumStats.pdf", year) {
  
  temp_dir   <- tempdir()
  temp_files <- c()
  
  for (i in seq_along(team_list)) {
    entry <- team_list[[i]]
    team  <- entry[[1]]
    sType <- entry[[2]]
    year <- entry[[3]]
    
    cat("Generating table", i, "of", length(team_list), ":", team, sType, "\n")
    
    gt_table  <- tableData(team, sType, year)
    
    temp_html <- file.path(temp_dir, paste0("table_", i, ".html"))
    temp_pdf  <- file.path(temp_dir, paste0("table_", i, ".pdf"))
    
    gtsave(gt_table, filename = temp_html)
    
    chrome_print(
      input   = normalizePath(temp_html),
      output  = temp_pdf,
      options = list(printBackground = TRUE)
    )
    
    temp_files <- c(temp_files, temp_pdf)
  }
  
  pdf_combine(input = temp_files, output = filename)
  file.remove(temp_files)
  
  cat("PDF saved as:", filename, "\n")
}

#get teams for pdf export
teams <- list(
  list("Clemson", "regular", 2025),
  list("LSU", "regular", 2025),
  list("Georgia Tech", "regular", 2025),
  list("Georgia Southern", "regular", 2025),
  list("North Carolina", "regular", 2025),
  list("California","regular", 2025),
  list("Miami", "regular",2025),
  list("Charleston Southern", "regular", 2025),
  list("Virginia Tech", "regular", 2025),
  list("Florida State", "regular", 2025),
  list("Syracuse","regular", 2025),
  list("Duke", "regular", 2025),
  list("South Carolina", "regular", 2025)
)

createTablePDF(teams)