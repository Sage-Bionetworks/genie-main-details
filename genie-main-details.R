# Description: Generate detailed error file for main GENIE centers on
#     inconsistencies in vital status files.  
# Author: Haley Hunter-Zinck
# Date: 2022-02-03

# pre-setup  ---------------------------

library(optparse)

# user input ----------------------------

option_list <- list( 
  make_option(c("-u", "--upload"), action="store_true", default = FALSE,
              help="Upload results to Synapse; otherwise, write locally"),
  make_option(c("-v", "--verbose"), action="store_true", default = FALSE, 
              help="Output script messages to the user.")
)
opt <- parse_args(OptionParser(option_list=option_list))

verbose <- opt$verbose
upload <- opt$upload

# setup ----------------------------

tic = as.double(Sys.time())

library(glue)
library(dplyr)
library(yaml)
library(rjson)
library(synapser)

secret <- Sys.getenv("SCHEDULED_JOB_SECRETS")
if (secret != "") {
  syn = synLogin(silent = T, authToken = fromJSON(secret)$SYNAPSE_AUTH_TOKEN)
} else {
  syn = synLogin(silent = T)
}

config <- read_yaml("config.yaml")

# functions ----------------------------

#' Download and load data stored in csv or other delimited format on Synapse
#' into an R data frame.
#' 
#' @param synapse_id Synapse ID
#' @version Version of the Synapse entity to download.  NA will load current
#' version
#' @param set Delimiter for file
#' @param na.strings Vector of strings to be read in as NA values
#' @param header TRUE if the file contains a header row; FALSE otherwise.
#' @param check_names TRUE if column names should be modified for compatibility 
#' with R upon reading; FALSE otherwise.
#' @param comment.char character designating comment lines to ignore
#' @return data frame
get_synapse_entity_data_in_csv <- function(synapse_id, 
                                           version = NA,
                                           sep = ",", 
                                           na.strings = c("NA"), 
                                           header = T,
                                           check_names = F,
                                           comment.char = "#") {
  
  if (is.na(version)) {
    entity <- synGet(synapse_id)
  } else {
    entity <- synGet(synapse_id, version = version)
  }
  
  data <- read.csv(entity$path, stringsAsFactors = F, 
                   na.strings = na.strings, sep = sep, check.names = check_names,
                   header = header, comment.char = comment.char)
  return(data)
}

#' Store a file on Synapse with options to define provenance.
#' 
#' @param path Path to the file on the local machine.
#' @param parent_id Synapse ID of the folder or project to which to load the file.
#' @param file_name Name of the Synapse entity once loaded
#' @param prov_name Provenance short description title
#' @param prov_desc Provenance long description
#' @param prov_used Vector of Synapse IDs of data used to create the current
#' file to be loaded.
#' @param prov_exec String representing URL to script used to create the file.
#' @return Synapse ID of entity representing file
save_to_synapse <- function(path, 
                            parent_id, 
                            file_name = NA, 
                            prov_name = NA, 
                            prov_desc = NA, 
                            prov_used = NA, 
                            prov_exec = NA) {
  
  if (is.na(file_name)) {
    file_name = path
  } 
  file <- File(path = path, parentId = parent_id, name = file_name)
  
  if (!is.na(prov_name) || !is.na(prov_desc) || !is.na(prov_used) || !is.na(prov_exec)) {
    act <- Activity(name = prov_name,
                    description = prov_desc,
                    used = prov_used,
                    executed = prov_exec)
    file <- synStore(file, activity = act)
  } else {
    file <- synStore(file)
  }
  
  return(file$properties$id)
}

get_synapse_current_version_of_entity <- function(synapse_id) {
  return(synGet(synapse_id, downloadFile = F)$properties$versionNumber)
}

check_vital_redaction <- function(pat) {
  res <- pat %>%
    filter(grepl(pattern = "[<>]", x = YEAR_DEATH) & !grepl(pattern = "[<>]", x = INT_DOD) |
             !grepl(pattern = "[<>]", x = YEAR_DEATH) & grepl(pattern = "[<>]", x = INT_DOD)) %>%
    mutate(YEAR = YEAR_DEATH) %>%
    mutate(INT = INT_DOD) %>%
    select(PATIENT_ID, YEAR, INT, DEAD)
  
  return(res)
}

check_vital_text <- function(pat, 
                             strs_text = c("Unknown", "Not Collected", "Not Applicable", "Not Released"),
                             exception = T) {
  res <- pat %>%
    filter((is.element(YEAR_DEATH, strs_text) | is.element(INT_DOD, strs_text)) & YEAR_DEATH != INT_DOD) %>%
    mutate(YEAR = YEAR_DEATH) %>%
    mutate(INT = INT_DOD) %>%
    select(PATIENT_ID, YEAR, INT, DEAD)
  
  if (exception) {
    res <- res %>%
      filter(!(!is.element(YEAR, strs_text) & INT == "Unknown")) %>%
      select(PATIENT_ID, YEAR, INT, DEAD)
  }
  
  return(res)
}

check_dead_true <- function(pat, 
                           strs_true = c("TRUE", "True")) {
  res <- pat %>% 
    filter(is.element(DEAD, strs_true) & (INT_DOD == 'Not Applicable' | YEAR_DEATH == 'Not Applicable')) %>%
    mutate(YEAR = YEAR_DEATH) %>%
    mutate(INT = INT_DOD) %>%
    select(PATIENT_ID, YEAR, INT, DEAD)
  
  return(res)
}

check_dead_false <- function(pat, 
                            strs_false = c("FALSE", "False")) {
  res <- pat %>% 
    filter(is.element(DEAD, strs_false) & (INT_DOD != 'Not Applicable' | YEAR_DEATH != 'Not Applicable')) %>%
    mutate(YEAR = YEAR_DEATH) %>%
    mutate(INT = INT_DOD) %>%
    select(PATIENT_ID, YEAR, INT, DEAD)
  
  return(res)
}

check_dead_text <- function(pat,  
                             strs_text = c("Unknown", "Not Collected", "Not Applicable", "Not Released")) {
  res <- pat %>% 
    filter(is.element(DEAD, strs_text) & (INT_DOD != 'Not Applicable' | YEAR_DEATH != 'Not Applicable') & DEAD != INT_DOD & DEAD != YEAR_DEATH) %>%
    mutate(YEAR = YEAR_DEATH) %>%
    mutate(INT = INT_DOD) %>%
    select(PATIENT_ID, YEAR, INT, DEAD)
  
  return(res)
}

check_dead_value <- function(pat, 
                             strs_dead = c("TRUE", "True", "FALSE", "False", "Unknown", "Not Collected", "Not Applicable", "Not Released")) {
  res <- pat %>%
    filter(!is.element(DEAD, strs_dead)) %>%
    mutate(YEAR = YEAR_CONTACT) %>%
    mutate(INT = INT_CONTACT) %>%
    select(PATIENT_ID, YEAR, INT, DEAD)
  
  return(res)
  
}

check_contact_redaction <- function(pat) {
  res <- pat %>%
    filter(grepl(pattern = "[<>]", x = YEAR_CONTACT) & !grepl(pattern = "[<>]", x = INT_CONTACT) |
             !grepl(pattern = "[<>]", x = YEAR_CONTACT) & grepl(pattern = "[<>]", x = INT_CONTACT)) %>%
    mutate(YEAR = YEAR_CONTACT) %>%
    mutate(INT = INT_CONTACT) %>%
    select(PATIENT_ID, YEAR, INT, DEAD)
  
  return(res)
}

check_contact_text <- function(pat, 
                               strs_text = c("Unknown", "Not Collected", "Not Applicable", "Not Released"),
                               exception = T) {
  res <- pat %>%
    filter((is.element(YEAR_CONTACT, strs_text) | is.element(INT_CONTACT, strs_text)) & YEAR_CONTACT != INT_CONTACT) %>%
    mutate(YEAR = YEAR_CONTACT) %>%
    mutate(INT = INT_CONTACT) %>%
    select(PATIENT_ID, YEAR, INT, DEAD)
  
  if (exception) {
    res <- res %>%
      filter(!(!is.element(YEAR, strs_text) & INT == "Unknown")) %>%
      select(PATIENT_ID, YEAR, INT, DEAD)
  }
  
  return(res)
}

#' Format column names of clinical files for standard processing.
#' - convert to all upper case
#' - replace any duplicate column names with unique column name
#' 
#' @param raw Vector of strings representing raw column names
#' @return Vector of strings
clean_column_names <- function(raw) {
  mod <- toupper(raw)
  mod[which(duplicated(raw))] <- glue("{mod[which(duplicated(raw))]}_2")
  return(mod)
}

format_output <- function(df, synid_file_pat, synid_ver_pat = NA, check_no = NA, error = "", request = "") {
  mod <- df %>% 
    mutate(synapse_id = synid_file_pat) %>%
    mutate(synapse_version = synid_ver_pat) %>%
    mutate(check_no = check_no) %>%
    mutate(error = error) %>%
    mutate(request = request)
  return(mod)
}

#' Return current time as a string.
#' 
#' @param timeOnly If TRUE, return only time; otherwise return date and time
#' @param tz Time Zone
#' @return Time stamp as string
#' @example 
#' now(timeOnly = T)
now <- function(timeOnly = F, tz = "US/Pacific") {
  
  Sys.setenv(TZ=tz)
  
  if(timeOnly) {
    return(format(Sys.time(), "%H:%M:%S"))
  }
  
  return(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}

read_data <- function(synid_file_pat, synid_ver_pat) {
  if (is.null(synid_ver_pat)) {
    synid_ver_pat <- get_synapse_current_version_of_entity(synid_file_pat)
    
  }
  
  pat <- get_synapse_entity_data_in_csv(synid_file_pat, version = synid_ver_pat, sep = "\t")
  colnames(pat) <- clean_column_names(colnames(pat))
  
  return(pat)
}

get_check_details <- function(pat, synid_file_pat, synid_ver_pat) {
  
  res <-c()
  res <- rbind(res, format_output(check_vital_redaction(pat), 
                                  synid_file_pat = synid_file_pat, 
                                  synid_ver_pat = synid_ver_pat,
                                  check_no = 1,
                                  error = 'Redaction is inconsistent between YEAR_DEATH and INT_DOD',
                                  request = 'Please redact both or neither value'))
  res <- rbind(res, format_output(check_vital_text(pat), 
                                  synid_file_pat = synid_file_pat, 
                                  synid_ver_pat = synid_ver_pat,
                                  check_no = 2,
                                  error = 'Values are inconsistent between YEAR_DEATH and INT_DOD',
                                  request = 'Please use numeric values for both or the same text value for both.'))
  res <- rbind(res, format_output(check_dead_true(pat), 
                                  synid_file_pat = synid_file_pat, 
                                  synid_ver_pat = synid_ver_pat,
                                  check_no = 3,
                                  error = 'YEAR_DEATH and/or INT_DOD cannot be \'Not Applicable\' when DEAD is TRUE',
                                  request = 'Please update the values to be consistent.'))
  res <- rbind(res, format_output(check_dead_false(pat), 
                                  synid_file_pat = synid_file_pat, 
                                  synid_ver_pat = synid_ver_pat,
                                  check_no = 4,
                                  error = 'If DEAD is false, YEAR_DEATH and INT_DOD must be \'Not Applicable\'',
                                  request = 'Please update the values to be consistent.'))
  res <- rbind(res, format_output(check_dead_text(pat), 
                                  synid_file_pat = synid_file_pat, 
                                  synid_ver_pat = synid_ver_pat,
                                  check_no = 5,
                                  error = 'If DEAD value is a text value, it must match YEAR_DEAD and INT_DOD',
                                  request = 'Please update the values to be consistent.'))
  res <- rbind(res, format_output(check_dead_value(pat), 
                                  synid_file_pat = synid_file_pat, 
                                  synid_ver_pat = synid_ver_pat,
                                  check_no = 6,
                                  error = 'DEAD variable must be one of the following values: "TRUE", "True", "FALSE", "False", "Unknown", "Not Collected"',
                                  request = 'Please update the DEAD value to one of the valid values.'))
  res <- rbind(res, format_output(check_contact_text(pat), 
                                  synid_file_pat = synid_file_pat, 
                                  synid_ver_pat = synid_ver_pat,
                                  check_no = 7,
                                  error = 'Numeric and text values are inconsistent between YEAR_CONTACT and INT_CONTACT',
                                  request = 'Please use numeric values for both or neither.'))
  res <- rbind(res, format_output(check_contact_redaction(pat), 
                                  synid_file_pat = synid_file_pat, 
                                  synid_ver_pat = synid_ver_pat,
                                  check_no = 8,
                                  error = 'Redaction is inconsistent between YEAR_CONTACT and INT_CONTACT',
                                  request = 'Please redact both or neither value.'))
  return(res)
}

write_check_details <- function(res, synid_file_pat, outfile, synid_folder_output = NA) {
  if (nrow(res) > 0) {
    to_write <- res %>%
      mutate(issue_no = 1:nrow(res), .before = PATIENT_ID) 
    
  } else {
    header <- c("issue_no",colnames(res))
    to_write <- matrix(NA, nrow = 0, ncol = length(header), dimnames = list(c(), header))
  }
  write.csv(to_write, file = outfile, row.names = F)
  
  synid_file_output <- ""
  if (!is.na(synid_folder_output)) {
    synid_file_output <- save_to_synapse(path = outfile,
                                         parent_id = synid_folder_output,
                                         prov_name = 'upload error details',
                                         prov_desc = 'row based error reporting and details for issues in uploaded files',
                                         prov_used = synid_file_pat,
                                         prov_exec = 'https://github.com/Sage-Bionetworks/genie-main-details/blob/main/genie-main-details.R')
    
    file.remove(outfile)
  }
  
  return(synid_file_output)
}

run_one_site <- function(center, synid_file_pat, synid_ver_pat = NA, synid_folder_output = NA, verbose = F) {
  
  outfile <- glue("{center}_errors_details.csv")
  
  pat <- read_data(synid_file_pat, synid_ver_pat)
  res <- get_check_details(pat, synid_file_pat, synid_ver_pat)
  
  synid_file_output <- write_check_details(res = res, synid_file_pat = synid_file_pat, outfile = outfile, synid_folder_output = synid_folder_output) 
  
  if (verbose) {
    
    print(glue("{now()}: {center}"))
    print(glue("{now()}: Number of unique error types: {res %>% select(check_no) %>% distinct() %>% count()}"))
    print(glue("{now()}: Number of error instances: {nrow(res)}"))
    
    if (!is.null(synid_folder_output) && upload) {
      print(glue("{now()}: Errors written to '{outfile}' ({synid_file_output})"))
    } else {
      print(glue("{now()}: Errors written locally to '{outfile}')"))
    }
    
    print("--------------")
  }
  
  return(synid_file_output)
}

run_all_sites <- function(config, verbose = F, upload = F) {
  
  synid_files_output <- c()
  
  for (center in names(config)) {
    config_site <- config[[center]]
    synid_file_output <- run_one_site(synid_file_pat = config_site$synid_file_pat, 
                                      center = center, 
                                      synid_folder_output = if (upload) config_site$synid_folder_output else NA, 
                                      verbose = verbose)
    synid_files_output <- append(synid_files_output, synid_file_output)
  }
  
  return(synid_files_output)
}

# run for all sites ----------------

synid_files_output <- run_all_sites(config, verbose = verbose, upload = upload)

# close out ----------------------------

if (verbose) {
  
  if (upload) {
    print(glue("output files: {paste0(synid_files_output, collapse = ', ')}"))
  }
  
  toc = as.double(Sys.time())
  print(glue("{now()}: Runtime: {round(toc - tic)} s"))
}
