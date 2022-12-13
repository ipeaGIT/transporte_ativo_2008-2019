get_pns_timeout <- function (year, selected = FALSE, anthropometry = FALSE, vars = NULL, 
          labels = TRUE, deflator = TRUE, design = TRUE, savedir = tempdir()) 
{
  if (year != 2013 & year != 2019) {
    message("Year must be equal to 2013 or 2019.")
    return(NULL)
  }
  if (!dir.exists(savedir)) {
    savedir <- tempdir()
    message(paste0("The directory provided does not exist, so the directory was set to '", 
                   savedir), "'.")
  }
  if (substr(savedir, nchar(savedir), nchar(savedir)) == "/" | 
      substr(savedir, nchar(savedir), nchar(savedir)) == "\\") {
    savedir <- substr(savedir, 1, nchar(savedir) - 1)
  }
  ftpdir <- paste0("https://ftp.ibge.gov.br/PNS/", year, "/Microdados/")
  if (!projmgr::check_internet()) {
    message("The internet connection is unavailable.")
    return(NULL)
  }
  if (httr::http_error(httr::GET(ftpdir, httr::timeout(600)))) {
    message("The microdata server is unavailable.")
    return(NULL)
  }
  options(timeout = max(3000, getOption("timeout")))
  ftpdata <- paste0(ftpdir, "Dados/")
  datayear <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", 
                                                                   "\n", RCurl::getURL(ftpdata, dirlistonly = TRUE)), "\n")), 
                                              "<a href=[[:punct:]]")), ".zip"))
  dataname <- datayear[which(startsWith(datayear, paste0("PNS_", 
                                                         year)))]
  if (length(dataname) == 0) {
    message("Data unavailable for selected year.")
    return(NULL)
  }
  else if (length(dataname) > 1) {
    message("There is more than one file available for the requested microdata, please contact the package maintainer.")
    return(NULL)
  }
  else {
    dataname <- paste0(dataname, ".zip")
  }
  utils::download.file(url = paste0(ftpdata, dataname), destfile = paste0(savedir, 
                                                                          "/", dataname), mode = "wb")
  if (suppressWarnings(class(try(utils::unzip(zipfile = paste0(savedir, 
                                                               "/", dataname), exdir = savedir), silent = TRUE)) == 
                       "try-error")) {
    message("The directory defined to save the downloaded data is denied permission to overwrite the existing files, please clear or change this directory.")
    return(NULL)
  }
  utils::unzip(zipfile = paste0(savedir, "/", dataname), exdir = savedir)
  docfiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", 
                                                                   "\n", RCurl::getURL(paste0(ftpdir, "Documentacao/"), 
                                                                                       dirlistonly = TRUE)), "\n")), "<a href=[[:punct:]]")), 
                              ".zip"))
  inputzip <- paste0(docfiles[which(startsWith(docfiles, "Dicionario_e_input"))], 
                     ".zip")
  utils::download.file(url = paste0(ftpdir, "Documentacao/", 
                                    inputzip), destfile = paste0(savedir, "/Dicionario_e_input.zip"), 
                       mode = "wb")
  utils::unzip(zipfile = paste0(savedir, "/Dicionario_e_input.zip"), 
               exdir = savedir)
  microdataname <- dir(savedir, pattern = paste0("^PNS_", 
                                                 year, ".*\\.txt$"), ignore.case = FALSE)
  microdatafile <- paste0(savedir, "/", microdataname)
  microdatafile <- rownames(file.info(microdatafile)[order(file.info(microdatafile)$ctime), 
  ])[length(microdatafile)]
  inputname <- dir(savedir, pattern = paste0("^input_PNS_", 
                                             year, ".*\\.txt$"), ignore.case = FALSE)
  inputfile <- paste0(savedir, "/", inputname)
  inputfile <- rownames(file.info(inputfile)[order(file.info(inputfile)$ctime), 
  ])[length(inputfile)]
  data_pns <- PNSIBGE::read_pns(microdata = microdatafile, 
                                input_txt = inputfile, vars = vars)

  else if (selected == TRUE | (selected == FALSE & anthropometry == 
                               TRUE)) {
    data_pns <- data_pns[(data_pns$M001 == "1" & !is.na(data_pns$M001)), 
    ]
    data_pns <- data_pns[, !(names(data_pns) %in% c("V0028", 
                                                    "V00281", "V00282", "V00283", "V0030", "V00301", 
                                                    "V00302", "V00303"))]
    if (selected == FALSE) {
      message("The selected argument was defined as true for the use of the anthropometry module, since the year is different from 2019.")
    }
  }
  else {
    data_pns <- data_pns[, !(names(data_pns) %in% c("V0029", 
                                                    "V00291", "V00292", "V00293", "V0030", "V00301", 
                                                    "V00302", "V00303"))]
  }
  if (labels == TRUE) {
    if (exists("pns_labeller", where = "package:PNSIBGE", 
               mode = "function")) {
      dicname <- dir(savedir, pattern = paste0("^dicionario_PNS_microdados_", 
                                               year, ".*\\.xls$"), ignore.case = FALSE)
      dicfile <- paste0(savedir, "/", dicname)
      dicfile <- rownames(file.info(dicfile)[order(file.info(dicfile)$ctime), 
      ])[length(dicfile)]
      data_pns <- PNSIBGE::pns_labeller(data_pns = data_pns, 
                                        dictionary.file = dicfile)
    }
    else {
      message("Labeller function is unavailable in package PNSIBGE.")
    }
  }
  if (deflator == TRUE) {
    if (exists("pns_deflator", where = "package:PNSIBGE", 
               mode = "function")) {
      ftpdef <- ("https://ftp.ibge.gov.br/PNS/Documentacao_Geral/")
      deffiles <- unlist(strsplit(unlist(strsplit(unlist(strsplit(gsub("\r\n", 
                                                                       "\n", RCurl::getURL(ftpdef, dirlistonly = TRUE)), 
                                                                  "\n")), "<a href=[[:punct:]]")), ".zip"))
      defzip <- paste0(deffiles[which(startsWith(deffiles, 
                                                 "Deflatores"))], ".zip")
      utils::download.file(url = paste0(ftpdef, defzip), 
                           destfile = paste0(savedir, "/Deflatores.zip"), 
                           mode = "wb")
      utils::unzip(zipfile = paste0(savedir, "/Deflatores.zip"), 
                   exdir = savedir)
      defname <- dir(savedir, pattern = paste0("^deflator_PNS.*\\.xls$"), 
                     ignore.case = FALSE)
      deffile <- paste0(savedir, "/", defname)
      deffile <- rownames(file.info(deffile)[order(file.info(deffile)$ctime), 
      ])[length(deffile)]
      data_pns <- PNSIBGE::pns_deflator(data_pns = data_pns, 
                                        deflator.file = deffile)
    }
    else {
      message("Deflator function is unavailable in package PNSIBGE.")
    }
  }
  if (design == TRUE) {
    if (exists("pns_design", where = "package:PNSIBGE", 
               mode = "function")) {
      data_pns <- PNSIBGE::pns_design(data_pns = data_pns)
    }
    else {
      message("Sample design function is unavailable in package PNSIBGE.")
    }
  }
  return(data_pns)
}