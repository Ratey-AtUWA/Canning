#### RIETVELD TABLE EXTRACTION FROM HIGHSCORE WORD REPORTS ####

if(!require(docxtractr)) install.packages("docxtractr")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(reshape2)) install.packages("reshape2")

library(docxtractr)
library(openxlsx)
library(reshape2)

#### EXTRACTION FUNCTION extract_phases() ####

# test: # (doc_file <- paste0(path1, f1[24]))

extract_phases <- function(doc_file){
  tmp <- tempdir() # temp files in temporary place
  # highscore reports are zip/docx with .DOC extension!
  # component with Rietveld table embedded in an .emf image
  parts <- unzip(doc_file, list = TRUE)
  peakplot <- parts[grep("emf",parts[,1]),1]
  unzip(doc_file, files = peakplot, exdir = tmp)
  emf <- file.path(tmp, peakplot)
  raw <- readBin(emf, "integer", n = file.info(emf)$size, size = 1, signed = F)
  chars <- intToUtf8(raw) # gives VERY LONG string
  hits <- 
    regmatches(chars,
               gregexpr("[A-Z][a-z][A-Za-z() /-]*\\s+[0-9]+\\.?[0-9]*\\s*%",
                        chars, perl = TRUE)
               )[[1]]
  outDF <- data.frame(Mineral = sub("\\s+[0-9].*$", "", hits),
             Percent = as.numeric(sub(".*?([0-9]+\\.?[0-9]*)\\s*%$", "\\1", hits)))
  return(outDF)
}

# 1. ENVT4461 2026-S2 ####
path1 <- paste0("C:/Users/00028958/OneDrive - The University of Western Australia",
                "/ENVT3361-4461/2026/Second Semester/Processed Data/Word Reports/")
(f1 <- list.files(path1, patt="DOC"))

for(i in seq_along(f1)){
  if(i == 1){
    xdf1 <- extract_phases(paste0(path1, f1[i]))
    xdf1 <- data.frame(Sample = rep(substr(f1[i],1,nchar(f1[i])-4), nrow(xdf1)),xdf1)
  } else {
    ndf1 <- extract_phases(paste0(path1, f1[i]))
    ndf1 <- data.frame(Sample = rep(substr(f1[i],1,nchar(f1[i])-4), nrow(ndf1)),ndf1)
    xdf1 <- rbind(xdf1, ndf1)
    }
}
xrdWB <- buildWorkbook(xdf1, sheetName="long-form")
setColWidths(xrdWB, "long-form", cols=c(1:ncol(xdf1)), widths="auto")

xdf1w <- dcast(xdf1, Sample ~ Mineral) # Sample ~ Mineral gives samples as rows

addWorksheet(xrdWB, sheetName="wide-form")
writeData(wb=xrdWB, sheet="wide-form", xdf1w) 
setColWidths(xrdWB, "wide-form", cols=c(1:ncol(xdf1w)), widths="auto")

saveWorkbook(xrdWB, file="summary-XRD-ENVT4461-2026-S2.xlsx") # won't overwrite if file exists

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
#                          _    _     _                        __ _             
# ___ __ __ ___  _ _  _  _| |_ | |_  (_) _ _   __ _     __ _  / _| |_  ___  _ _ 
#/ -_)\ V // -_)| '_|| || |  _|| ' \ | || ' \ / _` |   / _` ||  _|  _|/ -_)| '_|
#\___| \_/ \___||_|   \_, /\__||_||_||_||_||_|\__, |   \__,_||_|  \__|\___||_|  
#   _                 |__/  _          __  _  |___/            _                    
#  | |_   ___  _ _  ___    (_) ___    / _|(_) __ _  _  _  _ _ (_) _ _   __ _        
#  | ' \ / -_)| '_|/ -_)   | |(_-<   |  _|| |/ _` || || || '_|| || ' \ / _` |       
#  |_||_|\___||_|  \___|   |_|/__/   |_|  |_|\__, | \_,_||_|  |_||_||_|\__, |       
#              _       _    _                |___/   _    _            |___/        
#   ___  _  _ | |_    | |_ | |_   ___     ___  _ __ | |_ (_) ___  _ _   ___         
#  / _ \| || ||  _|   |  _|| ' \ / -_)   / _ \| '_ \|  _|| |/ _ \| ' \ (_-<         
#  \___/ \_,_| \__|    \__||_||_|\___|   \___/| .__/ \__||_|\___/|_||_|/__/         
#                                             |_|                                   
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
library(antiword)
(txt1 <- antiword(paste0(path1,f1[1])))

con <- file(paste0(path1, f1[1]), "rb")
raw_head <- readBin(con, "raw", 100)
close(con)

rawToChar(raw_head)

unzip(
  paste0(path1, f1[24]),
  list = TRUE
)


doc <- read_docx(paste0(path1,f1[1]))
txt <- docx_extract_all_text(doc)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

tmp <- tempdir()

unzip(
  zipfile = paste0(path1, f1[1]),
  files = "word/document.xml",
  exdir = tmp
)

xml_file <- file.path(tmp, "word", "document.xml")

library(xml2)

doc <- read_xml(
  paste0(paste0(path1, f1[1]), "/word/document.xml")
)

library(xml2)

txt <- xml_text(
  xml_find_all(paste0(path1, f1[1]), ".//w:t")
)

txt <- paste(txt, collapse = " ")

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

get_text <- function(file){
  
  tmp <- tempdir()
  
  unzip(
    file,
    files = "word/document.xml",
    exdir = tmp
  )
  
  doc <- read_xml(
    file.path(tmp, "word", "document.xml")
  )
  
  paste(
    xml_text(xml_find_all(doc, ".//w:t")),
    collapse = "\n"
  )
}

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

xrd0 <- get_text(paste0(path1, f1[1]))
cat(xrd0)

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# file <- paste0(path1, f1[2])

get_text <- function(file){
  
  tmp <- tempdir()
  
  unzip(
    file,
    files = "word/document.xml",
    exdir = tmp
  )
  
  doc <- read_xml(
    file.path(tmp, "word", "document.xml")
  )
  
  paras <- xml_find_all(doc, ".//w:p")
  
  txt <- sapply(paras, function(p) {
    paste(xml_text(xml_find_all(p, ".//w:t")),
          collapse = "")
  })
  
}

# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
sink(file="peakIDs_with_hornblende.txt")
  for(i in seq_along(f1)){
    xrd0 <- get_text(paste0(path1, f1[i]))
    xrdTab <- matrix(xrd0[seq((bmks[length(bmks)-2])+2,(bmks[length(bmks)-1])-1)],
                    ncol=7, byrow=TRUE) |> 
      as.data.frame()
    colnames(xrdTab) <- xrdTab[1,]
    xrdTab <- xrdTab[-1,]
    {cat("-=-=-=-=-=-",f1[i],"-=-=-=-=-=-","\n")
      print(xrdTab[,c(4,6)])}
  }
sink()



# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# file <- paste0(path1, f1[2])

get_text <- function(file){
  
  tmp <- tempdir()
  
  unzip(
    file,
    files = "word/document.xml",
    exdir = tmp
  )
  
  doc <- read_xml(
    file.path(tmp, "word", "document.xml")
  )
  
  rows <- xml_find_all(doc, ".//w:tr")
  
  tbl <- lapply(rows, function(row) {
    cells <- xml_find_all(row, ".//w:tc")
    sapply(cells, function(cell) {
      paste(xml_text(xml_find_all(cell, ".//w:t")),
            collapse = "")
    })
  })
  tbl <- do.call(rbind, tbl)
}

#### THIS ONE IS BEST ####
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# i <- 2
# path1 <- "C:/Users/00028958/OneDrive - The University of Western Australia/My Documents/Postgraduate/Yanyan-MEnvSci/XRD with actinolite/"
path1 <- "C:/Users/00028958/OneDrive - The University of Western Australia/My Documents/Postgraduate/Yanyan-MEnvSci/NAA/"
(f1 <- list.files(path1, patt="DOC"))

for(i in seq_along(f1)){
  xrd0 <- get_text(paste0(path1, f1[i]))
  if(i==1){
    xrdTab <- data.frame(Sample=rep(f1[i], 
                         length(seq(grep("Visible",tbl),grep("°2θ",tbl)[1]-1))),
                         tbl[seq(grep("Visible",tbl),grep("°2θ",tbl)[1]-1),])
    colnames(xrdTab)[2:ncol(xrdTab)] <- xrdTab[1,2:ncol(xrdTab)]
    xrdTab <- xrdTab[-1,]
  } else {
    xrdNext <- data.frame(Sample=rep(f1[i], 
                          length(seq(grep("Visible",tbl),grep("°2θ",tbl)[1]-1))),
                          tbl[seq(grep("Visible",tbl),grep("°2θ",tbl)[1]-1),])
    colnames(xrdNext)[2:ncol(xrdNext)] <- xrdNext[1,2:ncol(xrdNext)]
    xrdNext <- xrdNext[-1,]
    xrdTab <- rbind(xrdTab,xrdNext)
  }
}

#### TRYING TO GET THE RIETVELD TABLE OUT!!! ####

tmp <- tempdir()

# 1. EXTRACT THE VECTOR IMAGES 👎
unzip(
  file,
  files = c(
    "word/media/image1.wmf",
    "word/media/image2.emf"
  ),
  exdir = tmp
)

raw <- readBin(
  file.path(tmp, "word/media/image1.wmf"),
  "raw",
  100000
)

grep(
  "Quartz",
  rawToChar(raw[raw >= as.raw(32)], multiple = TRUE)
)
# 2. EXTRACT THE oleObject 👎
unzip(
  file,
  files = "word/embeddings/oleObject1.bin",
  exdir = tmp
)

raw <- readBin(
  file.path(tmp, "word/embeddings/oleObject1.bin"),
  "raw",
  500000
)
txt <- rawToChar(raw[raw >= as.raw(32)], multiple = TRUE)

# 3a. TRY AGAIN EMF -=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
unzip(paste0(path1, f1[2]), list = TRUE)
# 16 word/embeddings/oleObject1.bin  111104 1980-01-01
# 17          word/media/image1.wmf 5021032 1980-01-01
# 18          word/media/image2.emf  998328 1980-01-01

tmp <- tempdir()

unzip(
  file,
  files = "word/media/image2.emf",
  exdir = tmp
)

emf <- file.path(tmp, "word/media/image2.emf")

raw <- readBin(emf, "raw", file.info(emf)$size)

strings <- rawToChar(raw, multiple = TRUE)

head(strings, 100)

grep("Kaol", strings)

# OR

raw2 <- readBin(emf, "integer", n = file.info(emf)$size,
                size = 1, signed = FALSE)

chars <- intToUtf8(raw2)

substr(chars, 1, 5000)

grep("Quartz", chars)

loc <- regexpr("Quartz", chars)[1]

substr(chars, loc - 500, loc + 2000)

hits <- regmatches(
  chars,
  gregexpr(
    "[A-Z][a-z][A-Za-z() /-]+\\s+[0-9]+\\.?[0-9]*\\s*%",
    chars,
    perl = TRUE
  )
)[[1]]

hits

# _._.

# 3b. TRY AGAIN WMF -=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
tmp <- tempdir()

unzip(
  file,
  files = "word/media/image1.wmf",
  exdir = tmp
)

wmf <- file.path(tmp, "word/media/image1.wmf")

raw <- readBin(wmf, "raw", file.info(wmf)$size)

strings <- rawToChar(raw, multiple = TRUE)

head(strings, 100)

grep("Kaol", strings)

# 4. CONVERT WORD TO PDF  -=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- ####
library(RDCOMClient)

word <- COMCreate("Word.Application")

doc <- word$Documents()$Open(
  normalizePath("report.DOC")
)

pdf_file <- sub("\\.[Dd][Oo][CcXx]*$", ".pdf",
                normalizePath("report.DOC"))

doc$SaveAs(pdf_file, FileFormat = 17)

doc$Close(FALSE)
word$Quit()

