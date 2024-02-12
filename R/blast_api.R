# Functions for sending sequences to the NCBI BLAST REST API and
# retrieving the results

submitFASTA = function(fasta_seq,
                       email,
                       tool = "blastR",
                       ...,
                       n_seq,
                       blast_url = "https://blast.ncbi.nlm.nih.gov/Blast.cgi",
                       program = "blastn",
                       database = "nr",
                       megablast = "on")
{
  # Check fasta
  if(missing(n_seq)){
    n_seq = count_seq(fasta_seq)
  }
  
  if(n_seq == 0)
    stop("No valid sequences detected. Please check `fasta_seq`")
  
  fasta_seq = paste(fasta_seq, collapse = "\n")

  if(nchar(fasta_seq) > 1000)
    stop("Too many and/or too long of sequences submitted - please break into multiple queries")
  
  if(missing(email) && blast_url == "https://blast.ncbi.nlm.nih.gov/Blast.cgi"){
    stop("You must provide an email when submitting to NCBI")
    validate_email(email)
  }

  # Submit request
  ans = postForm(uri = blast_url,
                 CMD = "Put",
                 PROGRAM = program,
                 DATABASE = database,
                 METABLAST = megablast,
                 email = email,
                 tool = tool,
                 QUERY = fasta_seq,
                 ...)
  
  ## browser()
                 
  # Check for error
  check_response(ans)
  
  # Return RID and RTOE - and time of submission?
  structure(c(RID = get_RID_from_response(ans),
              RTOE = get_RTOE_from_response(ans),
              time_submitted = Sys.time()
              n_seq = n_seq,
              status = "Submitted"),
            class = c("NCBI_query", "list"))
  
}


count_seq = function(x)
{
  if(length(x) == 1)
    x = strsplit(x, "\n")[[1]]
  
  sum(grepl("^[^>]", x))
  
}

##' Adapted from \url{https://www.r-bloggers.com/2012/07/validating-email-adresses-in-r/}
##'
##' This function is very basic, and might reject a valid email address. 
##'
##' @title Validate Email
##' @param x an email address to test
##' @return NULL
##' @author Matt Espe
##' @export
validate_email = function(x)
  # called for side-effects
{
  if(length(x) != 1)
    stop("Please provide a single email address")

  if(!grepl("\\<?[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>?", as.character(x), ignore.case=TRUE))
    stop("Invalid email - please check")

  invisible(NULL)
}


fasta_test = c(">20210429FLDCT-2_Vulpes_lagopus_97",
"CACCGCGGTCATACGATTAACCCAAACTAATAGGACAACGGCGTAAAGCGTGTTTAAGATGACACATCACTAAAGTTAAAACTTAACTAAGCCGTAAAAAGCTACAGTTACAATAAAATATACTACGAAAGTGACTTTAAAATTTTCTGACTACACGATAGCTAAGACC",
">20210429FLDCT-2_Trachemys_scripta_80",
"CACCGCGGTTACACAAGAAGCCCGAACTAACAGACAACCGGCGTAAAATGTGATTAAAATTCTTAACCCATAAAATTAAGGTGAACCTACCACTTCACTGTCATACGTAAAAGTATATATTAACACATTATGAAAATAACCTTAATATAATAGGAATTATTTGAACCCACGATCGCTAAGACA")


check_response = function(response)
{
  e = xpathSApply(response, "//*[@class = 'error']")
  if(length(e) == 0)
    return(invisible(NULL))
  #handle errors here
  
}

get_RID_from_response = function(response)
{
  xmlGetAttr(xpathSApply(ans, "//*[@id='rid']")[[1]], "value")
}

get_RTOE_from_response = function(response)
{
  info = xmlValue(xpathSApply(ans, "//*/comment()"))
  i = grep("RTOE", info)
  as.numeric(gsub(".*RTOE \\= ([0-9]+).*", "\\1", info[i]))
}


check_NCBI_status = function(query, wait = TRUE,
                             blast_url = "https://blast.ncbi.nlm.nih.gov/Blast.cgi", ...)
{
  now = Sys.time()
  if(now < (query$time_submitted + query$RTOE)){
    message("Status check initiated before expected availability - waiting for expected time of availability")
    Sys.sleep(query$RTOE - (now - query$time_submitted))
  }
    
  WHILE(wait){
    ans = getForm(uri = blast_url,
                  CMD="Get",
                  RID=query$RID,
                  FORMAT_OBJECT="SearchInfo",
                  ...)
    
    status = parse_status(ans)
    if(status != "WAITING")
      break
    
    message("Results not ready. Waiting for one minute...")
    Sys.sleep(60)
  }
  
  query$status = status
  query
}


parse_status = function(response, response_types = c("WAITING", "FAILED", "UNKNOWN", "READY"))
{
  status = xmlValue(xpathSApply(response, "//*/comment()"))
  i = which(sapply(response_types, function(x) any(grepl(sprintf("Status\\=%s", x), status))))
  if(i == 4){
    hits = any(grepl("ThereAreHits\\=yes", status))
    if(!hits) return("No hits")
  }
  
  response_types[i] 
}


get_NCBI_result = function(query, wait = TRUE,
                           blast_url = "https://blast.ncbi.nlm.nih.gov/Blast.cgi",
                           ... )
{
  if(query$status != "READY")
    stop("Please use `check_NCBI_status()` to ensure results are ready first.")

  if(blast_url == "https://blast.ncbi.nlm.nih.gov/Blast.cgi" & !wait) {
    warning("Setting `wait = FALSE` not permitted when interacting with NCBI. Reactivating wait.")
    wait = TRUE
  }
  
  message("Retrieving results - expected time: ", query$n_seq, " minutes")
  lapply(seq(from = 0, length.out = query$n_seq, by = 1), function(i){
    ans = getFrom(uri = blast_url,
                  CMD = "Get",
                  RID = query$RID,
                  FORMAT_TYPE = "HTML",
                  QUERY_INDEX = i, ...)
    message("Received ", i, " results")
    if(wait && query$n_seq > 1) Sys.sleep(60)
    readHTMLTables(ans)$dscTable
  })
  
}


if(FALSE){
ans2 = readHTMLTable(htmlParse(result))

result2 = getURLContent(url = sprintf("https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Get&RID=%s&FORMAT_TYPE=HTML&QUERY_INDEX=1",
                                     "WD6CTSX3013"))
ans3 = readHTMLTable(htmlParse(result2))

aa = getURLContent(url = sprintf("https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Get&RID=%s&FORMAT_OBJECT=SearchInfo",
                                     "WD6CTSX3013"))

xpathSApply(ans, "//*[@class='hidden title']")
}
