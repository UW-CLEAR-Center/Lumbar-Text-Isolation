# function to remove sentences with cervical and thoracic

library(stringr)

cervicalThoracicRemoval = function(text,
                                   regExp="(?i)[^.]*((cervi[cx].*?\\.)|((C[1-7]|T[1-6])((?=\\.).*?\\.|\\D.*?\\.)))((?=[^.]*(?=L[1-5]\\D|lumbar|thoraci[cx]|T[1-9]\\D|(T1[0-2])|cervi[cx]|C[1-7]\\D).*?\\.)|(?=$))") {

  # remove leading and trailing white spaces
  text = stringr::str_replace_all(text, "^\\s+|\\s+$", "")

  # add period to the end of text if not present
  if (substr(text, nchar(text), nchar(text)) != ".") {
    text = paste0(text, ".")
  }

  # temporarily replace decimal with "^"
  text = stringr::str_replace_all(text, "(?<=\\d)\\.(?=\\d)", "^")

  # get cervical and thoracic sentences using regex
  removalTextSentences = stringr::str_extract_all(text, regExp)

  # if regex couldn't extract anything
  if (length(removalTextSentences[[1]]) == 0) {
    removalTextSentences = "NA"
  } else {
    removalTextSentencesFilteredForEdgeCases = c()
    for(sentences in removalTextSentences[[1]]) {

      ### split into individual sentences and get first sentence
      #### assumption that a sentence is defined a period then whitespace then any character
      firstSentence = unlist(strsplit(sentences, "(?<=\\.)\\s+(?=[[:alnum:]])", perl=TRUE))[1]

      ### check for edge-cases (2020-09-11 textExamination Entry 2020-09-25 and Entry 2020-09-29)
      if(grepl("L[1-5]", firstSentence) == FALSE &
         grepl("(?i)lumbar", firstSentence) == FALSE &
         grepl("T[7-9]\\D|T1[0-2]", firstSentence) == FALSE &
         grepl("T[1-2](-|\\s+)weight", firstSentence) == FALSE &
         grepl("T\\d+-S\\d+", firstSentence) == FALSE &
         grepl("(?i)T[1-2]\\s+(signal)", firstSentence) == FALSE &
         grepl("(?i)T[1-2]\\s+(hypo|hyper|iso)intens", firstSentence) == FALSE &
         grepl("(?i)T[1-2]\\s+(short)", firstSentence) == FALSE &
         grepl("(?i)T[1-2]\\s+(image)", firstSentence) == FALSE &
         grepl("(?i)(T[1-2]\\s+(axial|sagittal|coronal))|((axial|sagittal|coronal)\\s+T[1-2])", firstSentence) == FALSE
        ) {
        ## if first sentence is not any of these edge-cases then remove it from the text
        removalTextSentencesFilteredForEdgeCases = append(removalTextSentencesFilteredForEdgeCases, sentences)
        text = gsub(sentences, "", text, fixed=TRUE)
      }
    }

    ## no sentences were filtered out because they met some of the edge-cases
    if(length(removalTextSentencesFilteredForEdgeCases) == 0) {
      removalTextSentencesFilteredForEdgeCases = "NA"
    }

    ## combine groups of removed sentences into 1 string separated by |
    removalTextSentences = paste(unlist(removalTextSentencesFilteredForEdgeCases), collapse="|")
  }

  # return decimal using regex
  removalTextSentences = stringr::str_replace_all(removalTextSentences, "(?<=\\d)\\^(?=\\d)", ".")
  text = stringr::str_replace_all(text, "(?<=\\d)\\^(?=\\d)", ".")
  
  if(text == "") {
    text = " "
  }
  
  # return the two strings: processed string (lumbar-only text) then removal string (text that has been removed)
  return(c(text, removalTextSentences))
}

