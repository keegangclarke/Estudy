# library(sendmailR)
# 
# sendmail(from = "<clarkekeegan@gmail.com>",
#          to = "<CLRKEE001@myuct.ac.za>",
#          subject = "R Email Test",
#          body = "This was an email test using R to email, \n so that I can notify myself when something is done.",
#          control = list(smtpServer="smtp.gmail.com")
#          )
library(magrittr)
library(gmailr)

text_msg <- gm_mime() %>%
  gm_to("CLRKEE001@myuct.ac.za") %>%
  gm_from("clarkekeegan@gmail.com") %>%
  gm_text_body("This was an email test using R to email, \n so that I can notify myself when something is done.")
text_msg <- strwrap(as.character(text_msg))

gm_create_draft(text_msg)

