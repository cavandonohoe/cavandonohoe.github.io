send_gmail_notification <- function(
  to,
  subject,
  body,
  from = to,
  client_secret = "~/.R/gmailr/client_secret.json",
  cache = "~/.R/gmailr"
) {
  gmailr::gm_auth_configure(path = client_secret)
  gmailr::gm_auth(email = from, cache = cache)

  msg <- gmailr::gm_mime()
  msg <- gmailr::gm_to(msg, to)
  msg <- gmailr::gm_from(msg, from)
  msg <- gmailr::gm_subject(msg, subject)
  msg <- gmailr::gm_text_body(msg, body)

  gmailr::gm_send_message(msg)
}
