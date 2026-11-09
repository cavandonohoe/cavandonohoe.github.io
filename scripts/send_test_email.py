#!/usr/bin/env python3
"""Dry-run or send one personalized email through Gmail SMTP.

Sending requires --send plus three environment variables:
SMTP_USERNAME, SMTP_APP_PASSWORD, EMAIL_TEST_RECIPIENT.
No third-party Python packages are required.
"""

import argparse
import os
import smtplib
import ssl
from email.message import EmailMessage


def personalized_content(raw_name: str) -> tuple[str, str]:
    name = " ".join(raw_name.split())[:80] or "there"
    subject = f"GitHub Actions email test for {name}"
    body = (
        f"Hi {name},\n\n"
        "This is a real, personalized test email sent by a GitHub-hosted "
        "Actions runner using Python and Gmail SMTP.\n\n"
        "Your laptop did not need to be on.\n\n"
        "— GitHub Actions email test\n"
    )
    return subject, body


def send_email(subject: str, body: str) -> None:
    username = os.environ.get("SMTP_USERNAME", "").strip()
    password = os.environ.get("SMTP_APP_PASSWORD", "").strip()
    recipient = os.environ.get("EMAIL_TEST_RECIPIENT", "").strip()

    if not all((username, password, recipient)):
        raise SystemExit(
            "Missing required GitHub Actions secrets: SMTP_USERNAME, "
            "SMTP_APP_PASSWORD, EMAIL_TEST_RECIPIENT."
        )
    if (
        recipient.count("@") != 1
        or any(character.isspace() for character in recipient)
        or any(character in recipient for character in ",;<>")
    ):
        raise SystemExit("EMAIL_TEST_RECIPIENT must be one plain email address.")

    message = EmailMessage()
    message["From"] = username
    message["To"] = recipient
    message["Subject"] = subject
    message.set_content(body)

    with smtplib.SMTP("smtp.gmail.com", 587, timeout=30) as smtp:
        smtp.ehlo()
        smtp.starttls(context=ssl.create_default_context())
        smtp.ehlo()
        smtp.login(username, password)
        smtp.send_message(message)

    print("Gmail SMTP accepted the test message (recipient redacted from logs).")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--name", default="Cavan", help="Personalized greeting name")
    parser.add_argument(
        "--send", action="store_true", help="Send one real email (default: preview only)"
    )
    args = parser.parse_args()

    subject, body = personalized_content(args.name)
    print("Subject:", subject)
    print("Body preview:\n" + body)

    if args.send:
        send_email(subject, body)
    else:
        print("DRY RUN: no email sent; no credentials required.")


if __name__ == "__main__":
    main()
