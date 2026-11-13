# GitHub Actions: personalized email smoke test

This is a small experiment to demonstrate that GitHub-hosted Linux runners can send a **real personalized email** even when your own laptop is switched off.

## Safety and execution

- Opening or updating this PR runs **preview only**. No credentials are needed.
- The script defaults to a dry run, printing the subject and body but **never sending**.
- There is no schedule, push-triggered send, or automatic send on merge.
- After this workflow is merged into the repository's default branch, it can be launched manually from **Actions → Personalized email test → Run workflow**.
- For a real email, set the **Send one real email** option to true. Every real run sends **exactly one email** to the preconfigured secret recipient; you cannot specify an arbitrary recipient in the workflow UI.
- Do not paste passwords, tokens, or other private information into workflow inputs, code, or PR comments. Preview logs show the greeting name and message body.

## One-time setup (Gmail)

1. In this repo, open **Settings → Secrets and variables → Actions → New repository secret**.
2. Set these three secrets:

   | Secret | Value |
   |---|---|
   | `SMTP_USERNAME` | Your Gmail sending address |
   | `SMTP_APP_PASSWORD` | Gmail **app password** for the sending account (not its normal login password) |
   | `EMAIL_TEST_RECIPIENT` | A single email address you control where the test should arrive |

3. Gmail app passwords generally require two-step verification and may be unavailable on some managed accounts. Create one through your Google Account's security settings. Don't commit credentials. If your Google account cannot create app passwords, adapt the transport to Gmail API OAuth or an email delivery API instead.
4. Merge the PR when you are satisfied with the preview workflow. GitHub only exposes the **Run workflow** control for this new `workflow_dispatch` workflow once its file exists on the default branch.

## How to test

1. **On the PR:** inspect the "Personalized email test" check. It should print a sample subject and message followed by `DRY RUN: no email sent`.
2. **After merge:** open [Actions](https://github.com/cavandonohoe/cavandonohoe.github.io/actions), select **Personalized email test**, and choose **Run workflow**.
3. Leave **send_email** unchecked to preview with a personalized name, or check it to send one real message.
4. Check the recipient's inbox and spam folder. A successful SMTP submission doesn't guarantee inbox placement.

The runner uses the Python standard library's `smtplib` with TLS on `smtp.gmail.com:587`. Gmail quotas and sending restrictions still apply. No personal laptop or paid service is required for ordinary low-volume usage, assuming the account supports app passwords.

## Local dry run

```bash
python scripts/send_test_email.py --name "Cavan"
```

**Do not** run the script with `--send` unless you intend to send one email and have configured the three required environment variables.
