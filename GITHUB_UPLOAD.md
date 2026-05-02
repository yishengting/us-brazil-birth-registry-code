# GitHub Upload Instructions

This file is for the project owner. It should not contain passwords, tokens, or personal credentials.

## 1. Create A New GitHub Account

Create the account manually at:

```text
https://github.com/signup
```

Use an email address and account name appropriate for journal review. Complete any email verification, CAPTCHA, and two-factor authentication steps yourself.

## 2. Create An Empty Repository

After signing in to GitHub, create an empty repository named:

```text
us-brazil-birth-registry-code
```

Do not add a README, .gitignore, or license on the GitHub website because this local repository already contains those files.

Recommended visibility for journal submission:

- Use a public repository if the journal accepts public code disclosure during review.
- Use a private repository plus a blinded reviewer access mechanism if double-anonymized review is required by the journal.

## 3. Push This Local Repository

From this directory:

```sh
git remote add origin https://github.com/<NEW_GITHUB_USERNAME>/us-brazil-birth-registry-code.git
git push -u origin main
```

Replace `<NEW_GITHUB_USERNAME>` with the new account name.

If GitHub asks for authentication on the command line, use GitHub's current recommended authentication method, such as a personal access token or browser-based credential helper. Do not paste tokens into this file.

## 4. Before Journal Submission

Confirm on GitHub that the repository does not contain:

- manuscript files;
- submission packages;
- generated tables or figures;
- raw or harmonised individual-level data;
- DOCX, RTF, PDF, PNG, TIFF, ZIP, Parquet, or HTML outputs.

Then insert the final public URL or blinded reviewer link into the manuscript code availability statement.

