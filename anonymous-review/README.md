# Anonymous review site

This directory builds a double-anonymous version of the Quarto book from the
same source files used by the public site.

## Build

From PowerShell:

```powershell
.\anonymous-review\build.ps1
```

The rendered site is written to `docs/_book-anonymous/`. The build fails if
the HTML or search index contains the author's name, public GitHub username, or
personal domain.

## Deploy

After a successful build, deploy the prebuilt static directory with Netlify:

```powershell
netlify deploy --prod --no-build --dir docs/_book-anonymous
```

The deployed site includes `noindex` directives in HTML, `robots.txt`, and
Netlify response headers. Its source-code links point to the anonymous 4open
archive rather than the public GitHub repository.

These controls prevent direct identity leakage from the deployed files, but
they cannot guarantee double anonymity when a substantially similar public
version is already online. For a strict review process, consider temporarily
unpublishing or access-restricting the public site during review.
