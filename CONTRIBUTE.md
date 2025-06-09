# Contribute to baksnapper

This document is a guide on what you should think about when contributing to baksnapper.

## Cheatsheet
- Each PR should have an unreleased entry file in `CHANGELOG.d`.
- Try to do atomic commits.
- Use rebase when keeping PRs in-sync with upstream.
- Use `changelog:`, `build:` or `test:` prefix tag when applicable.
- Add `[#<id>]` to the end of the subject if commit is associated with an issue.
- Add a test if your PR is not covered by one already.
- Short subject for the commit messag.
- Detailed explanation of the reason behind the commit in the body.

## Changelog

Each pull request should have a file in `CHANGELOG.d` containing a short summary what your pull request does.  The file-naming scheme follows that of [denote](https://protesilaos.com/emacs/denote#h:4e9c7512-84dc-4dfb-9fa9-e15d51178e5d):
```
<timestamp>--<title>__<tag>.md
```

- `<timestamp>` follows the ISO 8601 standard: `<date>T<time>`.
- `<title>` description of the unreleased entry.
- `<tag>` is one of the tags Keep a changelog supports; `added`, `changed`, `deprecated`, `removed`, `fixed` or `security`.

The content of the file should be as if adding as an entry to an empty unreleased section. I.e. first the type of change, `### Added` or `### Changed` etc.  Then followed by an item giving a summary of what have changed.  The summary is user facing so phrase it as it can be understood by them. In other words avoid implementation details — these should be in the commit message.

If the entry is associated with an issue add a link to that issue at the end of entry.  This makes things easier to figure out what change belongs to what issue and what commit(s) introduced it.

E.g. The unreleased entry for migrating to a Keep a changelog:
```
20250606T144102--migrate-changelog-to-keep-a-changelog__changed.md
```
And its content:
```markdown
### Changed
- Migrate to a conflictless Keep A Changelog. [#40](https://github.com/plattfot/baksnapper/issues/40)
```

When a release is cut all these are combined into a release section in the `CHANGELOG.md` and all files are deleted.

### Creating an unreleased entry file

If you are using Emacs, you can use my [denote-keepachangelog](https://sr.ht/~plattfot/denote-keepachangelog/) package and call `denote-keepachangelog-create-unreleased-note`.  It will take care of generating the timestamp, prompt for a title and tag.

Otherwise you'll need to manually generate the filename.  You can use `date +%Y%m%dT%H%M%S` to get the timestamp, which is the most important part to get right.  The title and tag are to get a quick overview of what unreleased entries there are in `CHANGELOG.d` without needing to open each one.

## Commits

To help troubleshoot and ease of maintenance the commits, they should just contain one logical change each.  The logical change should be the smallest possible that make sense.  An atomic commit.  These can be hard to define what they encompass, but easiest rule of thumb is if you need to include "and" in the subject of the commit then it does not contain _one_ change and must be split.

This helps both when reviewing the code for a PR and when needing to hunt down when a bug got introduced. As you can use `git bisect` to help you narrow it down.  If a commit contain multiple changes it can be harder to figure out what of those changes introduced the bug without further digging.

### Suffix tag

Add `[#<id>]` to the end of the subject if commit is associated with an issue.  This will help with quickly find what commits where involved with addressing an issue and links the commits with the content of the `CHANGELOG.md`.

### Prefix tag

Use `changelog:` as a prefix tag when adding unreleased entries to `CHANGELOG.d` or when merging them into a release section in `CHANGELOG.md`.

Use `build:` as a prefix tag when making changes to any build system related files; `Makefile`, `build-aux/`, `guix.scm`, `meson.build`, `meson_options.txt`.

Use `test:` as a prefix tag when making changes to any of the test files in `tests/`.  If you are adding a test to `meson.build` it is considered a build system change and should use `build:`.

### Message

The subject of a commit message should be short, around 72 char not including the suffix tag.  This to avoid `git log` truncating or wrapping the commit messages making it harder to read.

The body of the message should explain the reason behind implementing it that way.  Avoid explaining what the code does, for that you can just read the code.  And if the code is too complicated that it needs an explanation then it is probably not the right way of solving the problem.  Not everything needs an explanation, e.g. adding a simple test or bumping a version only requires a subject.

But more complex changes like adding a feature or fixing a bug it is good to have text that explains the reasoning of why it was implemented at a certain way.  This is gold when troubleshooting or optimizing.  For example there might be an obvious looking way of implementing one feature, but there is a gotcha doing it that way that will cause a lot of headaches.  Mentioning that in the commit when the feature was added could help future maintainers in avoid stepping in that trap when they scratch their head trying to figure out why it wasn't written in that obvious looking way.

Avoid ambiguous short terms in the subject, e.g. "Fix bug", "Add test", "Update changelog", "Add feedback from PR".  These are not helpful when viewing the history, and you need to view each of these commit to actually see what they did.

For "Fix bug", use `fix:` prefix and then a short summary what actually changed.

For "Add test", use `test:` prefix and then the name of the test added.

For "Update changelog", use `changelog:` prefix and then what actually changed.  If an unreleased entry use a summary of the title or if a release section use the version.

For "Add feedback from PR".  Squash these into the commits they affect and force push them to the branch that is involved with the PR.

### Pull Request workflow

The workflow for baksnapper is to rebase PRs if you need to sync-up with the target branch and merge when it is ready to be merge into the master branch.  Feedback should be squashed into the commits they address and force pushed to the PR branch.  That way the history is kept clean from noise.

**NOTE:** If you are not comfortable rebasing and/or squashing commits, that is fine.  Just mention that in the pull request and the person reviewing will take care of that for you when merging the changes.
