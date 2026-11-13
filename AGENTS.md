# AGENTS.md

Repository guidance for coding agents working on biocroxytest. Apply it across
the repository, together with higher-priority instructions and the user's current
request. Read any more-specific directory instructions before editing there.

## Start with the task

- Read the target issue and its current comments, inspect `git status`, and read
  the relevant implementation and tests. Confirm the expected behavior before
  editing. Preserve unrelated work already in the checkout.
- For issue work, use `issue-<number>` from `devel`, matching dar. Assignment of
  a responsible person triggers `.github/workflows/issue-branch.yml` to create
  the branch and a draft PR. Fetch and reuse the linked branch and draft. Check
  existing assignments and branches before creating duplicates. If automation
  fails, diagnose it and continue locally on the same branch name where possible.
- Use the existing suitable checkout while bootstrapping this workflow or working
  without an issue. Keep changes focused on the request and its dependencies.
- Use `rg` for targeted discovery. Read the files needed for the change rather
  than loading the whole repository or repeating completed investigations.
- Continue routine implementation, validation and review within the authorized
  scope. Ask the user only for a material unresolved decision or missing access.
- Communicate with the maintainer in Spanish. Follow the repository's English
  conventions for code, documentation, issues and pull requests.

## Repository map and roadmap

This R package uses roxygen2 roclets to generate testthat files. Current public
entry points are `longtests_roclet()` and `use_longtests()`. Preserve their supported
behavior while adding the unified ordinary/long test workflow.

| Location | Purpose |
| --- | --- |
| `R/longtests_rcolet.R` | Roclet registration and lifecycle |
| `R/utils_longtests_rcolet.R` | Block processing, output and cleanup |
| `R/made_by.R` | Generated-file ownership |
| `R/use_longtests.R`, `R/verify_longtests.R` | Setup and configuration checks |
| `tests/testthat/` | Regression tests and snapshots |
| `README.Rmd`, `vignettes/biocroxytest.Rmd` | User documentation |
| `DESCRIPTION`, `NAMESPACE`, `.Rbuildignore` | Package metadata and build inputs |
| `.github/workflows/check-bioc.yml` | R/Bioconductor matrix, checks, coverage and Pages |
| `.github/workflows/issue-branch.yml`, `.github/issue-branch.yml` | Assignment-triggered branches and draft PRs |
| `.github/dependabot.yml` | Weekly updates of pinned Actions |

Read current issues for acceptance criteria and status. The roadmap starts with
[#2–7](https://github.com/xec-cm/biocroxytest/milestone/1) for reliable unified
generation, then [#8–11](https://github.com/xec-cm/biocroxytest/milestone/2) for
named Bioconductor scenarios, [#12–14](https://github.com/xec-cm/biocroxytest/milestone/3)
for execution evidence and replay, and [#15](https://github.com/xec-cm/biocroxytest/milestone/4)
for failure reduction. Implement the requested slice. Proposed API names in prior
discussions remain provisional until the relevant issue establishes them.

## Implementation rules

- Follow the surrounding R style. Favor small, readable functions and existing
  dependencies. Explain any new dependency with a concrete need.
- Edit roxygen comments in `R/` and regenerate `man/` and `NAMESPACE`. Edit
  `README.Rmd` and regenerate `README.md`. Inspect generated diffs for changes
  unrelated to the task. Add a concise `NEWS.md` entry for user-visible changes.
- Preserve R text semantics, including multiline strings and non-syntactic names.
  Retain each block's origin when documentation is shared through `@rdname`.
- Validate all generated destinations and syntax before updating owned outputs.
  Exercise `clean = TRUE`, collisions, retries and obsolete-file cleanup when
  changing this lifecycle. Establish ownership before replacing or deleting a
  file. Preserve manual tests, custom runners and snapshots.
- Build a common generation path for ordinary and long tests. Generated tests
  must run with standard testthat from the installed consumer package. Each suite
  must resolve its helpers without relying on the sibling test directory.
- During roxytest migration, verify both package-loading orders and a single
  active writer per destination. Preserve filenames and test descriptions that
  identify existing snapshots. Review snapshot changes individually. Preserve
  upstream attribution and applicable license notices when reusing code.
- Keep new case IDs explicit and stable, separate from source lines, titles and
  suite assignment. Match evidence to the code revision, fixtures and execution
  context. A partly executed or skipped case cannot satisfy a required case.
- For S4, preserve method provenance and test dispatch through the public API.
  For delayed/HDF5 cases, verify the actual backend and use an independent
  expected result where possible. Keep fixture setup and downloads out of the
  generator's evaluation of test declarations.
- Use small deterministic fixtures. Restore temporary options, files and parallel
  settings. Configure BiocParallel RNG explicitly when it is involved. Preserve
  the experimental design and the same failure when reducing biological data.
- Follow Bioconductor versioning for the actual branch. Keep compatible additions
  in the v1.x line. Check the upstream Bioconductor branch before changing a
  version or publishing, since the GitHub checkout may lag its version bumps.

## Validation commands

Use `DESCRIPTION` for dependency requirements. Use the existing R environment
where possible and BiocManager for compatible Bioconductor dependencies. Install
missing tools in a task-local library when practical. Record R and relevant
package versions with validation results. The commands below use `testthat`,
`pkgload` and `rcmdcheck`. Vignette builds also require Pandoc to be discoverable
by rmarkdown, through `PATH` or `RSTUDIO_PANDOC`. An existing Quarto installation
may supply it.

The shell examples below run from the repository root on macOS/Linux. Use an
equivalent isolated directory on Windows. Keep the same shell for these commands.

Use a disposable copy that includes the working changes when running tests
or regenerating documentation. Keep it outside the R session's temporary directory.

```sh
biocroxytest_scratch="$(mktemp -d)"
cp -R . "$biocroxytest_scratch/package"
NOT_CRAN=true Rscript --vanilla -e \
  'testthat::test_local(commandArgs(TRUE)[1], stop_on_failure = TRUE)' \
  "$biocroxytest_scratch/package"
```

During iteration, add a targeted filter such as `filter = "made_by"` to
`test_local()`. For behavioral changes, run the full suite before final review.
For a bug, demonstrate that the regression test fails on the previous behavior
and passes with the fix. Test observable behavior through normal entry points.

Run a package build/check when changing R code, dependencies, exports, generated
documentation or packaging. The command includes vignette building and uses a
check directory outside the R session's temporary directory.

```sh
Rscript --vanilla -e \
  'rcmdcheck::rcmdcheck(commandArgs(TRUE)[1], args = "--no-manual", build_args = "--no-manual", check_dir = commandArgs(TRUE)[2], error_on = "warning")' \
  "$biocroxytest_scratch/package" "$biocroxytest_scratch/check"
git diff --check
```

- Refresh the disposable copy after edits. Keep useful failure logs until the
  task is resolved. Run roxygen/documentation generation in the copy when its
  side effects could alter unrelated files, then inspect the intended outputs.
- For generator, runner or migration changes, also construct a minimal consumer
  package with a different name. Generate and build it, then check the tarball using
  `R CMD check` and `R CMD check --test-dir=longtests` in separate output
  directories. Include a deliberately failing test in each suite and verify the
  corresponding command detects it. Remove the deliberate defect for the final
  passing checks. Verify the generated tests run in a process whose library paths
  make biocroxytest and roxytest unavailable, with the consumer's declared runtime
  dependencies installed.
- For migration, compare test bodies, execution results and snapshot identities.
  A matching generated file alone does not establish execution compatibility.
- Run relevant BiocCheck and platform/CI checks for release-affecting changes.
  Use the workflow's dynamically resolved R/Bioconductor pair when reproducing CI.
- For instruction-only or prose changes, validate the document, referenced paths
  and changed commands. Check build exclusion when adding root-level files.
- Report check commands, outcomes, skips and limitations accurately. Resolve new
  failures. If required validation is unavailable, continue independent work and
  report the specific gap. A limited check must be identified as such.

For Actions changes, run `actionlint` when available, validate the configuration
YAML and exercise changed resolver/condition logic locally. Keep Actions pinned
to verified commits. CI checks pull requests with read permissions. Publishing
uses a separate job after all matrix checks pass on a push to `devel`.

Assignment automation becomes active when its workflow and configuration reach
the default branch. Draft PR creation requires GitHub's repository setting
allowing Actions to create pull requests. If the branch exists but its draft is
missing, recover the draft from that branch after inspecting the workflow logs.
Checks on the automatically created draft may need a maintainer to approve the
workflow run. Verify their status again after the contributor's first push.
Pages publication requires the repository's Pages source to be GitHub Actions.
Check these settings when activating workflows and distinguish local validation
from an observed GitHub run. Keep draft PRs pending until specialist approval.

## Mandatory specialist review

The maintainer requires implementation followed by independent multi-agent
review. This applies to every bug fix or feature, and to changes in tests,
documentation, configuration or these instructions.

The coordinating agent must obtain an explicit final approval from each of four
distinct specialist subagents. An agent that implemented the change cannot
approve its own work. Reviewers inspect and propose corrections without editing
the reviewed files. The coordinator integrates feedback.

| Reviewer | Required focus |
| --- | --- |
| Code and architecture | Correctness, failure paths, file safety, maintainability and compatibility |
| Bioconductor ecosystem | R packaging, roclets, S4, BBS/longtests, dependency and version rules relevant to the change |
| Testing and regression | Reproduction, meaningful expectations, actual execution, snapshots and validation gaps |
| Developer experience and software design | Ease of use, scope, useful improvements and reuse of established tools |

Add another specialist when the task requires expertise beyond these roles. All
reviewers assigned to the change must approve it. Give each role a fresh review
context containing the task and evidence, with other reviewers' verdicts withheld
until its independent first assessment. Run reviewers in batches when concurrency
is limited. Review-only tasks
return their verdict directly and do not start another review panel.

1. Implement the requested behavior and run the relevant checks.
2. Give reviewers the issue/request, acceptance criteria, files and complete diff
   including untracked additions, validation results and known limitations.
   Identify the reviewed snapshot by commit or by base commit plus a digest of
   the diff and untracked contents. Ask each reviewer to inspect independently
   before sharing the other reviewers' conclusions.
3. Each reviewer returns `APPROVE` or `CHANGES_REQUIRED`, the reviewed snapshot,
   concrete findings with file/line references, evidence inspected, and optional
   improvements. Findings must explain their effect on the requested behavior.
   Additional checks should target an evidence gap rather than repeat the full
   suite independently in every role.
4. Fix blocking findings, rerun affected checks and request another review.
   Resolve disagreements with evidence. The reviewer who raised a finding must
   explicitly accept its resolution or withdrawal. Record optional suggestions
   with a brief adoption or deferral reason and keep them within the task scope.
5. After the final edit, obtain every reviewer's explicit approval of that final
   snapshot. Reviewers may check only the intervening diff and confirm that
   earlier findings remain resolved. Previous approvals do not automatically
   cover subsequent changes.

Unanimous approval and completed required validation are prerequisites to
declaring the work finished, marking an issue/project item done, closing an issue
or merging. Silence, timeout and a majority vote are insufficient. If subagents
or a required reviewer are unavailable, keep the work pending review and report
the gap. Do not simulate independent approvals or remove an unresolved reviewer
to pass this requirement.

Approval establishes technical readiness. Perform remote updates, commits,
publishing and merges within the user's authorized workflow. Keep GitHub issues
and PR descriptions short. Cite the relevant issue and summarize behavior and
validation instead of repeating the investigation.

## Completion and handoff

Before the final response, inspect the final diff and new files, check that the
requested behavior and relevant generated outputs are covered, and verify that
all required approvals refer to the final snapshot. Keep a compact review table in the
task summary or PR with reviewer identities, role, verdict and snapshot. State
what changed, what passed, and any material remaining limitation.

When handing off unfinished work, include the issue, branch/current changes,
checks already run, unresolved findings and review status. Reuse that evidence
while verifying that it still applies to the current files.

Keep this file concise and current. Link detailed workflows when they exist,
and add nested `AGENTS.md` files only for genuinely different directory rules.
The review requirement may be changed by an explicit maintainer instruction.

Reference guidance is available in the [AGENTS.md format](https://agents.md/),
[Codex instruction guide](https://learn.chatgpt.com/docs/agent-configuration/agents-md)
and [Bioconductor versioning guide](https://contributions.bioconductor.org/versionnum.html).
