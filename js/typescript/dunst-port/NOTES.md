# Dunst Notes

## General

- Reference commit: `9e29406`

```
───────────────────────────────────────────────────────────────────────────────
Language                 Files     Lines     Code  Comments   Blanks Complexity
───────────────────────────────────────────────────────────────────────────────
C                           29     10118     7704       608     1806       1045
C Header                    18      2529     1578       634      317        187
SVG                          5       197      191         3        3          0
Shell                        5       483      301       111       71         18
Markdown                     3       321      251         0       70          0
YAML                         2       109      103         0        6          0
Makefile                     2       242      181        15       46          3
Autoconf                     2        17       14         0        3          0
License                      1        29       23         0        6          0
gitignore                    1        17       15         0        2          0
───────────────────────────────────────────────────────────────────────────────
Total                       68     14062    10361      1371     2330       1253
───────────────────────────────────────────────────────────────────────────────
```

## Infrastructure / Tooling

These commands are working:

- `make test`

## References

- GQueue: https://developer.gnome.org/glib/stable/glib-Double-ended-Queues.html

## Unstructured

- The `src/queues.c` file uses `GQueue` from `<glib>`, which is a double-linked queue
- The entry point is in `src/dunst.c#dunst_main`

## Initial Template

```
# Areas

- Levels
    - high: architecture, modules, flows
    - low: details, coding patterns, flows
    - periferial: build system, scripting, flows
    - external: dependencies
- Social
    - Contributors
    - Issues: check the "Good first issue", "Easy", etc.
    - Pull Requests
    - Stack Overflow, Slack, Mailing
- Documentation Sources
- Misc
    - Project length
    - Updates frequency
    - Languages, LoC, etc.
- Understanding
    - Reading without notes
    - Memorizing
    - Reading with notes
    - Processing notes
    - Making updates
    - Setting up infrastructure
- Performance / Motivation
    - Schedule
    - Use breaks and rest periods in advantage: revisit topics after a few days
    - Set goals
    - Set scope
    - Contribute
    - Alternate difficulty:
        - Difficult: answers, flows, fn definitions, summaries
        - Easy: doubts, reading, stats, setup
- Keep the todo (remaining, in progress) up to date
    - Add reasoning if necessary

# Structure

Important files / directories:

- Glossary
- Flows
- Stacks: Easy to build but bring less value than flows
- Answers: Completed doubts from `Todo`
- Reviews of issues / prs
- Resources: links, documentation, etc. Include vendors docs.
- Infrastructure / Tooling
- Source: Includes list with explanation of functions, classes, arguments, etc.
- Tests: Can be new tests (verified), the structure, or comments about specific tests
- Unstructured: Some notes that may be moved or removed in the future
- Todo:
    - Include once-time-only and periodic tasks
    - Include goals
    - Include questions (empty list is a bad sign)
    - Import doubts from [the generic list](./doubts.md) if necessary
- Setup: scripts, Dockerfiles, etc.
    - Add a README.md if necessary
    - Add custom installers (manual and automatic) with instructions
    - If any file needs to be added manually, also add instructions and links
- Memorization: tests, questions
- Examples: Can be comments about existing examples or new examples (they need to be run and verified)
    - Use the Tests document when possible which are a better type of example
    (when tests passing) than documentation examples

Ideas

- Summaries: Strictly derived from notes
- Context: Reasoning for the project or the internal parts

Workflow

- Start in two files (todo.md and REAMDE.md), then split into multiple files / directories
- Never let the todo list / doubts list to grow too much or be empty
- Never let the Unstructured Content grow too much (can be empty)
```
