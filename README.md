# tatr - A Simple Task Tracker

A lightweight task tracker written inspired by [tsoding](https://github.com/tsoding).

## Quick Start

```bash
cabal build
cabal install --installdir .
./tatr --help
```

## Usage Example

```bash
# Create a new task
tatr new "Implement feature X" -p 20 -t feature

# List all open tasks
tatr ls

# Check summary
tatr summary

# Close a task by editing the file
# Then view closed tasks
tatr ls --closed
```

## Tasks Storage

Tasks are stored as markdown files, making them easy to read and version control.

The directory name of the markdown file is timestamp in `YYYYMMDD-HHMMSS` format

```
.
└── 20260516-142605/
    └── TASK.md
```

Each TASK.md file contains following header:

```markdown
# Task Title

- STATUS: OPEN
- PRIORITY: 30
- TAGS: work,urgent
```

You can put anything after the header.

To close a task, simply edit the TASK.md file and change `STATUS: OPEN` to `STATUS: CLOSED`.
