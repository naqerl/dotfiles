---
name: commit
description: Commit current changes by analyzing git status and grouping changes into logical commits
---

Analyze all current git changes and group them into logically connected classes.

When the user asks to commit changes, use the Task tool with `subagent_type="git"` to launch the specialized git agent.

In the end all changes should be classified and committed.
