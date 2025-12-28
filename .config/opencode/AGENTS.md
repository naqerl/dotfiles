# Project linting and building
Do not run any linting tools directly line `npm eslit` or `go build`.
Always use project's `Makefile`, it's the only proper way to check validity of the code.
Makefiles may be nested in the corresponding directory in the project, so if you do not see needed target in the root one, try searching for other Makefiles

# Git commits
When the user asks to commit changes, always use the Task tool with `subagent_type="git"` to launch the specialized git agent. Do not handle git commits directly.

# Behaviour
- Do not create summary documents until directly asked to do it
- Do not write a summary of what you've done until directly asked to do it
