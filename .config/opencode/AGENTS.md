# Project linting and building
Do not run any linting tools directly line `npm eslit` or `go build`.
Always use project's `Makefile`, it's the only proper way to check validity of the code.
Makefiles may be nested in the corresponding directory in the project, so if you do not see needed target in the root one, try searching for other Makefiles

# Git commits
When the user asks to commit changes, always use the Task tool with `subagent_type="git"` to launch the specialized git agent. Do not handle git commits directly.

# Tmux sessions and process monitoring
When the user asks to check logs, monitor running processes, or run interactive/long-running commands, use the Task tool with `subagent_type="tmux"` to launch the specialized tmux agent. Do not handle tmux operations directly.

Common use cases:
- Checking logs for errors (especially in window index 1 where application logs typically run)
- Running long-running build/test commands that shouldn't block the main session
- Starting interactive commands that require user input
- Monitoring background processes

Example: When asked "are there any errors in the logs?", use the tmux agent to:
1. List available sessions
2. Capture pane content from window index 1 (common location for tail/log watching)
3. Search for error patterns in the captured output

# Behaviour
- Do not create summary documents until directly asked to do it
- Do not write a summary of what you've done until directly asked to do it
- Ensure that summary of changes will not be created after finishing work
