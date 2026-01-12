# Environment Context
This Claude Code session runs inside a tmux session on a VPS server. Most commands and inputs come from voice input on a mobile phone (Termux on Android), so expect:
- Natural language phrasing instead of exact technical terms
- Possible transcription errors or typos in commands
- Conversational style requests that need interpretation
- Be forgiving with command interpretation and clarify if truly ambiguous

# General Guidelines

- ALWAYS embrace Makefiles as the primary way to execute any project commands. Before running any command, discover all Makefiles in the project to understand available targets. Never bypass Makefiles by running commands directly.

- Only commit when directly asked (e.g., /commit or explicit request). Never run blindly `git add .` as user may work in parallel. Use the same commit style as exists in the project. If debugging something in multiple prompts, use `--amend --no-edit` to keep commits complete. Keep messages short and do not mention Claude Code

- always keep helper function under main / public ones, stuff is moved to helper function to reduce amount of stupid code, not to show it off

# Makefile Usage

**CRITICAL: Makefiles are the ONLY way to run project commands. ALL operations (linting, building, testing, deployment) MUST go through Makefiles.**

## Discovery Process
Before running ANY command in a project:
1. Search for ALL Makefiles in the project: `find . -name "Makefile" -o -name "*.mk"`
2. Read all discovered Makefiles to understand available targets
3. Makefiles may be nested in subdirectories (frontend/, backend/, deploy/, etc.)
4. Never assume a target doesn't exist - always search first

## Execution Rules
- **NEVER** run commands directly like `npm run build`, `go build`, `eslint`, `pytest`, etc.
- **ALWAYS** use `make <target>` for any project operation
- Use `make -C dir target` instead of `cd dir && make target` to run targets in subdirectories
- Example: `make -C backend test` NOT `cd backend && make test`

## When Make Targets Fail
If a make target fails:
1. **DO NOT** copy commands from the Makefile and run them directly
2. **DO** investigate the root cause of the failure
3. **DO** fix the underlying issue (code errors, missing dependencies, etc.)
4. **DO** re-run the make target after fixing
5. Only if the Makefile itself is broken, fix the Makefile

## Adding New Targets
If you need to run a command that could be useful in the future:
1. Add it as a new target in the appropriate Makefile
2. Document what the target does
3. Follow existing patterns in the Makefile
4. Consider dependencies between targets (use target prerequisites)

## Common Make Patterns
- `make` or `make help` - Show available targets
- `make build` - Build the project
- `make test` - Run tests
- `make lint` - Run linters
- `make clean` - Clean build artifacts
- `make dev` - Start development server
- `make deploy` - Deploy the project

## Environment Variables
- Makefiles often populate required `.env` files automatically
- Never manually edit `.env` files if the Makefile manages them
- If environment setup is needed, it should be in a `make setup` or similar target

# Git commits
When the user asks to commit changes, always use the Task tool with `subagent_type="git"` to launch the specialized git agent. Do not handle git commits directly.

The git agent should only commit changes when explicitly asked by the user. Do not automatically commit other changes or be proactive about committing - wait for explicit user instruction to commit.

# Deployment infrastructure
When the user asks to set up deployment, create deployment files, or configure CD/CI for Python, Golang, or React projects, use the Task tool with `subagent_type="deploy"` to launch the specialized deployment agent. Do not handle deployment setup directly.

Common use cases:
- Setting up initial deployment infrastructure (deploy/ directory, systemd services, Makefiles)
- Creating deployment configuration for new projects
- Upgrading existing deployment to newer vps-setup versions
- Adding staging/production environments
- Configuring Caddy reverse proxy for web services
- Migrating deployment patterns between projects

Example: When asked "set up deployment for this project", use the deploy agent to:
1. Analyze project to detect language and build process
2. Fetch templates from vps-setup repository (with version tracking)
3. Generate deploy/ directory with all necessary files
4. Provide clear documentation and next steps

# Go development patterns
When planning new features, adding handlers, services, or any significant code changes in Go projects, use the Task tool with `subagent_type="knowledge"` to fetch relevant patterns and best practices from the knowledge base.

Common use cases:
- Adding new HTTP handlers or routes
- Implementing services (interface-based or channel-based)
- Working with SQLC queries and transactions
- Adding HTMX interactivity
- Telegram bot integration

Example: When asked "add a new endpoint for user profiles", use the knowledge agent to:
1. Fetch relevant patterns for handlers and routing
2. Apply the patterns to implement the feature

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

# Responding to questions
When the user asks questions like "why", "what", "how", or any other informational question, treat it as a request for explanation or information. Do not interpret these questions as indirect requests to modify code or make changes. The user wants to understand reasons, decisions, and context - simply provide clear answers and explanations without making any modifications unless explicitly requested.

# Sending notifications via ntfy.sh
The ntfy topic is stored in the `$NTFY_TOPIC` environment variable.

**IMPORTANT:** Only send notifications when explicitly asked by the user (e.g., "send a notification to my phone"). Automatic notifications are handled by configured hooks (Stop, PermissionRequest). Do not send notifications proactively after completing tasks.

How to send when asked:
```bash
curl -H "Title: Your Title" -H "Priority: default" -H "Tags: bell" -H "Click: android-app://com.termux" -d "Message content" ntfy.sh/$NTFY_TOPIC
```

**Always include the Click header with `android-app://com.termux`** to allow tapping notifications to open Termux directly.

Available priorities: `min`, `low`, `default`, `high`, `urgent`
Common tags: `white_check_mark`, `warning`, `fire`, `rocket`, `bell`, `computer`, `tada`
