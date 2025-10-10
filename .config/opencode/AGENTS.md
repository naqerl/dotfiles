# Global Instructions  
  
When you complete any task or job, always run:  
`notify-send "OpenCode" "Task completed!" --expire-time=0`  
  
This ensures I get notified when work is finished.

Also commit your work after each task or job. Format should be the following

> part of the system: actual change

Example:
1. backend: implemented feature A
2. docs: actualized README
3. repo: make target for X

There could be other job done, so analyze git worktree and add only edited by you parts, do not do `git add .` blindly
