export const NotifyPlugin = async ({ project, directory, $ }) => {
  // Extract project name from directory or use basename
  const getProjectName = () => {
    if (project?.name) return project.name;
    const parts = directory.split('/');
    return parts[parts.length - 1] || 'opencode';
  };

  const projectName = getProjectName();
  
  // Track which permissions we've already notified about
  const notifiedPermissions = new Set();

  return {
    event: async ({ event }) => {
      try {
        // Session completed/idle - assistant finished work
        if (event.type === 'session.idle') {
          // Clear permission tracking when session becomes idle
          notifiedPermissions.clear();
          await $`notify-send "OpenCode - ${projectName}" "Session completed! Ready for your next request." -u normal -t 5000`;
        }
        
        // Session error - something went wrong
        if (event.type === 'session.error') {
          await $`notify-send "OpenCode - ${projectName}" "Session error occurred. Check the terminal for details." -u critical -t 10000`;
        }
        
        // Permission required - only notify once per unique permission
        if (event.type === 'permission.updated' && event.data?.status === 'pending') {
          const permissionId = event.data?.id;
          if (permissionId && !notifiedPermissions.has(permissionId)) {
            notifiedPermissions.add(permissionId);
            await $`notify-send "OpenCode - ${projectName}" "Permission required - check the terminal" -u normal -t 8000`;
          }
        }
      } catch (error) {
        // Silently fail if notify-send is not available or errors
        console.error('Notification plugin error:', error.message);
      }
    },
  };
};
