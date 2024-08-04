import os
import json

base_paths = (
    "/usr/share/applications/",
    os.path.expanduser("~/.local/share/applications/"),
)

for path in base_paths:
    files = os.listdir(path)
    for file in files:
        _, app = os.path.split(file)
        print(json.dumps({"app": app, "path": os.path.join(path, file)}))
