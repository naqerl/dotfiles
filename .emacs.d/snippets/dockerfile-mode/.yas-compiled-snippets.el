;;; Compiled snippets and support files for `dockerfile-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'dockerfile-mode
		     '(("python"
			"## ------------------------------- Builder Stage ------------------------------ ##\nFROM python:3.13-bookworm AS builder\n\nRUN apt-get update && apt-get install --no-install-recommends -y \\\n        build-essential && \\\n    apt-get clean && rm -rf /var/lib/apt/lists/*\n\nADD https://astral.sh/uv/install.sh /install.sh\nRUN chmod -R 655 /install.sh && /install.sh && rm /install.sh\n\nENV PATH=\"/root/.local/bin:$PATH\"\n\nWORKDIR /app\n\nCOPY ./pyproject.toml .\n\nRUN uv sync\n\n## ------------------------------- Production Stage ------------------------------ ##\nFROM python:3.13-slim-bookworm AS production\n\nWORKDIR /app\n\nRUN apt-get update -y \\\n    && apt-get install -y make \\\n    && apt-get purge -y --auto-remove \\\n    && rm -rf /var/lib/apt/lists/*\n\nCOPY /$1 $1\nCOPY __main__.py .\nCOPY Makefile .\nCOPY --from=builder /app/.venv .venv\n\nENTRYPOINT [\"make\"]"
			"python-containerfile" nil nil
			((yas-indent-line 'fixed)
			 (yas-wrap-around-region 'nil))
			"/home/user/.emacs.d/snippets/dockerfile-mode/python"
			nil nil)))


;;; Do not edit! File generated at Thu May 22 21:23:38 2025
