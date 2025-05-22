;;; Compiled snippets and support files for `yaml-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'yaml-mode
		     '(("debian-system"
			"---\n- name: \"Setup Debian\"\n  hosts: all\n  vars_files:\n    - vars/common.yml\n  tasks:\n    - name: \"Install packages\"\n      become: true\n      become_user: root\n      ansible.builtin.package:\n        update_cache: true\n        name:\n          - podman\n          - containers-storage\n          - cron\n\n    - name: \"Extend bash profile\"\n      ansible.builtin.blockinfile:\n        state: present\n        path: \"{{ ansible_env.HOME }}/.bashrc\"\n        block: |\n          alias p='podman'\n          alias pl=\"p logs --tail=50\"\n          alias plf=\"pl -f\"\n          df -h\n          p ps -a\n"
			"debian-setup" nil nil
			((yas-indent-line 'fixed)
			 (yas-wrap-around-region 'nil))
			"/home/user/.emacs.d/snippets/yaml-mode/debian-system"
			nil nil)))


;;; Do not edit! File generated at Thu May 22 14:46:02 2025
