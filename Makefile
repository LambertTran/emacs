EMACS_DIR := $(CURDIR)
HOME_EMACS_D := $(HOME)/.emacs.d
ZPROFILE := $(HOME)/.zprofile
BASH_ALIASES := $(HOME)/.bash_aliases

.PHONY: help setup symlink path-gopls alias-e agent-shell-deps

help:
	@echo "Manual one-time setup steps for this config (idempotent, safe to re-run):"
	@echo "  make setup            - run all steps below"
	@echo "  make symlink          - symlink ~/.emacs.d to this repo"
	@echo "  make path-gopls       - add \$$HOME/go/bin to PATH in ~/.zprofile (for gopls)"
	@echo "  make alias-e          - add an emacsclient-based 'e' shell function to ~/.bash_aliases"
	@echo "  make agent-shell-deps - npm install -g the claude-agent-acp bridge (for agent-shell)"
	@echo ""
	@echo "Not needed here (already automatic once Emacs starts against this config):"
	@echo "  - nerd-icons font install (features/fe-ui.el)"
	@echo "  - tree-sitter grammar install (features/fe-grammar.el)"
	@echo "  - gopls install itself (features/fe-golang.el)"

setup: symlink path-gopls alias-e agent-shell-deps

symlink:
	@if [ -L "$(HOME_EMACS_D)" ] && [ "$$(readlink "$(HOME_EMACS_D)")" = "$(EMACS_DIR)" ]; then \
		echo "~/.emacs.d already symlinked to $(EMACS_DIR)"; \
	else \
		if [ -e "$(HOME_EMACS_D)" ]; then \
			mv "$(HOME_EMACS_D)" "$(HOME_EMACS_D).bak.$$(date +%s)"; \
			echo "Backed up existing ~/.emacs.d"; \
		fi; \
		ln -s "$(EMACS_DIR)" "$(HOME_EMACS_D)"; \
		echo "Symlinked ~/.emacs.d -> $(EMACS_DIR)"; \
	fi

path-gopls:
	@if [ -f "$(ZPROFILE)" ] && grep -qF 'HOME/go/bin' "$(ZPROFILE)"; then \
		echo "~/.zprofile already has go/bin on PATH"; \
	else \
		printf '\nexport PATH="$$HOME/go/bin:$$PATH"\n' >> "$(ZPROFILE)"; \
		echo "Added go/bin to PATH in ~/.zprofile"; \
	fi

alias-e:
	@if [ -f "$(BASH_ALIASES)" ] && grep -q 'emacsclient -c -n -a ""' "$(BASH_ALIASES)"; then \
		echo "~/.bash_aliases already has the emacsclient e() function"; \
	else \
		printf '\ne() {\n    emacsclient -c -n -a "" "$$@"\n}\n' >> "$(BASH_ALIASES)"; \
		echo "Added e() function to ~/.bash_aliases"; \
	fi

agent-shell-deps:
	@if command -v claude-agent-acp >/dev/null 2>&1; then \
		echo "claude-agent-acp already installed"; \
	elif command -v npm >/dev/null 2>&1; then \
		npm install -g @agentclientprotocol/claude-agent-acp; \
	else \
		echo "npm not found; install Node.js first, then re-run 'make agent-shell-deps'"; \
		exit 1; \
	fi
