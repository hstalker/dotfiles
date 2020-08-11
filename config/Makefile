SOURCE_DIR ?= $(shell dirname $(abspath $(lastword $(MAKEFILE_LIST))))
TARGET_DIR ?= $(HOME)
PACKAGES ?= shells \
						xapplications \
						vim \
						emacs \
						ack \
						aspell \
						docker \
						vagrant \
						gnupg \
						pass \
						openssl \
						less \
						mpv \
						terminfo \
						wget \
						wine \
						xorg \
						gtk \
						kde \
						tmux \
						subversion \
						git \
						readline \
						python \
						ruby \
						javascript \
						dotnet \
						cpp \
						rust \
						haskell \
						go \
						java \
						themes/halogen
USEFUL_DIRS ?= $(TARGET_DIR)/.cache \
							 $(TARGET_DIR)/.local/bin \
							 $(TARGET_DIR)/.local/lib \
							 $(TARGET_DIR)/.local/include \
							 $(TARGET_DIR)/.local/share

MKDIR ?= mkdir -p
GIT ?= git
STOW ?= stow

all:
.PHONY: all create-directories link unlink

create-directories:
	@echo 'Making directories: $(USEFUL_DIRS) with "$(MKDIR)"...'
	@$(MKDIR) $(USEFUL_DIRS)

link: $(PACKAGES)
	@echo 'Linking $? with "$(STOW)"...'
	@echo 'Source directory: "$(SOURCE_DIR)"'
	@echo 'Target directory: "$(TARGET_DIR)"'
# We need to hack around stow being unable to handle nested package paths
	@for P in $(PACKAGES); do \
		if [ "." != "$$(dirname $$P)" ]; then \
			$(STOW) --no-folding \
				-d "$(SOURCE_DIR)/$$(dirname $$P)" \
				-t "$(TARGET_DIR)" \
				-vS "$$(basename $$P)"; \
		else \
			$(STOW) --no-folding \
				-d "$(SOURCE_DIR)" \
				-t "$(TARGET_DIR)" \
				-vS "$$P"; \
		fi; \
	done

unlink: $(PACKAGES)
	@echo 'Unlinking $? with "$(STOW)"...'
	@echo 'Source directory: $(SOURCE_DIR)'
	@echo 'Target directory: $(TARGET_DIR)'
# Same stow workaround as before
	@for P in $(PACKAGES); do \
		if [ "." != "$$(dirname $$P)" ]; then \
			$(STOW) --no-folding \
				-d "$(SOURCE_DIR)/$$(dirname $$P)" \
				-t "$(TARGET_DIR)" \
				-vD "$$(basename $$P)"; \
		else \
			$(STOW) --no-folding \
				-d "$(SOURCE_DIR)" \
				-t "$(TARGET_DIR)" \
				-vD "$$P"; \
		fi; \
	done

