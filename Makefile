SOURCE_DIR ?= $(shell dirname $(abspath $(lastword $(MAKEFILE_LIST))))
TARGET_DIR ?= $(HOME)
PACKAGES ?= vim \
					 emacs \
					 tmux \
					 git \
					 bash \
					 dircolors \
					 xorg \
					 gpg \
					 ssh \
					 irb \
					 npm \
					 ack \
					 readline
USEFUL_DIRS ?= $(TARGET_DIR)/projects \
							 $(TARGET_DIR)/.cache \
							 $(TARGET_DIR)/.local/bin \
							 $(TARGET_DIR)/.local/share

MKDIR ?= mkdir -p
GIT ?= git
STOW ?= stow

all: create-directories update-submodules link
.PHONY: all create-directories update-submodules link unlink

create-directories:
	@echo 'Making directories: $(USEFUL_DIRS) with "$(MKDIR)"...'
	@$(MKDIR) $(USEFUL_DIRS)

update-submodules:
	@echo 'Fetching and updating submodules with "$(GIT)"...'
	@$(GIT) submodule update --init --recursive

link: $(PACKAGES)
	@echo 'Linking $? with "$(STOW)"...'
	@echo 'Source directory: $(SOURCE_DIR)'
	@echo 'Target directory: $(TARGET_DIR)'
	@$(STOW) --no-folding -d "$(SOURCE_DIR)" -t $(TARGET_DIR) -S $(PACKAGES)

unlink: $(PACKAGES)
	@echo 'Unlinking $? with "$(STOW)"...'
	@echo 'Source directory: $(SOURCE_DIR)'
	@echo 'Target directory: $(TARGET_DIR)'
	@$(STOW) --no-folding -d "$(SOURCE_DIR)" -t $(TARGET_DIR) -D $(PACKAGES)

