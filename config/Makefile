NO_FOLDING ?= 1
ifneq ($(strip $(NO_FOLDING)),0)
	NO_FOLDING_FLAG=--no-folding
endif
VERBOSE ?= 0
ifneq ($(strip$(VERBOSE)),0)
	VERBOSE_FLAG=-v
endif
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
	dircolors \
	redshift \
	mozc \
	weechat \
	zoom \
	sway \
	themes/halogen
USEFUL_DIRS ?= $(TARGET_DIR)/.cache \
	$(TARGET_DIR)/.local/bin \
	$(TARGET_DIR)/.local/lib \
	$(TARGET_DIR)/.local/include \
	$(TARGET_DIR)/.local/share

MKDIR ?= mkdir -p
STOW ?= stow

all:

create-directories:
	@echo 'Making directories: $(USEFUL_DIRS) with "$(MKDIR)"...'
	@$(MKDIR) $(USEFUL_DIRS)

# STOW_TEMPLATE :: TGT_NAME, DESCRIPTIVE_STRING, STOW_FLAG
define STOW_TEMPLATE =
$(1): $$(PACKAGES)
	@echo '$(2) $$? with "$$(STOW)"...'
	@echo 'Source directory: "$$(SOURCE_DIR)"'
	@echo 'Target directory: "$$(TARGET_DIR)"'
# We need to hack around stow being unable to handle nested package paths
	@for P in $$(PACKAGES); do \
		if [ "." != "$$$$(dirname $$$$P)" ]; then \
			$$(STOW) $$(NO_FOLDING_FLAG) \
				-d "$$(SOURCE_DIR)/$$$$(dirname $$$$P)" \
				-t "$$(TARGET_DIR)" \
				$$(VERBOSE_FLAG) $(3) "$$$$(basename $$$$P)"; \
		else \
			$$(STOW) $$(NO_FOLDING_FLAG) \
				-d "$$(SOURCE_DIR)" \
				-t "$$(TARGET_DIR)" \
				$$(VERBOSE_FLAG) $(3) "$$$$P"; \
		fi; \
	done
AUTO_GENERATED_TARGETS+=$(1)
endef

$(eval $(call STOW_TEMPLATE,link,Linking,-S))
$(eval $(call STOW_TEMPLATE,unlink,Unlinking,-D))
$(eval $(call STOW_TEMPLATE,relink,Relinking,-R))

.PHONY: all create-directories $(AUTO_GENERATED_TARGETS)

