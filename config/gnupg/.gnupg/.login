#!/usr/bin/env sh

# Keeping this here for posterity, but we don't want to change the default
# directory, because this causes issues with GPG in general. For example, when
# you try to have a GPG agent managed by Systemd and/or socket activated,
# this won't work easily (it's not as straight-forward as just
# injecting GNUPGHOME into DBus). Generally it's just not worth messing with
# something as critical as encryption infra.
#assign_export GNUPGHOME "${XDG_CONFIG_HOME:-$HOME/.config}/gnupg"

