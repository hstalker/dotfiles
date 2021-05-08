#!/usr/bin/env sh

assign_export GPG_TTY "$(tty)"
if has_systemd; then
  # Manually force starting of the service because using gpg-connect-agent
  # seems to not support triggering socket activated Systemd managed
  # installations of GPG. You probably want to make sure you enable these the
  # services/sockets when you install the dotfiles.
  silence systemctl --user start gpg-agent.service
else
  # We don't have Systemd, so shell management is a-ok
  silence_output gpg-connect-agent updatestartuptty /bye
fi
assign_export SSH_AUTH_SOCK "$(gpgconf --list-dirs agent-ssh-socket)"
