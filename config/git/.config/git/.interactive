#!/usr/bin/env sh

alias g='git'

# Functions to remove all tracked folders and directories from a local git
# repo's directory
git_clean_tracked_files() {
  if [ $# != 0 ]; then
    report_error "USAGE: git_clean_tracked_files"
    return 255
  fi

  git ls-files -z | xargs -0 rm

  return 0
}
git_clean_tracked_dir() {
  if [ $# != 0 ]; then
    report_error "USAGE: git_clean_tracked_dir"
    return 255
  fi

  git ls-tree --name-only -d -r -z HEAD \
    | sort -rz \
    | xargs -0 rmdir

  return 0
}
git_clean_tracked() {
  if [ $# != 1 ]; then
    report_error "USAGE: git_clean_tracked"
    return 255
  fi

  git_clean_tracked_files; git_clean_tracked_dir

  return 0
}

# Gives a total summary report of changes in a git repo contributed by the
# given user.
git_contributions() {
  if [ $# -gt 1 ]; then
    report_error "USAGE: git_contributions [GIT-USER-NAME]"
    return 255
  fi

  git log \
    --author="${1:-$(git config user.name)}" --pretty=tformat: --numstat \
    | awk '
{
  add += $1;
  subs += $2;
  loc += $1 - $2;
} END {
  printf("Added: %s lines, ", add);
  printf("Removed: %s lines, ", subs);
  printf("Total Added: %s lines\n", loc);
}'

  return 0
}

# Gives a line count report of the given commit
git_commit_stats() {
  if [ $# -gt 1 ]; then
    report_error "USAGE: git_commit_stats [COMMIT-ID]"
    return 255
  fi

  git show \
    "${1:-HEAD}" --numstat --pretty=tformat: \
    | awk '
{
  add += $1;
  subs += $2;
  loc += $1 - $2;
} END {
  printf("Added: %s lines, ", add);
  printf("Removed: %s lines, ", subs);
  printf("Total Added: %s lines\n", loc)
}'

  return 0
}

# Gives list of N largest blobs in the current working directory's repo
git_largest_blobs() {
  if [ $# -gt 1 ]; then
    report_error "USAGE: git_largest_blobs [COUNT]"
    return 255
  fi

  git rev-list --all --objects | \
    git cat-file --batch-check='%(objectname) %(objecttype) %(objectsize) %(rest)' | \
    grep blob | sort -k3nr | head -n "${1:-10}"

  return 0
}

