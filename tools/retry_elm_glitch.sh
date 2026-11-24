#!/usr/bin/env bash

# Runs a command that compiles Elm, and runs it once more if it died of the
# compiler's runtime glitch:
#
#   thread blocked indefinitely in an MVar operation
#
# The Elm compiler is built with GHC's threaded runtime and uses every core.
# Now and then it deadlocks with this message, on code that compiles fine on the
# next run (CI run 45377559168 failed `make build` on be0d337a, while the tag
# run of the same commit passed). A real compile error never prints it, so only
# that message gets a second attempt; any other failure is reported at once.
#
# Pinning the compiler to one core (GHCRTS=-N1) would avoid the race too, but a
# clean compile then takes about 15 s instead of 5.5 s, on every build.
#
# Usage: tools/retry_elm_glitch.sh <command> [args...]

glitch='thread blocked indefinitely in an MVar operation'
log=$(mktemp)
trap 'rm -f "$log"' EXIT

for attempt in 1 2; do
  : > "$log"
  # both streams pass through unchanged and are also kept, to look for the message
  "$@" 2> >(tee -a "$log" >&2) | tee -a "$log"
  status=${PIPESTATUS[0]}
  wait $! 2>/dev/null # the stderr tee
  if [ "$status" -eq 0 ] || ! grep -qF "$glitch" "$log"; then
    exit "$status"
  fi
  if [ "$attempt" -eq 1 ]; then
    echo "retry_elm_glitch: the Elm compiler hit its MVar deadlock; running it once more" >&2
  fi
done
exit "$status"
