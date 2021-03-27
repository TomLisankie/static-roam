# Write every page and block that can be reached from an EntryPoint, including DNPs.
# TODO should be an even more inclusive mode that does everything regardless of reachability. Trivial to implement

export ROAM_DEV_MODE=true
export ROAM_UNEXCLUDE=true	
export ROAM_OUTPUT_DIR=private

./generate.sh
