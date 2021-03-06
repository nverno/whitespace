#!/usr/bin/env bash

# get resources:

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd)"
pushd "$DIR">/dev/null

files=("http://compsoc.dur.ac.uk/whitespace/whitespace-mode.el"
       "http://compsoc.dur.ac.uk/whitespace/count.ws"
       "http://compsoc.dur.ac.uk/whitespace/tutorial.html")

declare -A repos=( 
    ["https://github.com/nverno/whitespace"]="whitespace"
    ["https://github.com/nverno/whitespacers"]="interpreters"
)

# get files
get_resource_files () {
    for f in ${files[@]}; do
        if [[ ! -f $(basename $f) ]]; then
            wget $f
        fi
    done
}

# get / update resource repos
get_resource_repos () {
    for repo in "${!repos[@]}"; do
        if [[ ! -d "${repos["$repo"]}" ]]; then
            git clone --depth 1 "$repo" "${repos["$repo"]}"
        else
            pushd "${repos["$repo"]}">/dev/null
            git pull --depth 1
            popd>/dev/null
        fi
    done
}

# ------------------------------------------------------------
# get stuff

# [[ ! -f "blah" ]] && get_resource_files
[[ ! -f "whitespace-mode.el" ]] && get_resource_files
get_resource_repos

popd>/dev/null

# Local Variables:
# sh-shell: bash
# End:
