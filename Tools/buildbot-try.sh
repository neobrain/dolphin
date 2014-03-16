#! /bin/bash
#
# Submits a "buildbot try" message to the Dolphin buildbot with all the
# required options.

opt_file=$HOME/.buildbot/options

if ! [ -f "$opt_file" ]; then
    echo >&2 "error: no .buildbot/options configuration file found"
    echo >&2 "Read the docs: http://code.google.com/p/dolphin-emu/wiki/BuildbotTry"
    exit 1
fi

if ! which buildbot >/dev/null 2>&1; then
    echo >&2 "error: buildbot is not installed"
    echo >&2 "Install it from your package manager, or use 'pip install buildbot'"
    exit 1
fi

if ! git branch | grep -q '^* '; then
    echo "Unable to determine the current Git branch. Input the Git branch name:"
    read branchname
else
    branchname=$(git branch | grep '^* ' | cut -d ' ' -f 2-)
fi

shortrev=$(git describe --always --long --dirty=+ | sed 's/-g[0-9a-f]*\(+*\)$/\1/')

author=$(grep try_username "$opt_file" | cut -d "'" -f 2)

baserev=$(git merge-base HEAD origin/master)

echo "Branch name: $branchname"
echo "Change author: $author"
echo "Short rev: $shortrev"
echo "Base rev: $baserev"

git diff -r $baserev | buildbot try --properties=branchname=$branchname,author=$author,shortrev=$shortrev --diff=- -p1 --baserev $baserev $*
