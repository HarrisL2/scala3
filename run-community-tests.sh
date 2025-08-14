#!/bin/bash
PROJECTS=`cat community-build/test/scala/dotty/communitybuild/CommunityBuildTest.scala  | grep '^[ ]*@Test' | sed 's/.* def //g' | sed 's/ = projects.*//g'`

RESULTS_DIR=/tmp/comm-build-results

mkdir -p $RESULTS_DIR
echo "Found projects: $PROJECTS"
echo "Results will be stored in: $RESULTS_DIR"

for PROJECT in $PROJECTS; do
    echo "Starting build for project: $PROJECT"
    CMD="community-build/testOnly -- *$PROJECT"
    echo "Running: sbt \"$CMD\" 2>&1 | tee $RESULTS_DIR/$PROJECT.log"
    sbt "$CMD" 2>&1 | tee $RESULTS_DIR/$PROJECT.log
    echo "Completed project: $PROJECT (exit code: $?)"
    echo "----------------------------------------"
done
echo "All builds completed!"