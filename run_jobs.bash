#!/bin/bash

# Check we are set up to be notified of changes in the jobs directory.
if [ -z $JOBS_DIR ]; then
    echo "No jobs directory set! Please see add_job.bash."
    exit
fi
if [ -z $(which inotifywait) ]; then
    echo "inotifywait is not installed! Please run sudo apt-get install inotify-tools"
    exit
fi

# Make sure we are set up to send notifications to Discord.
# Will send to DISCORD_WEBHOOK_URL
if [ -z $(which knockknock) ]; then
    echo "knockknock is not installed! Please pip install knockknock to get notifications of job completion"
    exit
fi
if [ -z $DISCORD_WEBHOOK_URL ]; then
    echo "You are almost set up to ping your Discord, but you need to set the DISCORD_WEBHOOK_URL evironment variable!"
    exit
fi

# Set up the proper paths to the jobs directory.
TO_RUN="$JOBS_DIR/to_run"
RUNNING="$JOBS_DIR/running"
WORK_TREES="$JOBS_DIR/work_trees"

processJobs(){
    # Force the wildcard * to return empty if no matches.
    shopt -s nullglob

    # Populate our jobs list.
    jobs=( $TO_RUN/*.job )

    # Loop while our job list is not empty.
    while [ ${#jobs[@]} -gt 0 ]; do

        # Get the first job in the list.
        job=${jobs[0]}
        echo "Now running" $job
        
        # Parse the relevant parameters.
        CMD_LINE=$(grep "cmd: " $job)
        CMD=${CMD_LINE#"cmd: "}
        SHA_LINE=$(grep "SHA: " $job)
        SHA=${SHA_LINE#"SHA: "}
        REPO_LINE=$(grep "repo: " $job)
        REPO=${REPO_LINE#"repo: "}

        # If the command should be run with a git repo, check out the code.
        if [ $SHA ]; then
            cd $WORK_TREES/$REPO
            git checkout $SHA
            git submodule update --init --checkout
            echo "Code loaded!"
        fi

        # Mark the job as running and update its path.
        mv $job $RUNNING/
        NEW_JOB_PATH="$RUNNING/${job##*/}"

        # Log the start time.
        START_TIME=$(date +"%m-%d-%Y %r")
        echo "Start: $START_TIME" >> $NEW_JOB_PATH

        # Run the job and log its output. We use eval to allow chaining of
        # commands. Note that we assume this script was run with the proper
        # conda environment etc. for all of the commands.
        echo "Now running: "$CMD
        eval knockknock discord --webhook-url $DISCORD_WEBHOOK_URL $CMD > $NEW_JOB_PATH.log

        # Log the end time.
        END_TIME=$(date +"%m-%d-%Y %r")
        echo "End: $END_TIME" >> $NEW_JOB_PATH
        echo "Job complete!"

        # Check for more jobs.
        jobs=( $TO_RUN/*.job )
    done
}


# Run through all files in the directory. We check for jobs, process all
# available, then sleep until the jobs folder is modified.
processJobs
while inotifywait -e modify $TO_RUN; do
    processJobs
    echo "Now waiting for new jobs..."
done

    
           
           
