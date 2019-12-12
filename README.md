# bricksteR
Making Databricks easy to use for R developers.

Automating R Jobs on Databricks with bricksteR
================
Rafi Kurlansik, Solutions Architect, Databricks
2019-12-10

While Databricks supports R users through interactive notebooks and a hosted instance of RStudio Server, it can be cumbersome to convert R files into production jobs. `bricksteR` makes it easy to quickly turn .R and .Rmd files into automated jobs that run on Databricks by using the [Databricks REST API](https://docs.databricks.com/dev-tools/api/latest/).

Here are some highlights of what you can do with `bricksteR`:

-   Import any .R or .Rmd file into the Databricks Workspace, from anywhere
-   Create and run a new job in a single stroke
-   Check on the status of a job run by name **or** id

Getting Started
---------------

##### Get the Package

The first thing you'll need to get started is to install the package from GitHub.

``` r
devtools::install_github("RafiKurlansik/bricksteR")
library(bricksteR)
```

##### Authentication

You will need an [Access Token](https://docs.databricks.com/dev-tools/api/latest/authentication.html#authentication) in order to authenticate with the Databricks REST API.

If you are working locally or using [DB Connect](https://docs.databricks.com/dev-tools/databricks-connect.html#databricks-connect), a good way to authenticate is to use a [.netrc](https://docs.databricks.com/dev-tools/api/latest/authentication.html#store-token-in-netrc-file-and-use-in-curl) file. By default, `bricksteR` will look for the `.netrc` file before checking for a token in the function call. If you don't already have one, create a .netrc file in your home directory that looks like this:

    machine <databricks-instance>
    login token
    password <personal-access-token-value>

You can also authenticate by [passing a token to Bearer authentication](https://docs.databricks.com/dev-tools/api/latest/authentication.html#pass-token-to-bearer-authentication). To be clear, **it is not recommended to store your credentials directly in your code**. If you are working on Databricks in a notebook, you can use `dbutils.secrets.get()` to avoid printing your token.

Finally, you will need to identify the workspace URL of your Databricks instance. On AWS this typically has the form `https://dbc-a64b1209f-d811.cloud.databricks.com`, or if you have a vanity URL it may look more like `https://mycompany.cloud.databricks.com`. On Azure it will have the form `https://eastus2.azuredatabricks.net`, or whichever region your instance is located in.

Importing R Scripts to Databricks
---------------------------------

A common use case is moving R scripts or R Markdown files to Databricks to run as a notebook. We can automate this process using `import_to_workspace()`.

``` r
import_to_workspace(file = "/Users/rafi.kurlansik/RProjects/AnRMarkdownDoc.Rmd",
                    notebook_path = "/Users/rafi.kurlansik@databricks.com/AnRMarkDownDoc",
                    workspace = workspace,
                    overwrite = 'true')
#>' Status: 200
#>' Object: /Users/rafi.kurlansik/RProjects/AnRMarkdownDoc.Rmd was added to the workspace at /Users/rafi.kurlansik@databricks.com/AnRMarkDownDoc
#>' Response [https://demo.cloud.databricks.com/api/2.0/workspace/import]
#>'   Date: 2019-12-10 15:37
#>'   Status: 200
#>'   Content-Type: application/json;charset=utf-8
#>'   Size: 3 B
#>' {}
```

Jobs
----

##### Create a Job

Now that the file is in our workspace, we can create a job that will run it as a [Notebook Task](https://docs.databricks.com/dev-tools/api/latest/jobs.html#jobsnotebooktask).

``` r
new_job <- create_job(notebook_path = "/Users/rafi.kurlansik@databricks.com/AnRMarkDownDoc", 
                       name = "Beer Sales Forecasting Job",
                       job_config = "default",
                       workspace = workspace)
#>' Status: 200
#>' Job "Beer Sales Forecasting Job" created.
#>' Job ID: 30805

job_id <- new_job$job_id
```

Each job requires a `job_config`, which is a JSON formatted string specifyiing the type of infrastructure the job will run on. If none is supplied, a 'default' cluster is created with 1 driver and 2 workers. Here's what that default configuration looks like:

``` r
# Default JSON configs
 job_config <- paste0('{
  "name": ', name,'
  "new_cluster": {
    "spark_version": "5.3.x-scala2.11",
    "node_type_id": "r3.xlarge",
    "aws_attributes": {
      "availability": "ON_DEMAND"
    },
    "num_workers": 2
  },
  "email_notifications": {
    "on_start": [],
    "on_success": [],
    "on_failure": []
  },
  "notebook_task": {
    "notebook_path": ', notebook_path, '
  }
}')
```

Where `name` is the name of your job and `notebook_path` is the path to the notebook in your workspace that will be run.

##### Run a Job

Creating a job by itself will not actually execute any code. If we want our beer forecast - and we do - then we'll need to actually run the job. We can run the job by id, or by name.

``` r
# Specifying the job_id
run_job(job_id = job_id, workspace = workspace, token = NULL)
#>' Status: 200
#>' Run ID: 2392911
#>' Number in Job: 1

# Specifying the job name
run_job(name = "Beer Sales Forecasting Job", workspace = workspace, token = NULL)
#>' Job "Beer Sales Forecasting Job" found with 30805.
#>' Status: 200
#>' Run ID: 2392912
#>' Number in Job: 2
```

Running a job by name is convenient, but there's nothing stopping you from giving two jobs the same name on Databricks. The way `bricksteR` handles those conflicts today is by forcing you to use the unique Job ID or rename the job you want to run with a distinct moniker.

##### Create and Run a Job

Like to move fast? Well, now you can create and run a job in a single line:

``` r
running_new_job <- create_and_run_job(name = "Lager Sales Forecasting Job", notebook_path = "/Users/rafi.kurlansik@databricks.com/AnRMarkDownDoc", 
                                      job_config = "default", workspace = workspace)
#>' Job "Lager Sales Forecasting Job" successfully created...  
#>' The Job ID is: 30806
#>' 
#>' Run successfully launched...  
#>' The run_id is: 2392913
#>' 
#>' You can check the status of this run at: 
#>'  https://demo.cloud.databricks.com#job/30806/run/1
```

##### View Jobs and Runs

There are two handy functions that will return a list of Jobs and their runs - `jobs_list()` and `runs_list()`. One of the objects returned is a concise R dataframe that allows for quick inspection.

``` r
jobs <- jobs_list(workspace = workspace)
#>' Status: 200
#>' Number of jobs: 620
beer_runs <- runs_list(name = "Lager Sales Forecasting Job", workspace = workspace)
#>' Job "Lager Sales Forecasting Job" found with 30806.
#>' Status: 200
#>' Job ID: 30806
#>' Number of runs: 1
#>' 
#>' Are there more runs to list? 
#>'  FALSE

head(jobs$response_tidy)
#>'   job_id          settings.name        created_time
#>' 1  12017  test-job-dollar-signs 2018-12-04 15:55:01
#>' 2  18676 Arun_Spark_Submit_Test 2019-08-28 18:32:49
#>' 3  21042              test-seif 2019-10-30 05:06:32
#>' 4   4441        Ad hoc Analysis 2017-08-29 20:21:20
#>' 5  29355       Start ETL Stream 2019-12-04 18:26:40
#>' 6  13941           testingJobId 2019-03-06 17:00:10
#>'                 creator_user_name
#>' 1     sid.murching@databricks.com
#>' 2  arun.pamulapati@databricks.com
#>' 3 seifeddine.saafi@databricks.com
#>' 4            parag@databricks.com
#>' 5              mwc@databricks.com
#>' 6            caryl@databricks.com
head(beer_runs$response_df)
#>'   runs.job_id runs.run_id runs.number_in_job runs.original_attempt_run_id
#>' 1       30806     2392913                  1                      2392913
#>'   runs.state.life_cycle_state runs.state.state_message
#>' 1                     PENDING      Waiting for cluster
#>'                                         notebook_path
#>' 1 /Users/rafi.kurlansik@databricks.com/AnRMarkDownDoc
#>'   runs.cluster_spec.new_cluster.spark_version
#>' 1                             5.5.x-scala2.11
#>'   runs.cluster_spec.new_cluster.aws_attributes.zone_id
#>' 1                                           us-west-2c
#>'   runs.cluster_spec.new_cluster.aws_attributes.availability
#>' 1                                        SPOT_WITH_FALLBACK
#>'                                 runs.cluster_spec.new_cluster.aws_attributes.instance_profile_arn
#>' 1 arn:aws:iam::414351767826:instance-profile/fieldeng-s3-bucket-rw-IAMWriterProfile-1JZBYQRCWBDCA
#>'   runs.cluster_spec.new_cluster.node_type_id
#>' 1                                  r3.xlarge
#>'   runs.cluster_spec.new_cluster.enable_elastic_disk
#>' 1                                             FALSE
#>'   runs.cluster_spec.new_cluster.num_workers           cluster_id
#>' 1                                         2 1210-153710-shalt269
#>'   runs.start_time runs.setup_duration runs.execution_duration
#>' 1    1.575992e+12                   0                       0
#>'   runs.cleanup_duration runs.trigger        runs.creator_user_name
#>' 1                     0     ONE_TIME rafi.kurlansik@databricks.com
#>'                 runs.run_name
#>' 1 Lager Sales Forecasting Job
#>'                                   runs.run_page_url runs.run_type has_more
#>' 1 https://demo.cloud.databricks.com#job/30806/run/1       JOB_RUN    FALSE
```

##### Delete a Job

If you need to clean up a job from the workspace, use `delete_job()`.

``` r
delete_job(job_id = job_id, workspace = workspace)
#>' Status: 200
#>' { "job_id": 30805 } has been deleted.
```

Conclusion and Future Plans
---------------------------

That's about all there is to it to get started with automating R on Databricks! Whether you are using `blastula` to email a report or are spinning up a massive cluster to perform ETL or model training, these APIs will make it easy for you to define, schedule, and monitor those jobs from R.

The near term plan for `bricksteR` is improve and simplify the existing code, then to continue adding functions that wrap around the Databricks REST API. After that, I'd like to also add helper functions for working with DBFS to maintain R libraries and package versions. All in due time. :) \_\_\_

Questions? Feel free to contact me at <rafi.kurlansik@databricks.com>.
