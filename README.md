# Main GENIE detailed error reporting

## Installation

Clone this repository and navigate to the directory:
```
git clone git@github.com:Sage-Bionetworks/genie-main-details.git
cd genie-main-details
```

If running without Docker, install all required R packages:
```
R -e 'renv::restore()'
```

If running with Docker, build the containter:
```
docker build -t genie-main-details .
```

## Synapse credentials

Cache your Synapse personal access token (PAT) as an environmental variable:
```
export SYNAPSE_AUTH_TOKEN={your_personal_access_token_here}
```

## Usage without Docker

To display the command line interface:
```
Rscript mg_vital_error_details.R -h
```

The command line interface will display as follows:
```
Usage: mg_vital_error_details.R [options]


Options:
        -u, --upload
                Upload results to Synapse; otherwise, write locally

        -v, --verbose
                Output script messages to the user.

        -h, --help
                Show this help message and exit
```

Example run: 
```
Rscript mg_vital_error_details.R -v -u
```

## Usage with Docker

To display the command line interface:
```
docker run -e SYNAPSE_AUTH_TOKEN=$SYNAPSE_AUTH_TOKEN --rm genie-main-details -h
```

The command line interface will display as follows:
```
Usage: mg_vital_error_details.R [options]


Options:
        -u, --upload
                Upload results to Synapse; otherwise, write locally

        -v, --verbose
                Output script messages to the user.

        -h, --help
                Show this help message and exit
```

Example run: 
```
docker run -e SYNAPSE_AUTH_TOKEN=$SYNAPSE_AUTH_TOKEN --rm genie-main-details -v -u
```
