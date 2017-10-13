######################################################################
### Paths
######################################################################

### Add any relevant paths to this vector. The script will select the
### correct path itself.

basePathVector <- c("B:/Data/research/Self-Identity/Study 3",
                    "C:/Users/marwi/Documents/Sync/Research/Self-Identity/Study 3",
                    "C:/Sync/Research/Self-Identity/Study 3");

######################################################################
### Set the variables with the paths
######################################################################

### Check which paths exist
existingDirs <- basePathVector[sapply(basePathVector, dir.exists)];

### Select first existing path as base path
basePath <- existingDirs[1];
repoPath <- file.path(basePath, 'scale-development-scripts');
workingPath <- file.path(basePath, 'scale-development-scripts');

### Query hits path
queryHitsPath <- file.path(repoPath, "sysrev");

### Filenames
queryHitsFiles <- c("Query1and2.bib", "direct_hits.bib", "indirect_hits.bib");
#queryHitsFiles <- c("temporary.bib");

################################################################################
### Load packages
################################################################################

require('userfriendlyscience');
require('metabefor');
safeRequire('plyr');

################################################################################
### Import references
################################################################################

queryHits <- lapply(queryHitsFiles,
                    function(filename) {
                      return(importBibtex(file.path(queryHitsPath, filename)));
                    });
names(queryHits) <- queryHitsFiles;

resultsTable <- ldply(queryHits,
                      function(queryHitsObject) {
                        res <- data.frame(bibtexkey = queryHitsObject$records$bibtexkey,
                                          author = queryHitsObject$records$author,
                                          year = queryHitsObject$records$year)
                        res$operationalisations <-
                          sapply(strsplit(queryHitsObject$records$operationalisations, "\\|\\|"),
                                 paste,
                                 collapse="\n");
                        return(res);
                      });

write.csv(resultsTable,
          file = file.path(workingPath, "list of operationalisations.csv"),
          row.names = FALSE);


