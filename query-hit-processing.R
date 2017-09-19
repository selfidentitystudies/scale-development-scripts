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

### Query hits path
queryHitsPath <- file.path(repoPath, "sysrev");

### Filenames
queryHitsFiles <- c("Query1and2.bib");
queryHitsFiles <- c("temporary.bib");

################################################################################
### Load packages
################################################################################

require('userfriendlyscience');
require('metabefor');
safeRequire('RefManageR');

################################################################################
### Import references
################################################################################

queryHits <- list();

# ### First read file to remove the JabRef comment
# cleanFile <- readLines(file.path(queryHitsPath, queryHitsFiles));
#
# ### Paste all strings together
# cleanFile <- paste(cleanFile, collapse="\n");
#
# ### Remove jabref comments
# cleanFile <- gsub("(?s)@[Cc]omment\\{jabref-meta:[^\\}]*\\}", "", cleanFile, perl=TRUE);
#
# ### Write clean file to disk
# writeLines(cleanFile, con=file.path(queryHitsPath, "tmp-clean-file.bib"));
#
# ### Import references
# queryHits[['1and2']] <- ReadBib(file.path(queryHitsPath, "tmp-clean-file.bib"));
queryHits[['1and2']] <- importBibtex(file.path(queryHitsPath, "tmp-clean-file.bib"));
strsplit(queryHits[['1and2']]$records$operationalisations, "\\|\\|");

