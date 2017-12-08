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
                                          title = queryHitsObject$records$title,
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

###############################################################################
###############################################################################
### Convert to long dataframe (with one row for each operationalisation
###############################################################################
###############################################################################

longResults <- ldply(queryHits,
                     function(queryHitsObject) {
                       res <- data.frame(bibtexkey = queryHitsObject$records$bibtexkey,
                                         author = queryHitsObject$records$author,
                                         title = queryHitsObject$records$title,
                                         year = queryHitsObject$records$year,
                                         operationalisations = queryHitsObject$records$operationalisations)
                       return(res);
                     });

longResults <- do.call(rbind,
                       apply(longResults,
                           1,
                           function(dfRow) {
                             op <- unlist(strsplit(dfRow['operationalisations'], "\\|\\|"));
                             ln <- length(op);
                             return(data.frame(operationalisation = op,
                                               bibtexkey = rep(dfRow['bibtexkey'], ln),
                                               author = rep(dfRow['author'], ln),
                                               title = rep(dfRow['title'], ln),
                                               year = rep(dfRow['year'], ln)));
                           }));

row.names(longResults) <- NULL;

###############################################################################
###############################################################################
### Match typical aspect operationalisation texts
###############################################################################
###############################################################################

aspectStimulusFragments <- list(seeMyselfAs = "see myself as",
                                importantPart = "important part",
                                rarelyEvenThinkAbout = "rarely even think about",
                                kindOfPerson = "kind of person",
                                concernedAbout = "concerned about",
                                feelAtALoss = "feel at a loss");

invisible(sapply(seq_along(aspectStimulusFragments), function(i) {
  if (length(aspectStimulusFragments[[i]]) > 1) {
    stop("Processing multiple matching regexes not implemented yet!");
  } else {
    longResults[, names(aspectStimulusFragments)[[i]]] <<-
      grepl(aspectStimulusFragments[[i]], longResults$operationalisation, ignore.case=TRUE);
  }
}));

longResults$anyMatch <- apply(longResults[, names(aspectStimulusFragments)], 1, any);

multiVarFreq(longResults, names(aspectStimulusFragments));
sum(longResults$anyMatch);


