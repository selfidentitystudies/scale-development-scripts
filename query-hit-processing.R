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
queryHitsFiles <- c("Query1and2_NEW.bib", "direct_hits_NEW.bib", "indirect_hits_NEW.bib");
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
                                          year = queryHitsObject$records$year,
                                          review_items = queryHitsObject$records$review_items)
                        res$operationalisations <-
                          sapply(strsplit(queryHitsObject$records$operationalisations, "\\|\\|"),
                                 paste,
                                 collapse="\n");
                        return(res[res$review_items=='incl', ]);
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
                                         operationalisations = queryHitsObject$records$operationalisations,
                                         review_items = queryHitsObject$records$review_items)
                       return(res[res$review_items=='incl', ]);
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

### Note that because we investigate self-identity as potential
### 'fourth RAA variable', consistent with RAA's TACT principle, only
### operationalisations that contain the target behavior are included;
### others were marked 'nosi' (no self-identity) during extraction.

aspectStimulusFragments <- list(importantPart = "important part",
                                importantToMe = "important to me",
                                believeIn = "believe in",
                                meansMoreThan = "means more than",
                                responsibility = "I have a responsibility",
                                howIWantToLive = "want(ed)* to live",
                                rarelyEvenThinkAbout = "rarely (even )*think about",
                                kindOfPerson = c("kind of person", "type of (a )*person",
                                                 "fits who they are", "typical (person|for me)", "out of character",
                                                 "alien", "in my character", "I(.{1,2}m) a good example of",
                                                 "profile of someone", "generally recognises"),
                                considerMyself = "(class|consider|considered|see|saw) (my|your|them)sel(f|ves)",
                                concernedAbout = "concerned (are you )*(about|with)",
                                feelAtALoss = c("feel (that I missed out|lost|upset|a loss|at a loss)",
                                                "los(t|e) something"),
                                amSomeoneWho = c("I am someone who", "I am somebody who"),
                                acceptability = "accept",
                                perceivedProjectedImage = c("might see me as", "people to see me as"),
                                iAmA = c("I am a", "I am meat eater", "thinker", "person who teaches"),
                                iAm = "I am",
                                roleIdentity = "role identity",
                                appropriateForMe = "appropriate for me",
                                should = c("should"),
                                miscellaneous = c("self-confident", "will study hard",
                                                  "enjoyed", "to what extent .* affect you",
                                                  "quite frankly", "normal part of everyday life",
                                                  "part of my life",
                                                  "most important issue(s)*",
                                                  "I involve in", "feelings",
                                                  "goals related to",
                                                  "relevant to me", "each of us"),
                                domainSpecific = c("read the list of"),
                                selfAs = "self (as|to be)",
                                role = c("role", "person primarily responsible"),
                                embarrassed = "embarrassed",
                                express = "express",
                                lifestyle = "lifestyle",
                                image = "image",
                                values = "values",
                                beingUnique = "being unique",
                                important = "important",
                                personal = "personal",
                                characteristics = "characteristics",
                                self = "self");

longResults$anyMatch <- FALSE;

for (i in seq_along(aspectStimulusFragments)) {
  if (length(aspectStimulusFragments[[i]]) > 1) {
    longResults[, names(aspectStimulusFragments)[[i]]] <-
      !longResults$anyMatch &
      grepl(paste0(aspectStimulusFragments[[i]], collapse="|"),
            longResults$operationalisation,
            ignore.case=TRUE);
  } else {
    longResults[, names(aspectStimulusFragments)[[i]]] <-
      !longResults$anyMatch &
      grepl(aspectStimulusFragments[[i]],
            longResults$operationalisation,
            ignore.case=TRUE);
  }
  longResults$anyMatch <- longResults$anyMatch | longResults[, names(aspectStimulusFragments)[[i]]];
}

multiVarFreq(longResults, names(aspectStimulusFragments));
sum(longResults$anyMatch);
nrow(longResults) - sum(longResults$anyMatch);

### Verify that all operationalisations were only categorised once
apply(longResults[, names(aspectStimulusFragments)], 1, sum)

### Store categorisation in one column
longResults$category <-
  unlist(lapply(apply(longResults[, names(aspectStimulusFragments)], 1, which),
                function(x) return(ifelse(is.logical(x), 'uncategorized',
                                          names(x)))));

### Sort by category
longResults <- longResults[order(longResults$category), ];

### Write to .csv file (only export rows where 'longResults$anyMatch'
### is true to only export unmatched entries; remove the '|1' below)
write.csv(longResults[longResults$anyMatch|1, c('category',
                                                'operationalisation',
                                                'bibtexkey',
                                                'author',
                                                'title',
                                                'year')],
          file = file.path(workingPath, "operationalisations.csv"));

###############################################################################
###############################################################################
### Discussion of operationalisation categories
###############################################################################
###############################################################################



aspectStimulusFragments_refined <-
   list(importantPart = "important part",
        importantToMe = "important to me",
        believeIn = "believe in",
        meansMoreThan = "means more than",
        responsibility = "I have a responsibility",
        howIWantToLive = "want(ed)* to live",
        rarelyEvenThinkAbout = "rarely (even )*think about",
        kindOfPerson = c("kind of person", "type of (a )*person",
                         "fits who they are", "typical (person|for me)", "out of character",
                         "alien", "in my character", "I(.{1,2}m) a good example of",
                         "profile of someone", "generally recognises"),
        considerMyself = "(class|consider|considered|see|saw) (my|your|them)sel(f|ves)",
        concernedAbout = "concerned (are you )*(about|with)",
        feelAtALoss = c("feel (that I missed out|lost|upset|a loss|at a loss)",
                        "los(t|e) something"),
        amSomeoneWho = c("I am someone who", "I am somebody who"),
        acceptability = "accept",
        perceivedProjectedImage = c("might see me as", "people to see me as"),
        iAmA = c("I am a", "I am meat eater", "thinker", "person who teaches"),
        iAm = "I am",
        roleIdentity = "role identity",
        appropriateForMe = "appropriate for me",
        should = c("should"),
        miscellaneous = c("self-confident", "will study hard",
                          "enjoyed", "to what extent .* affect you",
                          "quite frankly", "normal part of everyday life",
                          "part of my life",
                          "most important issue(s)*",
                          "I involve in", "feelings",
                          "goals related to",
                          "relevant to me", "each of us"),
        domainSpecific = c("read the list of"),
        selfAs = "self (as|to be)",
        role = c("role", "person primarily responsible"),
        embarrassed = "embarrassed",
        express = "express",
        lifestyle = "lifestyle",
        image = "image",
        values = "values",
        beingUnique = "being unique",
        important = "important",
        personal = "personal",
        characteristics = "characteristics",
        self = "self");
