### Set basePath, depending on which PC we're running

if (file.exists('B:/Data/research/Self-Identity/Study 3')) {
  basePath <- 'B:/Data/research/Self-Identity/Study 3';
} else if (file.exists('C:/Users/marwi/Documents/Sync/Research/Self-Identity/Study 3')) {
  basePath <- 'C:/Users/marwi/Documents/Sync/Research/Self-Identity/Study 3';
} else {
  basePath <- 'C:/Sync/Research/Self-Identity/Study 3';
}

# install.packages('userfriendlyscience');
# install.packages('rio');
# install.packages('diptest');
# install.packages('userfriendlyscience',
#                  contriburl='http://userfriendlyscience.com/src/contrib',
#                  type='source');

### Update packages from CRAN
# update.packages(ask=FALSE, repos='http://cran.rstudio.com');

### Set other directories for importing data etc
workingPath <- basePath;
outputPath <- file.path(basePath, 'results - intermediate output');
scriptPath <- file.path(basePath, 'results - analysis scripts');
dataPath <- file.path(basePath, 'data');

### Whether to update the userfriendlyscience package,
### and which versions were used for this script
userfriendlyscienceVersion <- "0.4-2";
updateUserfriendlyscience = FALSE;

### Set parameters for running the script
### concerning factor-analyses

### Number of iterations to use when bootstrapping the confidence
### intervals for the factor analysis
n.iter <- 100;

### Method to use for the factor analyses, e.g. minres, ml, wls, etc (see ?fa)
faMethod <- 'minres';

### Function for factor analyses
fullFact <- function(dat = NULL, items=NULL, rotate='oblimin') {
  
  res <- list(input = as.list(environment()),
              intermediate = list(),
              output = list());
  
  if (is.null(dat)) {
    dat <- getData();
  }
  
  if (is.null(items)) {
    items <- names(dat);
  }
  
  res$output$parallel <- fa.parallel(dat[, items]);
  res$output$vss <- vss(dat[, items], rotate=rotate);
  
  class(res) <- 'fullFact';
  
  return(res);
  
}

print.fullFact <- function(x, ...) {
  print(x$output);
}

### Function to check for, maybe upload, maybe download, install, and load1 
### a package
updatePackageInProgress <-
  function(packagename, packageversion, packageWorkingPath,
           packageBuildingPath = "B:/Data/statistics/R/library",
           userfriendlyscienceURL = "http://userfriendlyscience.com",
           silent=FALSE, type=c('bin', 'source')) {
    
    fullRepositories <- c(c('userfriendlyscience'=userfriendlyscienceURL),
                          getOption("repos"));
    
    ### Establish name of the package file
    srcFile <- paste0(packagename, "_", packageversion, ".tar.gz");
    binFile <- paste0(packagename, "_", packageversion, ".zip");
    
    ### Copy potentially newly created and compiled versions of the package
    ### to the directory from where we'll include it
    if (file.exists(file.path(packageBuildingPath, binFile))) {
      if (!silent) cat("Copied '", file.path(packageBuildingPath, binFile),
                       "' to '", packageWorkingPath, "'.\n", sep="");
      file.copy(file.path(packageBuildingPath, binFile),
                packageWorkingPath, overwrite = TRUE);
      if (require('RCurl')) {
        if (url.exists('userfriendlyscience.com')) {
          ftpUpload(file.path(packageBuildingPath, binFile),
                    paste0("ftp://userfriendlyscience.com/", binFile),
                    userpwd="package@userfriendlyscience.com:uploadPackage");
          if (!silent) cat("Uploaded '", file.path(packageBuildingPath, binFile),
                           "' to 'ftp://userfriendlyscience.com'.\n", sep="");
        }
      }
    }
    if (file.exists(file.path(packageBuildingPath, srcFile))) {
      if (!silent) cat("Copied '", file.path(packageBuildingPath, srcFile),
                       "' to '", packageWorkingPath, "'.\n", sep="");
      file.copy(file.path(packageBuildingPath, srcFile),
                packageWorkingPath, overwrite = TRUE);
      if (require('RCurl')) {
        if (url.exists('userfriendlyscience.com')) {
          ftpUpload(file.path(packageBuildingPath, srcFile),
                    paste0("ftp://userfriendlyscience.com/", srcFile),
                    userpwd="package@userfriendlyscience.com:uploadPackage");
          if (!silent) cat("Uploaded '", file.path(packageBuildingPath, srcFile),
                           "' to 'ftp://userfriendlyscience.com'.\n", sep="");
        }
      }
    }
    
    if (paste0("package:", packagename) %in% search()) {
      if (!silent) cat("Package '", packagename,
                       "' is loaded; unloading.\n", sep="");
      detach(paste0("package:", packagename),
             character.only=TRUE, unload=TRUE);
      if (!silent) cat("Detached package '", packagename,
                       "'.\n", sep="");
    }
    if (is.element(packagename, installed.packages()[, 1])) {
      remove.packages(packagename);
      if (!silent) cat("Uninstalled package '", packagename,
                       "'.\n", sep="");
    }
    
    ### If we should try to load the bin package, try that first
    if (file.exists(file.path(packageWorkingPath, binFile)) &&
        ('bin' %in% type)) {
      install.packages(file.path(packageWorkingPath, binFile),
                       type='win.binary', repos=NULL);
      cat("Installed package '", packagename, "'.\n");
    } else if (file.exists(file.path(packageWorkingPath, srcFile)) &&
               ('source' %in% type)) {
      install.packages(file.path(packageWorkingPath, srcFile),
                       type='source', repos=NULL);
      cat("Installed package '", packagename, "'.\n");
    } else {
      ### Otherwise download it from the userfriendlyscience repository
      if ('bin' %in% type) {
        install.packages(packagename, type='win.binary', repos=fullRepositories);
      } else if ('source' %in% type) {
        install.packages(packagename, type='source', repos=fullRepositories);
      }
    }
    require(package=packagename, character.only=TRUE);
  }

if (updateUserfriendlyscience) {
  updatePackageInProgress('userfriendlyscience', userfriendlyscienceVersion,
                          packageWorkingPath=workingPath);
} else {
  require('userfriendlyscience');
}

################################################################################

faDfDiamondCIplot <- function(faDf, autoSize=NULL, fixedSize=.25,
                              xlab='Factor Loading',
                              geomAlpha=.3,
                              colors = c('red', 'green')) {
  
  ### Create empty
  res <- ggplot();
  
  CIs <- lapply(seq(1, ncol(faDf), by=3), function(x) {
    return(cbind(faDf[, x:(x+2)], data.frame(row=1:nrow(faDf))));
  });
  
  for (currentFactor in 1:length(CIs)) {
    res <- res + apply(CIs[[currentFactor]], 1, function(x, aSize=autoSize,
                                                         fSize = fixedSize, alpha=geomAlpha,
                                                         color = colors[currentFactor]) {
      return(geom_polygon(diamondCoordinates(unlist(x[1:3]),
                                             otherAxisValue=x[4],
                                             autoSize = aSize,
                                             fixedSize = fSize),
                          mapping=aes(x=x, y=y), alpha=alpha,
                          color=color, fill=color));
    });
  }
  
  res <- res + scale_y_reverse(breaks=1:nrow(faDf),
                               labels=rownames(faDf)) +
    ylab(NULL) + xlab(xlab) + theme_bw() + geom_vline(xintercept=0);
  
  return(res);
}


### Generate tableGrob with headers for the
### table with factor loadings (see
### http://stackoverflow.com/questions/33214671/merging-table-header-cells-using-tablegrob
### and https://github.com/baptiste/gridextra/wiki/tableGrob
makeHeaderTable <- function(vector, startCol=2, colSpan=3) {
  nCol <- length(vector);
  tmpDf <- t(matrix(1:nCol));
  colnames(tmpDf) <- vector;
  rownames(tmpDf) <- 'rowName';
  headerTable <- tableGrob(tmpDf);
  headerTable$grobs[[1]] <- textGrob('');
  rm(tmpDf);
  headerTable <- headerTable[1, ];
  
  ### Compute where each column should start and end
  startCols <- seq(startCol, colSpan * nCol + startCol - 1, by=colSpan);
  endCols <- startCols + colSpan - 1;
  
  headerTable$layout[3:nrow(headerTable$layout), c("l", "r")] <-
    list(startCols, endCols);
  return(headerTable);
}

################################################################################

### Getting other packages
library('userfriendlyscience');
safeRequire('ggplot2');
safeRequire('lavaan');
safeRequire('plyr')
safeRequire('dplyr')
safeRequire('car');
safeRequire('rmarkdown');
safeRequire('grid');
safeRequire('gridExtra');
safeRequire('pander');
safeRequire('psych');
safeRequire('GGally');

########################################################################
########################################################################
### Define scales that will be used
########################################################################
########################################################################

### setting up the scales for further analyses (nieuw GJ! - zijn dit nu ook al nieuwe variabelen?)

scales <- list();

### Selection
### old measures of self-identity selected
###scales$selfIdentitySelected <- c('Selfidentity_rarelythinkabout', 
###                                 'Selfidentity_kindofperson',
###                                 'Selfidentity_seemyselfas', 
###                                 'Selfidentity_doingbehaviorimportant', 
###                                 'Selfidentity_importantpart');

### new measures of self-identity selected based on masterthesis
scales$selfIdentitySelected <- c('Selfidentity_kindofperson',
                                 'Selfidentity_seemyselfas',
                                 'Selfidentity_concernedwithdoingtherightbehavior', 
                                 'Selfidentity_seemyselffollowingthebehaviorguideline');

scales$selfIdentity <- c('Self-identity:\nSomething I\nrarely think about' = 'Selfidentity_rarelythinkabout', 
                         'Self-identity:\n' = 'Selfidentity_kindofperson',
                         'Self-identity:\n' = 'Selfidentity_seemyselfas', 
                         'Self-identity:\n' = 'Selfidentity_concernedwithnotdoingthebehaviorenough', 
                         'Self-identity:\n' = 'Selfidentity_doingbehaviorimportant', 
                         'Self-identity:\n' = 'Selfidentity_importantpart', 
                         'Self-identity:\n' = 'Selfidentity_seemyselffollowingthebehaviorguideline', 
                         'Self-identity:\n' = 'Selfidentity_wouldfeelatalossgivingupwrongbehavior', 
                         'Self-identity:\n' = 'Selfidentity_concernedwithdoingtherightbehavior',
                         'Self-identity:\n' = 'Selfidentity_wrongbehaviormeansmorethanjusttheact', 
                         'Self-identity:\n' = 'Selfidentity_behaviormeansmoretantheactself');

scales$attitude <- c('Attitude_bad_good', 'Attitude_unpleasant_pleasant',
                     'Attitude_harmful_beneficial', 'Attitude_boring_interesting');

scales$importance <- c('Importancescale_unimportant_important',
                       'Importancescale_notessential_essential',
                       'Importancescale_notsignificant_significant');

scales$attitudeImportance <- c(scales$attitude, scales$importance);

scales$perceivedNorms <- c('Injunctivenorm_importantpeople',
                           'Injunctivenorm_mostpeopleapprove',
                           'Descriptivenorm_closefriends',
                           'Descriptivenorm_peoplelikeme');

scales$pbc <- c('Perceivedcontrol_forme',
                'Perceivedcontrol_reallywantto',
                'Perceivedcontrol_confident');

scales$intention <- c('Intention_intend',
                      'Intention2willing',
                      'Intention3expect');

scales$pastBehavior <- c('Past_haveused', 'Past_howoften');

scales$currentBehavior <- c('curBeh');

invertedItems <- c('Selfidentity_rarelythinkabout',
                   'Selfidentity_wouldfeelatalossgivingupwrongbehavior');

### CFA measurement models for each variable;
### will be used in the loop to also run CFA's
measurementModelSpecs <- list();
for (currentScale in names(scales)) {
  measurementModelSpecs[[currentScale]] <- paste0(currentScale, ' =~ ',
                                                  paste0(scales[[currentScale]],
                                                         collapse=" + "));
}

nonSIvars <- scales[names(scales) != 'selfIdentity'];

### Generate abbreviated variable names
abbr <- abbreviate(names(scales))

########################################################################
########################################################################
### Load data
########################################################################
########################################################################

### Load datafiles in data path
dataFilenames <- list.files(dataPath);

siDat <- list();

for (currentDataFile in dataFilenames) {
  
  currentBehavior <- sub('([^\\.]+)\\..*', '\\1', currentDataFile);
  
  ### Read the data and store in a dataframe
  siDat[[currentBehavior]] <- list();
  siDat[[currentBehavior]]$raw <-
    read.csv(file.path(dataPath, currentDataFile),
             header = TRUE, sep = ",", row.names=NULL,
             skip=1, stringsAsFactors = FALSE);
  
  ### Remove last variable
  siDat[[currentBehavior]]$raw <-
    siDat[[currentBehavior]]$raw[, names(siDat[[currentBehavior]]$raw) != 'X'];

  ### Store with different name as cleaned version
  siDat[[currentBehavior]]$cln <- siDat[[currentBehavior]]$raw;
  
  ### Run file-specific clean-up analyses if any exist
  if (file.exists(file.path(scriptPath, paste0(currentBehavior, " - cleaning.R")))) {
    source(file.path(scriptPath, paste0(currentBehavior, " - cleaning.R")));
    cat0("\nRan clean-up procedures in '",
         file.path(scriptPath, paste0(currentBehavior, " - cleaning.R")),
         "'\n");
  }
  
  ### Fix some weird codings from Qualtrics
  siDat[[currentBehavior]]$cln$Intention_intend <-
    recode(siDat[[currentBehavior]]$cln$Intention_intend, "20=1; 21=2; 22=3; 23=4; 24=5; 25=6; 26=7");
  siDat[[currentBehavior]]$cln$Intention2willing <-
    recode(siDat[[currentBehavior]]$cln$Intention2willing, "43=1; 44=2; 45=3; 46=4; 47=5; 27=6; 28=7"); 
  siDat[[currentBehavior]]$cln$Intention3expect <-
    recode(siDat[[currentBehavior]]$cln$Intention3expect, "14=1; 15=2; 16=3; 17=4; 18=5; 19=6; 20=7");
  siDat[[currentBehavior]]$cln$curBeh <- 
    recode(siDat[[currentBehavior]]$cln$curBeh, "9=1; 10=2; 11=3; 12=4; 14=5; 15=6; 16=7");
  siDat[[currentBehavior]]$cln$Injunctivenorm_importantpeople <-
    recode(siDat[[currentBehavior]]$cln$Injunctivenorm_importantpeople, "40=1; 41=2; 42=3; 43=4; 44=5; 45=6; 46=7; 47=NA");
  siDat[[currentBehavior]]$cln$Descriptivenorm_peoplelikeme <-
    recode(siDat[[currentBehavior]]$cln$Descriptivenorm_peoplelikeme, "15=1; 16=2; 17=3; 18=4; 19=5; 20=6; 21=7; 22=NA");
  siDat[[currentBehavior]]$cln$Perceivedcontrol_forme <-
    recode(siDat[[currentBehavior]]$cln$Perceivedcontrol_forme, "22=1; 23=2; 24=3; 25=4; 26=5; 27=6; 28=7");
  siDat[[currentBehavior]]$cln$Perceivedcontrol_reallywantto <-
    recode(siDat[[currentBehavior]]$cln$Perceivedcontrol_reallywantto, "9=1; 18=2; 10=3; 11=4; 12=5; 13=6; 14=7");
  siDat[[currentBehavior]]$cln$Past_howoften <-
    recode(siDat[[currentBehavior]]$cln$Past_howoften, "28=1; 29=2; 30=3; 31=4; 32=5; 33=6; 34=7"); 
  siDat[[currentBehavior]]$cln$Descriptivenorm_closefriends <-
    recode(siDat[[currentBehavior]]$cln$Descriptivenorm_closefriends, "8=NA");
  siDat[[currentBehavior]]$cln$Injunctivenorm_mostpeopleapprove <-
    recode(siDat[[currentBehavior]]$cln$Injunctivenorm_mostpeopleapprove, "8=NA");
  
  ########################################################################
  ### Invert items
  ########################################################################
  
  siDat[[currentBehavior]]$cln <-
    invertItems(siDat[[currentBehavior]]$cln, invertedItems);

  # siDat[[currentBehavior]]$cln$Injunctivenorm_mostpeopleapprove <-
  #   invertItem(siDat[[currentBehavior]]$cln$Injunctivenorm_mostpeopleapprove, range = c(1, 7));
  
  ########################################################################
  ### Split dataframe into two dataframes; one per country
  ########################################################################
  
  siDat[[paste0(currentBehavior, '-us')]] <- list(cln =
                                                    siDat[[currentBehavior]]$cln[siDat[[currentBehavior]]$cln$country == 1, ]);
  siDat[[paste0(currentBehavior, '-india')]] <- list(cln =
                                                       siDat[[currentBehavior]]$cln[siDat[[currentBehavior]]$cln$country == 2, ]);
  
  ########################################################################
  ### Create and add scales
  ########################################################################
  
  siDat[[currentBehavior]]$cln <-
    makeScales(siDat[[currentBehavior]]$cln, scales);
  siDat[[paste0(currentBehavior, '-us')]]$cln <-
    makeScales(siDat[[paste0(currentBehavior, '-us')]]$cln, scales);
  siDat[[paste0(currentBehavior, '-india')]]$cln <-
    makeScales(siDat[[paste0(currentBehavior, '-india')]]$cln, scales);
  
}

########################################################################
### Add dataframe with all behaviors and all cultures; only retain
### scales and the items in those scales, as well as some generic
### variables such as demography etc; make sure those are defines as
### a vector somewhere in the beginning.
########################################################################





########################################################################
### Select data to run
########################################################################

### For now we use the condom use data in the US.

# siDatBackup <- siDat;
# siDat <- siDatBackup;
# dataframeName <- 'condoms';
# siDat[!names(siDat)==dataframeName] <- NULL;

########################################################################
### Exploring the data
########################################################################

for (dataframeName in names(siDat)) {
  
  dat <- siDat[[dataframeName]]$cln;

  # ### Start writing output to a file
  # sink(file = file.path(workingPath, paste0("SI output - ", dataframeName)),
  #      split=TRUE);
  # 
  # cat0(repeatStr("#", 80), "\n");
  # cat0("### Self-identity output for dataset '", dataframeName, "'\n");
  # cat0(repeatStr("#", 80), "\n\n");
  
  ### Set title of rmarkdown report
  RmdTitle <- paste('Report of', dataframeName);
  
  ### Knit and render rmarkdown report
  render(file.path(scriptPath, 'report.Rmd'),
         output_file = file.path(outputPath, paste0(RmdTitle, '.html')),
         intermediates_dir = outputPath);

  ### Stop writing to file
  # sink();
  
}
