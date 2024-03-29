#' The Color Red
#'
#' Quick function for "red". Saves two characters
#' @export
#'
red <- "red"

#' The Color Blue
#'
#' Quick function for "blue". Saves two characters
#' @export
#'
blue <- "blue"

#' University of Chicago Primary Colors
#'
#' The Maroon and Grey of the University of Chicago
#' @export
#'
uc_primary_pal <- c("#800000", "#767676", "#D6D6CE")

#' University of Chicago Secondary Colors
#'
#' Secondary palette from the graphic identity guidelines. Use this
#' for all sorts of things, including visualizations.
#' 
#' @export
#'
uc_second_pal <- c("#FFA319", "#C16622", "#8F3931",
                  "#8A9045", "#58593F", "#155F83", "#350E20")

#' University of Chicago Tinted Colors
#'
#' Tinted palette from the graphic identity guidelines. Use this for slightly
#' different colors from the secondary palette.
#' 
#' @export
#'
uc_tinted_pal <- c("#FFB547", "#D49464", "#B1746F",
                  "#ADB17D", "#8A8B79", "#5B8FA8", "#725663")

#' University of Chicago Shaded Colors
#'
#' Shaded palette from the graphic identity guidelines. Use this for some darker
#' colors from the secondary palette
#' 
#' @export
#'
uc_shade_pal <- c("#C68220", "#9A5324", "#642822",
                  "#616530", "#3E3E23", "#0F425C")

#' University of Chicago Secondary Colors
#'
#' Violate palette from the graphic identity guidelines. use this for
#' highlighting bad things.
#' 
#' @export
#'
uc_violate_pal <- c("#47B5FF", "#FF3399")

#' My Own Palette
#'
#' A random palette of colors that I put together that are all sort of blue.
#' Probably not good for much of anything.
#' 
#' @export
#'
te_pal <- c("#2A5B96", "#1868AA", "#7EBCEB",
            "#AADAFF", "#9AD1FA")

#' 2023 Palette
#'
#' A palette of colors for 2023, includes "skyblue", "coral", "seafoam green",
#' "mauve", and a slate grey. Good for visualizations.
#' 
#' @export
#'
twenty_twenty_three_pal <- c("#87CEEB", "#9FE2BF", "#FA8072",
                             "#CC8899", "#D3D3D3")
#' Count Unique Values
#'
#' Counts the unique values in a vector or vector like object
#' such as a data frame.
#'
#' @param x A vector like object such as a list or data frame column
#' @export
#' 
howmuch <- function(x){
  n <- length(unique(x))
  return(n)
}


#' View Palette of Colors
#'
#' View the palette of colors that you specify.
#'
#' @param x A palette of colors in a vector.
#' @export
#' 
viewpal <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
    ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}

#' Edge list from Matrix
#' 
#' Creates an edge list from an adjacency matrix.
#' @export
#' 
matrix2edges <- function(mat, symmetric = TRUE,
                         diagonal = FALSE, text = FALSE) {
    #create edge list from matrix
    # if symmetric duplicates are removed
    mat <- as.matrix(mat)
    id <- is.na(mat) # used to allow missing
    mat[id] <- "nna"
    if(symmetric) {mat[lower.tri(mat)] <- "na"} # use to allow missing values
    if(!diagonal) {diag(mat)<-"na"}
    obj<-reshape2::melt(mat)
    colnames(obj)<-c("source","target","value")
    obj<-obj[!obj$value == "na",]
    obj$value[obj$value == "nna"] <- NA
    if(!text){obj$value <- as.numeric(as.character(obj$value))
    }
    return(obj)
    # remove duplicates
}

#' Summarize a Dataframe
#'
#' I found this in some library called "psych".
#' @param x A dataframe
#' @export
#' 
describe <- function(x, na.rm = TRUE, interp = FALSE, skew = TRUE,
                     ranges = TRUE, trim = 0.1, type = 3, check = TRUE,
                     fast = NULL, quant = NULL, IQR = FALSE, omit = FALSE,
                     data = NULL) {
    if (inherits(x, "formula")) {
        ps <- fparse(x)
        if (missing(data)) {
            x <- get(ps$y)
            group <- x[, ps$x]
        }
        else {
            x <- data[ps$y]
            group <- data[ps$x]
        }
        describeBy(x, group = group, na.rm = na.rm, interp = interp,
            skew = skew, ranges = ranges, trim = trim, type = type,
            check = check, fast = fast, quant = quant, IQR = IQR,
            omit = omit, data = data)
    }
    else {
        cl <- match.call()
        valid <- function(x) {
            sum(!is.na(x))
        }
        if (!na.rm)
            x <- na.omit(x)
        if (is.null(fast)) {
            if (prod(dim(x)) > 10^7) {
                fast <- TRUE
            }
            else {
                fast <- FALSE
            }
        }
        if (fast) {
            skew <- FALSE
        }
        numstats <- 10 + length(quant) + IQR
        if (NCOL(x) < 2) {
            if (is.data.frame(x)) {
                if (!is.numeric(x[, 1])) {
                  warning("You were trying to describe a non-numeric data.frame
                          or vector which describe converted to numeric.")
                  x[, 1] <- as.numeric(as.factor(x[, ]))
                }
                x <- x[, 1]
            }
            len <- 1
            nvar <- 1
            stats = matrix(rep(NA, numstats), ncol = numstats)
            stats[1, 1] <- valid(x)
            stats[1, 2] <- mean(x, na.rm = na.rm)
            stats[1, 10] <- sd(x, na.rm = na.rm)
            if (interp) {
                stats[1, 3] <- interp.median(x, na.rm = na.rm)
            }
            else {
                stats[1, 3] <- median(x, na.rm = na.rm)
            }
            stats[1, 9] <- mean(x, na.rm = na.rm, trim = trim)
            stats[1, 4] <- min(x, na.rm = na.rm)
            stats[1, 5] <- max(x, na.rm = na.rm)
            stats[1, 6] <- skew(x, na.rm = na.rm, type = type)
            stats[1, 7] <- mad(x, na.rm = na.rm)
            stats[1, 8] <- kurtosi(x, na.rm = na.rm, type = type)
            vars <- 1
            if (!is.null(quant)) {
                Qnt <- quantile(x, prob = quant, na.rm = TRUE)
                stats[1, (IQR + 11):numstats] <- t(Qnt)
            }
            if (IQR) {
                Quart <- t(quantile(x, prob = c(0.25, 0.75),
                  na.rm = TRUE))
                Iqr <- Quart[, 2] - Quart[, 1]
                stats[1, 11] <- Iqr
            }
            rownames(stats) <- "X1"
        }
        else {
            nvar <- ncol(x)
            stats = matrix(rep(NA, nvar * numstats), ncol = numstats)
            if (is.null(colnames(x)))
                colnames(x) <- paste0("X", 1:ncol(x))
            rownames(stats) <- colnames(x)
            stats[, 1] <- apply(x, 2, valid)
            vars <- c(1:nvar)
            select <- 1:nvar
            if (!is.matrix(x) && check) {
                for (i in 1:nvar) {
                  if (!is.numeric(x[[i]])) {
                    if (fast) {
                      x[[i]] <- NA
                    }
                    else {
                      if (omit) {
                        select[i] <- NA
                      }
                      if (is.factor(unlist(x[[i]])) | is.character(unlist(x[[i]]))) {
                        x[[i]] <- as.numeric(as.factor(x[[i]]))
                        rownames(stats)[i] <- paste(rownames(stats)[i],
                          "*", sep = "")
                      }
                      else {
                        x[[i]] <- NA
                      }
                    }
                  }
                }
            }
            select <- select[!is.na(select)]
            x <- as.matrix(x[, select])
            vars <- vars[select]
            stats <- stats[select, ]
            if (!is.numeric(x)) {
                message("Converted non-numeric matrix input to numeric.
                        Are you sure you wanted to do this. Please check your data")
                x <- matrix(as.numeric(x), ncol = nvar)
                rownames(stats) <- paste0(rownames(stats), "*")
            }
            stats[, 2] <- apply(x, 2, mean, na.rm = na.rm)
            stats[, 10] <- apply(x, 2, sd, na.rm = na.rm)
            if (skew) {
                stats[, 6] <- skew(x, na.rm = na.rm, type = type)
                stats[, 8] <- kurtosi(x, na.rm = na.rm, type = type)
            }
            if (ranges) {
                if (fast) {
                  stats[, 4] <- apply(x, 2, min, na.rm = na.rm)
                  stats[, 5] <- apply(x, 2, max, na.rm = na.rm)
                }
                else {
                  stats[, 4] <- apply(x, 2, min, na.rm = na.rm)
                  stats[, 5] <- apply(x, 2, max, na.rm = na.rm)
                  stats[, 7] <- apply(x, 2, mad, na.rm = na.rm)
                  stats[, 9] <- apply(x, 2, mean, na.rm = na.rm,
                    trim = trim)
                  if (interp) {
                    stats[, 3] <- apply(x, 2, interp.median,
                      na.rm = na.rm)
                  }
                  else {
                    stats[, 3] <- apply(x, 2, median, na.rm = na.rm)
                  }
                }
            }
            if (!is.null(quant)) {
                Qnt <- apply(x, 2, quantile, prob = quant, na.rm = TRUE)
                stats[, (IQR + 11):numstats] <- t(Qnt)
            }
            if (IQR) {
                Quart <- t(apply(x, 2, quantile, prob = c(0.25,
                  0.75), na.rm = TRUE))
                Iqr <- Quart[, 2] - Quart[, 1]
                stats[, 11] <- Iqr
            }
        }
        if (numstats > (10 + IQR)) {
            colnames(stats)[(11 + IQR):numstats] <- paste0("Q",
                quant[1:length(quant)])
        }
        if (fast) {
            answer <- data.frame(vars = vars, n = stats[, 1],
                mean = stats[, 2], sd = stats[, 10], se = stats[,
                  10]/sqrt(stats[, 1]))
        }
        if (skew) {
            if (ranges) {
                answer <- data.frame(vars = vars, n = stats[,
                  1], mean = stats[, 2], sd = stats[, 10], median = stats[,
                  3], trimmed = stats[, 9], mad = stats[, 7],
                  min = stats[, 4], max = stats[, 5], range = stats[,
                    5] - stats[, 4], skew = stats[, 6], kurtosis = stats[,
                    8], se = stats[, 10]/sqrt(stats[, 1]))
            }
            else {
                answer <- data.frame(vars = vars, n = stats[,
                  1], mean = stats[, 2], sd = stats[, 10], skew = stats[,
                  6], kurtosis = stats[, 8], se = stats[, 10]/sqrt(stats[,
                  1]))
            }
        }
        else {
            if (ranges) {
                answer <- data.frame(vars = vars, n = stats[,
                  1], mean = stats[, 2], sd = stats[, 10], min = stats[,
                  4], max = stats[, 5], range = stats[, 5] -
                  stats[, 4], se = stats[, 10]/sqrt(stats[, 1]))
            }
            else {
                answer <- data.frame(vars = vars, n = stats[,
                  1], mean = stats[, 2], sd = stats[, 10], se = stats[,
                  10]/sqrt(stats[, 1]))
            }
        }
        if (IQR)
            answer <- data.frame(answer, IQR = stats[, 11])
        if (numstats > (10 + IQR)) {
            if (nvar > 1) {
                answer <- data.frame(answer, stats[, (IQR + 11):numstats])
            }
            else {
                answer <- data.frame(answer, t(stats[, (IQR +
                  11):numstats]))
            }
        }
        class(answer) <- c("psych", "describe", "data.frame")
        return(answer)
    }
}

#' Round Dataframe Values
#'
#' Rounds the numeric columns in a data frame to the
#' specified digit. Good for outputting tables in an
#' RMarkdown document.
#'
#' @param x A dataframe
#' @param digits The number of digits to round to
#' @returns A dataframe with numeric columns rounded
#' @export
#'
#' @examples
#' df <- round_df(df)
#'
round_df <- function(x, digits) {
    # round all numeric variables
    # x: data frame
    # digits: number of digits to round
    numeric_columns <- sapply(x, mode) == 'numeric'
    x[numeric_columns] <-  round(x[numeric_columns], digits)
    x
}

#' Calculate Network Statistics and Pack into DataFrame
#'
#' Calculates a number of network statistics and packs them
#' all into a dataframe.
#' @param g An igraph network object
#' @returns Dataframe with network statistics from network.
#' @export
#'
#' @examples
#' df <- net2df(g)
#'
net2df <- function(g){
    if(!is.igraph(g)) stop("g must be an igraph network")

    net_df <- tibble(name = names(V(g)),
                     short_name = str_trunc(names(V(g)), 30),
                         indegree = degree(g, mode = "in"),
                         outdegree = degree(g, mode = "out"),
                         eig = eigen_centrality(g, scale = TRUE)$vector)
    return(net_df)
}

#' List to String
#'
#' Convert a list to a string with commas between
#' list elements.
#' @param x A list
#' @returns A string
#' @export
#'
ls2str <- function(x){
  paste(unlist(x), collapse = ", ")
}

#' Scatterplot Theme
#'
#' A minimal Scatter plot theme for ggplot and sets font to Roboto
#' @export
#' 
#' @example
#' ggplot(data, aes(x = X, y = Y)) + geom_point() + scatter_theme
#'
scatter_theme <- ggplot2::theme(text = ggplot2::element_text(family = "Roboto", size = 10),
                       axis.text.x = ggplot2::element_text(size = 10),
                       axis.text.y = ggplot2::element_text(size = 10),
                       axis.title.x = ggplot2::element_text(size = 10),
                       axis.title.y = ggplot2::element_text(size = 10))

#' General Purpose Plot Theme
#' 
#' Use for plots in general to make them look better
#' @export
#' 
te_theme <- ggplot2::theme(text = ggplot2::element_text(family = "Roboto", size = 10),
                       axis.text.x = ggplot2::element_text(size = 10),
                       axis.text.y = ggplot2::element_text(size = 10),
                       axis.title.x = ggplot2::element_text(size = 10),
                       axis.title.y = ggplot2::element_text(size = 10))

#' Blank X and Y Axes
#' 
#' @export
#' 
blank_theme <- ggplot2::theme(text = ggplot2::element_text(family = "Roboto", size = 10), 
                     axis.title.y = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank())


#' Negation
#'
#' A "not in" operator for working with lists.
#' 
#' @export
#' @examples
#' bad <- c("this", "that", "other_thing")
#' good <- c("this", "cat", "that", "dog", "other_thing", "bat")
#' good %!in% bad
#'
`%!in%` <- Negate(`%in%`)

#' Get Mode
#' 
#' Get the mode from a vector of values
#' @export
#'
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Inverse Logit
#'
#' Returns odds ratio from logit
#' @export
#'
inv.logit <- function(x) {
  1/(1+exp(-x))
}

#' Logit to Progbability
#' 
#' Converts logits to probabilities
#' @export
#' 
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#' List of States
#' @noRd 
us_states <- c(
  "Alabama", "Alaska", "Arizona", "Arkansas", "California",
  "Colorado", "Connecticut", "Delaware", "Florida", "Georgia",
  "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
  "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
  "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
  "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
  "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
  "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
  "South Dakota", "Tennessee", "Texas", "Utah", "Vermont",
  "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
)

#' List of State Abbreviations
#' @noRd
state_abbreviations <- c(
  "AL", "AK", "AZ", "AR", "CA",
  "CO", "CT", "DE", "FL", "GA",
  "HI", "ID", "IL", "IN", "IA",
  "KS", "KY", "LA", "ME", "MD",
  "MA", "MI", "MN", "MS", "MO",
  "MT", "NE", "NV", "NH", "NJ",
  "NM", "NY", "NC", "ND", "OH",
  "OK", "OR", "PA", "RI", "SC",
  "SD", "TN", "TX", "UT", "VT",
  "VA", "WA", "WV", "WI", "WY"
)

#' List of States and their    Abbreviations
#' @describeIn Create a named vector mapping state names to abbreviations
#' @noRd 
state_dict <- c(
  "Alabama" = "AL",
  "Alaska" = "AK",
  "Arizona" = "AZ",
  "Arkansas" = "AR",
  "California" = "CA",
  "Colorado" = "CO",
  "Connecticut" = "CT",
  "Delaware" = "DE",
  "Florida" = "FL",
  "Georgia" = "GA",
  "Hawaii" = "HI",
  "Idaho" = "ID",
  "Illinois" = "IL",
  "Indiana" = "IN",
  "Iowa" = "IA",
  "Kansas" = "KS",
  "Kentucky" = "KY",
  "Louisiana" = "LA",
  "Maine" = "ME",
  "Maryland" = "MD",
  "Massachusetts" = "MA",
  "Michigan" = "MI",
  "Minnesota" = "MN",
  "Mississippi" = "MS",
  "Missouri" = "MO",
  "Montana" = "MT",
  "Nebraska" = "NE",
  "Nevada" = "NV",
  "New Hampshire" = "NH",
  "New Jersey" = "NJ",
  "New Mexico" = "NM",
  "New York" = "NY",
  "North Carolina" = "NC",
  "North Dakota" = "ND",
  "Ohio" = "OH",
  "Oklahoma" = "OK",
  "Oregon" = "OR",
  "Pennsylvania" = "PA",
  "Rhode Island" = "RI",
  "South Carolina" = "SC",
  "South Dakota" = "SD",
  "Tennessee" = "TN",
  "Texas" = "TX",
  "Utah" = "UT",
  "Vermont" = "VT",
  "Virginia" = "VA",
  "Washington" = "WA",
  "West Virginia" = "WV",
  "Wisconsin" = "WI",
  "Wyoming" = "WY"
)


#' Convert State to Abbreviation
#' 
#' Function to onvert state names to abbreviations
#' @export
#' @example
#' state_name <- "California"
#' abbreviation <- state_to_abbreviation(state_name)
#' cat(paste(state_name, "is abbreviated as", abbreviation))
#'
state2abbr <- function(x) {

  x <- trimws(x)

  if(x %in% us_states) {
    x <- state_dict[[x]][[1]]
  } else if (x %in% state_dict) {
     x <- x
  } else {
    x <- NA
  }
  return(x)
}
