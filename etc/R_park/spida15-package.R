#' The Adult dataset
#' 
#' This dataset contains census data and is available from the UCI Machine
#' Learning Repository. There are three variables with missing values.
#' 
#' The values of the nominal attributes are as follows: \cr
#' 
#' workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov,
#' State-gov, Without-pay, Never-worked. \cr education: Preschool, 1st-4th,
#' 5th-6th, 7th-8th, 9th, 10th, 11th, 12th, HS-grad, Some-college, Assoc-voc,
#' Assoc-acdm, Bachelors, Masters, Prof-school, Doctorate \cr marital-status:
#' Married-civ-spouse, Divorced, Never-married, Separated, Widowed,
#' Married-spouse-absent, Married-AF-spouse. \cr occupation: Tech-support,
#' Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty,
#' Handlers -cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing,
#' Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces. \cr
#' relationship: Wife, Own-child, Husband, Not-in-family, Other-relative,
#' Unmarried. \cr race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other,
#' Black. \cr sex: Female, Male. \cr native-country: United-States, Cambodia,
#' England, Puerto-Rico, Canada, Germany, Outlying- US (Guam-USVI-etc), India,
#' Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy,
#' Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France,
#' Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary,
#' Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador,
#' Trinadad&Tobago, Peru, Hong, Holand-Netherlands. \cr Income class: >50K,
#' <=50K. \cr
#' 
#' @name adult
#' @docType data
#' @format A data frame with 48842 observations on the following 14 variables.
#' \describe{ \item{list("V1")}{age: continuous variable}
#' \item{list("V2")}{workclass: nominal variable} \item{list("V3")}{fnlwgt:
#' continuous variable} \item{list("V4")}{education: nominal variable}
#' \item{list("V5")}{marital-status: nominal variable }
#' \item{list("V6")}{occupation: nominal variable}
#' \item{list("V7")}{relationship: nominal variable } \item{list("V8")}{race:
#' nominal variable} \item{list("V9")}{sex: nominal variable}
#' \item{list("V10")}{capital-gain: continuous variable}
#' \item{list("V11")}{capital-loss: continuous variable}
#' \item{list("V12")}{hours-per-week: continuous variable}
#' \item{list("V13")}{native-country: nominal variable}
#' \item{list("V14")}{Income class: nominal variable} }
#' @source The UCI Machine Learning Database Repository at: \cr
#' \url{http://archive.ics.uci.edu/ml/}
#' @keywords datasets
#' @examples
#' 
#' data(adult)
#' summary(adult)
#' %%## maybe str(adult) ; plot(adult) ...
#' 
NULL




#' Confidence ellipse for estimated parameters in Wald test
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param obj %% ~~Describe \code{obj} here~~
#' @param which.coef %% ~~Describe \code{which.coef} here~~
#' @param levels %% ~~Describe \code{levels} here~~
#' @param Scheffe %% ~~Describe \code{Scheffe} here~~
#' @param dfn %% ~~Describe \code{dfn} here~~
#' @param center.pch %% ~~Describe \code{center.pch} here~~
#' @param center.cex %% ~~Describe \code{center.cex} here~~
#' @param segments %% ~~Describe \code{segments} here~~
#' @param xlab %% ~~Describe \code{xlab} here~~
#' @param ylab %% ~~Describe \code{ylab} here~~
#' @param las %% ~~Describe \code{las} here~~
#' @param col %% ~~Describe \code{col} here~~
#' @param lwd %% ~~Describe \code{lwd} here~~
#' @param lty %% ~~Describe \code{lty} here~~
#' @param add %% ~~Describe \code{add} here~~
#' @param \dots %% ~~Describe \code{\dots} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (obj, which.coef = 1:2, levels = 0.95, Scheffe = FALSE, 
#'     dfn = 2, center.pch = 19, center.cex = 1.5, segments = 51, 
#'     xlab, ylab, las = par("las"), col = palette()[2], lwd = 2, 
#'     lty = 1, add = FALSE, ...) 
#' {
#'     obj <- obj[[1]]
#'     coef <- obj$coef[which.coef]
#'     xlab <- if (missing(xlab)) 
#'         paste(names(coef)[1], "coefficient")
#'     ylab <- if (missing(ylab)) 
#'         paste(names(coef)[2], "coefficient")
#'     dfd <- obj$anova$denDF
#'     shape <- obj$vcov[which.coef, which.coef]
#'     ret <- ell(coef, shape, sqrt(dfn * qf(levels, dfn, dfd)))
#'     colnames(ret) <- c(xlab, ylab)
#'     ret
#'   }
#' 
NULL





#' Artificial data set on Coffee Consumption, Heart Damage Index and Stress
#' 
#' Artifical data on 20 subjects to illustrate issues of causality with
#' observational data
#' 
#' %% ~~ If necessary, more details than the __description__ above ~~
#' 
#' @name coffee
#' @docType data
#' @format A data frame with 20 observations on the following 5 variables.
#' \describe{ \item{list("Occupation")}{a factor with levels
#' \code{Grad_Student} \code{Professor} \code{Student}}
#' \item{list("Coffee")}{Index of coffee consumption}
#' \item{list("Stress")}{Index of stress} \item{list("Heart")}{Heart Index:
#' higher indicates a less healthy heart} \item{list("Stress2")}{Index of
#' stress that is less reliable than 'Stress'} }
#' @references %% ~~ possibly secondary sources and usages ~~
#' @keywords datasets
#' @examples
#' 
#' data(coffee)
#' ## maybe str(coffee) ; plot(coffee) ...
#' 
NULL





#' Longitudinal study of schizophrenia symptoms with non-randomized treatment
#' 
#' This is a subset of a data set analyzed by Y. Korman on schizophrenia
#' symptoms among patients who were assessed at 6 annual checkups. At each
#' checkup, the type of drug currently prescribed by the treating physician was
#' recorded.
#' 
#' %% ~~ If necessary, more details than the __description__ above ~~
#' 
#' @name Drugs
#' @docType data
#' @format A data frame with 318 observations on 53 subjects each observed at 6
#' annual checkups with the following 11 variables.  \describe{
#' \item{list("age1")}{age in years at first checkup}
#' \item{list("yrsill1")}{years since initial diagnosis of schizophrenia at
#' time of first checkup} \item{list("pos")}{number of positive symptoms
#' (symptoms that are not typically present in a normal subject}
#' \item{list("neg")}{number of negative symptoms (characteristics that are
#' absent but normally present in normal subject} \item{list("gen")}{other
#' symptoms} \item{list("total")}{sum of previous three}
#' \item{list("year")}{time of measurement (1 to 6)} \item{list("drug")}{a
#' factor with levels \code{Atypical} \code{Clozapine} \code{Typical}}
#' \item{list("status")}{a factor with levels \code{Married}
#' \code{Separated/Divorced} \code{Single}} \item{list("Sex")}{a factor with
#' levels \code{F} \code{M}} \item{list("Subj")}{a factor identifying each
#' subject} }
#' @references %% ~~ possibly secondary sources and usages ~~
#' @source %% ~~ reference to a publication or URL from which the data were
#' obtained ~~
#' @keywords datasets
#' @examples
#' 
#' data(Drugs)
#' ## maybe str(Drugs) ; plot(Drugs) ...
#' 
NULL





#' Generalized inverse -- test version
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x %% ~~Describe \code{x} here~~
#' @param tol %% ~~Describe \code{tol} here~~
#' @return %% ~Describe the value returned
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (x, tol = .Machine$double.eps * d[1] * max(nrow(x), 
#'     ncol(x))) 
#' {
#'     r = svd(x)
#'     d = r$d
#'     dinv = ifelse(d < tol, 0, 1/d)
#'     ret = r$v %*% (dinv * t(r$u))
#'     dimnames(ret) = dimnames(x)[2:1]
#'     ret
#'   }
#' 
NULL





#' Generalized inverse -- using version in MASS
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param \dots %% ~~Describe \code{\dots} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (...) 
#' {
#'     require(MASS)
#'     MASS::ginv(...)
#'   }
#' 
NULL





#' Mark for deletion since it now exists in base
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param pattern %% ~~Describe \code{pattern} here~~
#' @param x %% ~~Describe \code{x} here~~
#' @param \dots %% ~~Describe \code{\dots} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (pattern, x, ...) 
#' match(1:length(x), grep(pattern, x, ...), 0) > 0
#' 
NULL





#' Gsp -- older version
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x %% ~~Describe \code{x} here~~
#' @param k %% ~~Describe \code{k} here~~
#' @param degree %% ~~Describe \code{degree} here~~
#' @param smooth %% ~~Describe \code{smooth} here~~
#' @param intercept %% ~~Describe \code{intercept} here~~
#' @param debug %% ~~Describe \code{debug} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (x, k, degree = rep(3, length(k) + 1), smooth = rep(2, 
#'     length(k)), intercept = FALSE, debug = FALSE) 
#' {
#'     if (!debug) 
#'         disp = function(x) invisible(0)
#'     kernel = function(L) {
#'         QR = qr(t(L))
#'         qr.Q(QR, complete = T)[, -seq_len(QR$rank)]
#'     }
#'     Xfull = function(x, k, degree) {
#'         if (length(degree) != length(k) + 1) 
#'             stop("length( degree ) must = length( k ) + 1")
#'         Xmat = function(x, degree) {
#'             do.call("cbind", lapply(0:degree, function(i) if (i == 
#'                 0) 
#'                 rep(1, length(x))
#'             else x^i))
#'         }
#'         k = sort(k)
#'         g = cut(x, c(-Inf, k, Inf))
#'         Xraw = Xmat(x, max(degree))
#'         do.call("cbind", lapply(seq_along(degree), function(iint) (g == 
#'             levels(g)[iint]) * Xraw[, 1:(degree[iint] + 1), drop = F]))
#'     }
#'     X0 = Xfull(0, k, degree)
#'     if (FALSE) {
#'         nk = length(k)
#'         extend = (max(k) - min(k) + 1)/nk
#'         ints = c(min(k) - extend, sort(k), max(k) + extend)
#'         xs = approx(ints, n = (max(degree) + 1) * (nk + 1))$y
#'         xs = c(-xs, xs)
#'         X.full = Xfull(xs, k, degree)
#'         disp(X.full)
#'     }
#'     fs = list(f = function(x) {
#'         c(1, x, x^2, x^3, x^4)
#'     }, f1 = function(x) {
#'         c(0, 1, 2 * x, 3 * x^2, 4 * x^3)
#'     }, f2 = function(x) {
#'         c(0, 0, 2, 6 * x, 12 * x^2)
#'     }, f3 = function(x) {
#'         c(0, 0, 0, 6, 24 * x)
#'     }, f4 = function(x) {
#'         c(0, 0, 0, 0, 24)
#'     })
#'     nints = length(k) + 1
#'     endcols = cumsum(degree + 1)
#'     disp(endcols)
#'     startcols = c(1, endcols[-length(endcols)] + 1)
#'     disp(startcols)
#'     ncols = degree + 1
#'     disp(ncols)
#'     Lsmooth = matrix(0, ncol = ncol(X0), nrow = sum(smooth + 
#'         1))
#'     irow = 0
#'     for (i in seq_along(k)) {
#'         disp(i)
#'         knot = k[i]
#'         iplus = startcols[i]:endcols[i]
#'         disp(iplus)
#'         iminus = startcols[i + 1]:endcols[i + 1]
#'         disp(iminus)
#'         for (j in 0:smooth[i]) {
#'             irow = irow + 1
#'             Lsmooth[irow, iplus] = (fs[[j + 1]](knot)[1:ncols[i]])
#'             Lsmooth[irow, iminus] = (-fs[[j + 1]](knot)[1:ncols[i + 
#'                 1]])
#'         }
#'     }
#'     disp(Lsmooth)
#'     disp(Lsmooth %*% kernel(Lsmooth))
#'     if (intercept == FALSE) 
#'         Lsmooth = rbind(Lsmooth, X0)
#'     H = kernel(Lsmooth)
#'     Xret = Xfull(x, k, degree) %*% H
#'     Xret
#'   }
#' 
NULL





#' Math achievement and ses in a sample of 160 U.S. schools from the 1982 study
#' ``High School and Beyond''. %% ~~ data name/kind ... ~~
#' 
#' This is a classical data set from the field of education used to illustrate
#' multilevel data and models. It is used in the first edition of Bryk and
#' Raudenbush. \code{hsfull} is the complete data set with 160 high schools,
#' \code{hs} is a random subset of 40 high schools, \code{hs1} is a random
#' subset of 80 schools and \code{h2} contains the complement of \code{hs1}.
#' These two subsets can be used to illustrate split sample validation: develop
#' a model on one half of the data and assess its performance on the other.
#' 
#' Each row consists of the data for one student.  \code{hsfull} is the
#' complete data set. \code{hs1} and \code{hs2} are complementary split halves
#' of the schools in the data. \code{hs} is a selection of 40 schools which
#' seems to be a good number of clusters for presentations in class. %% ~~ If
#' necessary, more details than the __description__ above ~~
#' 
#' @name hsfull
#' @aliases hsfull hs hs1 hs2
#' @docType data
#' @format A data frame with 7185 observations on the following 9 variables.
#' \describe{ \item{list("school")}{school id} \item{list("mathach")}{measure
#' of math achievment} \item{list("ses")}{socio-economic status of family}
#' \item{list("Sex")}{a factor with levels \code{Female} \code{Male}}
#' \item{list("Minority")}{a factor with levels \code{No} \code{Yes}}
#' \item{list("Size")}{the size of the school} \item{list("Sector")}{a factor
#' with levels \code{Catholic} \code{Public}} \item{list("PRACAD")}{a measure
#' of the priority given by the school to academic subjects}
#' \item{list("DISCLIM")}{a measure of the disciplinary climate in the school}
#' }
#' @references %% ~~ possibly secondary sources and usages ~~ Raudenbush,
#' Stephen and Bryk, Anthony (2002), Hierarchical Linear Models: Applications
#' and Data Analysis Methods, Sage (chapter 4).
#' @source Bryk and Raudenbush (COMPLETE)
#' @keywords datasets
#' @examples
#' 
#' xqplot(hsfull)
#' xqplot( up( hsfull, ~ school) )
#' ## maybe str(hsfull) ; plot(hsfull) ...
#' 
NULL





#' Articial data with classical outliers.
#' 
#' Articial data on health as predicted by height and weight with outliers to
#' show the effect of different types of outliers.
#' 
#' \code{hw} has four datasets identified by \code{Outlier} indicating the
#' 'type' of outlier depicted in each dataset: none, Type 1: large residual for
#' y with typical value of x, Type 2: small residual for y with atypical value
#' of x, and Type 3: atypical x and poorly fitting y.  \code{hwoutliers}
#' contains the rows of 'acceptable' data and one row for each of the three
#' types of outlier. This format lends itself better to 3-D plots.  Note that
#' this nomenclature for outlier is not widely adopted!
#' 
#' @name hw
#' @aliases hw hwoutliers
#' @docType data
#' @format A data frame with 4 datasets, indexed by \code{Type}, each with 15
#' 'good' observations. The last 3 datasets eadh contain, in addition to the
#' 'good' data, an outlier.  \describe{ \item{list("Weight")}{a measure of body
#' weight} \item{list("Height")}{a measure of height} \item{list("Health")}{an
#' index of health: higher is better} \item{list("Type")}{numerical type, 0, 1,
#' 2 or 3} \item{list("Outlier")}{a factor with levels \code{none} \code{Type
#' 1} \code{Type 2} \code{Type 3}}
#' 
#' }
#' @references %% ~~ possibly secondary sources and usages ~~
#' @source %% ~~ reference to a publication or URL from which the data were
#' obtained ~~
#' @keywords datasets
#' @examples
#' 
#' data(hw)
#' ## maybe str(hw) ; plot(hw) ...
#' 
NULL





#' Respiratory infections among Indonesian children. Longitudinal observational
#' study with no treatment
#' 
#' 275 preschool children in Indonesia were examined for up to 6 consecutive
#' quarters for the present of respiratory infection. Also recorded presence of
#' xerophthalmia, an eye condition indicating a vitamin A deficiency.
#' 
#' Desc: 275 preschool children in Indonesia were examined for up to 6
#' consecutive quarters for the present of respiratory infection.  Also
#' recorded presence of xerophthalmia, an eye condition indicating a vitamin A
#' deficiency.
#' 
#' @name Indonesia
#' @docType data
#' @format A data frame with 1194 observations on 275 children each measured on
#' 1 to 6 occasions on the following 10 variables.  \describe{
#' \item{list("id")}{subject id} \item{list("resp")}{indicator for a
#' respiratory infection} \item{list("age")}{age in months at time of
#' measurement} \item{list("baseline_age")}{age at start of program}
#' \item{list("xerophthalmia")}{indicator for the presence of xerophthalmia}
#' \item{list("sex")}{indicator for sex} \item{list("height_for_age")}{height
#' relative to age} \item{list("stunted")}{indicator} \item{list("time")}{time
#' of measurement in quarters (3 month intervals)}
#' \item{list("season")}{1=Spring, 2=Summer, 3=Autumn, 4=Winter} }
#' @references %% ~~ possibly secondary sources and usages ~~
#' @source Subset of cohort from Summer et al. (1984) reported in Diggle et al.
#' (2002) 'Analysis of Longitudinal Data', 2nd ed.  See: Zeger and Karim, JASA
#' (1991)
#' @keywords datasets
#' @examples
#' 
#' data(Indonesia)
#' ## maybe str(Indonesia) ; plot(Indonesia) ...
#' 
NULL





#' Longitudinal study of IQ after traumatic brain injuries
#' 
#' A subset of data gathered on verbal and performance IQ of patients
#' recovering from coma after traumatic brain injuries
#' 
#' \code{iq} is a subset of the original data, \code{iqsim} is simulated set of
#' 10 hypothetical observations on one subject. %% ~~ If necessary, more
#' details than the __description__ above ~~
#' 
#' @name iq
#' @aliases iq iqsim
#' @docType data
#' @format A longitudinal with 331 observations 200 subjects measured on a
#' varying number of occasions (ranging from 1 to 5) on the following 7
#' variables.  \describe{ \item{list("DAYSPC")}{time of measurement in days
#' post recovery from coma} \item{list("DCOMA")}{duration of coma rounded to
#' nearest day} \item{list("SEX")}{a factor with levels \code{Female}
#' \code{Male}} \item{list("AgeIn")}{age at time of injury}
#' \item{list("ID")}{identifying subjects} \item{list("PIQ")}{performance (or
#' mathematical) IQ} \item{list("VIQ")}{verbal IQ} }
#' @references %% ~~ possibly secondary sources and usages ~~
#' @source %% ~~ reference to a publication or URL from which the data were
#' obtained ~~
#' @keywords datasets
#' @examples
#' 
#' data(iq)
#' ## maybe str(iq) ; plot(iq) ...
#' 
NULL





#' Synomym for simpleKey
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param \dots %% ~~Describe \code{\dots} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (...) 
#' simpleKey(...)
#' 
NULL





#' Effectiveness of migraine treatment
#' 
#' Subset of data on migraine treatments collected by Tammy Kostecki-Dillon.
#' The data consists of headache logs kept by 133 patients in a treatment
#' program in which bio-feedback was used to attempt to reduce migraine
#' frequency and severity. Patients entered the program at different times over
#' a period of about 3 years. Patients were encouraged to begin their logs four
#' weeks before the onset of treatment and to continue for one month
#' afterwards, but only 55 patients have data preceding the onset of treatment.
#' 
#' %% ~~ If necessary, more details than the __description__ above ~~
#' 
#' @name migraines
#' @docType data
#' @format A data frame with 4152 observations on the following 9 variables.
#' \describe{ \item{list("id")}{patient id} \item{list("ha")}{headache
#' indictor: 1 if a headache has occurred on the day recorded, 0 otherwise}
#' \item{list("time")}{time in days relative to the onset of treatment which
#' occurs at time == 0} \item{list("dos")}{day of study in days from January 1
#' in the first year of the study} \item{list("hatype")}{type of migraine
#' headache experienced by a subject: \code{Aura} \code{Mixed} \code{No Aura}}
#' \item{list("female")}{an indicator variable} \item{list("age")}{age at onset
#' of treatment} \item{list("airq")}{a measure of air quality}
#' \item{list("medication")}{medication status: \code{Reduced}
#' \code{Continuing} \code{None}} }
#' @references %% ~~ possibly secondary sources and usages ~~
#' @source %% ~~ reference to a publication or URL from which the data were
#' obtained ~~
#' @keywords datasets
#' @examples
#' 
#' data(migraines)
#' ## maybe str(migraines) ; plot(migraines) ...
#' 
NULL





#' Mothers' Stress and Children's Morbidity (MSCM) study: Longitudinal
#' dichotomous response
#' 
#' The Mothers' Stress and Children's Morbidity (MSCM) study is a longitudinal
#' observational study of the causal effect of maternal stress on childhood
#' illness (Zeger and Liang 1986, pp. 125-128).
#' 
#' The Mothers' Stress and Children's Morbidity (MSCM) study is a longitudinal
#' observational study of the causal effect of maternal stress on childhood
#' illness (Zeger and Liang 1986, pp. 125-128). In the MSCM data, the daily
#' prevalence of childhood illness was 14%. Consider the following causal
#' questions. How would the prevalence change if an ongoing, fully effective
#' stress-reduction intervention program were instituted? How would prevalence
#' change if conditions worsened and all mothers were subjected to substantial
#' stress on a daily basis? To attempt to answer these questions, we use a
#' formal model for causal effects in longitudinal studies introduced by Robins
#' (1986, 1987a,b). This model extends Neyman's (1923) counterfactual causal
#' model for "point" treatment studies to longitudinal studies with
#' time-varying treatments and confounders. We show that the methods for causal
#' inference developed by Robins provide a better justified basis for answering
#' the foregoing causal questions in longitudinal data in general and in the
#' MSCM study in particular than do methods based on generalized estimating
#' equations (GEE's).
#' 
#' @name mscm
#' @docType data
#' @format A data frame with 5010 observations on 167 mother/child dyads each
#' observed on 30 successive days with the following 14 variables.  \describe{
#' \item{list("id")}{id for dyad} \item{list("day")}{day of observation: 1 to
#' 30 for each dyad} \item{list("stress")}{indicator for maternal stress}
#' \item{list("illness")}{indicator for child illness} \item{list("married")}{a
#' factor with levels \code{married} \code{other}} \item{list("education")}{a
#' factor with levels \code{less than high school} \code{some high school}
#' \code{high school graduate} \code{some college} \code{college graduate}}
#' \item{list("employed")}{indicator for employment} \item{list("chlth")}{
#' child health status at baseline: 1=very poor 2=poor 3=fair 4=good 5=very
#' good} \item{list("mhlth")}{mother health status at baseline: 1=very poor
#' 2=poor 3=fair 4=good 5=very good} \item{list("race")}{a factor with levels
#' \code{white} \code{non-white}} \item{list("csex")}{a factor with levels
#' \code{male} \code{female}} \item{list("housize")}{a factor with levels
#' \code{2-3 people} \code{more than 3 people}}
#' \item{list("bIllness")}{proportion of child days ill in first 7 days}
#' \item{list("bStress")}{proportion of maternal days with Stress in first 7
#' days} }
#' @references %% ~~ possibly secondary sources and usages ~~
#' @source Estimation of the Causal Effect of a Time-Varying Exposure on the
#' Marginal Mean of a Repeated Binary Outcome Journal article by James M.
#' Robins, Sander Greenland, Fu-Chang Hu; Journal of the American Statistical
#' Association, Vol. 94, 1999
#' @keywords datasets
#' @examples
#' 
#' data(mscm)
#' ## maybe str(mscm) ; plot(mscm) ...
#' 
NULL





#' Make reorder work on non-factors
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x %% ~~Describe \code{x} here~~
#' @param \dots %% ~~Describe \code{\dots} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (x, ...) 
#' reorder(factor(x), ...)
#' 
NULL





#' Longitudinal response of depressed patients to imipramine and desipramine
#' 
#' One group pre-test post-test design with 2 weeks of placebo followed by up
#' to 3 weeks of treatment.
#' 
#' 
#' From Hedeker:
#' http://www.uic.edu/classes/bstt/bstt513/Kaplan%20Chapter%2012.pdf
#' http://tigger.uic.edu/~hedeker/long.html
#' 
#' Structure: Longitudinal, up to 5 occasions on 66 depressed subjects
#' 
#' To illustrate an HLM application, we will consider data from a psychiatric
#' study described in Reisby et al. (1977). This study focused on the
#' longitudinal relationship between imipramine (IMI) and desipramine (DMI)
#' plasma levels and clinical response in 66 depressed inpatients. Imipramine
#' is the prototypic drug in the series of compounds known as tricyclic
#' antidepressants and is commonly prescribed for the treatment of major
#' depression (Seiden & Dykstra, 1977). Because imipramine biotransforms into
#' the active metabolite desmethylimipramine (or desipramine), measurement of
#' desipramine was also done in this study. Major depression is often
#' classified in terms of two types. The first type, nonendogenous or reactive
#' depression, is associated with some tragic life event such as the death of a
#' close friend or family member, whereas the second type, endogenous
#' depression, is not a result of any specific event and appears to occur
#' spontaneously. It is sometimes held that antidepressant medications are more
#' effective for endogenous depression (Willner, 1985). In this sample, 29
#' patients were classified as nonendogenous, and the remaining 37 patients
#' were deemed to be endogenous.
#' 
#' The study design was as follows. Following a placebo period of 1 week,
#' patients received 225-mg/ day doses of imipramine for 4 weeks. In this
#' study, subjects were rated with the Hamilton depression (HD) rating scale
#' (Hamilton, 1960) twice during the baseline placebo week (at the start and
#' end of this week), as well as at the end of each of the 4 treatment weeks of
#' the study. Plasma level measurements of both IMI and its metabolite DMI were
#' made at the end of each treatment week. The sex and age of each patient were
#' recorded, and a diagnosis of endogenous or nonendogenous depression was made
#' for each patient. Although the total number of subjects in this study was
#' 66, the number of subjects with all measures at each of the weeks
#' fluctuated: 61 atWeek 0 (start of placebo week), 63 at Week 1 (end of
#' placebo week), 65 at Week 2 (end of first drug treatment week), 65 at Week 3
#' (end of second drug treatment week), 63 at Week 4 (end of third drug
#' treatment week), and 58 at Week 5 (end of fourth drug treatment week). Of
#' the 66 subjects, only 46 had complete data at all time points. Thus,
#' complete case analysis under repeated-measures MANOVA, for example, would
#' discard approximately one third of the data set. HLM, alternatively, uses
#' the data that are available from all 66 subjects.
#' 
#' Note from http://healthnet.umassmed.edu/mhealth/HAMD.pdf Hamilton Scale goes
#' from 0 to 60 For the 17-item version, scores can range from 0 to 54. One
#' formulation suggests that scores between 0 and 6 indicate a normal person
#' with regard to depression, scores between 7 and 17 indicate mild depression,
#' scores between 18 and 24 indicate moderate depression, and scores over 24
#' indicate severe depression.
#' 
#' @name Riesby
#' @docType data
#' @format A data frame with 250 observations on the following 7 variables.
#' \describe{ \item{list("id")}{subject identifier} \item{list("hd")}{Hamilton
#' Depression Index} \item{list("week")}{time of measurement in weeks from 0 to
#' 4: 0 is begining of study, 1 is end of 1st placebo week, 2, 3, 4 are at end
#' of treatment weeks} \item{list("sex")}{a factor with levels \code{female}
#' \code{male}} \item{list("lnimi")}{Plasma log concentration of IMI}
#' \item{list("lndmi")}{Plasma log concentration of DMI -- an IMI metabolite}
#' \item{list("depression")}{a factor with levels \code{Endogenous}
#' \code{Reactive}} }
#' @references %% ~~ possibly secondary sources and usages ~~
#' @source %% ~~ reference to a publication or URL from which the data were
#' obtained ~~
#' @keywords datasets
#' @examples
#' 
#' data(Riesby)
#' ## maybe str(Riesby) ; plot(Riesby) ...
#' 
NULL





#' Collection of utility functions for mixed models, etc. for SPIDA 2009+ (beta
#' version)
#' 
#' Collection of utility functions for mixed models including \tabular{ll}{
#' wald: \tab Wald test with functions to generate hypothesis matrices\cr gsp:
#' \tab general spline with any degree and smoothness with additional function
#' 'sc' to contruct hypotheses, smsp - easy smoothing splines with lme and nlme
#' \cr capply: \tab create contextual variables. Also: cvar, dvar, up \cr Misc:
#' \tab vif.lme; xqplot; tab; panel.subgroups (additional level of subgroups in
#' xyplot) \cr }
#' 
#' \tabular{ll}{ Package: \tab spida\cr Type: \tab Package\cr Version: \tab
#' 0.1-1\cr Date: \tab 2010-07-19\cr License: \tab GPL (>= 2)\cr LazyLoad: \tab
#' yes\cr } A local R package currently available from
#' 
#' http://www.math.yorku.ca/~georges/R/spida_0.1.zip
#' 
#' containing functions and some datasets developed for the courses PSYC6140,
#' MATH6630, MATH6643, MATH6627, SCS Workshops on Mixed Models, the Summer
#' Program in Data Analysis organized by SCS at York in June 2009 and MGT 1382
#' (in the MBA program at Rotman).
#' 
#' Many functions are inspired by, extensions of, or simply cobbled from
#' functions in the 'car' package by John Fox.
#' 
#' Please send any comments or changes to: Georges Monette <georges@@yorku.ca>
#' 
#' Partial list of functions: \subsection{Tables: \itemize{ \item atotal:
#' border an array with sums \item abind : glue two arrays together on a
#' selected dimension \item tab( fmla, data ) : includes NAs } }
#' \subsection{Graphics:
#' 
#' td : easy front end to trellis.device and related functions xqplot: extended
#' quantile plots panel.subgroups : to allow an extra level of subgroups in
#' lattice graphics } \subsection{Splines: \itemize{ \item lsp : linear \item
#' qs : quadratic \item cs : cubic \item gsp : general spline with arbitrary
#' degree in each interval and arbitrary constraint at each knot \item sc :
#' function to generate hypothesis matrices for 'gsp' for use with Wald tests
#' \item smsp: smoothing splines using mixed models } } \subsection{Inference
#' \itemize{ \item glh - same as wald below but name retained for compatibility
#' \item wald - General Linear Hypothesis of fixed effects using a Wald test
#' \itemize{ \item should work on lm, lme, nlme and lmer objects \item wald(
#' fit, L ) where 'L' is a hypothesis matrix performs a Wald test \item wald(
#' fit, 'pat' ) where 'pat' is a regular expression (see ?regex) uses an L
#' matrix that tests all fixed effects whose names match 'pat'.  For example >
#' wald( fit, ':.*:' ) will test all 2- and higher-way interactions together.
#' \item wald( fit, c(2,3,5) ) will test the 2nd, 3rd and 5th coefficients.  }
#' \item Lmat - Lmat ( fit, 'pat' ) constructs a hypothesis matrix. \item Lform
#' - Lform( fit, list.of.formulas, data.frame ) constructs an L matrix applying
#' each formula in the data.frame environment to create the corresponding
#' column of the L matrix. Single scalars (e.g. 0 or 1) expand to fill the
#' column.
#' 
#' \item Leff and related tools (fdiff, flevs) simplify the visualization,
#' estimation, testing and display of specific effects in models with complex
#' interactions. See \code{\link{Leff}}.
#' 
#' \item as.data.frame( wald( fit, Lform( fit, list.of.formulas, data.frame)),
#' se = 2) returns a data frame with 'coef', 'coefp' = coef + se * std.error,
#' 'coefm' = coef - se * std.error.  This can be used for plotting fitted
#' values with SEs for functions (e.g. lme) where the predict method does not
#' produce standard errors, or where there is no predict method (e.g. lmer).
#' 
#' \item Lmu
#' 
#' \item Ldiff
#' 
#' \item L2 - to come: will generate a hypothesis matrix that produces
#' Wald-type Type II tests
#' 
#' \item sc - hypothesis matrices for splines
#' 
#' \item cell - a modified version of car::confidence.ellipse.lm that can add
#' to a plot
#' 
#' \item vif.lme - method for vif modelled on vif.lm from library car (note
#' that model.frame and model.matrix are not implemented for lme objects) } }
#' \subsection{Linear algebra \itemize{ \item OrthoComp - a basis for the
#' orthogonal complement of the orthogonal projection of span(X) into span(Z)
#' within span(Z). \item ConjComp - similar with respect to an inner product. }
#' } \subsection{Graphics for linear algebra \itemize{ \item vplot - plots
#' column vectors adding to current plot \item vell - ellipse as a 2 x n matrix
#' \item vbox - box around unit circle \item orthog - 2 x 2 rotation \item
#' orthog.prog 2 x 2 matrix of orthog projection } } \subsection{File utilities
#' \itemize{ \item Read.spss : uses spss.get in library (Hmisc) \item Read.dta
#' : uses read.dta in library (foreign) with trimming and conversion to
#' factors. Reading an spss file that has been saved, in SPSS, as a Stata 'dta'
#' file seems to produce proper date variables. \item trim : removes trailing
#' blanks in character variables and factor levels - useful to post-process
#' files from SPSS read with read.dta } } \subsection{Regression utilities:
#' \itemize{ \item na.resid, na.fitted: pads with NAs to fit original data
#' frame.  These can be extended to additional classes by writing a method for
#' 'na.pad'
#' 
#' \item com, na.com: com(fit,'age') returns the component in 'fit'
#' corresponding to effect that matches the regular expression 'age', na.com
#' returns a vector padded with NAs to match the original data frame.
#' 
#' \item comres, na.comres: component + residuals } } \subsection{Multilevel
#' utilities: \itemize{ \item varLevel( dd, fmla ) shows the level of each
#' variable in data.frame dd - e.g. varLevel( dd, ~Board/School/Class/Subject )
#' or just - varLevel( dd, ~ID ) to identify inner and outer variables
#' 
#' \item up ( dd, fmla , keep ) creates a summary data set using the first row
#' of each set of rows identified by 'fmla'. 'keep' is the level below which to
#' keep variables. By default, only 'outer' variables are kept.  Caution:
#' currently uses library(nlme) which conflicts with library(lme4)
#' 
#' \item capply ( x, data, FUN, ...) creates 'contextual' variables - like
#' tapply but replicating the original shape - the ouput of FUN can also be
#' time-varying with the same length as the argument
#' 
#' \item xapply.formula ( fmla, by, FUN, ...) a formula interface to
#' 'aggregate' - can be used with 'expand.grid' to create a variable whose
#' values depend on other variables - useful to create 'prediction data frames'
#' 
#' \item fill ( x, by, FUN , ... ) uses capply to create contextual variable by
#' selecting a value. Works intelligently with factors and 'Date's.  - the
#' default FUN selects a unique value and can be used when a long data frame
#' has been created with values for macro variables only in the first position.
#' 
#' \item cvar ( x, id ) creates 'contextual' variables like 'capply' but using
#' the mean.  cvar also works when 'x' is a factor and returns a matrix with
#' (length( levels(x) ) - 1) columns with group mean values of the coding
#' matrix.
#' 
#' \item dvar ( x, id ) is almost equivalent to x - cvar( x, id ) except that
#' it is interpreted correctly when 'x' is a factor. } }
#' \subsection{Miscellaneous utilities: \itemize{ \item labs( dframe ) <-
#' c('rows','colums') adds an attribute 'labs' to a data frame and adds the
#' class 'data.frame.lab'. 'print.data.frame.lab' uses 'labs' as headings for
#' rownames and column names, in parallel to the effect
#' 
#' If 'mat' is a matrix, labs( mat ) is equivalent to: > names(dimnames(mat))
#' <- c('rows','columns') } } \subsection{Recent Changes: \itemize{ \item
#' December 8, 2008 predict.mer - a crude predict method for 'mer' objects
#' requires a repetition of the model formula
#' 
#' \item August 8, 2008 sc = generate coefficients for a spline for glh smsp :
#' basis for a smoothing spline
#' 
#' \item July 31, 2008 -- new version: gsp ( x, k, degree, smooth) : general
#' polynomial spline with variable degrees in each interval and variable
#' smoothness at each knot.
#' 
#' \item July 29, 2008 eg -- INCOMPLETE a more efficient version expand.grid
#' that takes level information from a data frame. } }
#' 
#' @name spida-package
#' @aliases spida.beta-package spida spida.beta
#' @docType package
#' @author Georges Monette <georges@@yorku.ca>
#' @seealso %~~ Optional links to other man pages, e.g. ~~
#' \code{\link[nlme]{lme}} ~~
#' @references Monette, G. (1990). Geometry of Multiple Regression and
#' Interactive 3-D Graphics. In Fox, J. & Long, S. (ed.)  \emph{Modern Methods
#' of Data Analysis}, Sage Publications, 209-256.
#' @keywords package
#' @examples
#' 
#' ~~ simple examples of the most important functions ~~
#' 
NULL





#' Sunspots
#' 
#' Monthly sunspot counts in 20th century
#' 
#' %% ~~ If necessary, more details than the __description__ above ~~
#' 
#' @name sun
#' @docType data
#' @format A data frame with 1175 observations on the following 2 variables.
#' \describe{ \item{list("year")}{time of observation (each month)}
#' \item{list("spots")}{number of sunspots} }
#' @references %% ~~ possibly secondary sources and usages ~~
#' @source %% ~~ reference to a publication or URL from which the data were
#' obtained ~~
#' @keywords datasets
#' @examples
#' 
#' data(sun)
#' ## maybe str(sun) ; plot(sun) ...
#' 
NULL



