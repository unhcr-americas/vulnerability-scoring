### install.packages("MultiLCIRT")

library(MultiLCIRT)

### install.packages("mirt")
library(mirt)

library(mirt)


## Building composite indicators with IRT ########

## Step 1 : Data (test) dimensionality assessment → Multidimensional Latent Class IRT model + clustering algorithm
## Step 3 : Adoption the best model for assigning weights to the test items
## Step 3 : Item aggregation using the weights obtained at the previous step → construction of a composite indicator for each dimension


## Step 1 : Clustering algorithm / Dimensionality assessment ###################
## Data from Bock & Lieberman (1970); contains 5 dichotomously scored items obtained from the Law School Admissions Test, section 7.
data(LSAT7)
head(LSAT7)

S <- as.matrix(LSAT7[,1:5])
yv <- as.vector(LSAT7[,6])

# hierarchical clustering of 5 items based on the Rasch model

#The dendrogram highlights three dimensions composing the multidimensional construct “Mathematical ability”.
#Considering the item contents, the assessed dimension can be interpreted

out <- class_item(S,
                  yv,
                  k = 3, # latentclasses (partial output is omitted)
                  link = 1, # it is the same with link = 2
                  disc = 0)



plot(out$dend)
summary(out)

# The first two columns (entitled ‘‘items’’) indicate items or groups collapsed at each step of the clustering procedure,
# the third column(‘‘deviance’’) reports the corresponding LR statistic,
# the fourth column(‘‘df’’)reports the number of degrees of freedom,
# the fifth (‘‘p-value’’) reports the p-value, and
# the last column (‘‘newgroup’’) contains the new group of items that is formed at each step.


## Step 2 : Adoption the best model for assigning weights to the test items / Extraction of the estimated discrimination power parameters ###################
# Model selection procedure and estimation of ordinal polytomous multidimensional LCIRT model
# Dataset about measurement of anxiety and depression in oncological patients
# A data frame with 201 observations on 14 items, measuring depression or anxiety

data(hads)
X <- as.matrix(hads)
# to make the estimation of the proposed models and clustering of items faster
# perform the analyses after aggregating the original records which correspond to the same response patternso
# as to obtain a matrix with a record for each distinct response configuration (rather than for each statistical unit).

# For this aim,   use function aggr_data, which requires as input an object of type matrix (and not of type data.frame)
#corresponding to the unit-by-unit response configurations. Output from function is
#  • data_dis: matrix of distinct configurations;
#  • freq : vector of corresponding frequencies;
#  • label: index of each original response configuration among the distinct ones
out <- aggr_data(X)
S <- out$data_dis
yv <- out$freq

nrow(X)
nrow(S)

# Parameter estimation for multidimensional IRT models based on discreteness of latent traits is performed through function est_multi_poly
# requires the following main input:

#   • S: matrix of all response sequences observed at least once in the sample and listed row-by-row.
# Usually,S is a matrix of type data_dis obtained by applying function aggr_data to the original data. Missing responses are allowed and they are coded as NaN;

#   • yv: vector of the frequencies of every response configuration in S corresponding to the output freq of function aggr_data
#(default value is given by a vector of ones, implying a unit weight for each record in S);

#  • k : number of latent classes;

#  • X : matrix of observed covariates, having the same dimension as S (default value is  NULL, indicating the absence of covariates in the study);

# • start: method of initialization of the algorithm:
# 0 (  =  default value) for deterministic starting values, 1 for random starting values, and 2 for arguments given bytheuser.
# If start = 2 ,we also need to specify as input the initial values of weights, support points, and discriminating an ddifficulty item parameters
# (using additional input arguments that are set equal to  NULL otherwise);

# • link : type of link function:  0 (= default value) for the standard LC model (i.e., no link function is specified),1 for global logits, and 2 for local logits.
# In the caseof dichotomous responses, it is the same to specify link =  1 or link = 2;

# • disc : indicator of constraints on the discriminating item parameters: 0 (= default value) if γj= 1, j=1 ,..., r, and 1 otherwise;

# • difl : indicator of constraints on the difficulty item parameters: 0 (= default value) if difficulties are free and 1 if βjx= βj+ τx

# • multi : matrix with a number of rows equal to the number of dimensions and elements in each row equal to the indices of the items measuring the dimension
# corresponding to that row. Cases where dimensions are measured by a different number of items are allowed, and the number of columns of matrix multi
# corresponds to the number of items in the largest dimension.



# Function est_multi_poly supplies the following output:

#  • Piv: optional object of type matrix containing the estimated weights of thelatent classes subject-by-subject
# (the weights may be different across subjects in the presence of covariates);

# • Th: estimated matrix of ability levels (support points) for each dimension (= row of matrix Th) and
# latent class (= column of matrix Th);

# • Bec :estimated vector of difficulty item parameters (split in two vectors if difl = 1);

# • gac :estimated vector of discriminating item parameters; if disc = 0 (Rasch-type model), all values of vector gac are constrained to 1;

# • fv :vector indicating the reference item chosen for each latent dimension;

# • Phi: optional object of type array containing the conditional response probabilities (see Eq.(1)) for every item and latent class.
# The array is made of as many matrices as the latent classes; moreover, the j -th column of each of such matrices refers to item j ,
# where as the x-th row of each matrix refers to the x- th responsecategory(x = 0,..., lj − 1) of item j.
# In the case of items differing in the number of response categories, zeros are included in the corresponding cells;

# • Pp : optional object of type matrix containing the posterior probabilities of belonging to latent class c (column  c of the Pp matrix),
# given the response configuration (row of the Pp   matrix);

# • lk : log-likelihood at convergence of the EM algorithm;

# • np: number of free parameters;

# • aic: Akaike Information Criterion index (Akaike,1973);

# •bic : Bayesian Information Criterion index (Schwarz,1978)


### Check how many latent classes to consider!
out1 <- est_multi_poly(S,yv,
                       k = 1, # number of ability levels (or latent classes)
                       start = 0, # method of initialization of the algorithm (0 = deterministic, 1 = random, 2 = arguments given as input)
                       link = 0) # type of link function (0 = no link function, 1 = global logits, 2 = local logits);

out2 <- est_multi_poly(S,yv,
                       k = 2,
                       start = 0,
                       link = 0)

out3 <- est_multi_poly(S, yv,
                       k = 3,
                       start = 0,
                       link = 0)

out4 <- est_multi_poly(S,yv,
                       k = 4,
                       start = 0,
                       link = 0)


compare_models(out1, out2, out3, out4)


### Check if local or global logit to consider
out31 <- est_multi_poly(S, yv,
                        k = 3,
                        start = 0,
                        link = 1, # Global logit
                        disc = 1, # indicator of constraints on the discriminating indices (0 = all equal to one, 1 = free)
                        difl = 0, # indicator of constraints on the difficulty levels (0 = free, 1 = rating scale parameterization)
                        multi = cbind(1:ncol(S)))

out32 <- est_multi_poly(S, yv,
                        k = 3,
                        start = 0,
                        link = 2, # Local logit
                        disc = 1,
                        difl = 0,
                        multi = cbind(1:ncol(S)))


## Findout if global or local logit is better
compare_models(out31,out32)

## Set up the restricted model for the rest of analysis
multi2 <- rbind(c(2,6,7,8,10,11,12),
                c(1,3,4,5,9,13,14))

# Then, test_dim function is launched and its output is printed in the usual way (partial output is omitted):
# Likelihood ratio testing between nested multidimensional LC IRT models
# tests a certain multidimensional model (restricted model) against a larger multidimensional model based on a higher number of dimensions.
# A typical example is testing a unidimensional model (and then the hypothesis of unidimensionality) against a bidimensional model.
# Both models are estimated by est_multi_poly.

out5 <- test_dim(S, yv,
                 k = 3,
                 link = 1, # type of link function (1 = global logits, 2 = local logits); with global logits the Graded Response model results;
                           # with local logits the Partial Credit results (with dichotomous responses, global logits is the same as using local logits
                           # resulting in the Rasch or the 2PL model depending on the value assigned to disc)
                 disc = 1, # Indicator of constraints on the discriminating indices (0 = all equal to one, 1 = free)
                 difl = 0, # indicator of constraints on the difficulty levels (0 = free, 1 = rating scale parametrization)
                 multi0 = multi2,  # matrix specifying the multidimensional structure of the
                 multi1 = cbind(1:ncol(S))) # matrix specifying the multidimensional structure of the larger model
summary(out5)

out6 <- test_dim(S, yv,
                 k = 3,
                 link = 1,
                 disc = 1,
                 difl = 0,
                 multi1 = multi2)
summary(out6)



## Once the global logit has been chosen as the best link function, we carry on with the test of unidimensionality.

# An LR test is used to comparemodels which differ in terms of the dimensional structure,
# all other elements being equal (i.e.,free item discriminating and difficulty parameters), that is,
## (i) a graded response model with an r-dimensional structure,
## (ii) a graded response model with a bidimensional structure (i.e., anxiety and depression), as suggested by the structure of the questionnaire, and
## (iii) a graded response model with a unidimensional structure (i.e., all the items belong to the same dimension).

# The LR tests are performed through function test_dim, first applied to comparemodels at points(i)and(ii),
# and then applied tocomparemodels at points (ii) and (iii).



# Unidimensional GRM
out311 <- est_multi_poly(S, yv,
                         k = 3,
                         start = 0,
                         link = 1,
                         disc = 1,
                         difl = 0)
# Unidimensional  RS-GRM
out3111 <- est_multi_poly(S, yv,
                          k = 3,
                          start = 0,
                          link = 1,
                          disc = 1,
                          difl = 1)
# Unidimensional  1P-GRM
out3112 <- est_multi_poly(S, yv,
                          k = 3,
                          start = 0,
                          link = 1,
                          disc = 0,
                          difl = 0)

# Unidimensional  1P-RS-GRM
out3113 <- est_multi_poly(S,yv,
                          k = 3,
                          start = 0,
                          link = 1,
                          disc = 0,
                          difl = 1)

# comparison of RS-GRM and 1P-GRM with GRM
compare_models(out311,
               out3111,
               out3112,
               nested = TRUE)

compare_models(out3112,
               out3113,
               nested = TRUE)

rbind(out3112$Th, prob = out3112$piv)

## Step 3 :Item aggregation using the weights ##########
## Weights = discrimination parameters, estimated through a Multidimensional IRT model, using a 2PL parametrisation
