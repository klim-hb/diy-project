# Project R code: 
# Understanding the Future of Higher Ed Landscape in Dubai
# Full version avaliable on https://github.com/klim-hb/diy-project

#Install Pacman to check which libraries are missing and only install the missing ones
if (!require("pacman")) install.packages("pacman", repos = "http://cran.us.r-project.org")
pacman::p_load(readr,         # used to read downloaded csv files     
               tidyverse,     # Includes dplyr, ggplot2, and others
               caret,         # used for rating model 
               data.table,    # for working with datagrames and matricies     
               kableExtra,    # for table formatting
               knitr,         # to keep it knit :)
               tidyr,         # to keep it tidy
               dataPreparation, # used to clean the review dataset
               gtrendsR,      # used to gather google trends data
               reshape2,      # used to present google trends data
               forecast,      # used for time-series models
               highcharter,   # to work with display of timeseries
               webshot,       # saving heavy js/HTML to images for the report to be run smoothly
               GGally,        # creation of correlogram
               CatEncoders,   # Used for the label encoding/replacing categorical variable by numeric values
               randomForest,  # randomForest model
               randomForestExplainer, #Explainer for RF
               quantreg,      # Quantile Regression Model
               svglite,       # Take it easy on space for svg files 
               skimr,         # Provide summary stats 
               pander,        # for session info 
               recosystem,    # for factorization model     
               gridExtra,     # display plots in grid
               wordcloud,     # for word cloud
               syuzhet,       # for sentiment analysis
               tidytext,      # for sentiment
               tm)            # for sentiment corpus


# Ensure phantomJS is installed successfully with webshot package - it had problem on my machine
if (is_phantomjs_installed() != TRUE) webshot::install_phantomjs()

#installation of TinyTeX in R (was required on the first attemp to create PDF: tinytex::install_tinytex()
if (tinytex:::is_tinytex()!= TRUE) tinytex::install_tinytex()

#Downloading Data
download.file("https://github.com/klim-hb/diy-project/raw/master/data.zip", "data.zip")
unzip("data.zip", junkpaths=TRUE)
      
# Reading the "HigherEdData.csv"
HigherEdData <- read.csv("HigherEdData.csv")

# Reading the "Unis-names.csv"
Unis_names <- read.csv("Unis-names.csv")

#create and display table with names of variables and comments
text_tbl1 <- data.frame(
  Name = c(names(HigherEdData),names(Unis_names)),
  Comment = c("Year of census collected","University Name","Location","Lattitude of location","Longtitude of location","Major or specialization of students in census","Undergraduate or Postgraduate Students","Enrolled or Graduated","Number of students in the census", "University Id","Name of the University","Avarege Tuition Fees","Ranking of the university as of 2019","Rating of the curriculum depending on the country of origin","Number of years operating in Dubai"),
  Dataset = c("HigherEdData","HigherEdData","HigherEdData","HigherEdData","HigherEdData","HigherEdData","HigherEdData","HigherEdData","HigherEdData","Unis-names","Unis-names","Unis-names","Unis-names","Unis-names","Unis-names"))

kable(text_tbl1, caption = "Overview of variables in HigherEdData and Unis-names") %>%
  kable_styling(position = "center",
                font_size = 7,
                full_width = FALSE, latex_options = "hold_position") %>%
  column_spec(1, bold =T) %>%
  column_spec(2, bold=F, width = "25em") %>%
  row_spec(0, bold = T, color = "white", background = "#D7261E")

#show head of HigherEdData data
HigherEdData  %>%
  head()  %>%
  kable(caption = "First six records of HigherEdData") %>%
  kable_styling(full_width = F, position = "center",
                font_size = 7, latex_options = "hold_position") %>%
  column_spec(1, bold = T) %>%
  column_spec(2, bold=F, width = "7em") %>%
  column_spec(5:6, bold=F, width = "5em") %>%  
  row_spec(0, bold = T, color = "white", background = "#D7261E")

#show head of Unis_names data
Unis_names  %>%
  head()  %>%
  kable(caption = "First six records of Unisnames") %>%
  kable_styling(full_width = F, position = "center",
                font_size = 7, latex_options = "hold_position") %>%
  row_spec(0, bold = T, color = "white", background = "#D7261E")

#### University Review Data - Google Maps 

#Unzipping required files for the first model
# unzip("data.zip", "googleUni.csv")
# unzip("data.zip", "review.csv")

# Reading the "googleUni.csv"
GUni <- read.csv("googleUni.csv")

# Reading the "reviews.csv"
GReviews <- read.csv("reviews.csv")

#create and display table with names of variables and comments
text_tbl2 <- data.frame(
  Name = c(names(GUni),names(GReviews)),
  Comment = c("Name of the University","Latitude of the location","Longtitude of the location","Total number of reviews","Total number of photos assosiated with the reviews","Google Id of the university","Google Id of the university","Author Id of the review","Total score for rating (integer)","Timestamp in EPOCH format (seconds since Jan 1, 1970)","Text of the review, translated text of the review"),
  Dataset = c("GUni","GUni","GUni","GUni","GUni","GUni","GReviews","GReviews","GReviews","GReviews","GReviews"))

kable(text_tbl2, caption = "Overview of Guni and GReviews") %>%
  kable_styling(position = "center",
                font_size = 7,
                full_width = FALSE, latex_options = "hold_position") %>%
  column_spec(1, bold =T) %>%
  column_spec(2, bold=F, width = "30em") %>%
  row_spec(0, bold = T, color = "white", background = "#D7261E")

#show head of GUni data
GUni  %>%
  head()  %>%
  kable(caption = "First six records of GUni") %>%
  kable_styling(full_width = F, position = "center",
                font_size = 5, latex_options = "hold_position") %>%
  column_spec(1, bold=F, width = "15em") %>%  
  row_spec(0, bold = T, color = "white", background = "#D7261E")

#show head of GReviews data
GReviews  %>%
  head()  %>%
  kable(caption = "First six records of GReviews") %>%
  kable_styling(full_width = F, position = "center",
                font_size = 4, latex_options = "hold_position") %>%
  column_spec(5, bold=F, width = "35em") %>%  
  row_spec(0, bold = T, color = "white", background = "#D7261E")

# Universities Census Analysis

## Preprocessing the data

#### Cleaning the data

# Getting the column names of the HigherEdData.csv file
#colnames(HigherEdData)
# Chnaging the column names by index
colnames(HigherEdData)[1] = "Year" 
colnames(HigherEdData)[2] = "University_Name"

# Getting the column names for Unis_names file
#colnames(Unis_names)
# Chnaging the column names by index
colnames(Unis_names)[2] = "University_Name"
colnames(Unis_names)[3] = "Avarage_Tuition_Fee"
colnames(Unis_names)[4] = "Ranking"
colnames(Unis_names)[5] = "Curriculum_Rating"

# Merging both files by the University_Name column
HigherEdData <- merge(HigherEdData,Unis_names,by="University_Name")

# Viewing the updated dataframe 
# View(HigherEdData)

# Getting the unique values of the Status column
#unique(HigherEdData$Status)

# Dropping the rows if status column contains Graduated
HigherEdData<-HigherEdData[!(HigherEdData$Status=="Graduated"),]

# Applying the Label Encoder of each catefogrical column
University_Name = LabelEncoder.fit(HigherEdData$University_Name)
HigherEdData$University_Name = transform(University_Name,HigherEdData$University_Name)
Major = LabelEncoder.fit(HigherEdData$Major)
HigherEdData$Major = transform(Major,HigherEdData$Major)
Level = LabelEncoder.fit(HigherEdData$Level)
HigherEdData$Level = transform(Level,HigherEdData$Level)

skim(HigherEdData) %>% focus(n_missing, numeric.p0, numeric.mean, numeric.sd, numeric.p100, numeric.hist) %>%
  kable(caption = "Summary of HigherEdData") %>%
  kable_styling(full_width = F, position = "center",
                font_size = 9, latex_options = "hold_position") 
#%>% column_spec(5, bold=F, width = "35em") %>%  
#   row_spec(0, bold = T, color = "white", background = "#D7261E")



## Data Exploration

#plot data with geom_smooth Uni vs ranking
HigherEdData %>% 
  ggplot(aes(University_Name, Ranking)) +
  geom_point() +
  geom_smooth()

#graph students by university over the years
HigherEdData %>%  ggplot(aes(University_Name, Students, color=Year)) +
  geom_point(stat="identity") + 
  ggtitle("Total Student records in all universities over the years") + 
  geom_smooth()

## correlogram for Higher Ed Data saved as png to make pdf lighter:
svglite("myPlot.svg", height = 20, width = 20)
tmp <- HigherEdData %>% select(Location,University_Name,Major,Level,Students,Avarage_Tuition_Fee,Ranking,Curriculum_Rating,YearsinDubai) %>% ggpairs(title="Correlogram for HigherEd data with highlighted difference on Location", ggplot2::aes(colour=Location)) 
print(tmp)
dev.off()

## saving svg to png as screenshot
webshot::webshot(url="myPlot.svg",file="myplot.png",vwidth=1200,vheight = 1200)

## Data Split

# Dividing the dataset into the train and test
smp_size <- floor(0.8 * nrow(HigherEdData))
train_ind <- sample(seq_len(nrow(HigherEdData)), size = smp_size)
train <- HigherEdData[train_ind, ]
test <- HigherEdData[-train_ind, ]

#### Random Forest Model

## Universities Census Analysis Results

### Multiple Linear Regression Model

print(paste('Naive avarage:',mean(train$Students)))
print(paste('Avarage with log1p:',log1p(mean(train$Students))))

# Applying Muliple Linear Regression
regressor = lm(Students ~ Year+University_Name+Major+Level+
                 Ranking+Avarage_Tuition_Fee+Curriculum_Rating+
                 YearsinDubai,data = train)

# Cearting the input test dataframe for testing the model for the prediction.
x_test = test[, c("Year","University_Name","Major","Level",
                  "Avarage_Tuition_Fee","Ranking","Curriculum_Rating","YearsinDubai")]

# Actual output of the Testing column
y_test = test$"Students"

# Applying the prediction of the regression model on the test dataframe.
y_pred = predict(regressor, newdata = x_test)

# Getting the absolute if there is any negitive value.
y_pred = abs(y_pred)

# Combining the Predicted Values and Actual Values into one dataframe.
actuals_preds <- data.frame(cbind(actuals=y_test, predicteds=y_pred)) 

# Computing the RMSE.
rmse <- sqrt(mean(as.matrix(log1p(y_test) - log1p(y_pred))^2))
print(paste('RMSE:', rmse))

# MeanAbsolutePercentageError (MAPE)
mape <- mean(abs((y_pred - y_test))/y_test)
print(paste('MAPE:',mape))

### Quantile Linear Regression Model

# Applying the Quantile Regression Model
model1 = rq(Students ~ Year+University_Name+Major+Level+Ranking+Avarage_Tuition_Fee+Curriculum_Rating+YearsinDubai,
            data = train,tau = 0.25)
y_pred = predict(model1, newdata = x_test)
y_pred = abs(y_pred)

# Combining the Predicted Values and Actual Values into one dataframe.
actuals_preds <- data.frame(cbind(actuals=y_test, predicteds=y_pred))

# Computing the RMSE.
rmse <- sqrt(mean(as.matrix(log1p(y_test) - log1p(y_pred))^2))
print(paste('RMSE:', rmse))

# MeanAbsolutePercentageError (MAPE)
mape <- mean(abs((y_pred - y_test))/y_test)
print(paste('MAPE:', mape))

#Print summary of predictions
summary(actuals_preds)

#Plotting 
plot(actuals_preds)


### Random Forest Model
library(rpart) # creates decision trees
#  make the tree prety
tree <- rpart(Students ~ ., train)
library(rpart.plot)  # pretty trees
rpart.plot(tree, type=1)

#Applying the random Forest
rf <-randomForest(Students ~ Year+University_Name+Major+Level+Ranking+Avarage_Tuition_Fee+Curriculum_Rating+YearsinDubai ,data = train, ntree=500) 

# Applying the prediction of the random Forest model on the test dataframe.
y_pred = predict(rf, newdata = x_test)

# Combining the Predicted Values and Actual Values into one dataframe.
actuals_preds <- data.frame(cbind(actuals=y_test, predicteds=y_pred))

# Computing the RMSE.
rmse <- sqrt(mean(as.matrix(log1p(y_test) - log1p(y_pred))^2))
print(paste('RMSE:', rmse))

# MeanAbsolutePercentageError (MAPE)
mape <- mean(abs((y_pred - y_test))/y_test)
print(paste('MAPE',mape))

# UNCOMMENT If would like to run on your own - warning, it takes too much time. For the sake of the project, we uploaded the outcome together with data
# devtools::install_github("MI2DataLab/randomForestExplainer")
#library(randomForestExplainer)

#forest <- randomForest(Students ~ Year+University_Name+Major+Level+Ranking+Avarage_Tuition_Fee+Curriculum_Rating+YearsinDubai,data = train, ntree=500, localImp = TRUE)

#explain_forest(forest, interactions = TRUE, data = train)

#Multiple Selector Based Screenshots
webshot("Your_forest_explained.html",
 file = c("mindepth.png"),
 selector = list("#distribution-of-minimal-depth,img"),
 expand = c(-150, 250, -100, -185))

# **Multi-way importance plot**

#Multiple Selector Based Screenshots
webshot("Your_forest_explained.html",
 file = c("multi.png","multi1.png"),
 selector = list("#multi-way-importance-plot","#multi-way-importance-plot"),
 expand = list("-235, 250, -580, -185","-800,250,0,-185"))

#Multiple Selector Based Screenshots
webshot("Your_forest_explained.html",
 file = c("compare.png"),
 selector = list("#compare-rankings-of-variables"),
 expand = c(-210, 350, 250, -185))

#Multiple Selector Based Screenshots
webshot("Your_forest_explained.html",
 file = c("varint.png"),
 selector = list("#conditional-minimal-depth"),
 expand = c(-575, 355, -270, -185))


# Google Map Reviews Analysis

## Preprocessing the data

#### Cleaning the data

#Selecting only fields which we need from GUni - for factorization model

GFUni <- GUni %>% select(google_id,name,photos_count)

#Selecting for Sentiment Analysis later in the project
GFReviewsS <- GReviews %>% select(google_id,autor_id,review_rating,review_timestamp, review_text)

#Selecting only fields which we need from GReviews - for factorization model
GFReviews <- GReviews %>% select(google_id,autor_id,review_rating,review_timestamp)

GF <- inner_join(GFReviews,GFUni)

#Assigning proper IDs to google_id and autor_id
setDT(GF)[, unid := .GRP, by = google_id]
setDT(GF)[, userid := .GRP, by = autor_id]

#Selecting columns which we need
GF <- GF %>% select(unid,userid,review_rating,review_timestamp,name)

#convert EPOCH time to readable format
GF$review_timestamp <- as.POSIXct(GF$review_timestamp, origin="1970-01-01")

#Factorize rating
#not now

#Display summary of the updated data
head(GF) %>%
kable(caption = "First six records of GF") %>%
  kable_styling(full_width = F, position = "center",
                font_size = 8, latex_options = "hold_position") %>%
   column_spec(5, bold=F, width = "25em") %>%  
   row_spec(0, bold = T, color = "white", background = "#D7261E")

#Utilizing skim package to provide sumary
skim(GF) %>% focus(n_missing, numeric.p0, numeric.mean, numeric.sd, numeric.p100, numeric.hist) %>%
kable(caption = "Summary of GF") %>%
kable_styling(full_width = F, position = "center",
                font_size = 9, latex_options = "hold_position") 

## Data Exploration 

# create plots - all below
GF  %>%
	group_by(review_rating) %>%
	summarize(count = n()/1000) %>%
	ggplot(aes(x = review_rating, y = count)) +
	geom_line() + xlab("Rating 0-5") + ylab("# Ratings, thousands") +ggtitle("Distribution of Ratings")

GF %>% 
  count(unid) %>% 
  ggplot(aes(n)) + 
  geom_histogram(color = "red") +
  ggtitle("Universities") +
  labs(subtitle  ="number of ratings by unid", 
       x="unid" , 
       y="number of ratings") +
  theme(panel.border = element_rect(colour="black", fill=NA)) 

GF %>% 
  count(userid) %>% 
  ggplot(aes(n)) + 
  geom_histogram(color = "red") +
  ggtitle("Users") +
  labs(subtitle ="number of ratings by UserId", 
       x="userId" , 
       y="number of ratings") +
  theme(panel.border = element_rect(colour="black", fill=NA)) 

GF %>% 
  mutate(date = round_date(as_datetime(review_timestamp), unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(review_rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Timestamp, time unit : week")+
  labs(subtitle = "average ratings")

GF <- GF %>% mutate(review_year = round_date(as_datetime(review_timestamp), unit = "year"))

## correlogram for GF Data saved as png to make pdf lighter:
svglite("myPlotGF.svg")
tmp1 <- GF %>% select(unid,userid,review_rating,review_year) %>% ggpairs(title="Correlogram for GF data with highlighted difference on time", ggplot2::aes(colour=review_year)) 
print(tmp1)
dev.off()

## saving svg to png as screenshot
webshot::webshot(url="myPlotGF.svg",file="myplotGF.png",vwidth=800,vheight = 800)

## Data Split

library(dataPreparation)

## Arrange edx as training and test datasets as 80/20%
set.seed(1, sample.kind="Rounding")

# Random sample indexes
train_index_GF <- sample(1:nrow(GF), 0.8 * nrow(GF))
test_index_GF <- setdiff(1:nrow(GF), train_index_GF)

# Build X_train, y_train, X_test, y_test
X_train_GF <- GF[train_index_GF, -5]
y_train_GF <- GF[train_index_GF, "review_rating"]

X_test_GF <- GF[test_index_GF, -5]
y_test_GF <- GF[test_index_GF, "review_rating"]

#Identifying constant, double and bijection columns
constant_cols <- whichAreConstant(GF)
double_cols <- whichAreInDouble(GF)
bijections_cols <- whichAreBijection(GF)

#Removing bijection column -name
X_train_GF$name = NULL
X_test_GF$name = NULL

# There are no constant, double columns. The bijection column name was removed from our dataset. 
# 
# #### Scaling
# Nothing to do

# #### Discretization
# 
# We do not have variables to perform discretization on.
# 
# #### Encoding Categorical
# 
# We do not have categorical data for this dataset.

#controlling the shape
X_test_GF <- sameShape(X_test_GF, referenceSet = X_train_GF, verbose = TRUE)


#TrainvsTest sets overview

 trainSet <- X_train_GF
 testSet <- X_test_GF

 text_tbl211 <- data.frame(
   Feature = c("Number of Rows", "Number of Columns", "Unique Universities","Unique Users","Max rating"),
   Train = c(nrow(trainSet), ncol(trainSet),n_distinct(trainSet$unid),n_distinct(trainSet$userid),n_distinct(trainSet$review_rating)), 
   Test = c(nrow(testSet), ncol(testSet),n_distinct(testSet$unid),n_distinct(testSet$userid),n_distinct(testSet$review_rating)))
 
 kable(text_tbl211) %>%
   kable_styling(full_width = F) %>%
   row_spec(0, bold = T, color = "white", background = "#D7261E")
 

### Data for Sentiment Analysis

#Selecting only fields which we need from GUni - for factorization model

GFS <- inner_join(GFReviewsS,GFUni)

#Assigning proper IDs to google_id and autor_id

setDT(GFS)[, unid := .GRP, by = google_id]
setDT(GFS)[, userid := .GRP, by = autor_id]

#Selecting columns which we need
GFS <- GFS %>% select(unid,userid,review_rating,review_timestamp,review_text)

#convert EPOCH time to readable format
GFS$review_timestamp <- as.POSIXct(GFS$review_timestamp, origin="1970-01-01")

#Factorizing the rating
GFS$review_rating <- as.factor(GFS$review_rating)

GFS <- GFS %>% filter(review_text != "")

#Display summary of the updated data
head(GFS) %>%
kable(caption = "First six records of GFS") %>%
  kable_styling(full_width = F, position = "center",
                font_size = 8, latex_options = "hold_position") %>%
   column_spec(5, bold=F, width = "25em") %>%  
   row_spec(0, bold = T, color = "white", background = "#D7261E")

#Utilizing skim package to provide sumary
skim(GFS) %>% focus(character.min,character.max,character.empty,character.n_unique,character.whitespace,numeric.hist,POSIXct.n_unique) %>%
kable(caption = "Summary of GFS") %>%
kable_styling(full_width = F, position = "center",
                font_size = 9, latex_options = "hold_position") 

#### Data Cleaning

# library("wordcloud")
# library(syuzhet)
# library(tidytext)

#changing name of column to text
reviews <- GFS %>% select(review_text)
colnames(reviews)[1] = "text" 

#filtering the data
review_words <- reviews %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "^[a-z']+$"))

#get the sentiment
syuzhet_vector <- get_sentiment(GFS$review_text, method="syuzhet")

bing_vector <- get_sentiment(GFS$review_text, method="bing")
afinn_vector <- get_sentiment(GFS$review_tex, method="afinn")
nrc_vector <- get_sentiment(GFS$review_tex, method="nrc", lang = "english")

#combining results
rbind(sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector)),
  sign(head(nrc_vector)))

#DataGMSummarySkimSentimentResponseSkim

summary(syuzhet_vector) 

# Plot DataGMSummarySkimSentimentResponse
plot(
  syuzhet_vector, 
  type="h", 
  main="Emotional Response of the avaliable reviews", 
  xlab = "Review Progression", 
  ylab= "Emotional Response"
  )

## Google Map Reviews Analysis Results

### Matrix Factorization

#RecosystemScript

library(recosystem)

set.seed(123, sample.kind = "Rounding") # This is a randomized algorithm
# Convert the train and test sets into recosystem input format

train_data <-  with(trainSet, data_memory(user_index = userid,
                                          item_index = unid,
                                          rating     = review_rating))
test_data  <-  with(testSet,  data_memory(user_index = userid,
                                          item_index = unid,
                                          rating     = review_rating))

# Create the model object
r <-  recosystem::Reco()

# Select the best tuning parameters
opts <- r$tune(train_data, opts = list(dim = c(10, 20, 30),
                                      lrate = c(0.1, 0.2),
                                      costp_l2 = c(0.01, 0.1),
                                      costq_l2 = c(0.01, 0.1),
                                      nthread  = 4, niter = 10))

# Train the algorithm
r$train(train_data, opts = c(opts$min, nthread = 4, niter = 20))
 
#RecoSystemResults

# Calculate the predicted values  
 y_hat_reco <-  r$predict(test_data, out_memory())
 head(y_hat_reco, 10)
 
# Computing the RMSE.
rmse <- sqrt(mean(testSet$review_rating - y_hat_reco)^2)
print(paste('RMSE:', rmse))

# MeanAbsolutePercentageError (MAPE)
mape <- mean(abs((y_hat_reco - testSet$review_rating))/testSet$review_rating)
print(paste('MAPE:', mape))




### Sentiment Analysis

#Build plot DataGMSummarySkimSentimentResponse1

GFS$syuzhet <- syuzhet_vector
GFS %>% ggplot(aes(unid,syuzhet, color=review_timestamp)) + geom_point() + geom_smooth()


#DataGMSummarySkimSentimentResponse2

docs <- Corpus(VectorSource(GFS$review_text))
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("Dubai", "university", "khda", "ministry", "one","education","college")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
# Text stemming
# docs <- tm_map(docs, stemDocument)

#as matrix
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

#Make the wordcloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 50,
          max.words=100, random.order=TRUE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#TrendSetup

library(gtrendsR)
library(reshape2)
  
#Callin all keywords, which we are interested in, separetely, as otherwise the date will become relative to keywords.
#time=all means data collected from 2004 till date, gprop=web online web searches (no youtube etc) 
#geo-AE means inquiries from UAE based users, otherwise - worldwide

google.trends = gtrends(c("University in Dubai"), gprop = "web", time = "all")[[1]]
google.trends = reshape2::dcast(google.trends, date ~ keyword + geo, value.var = "hits")
colnames(google.trends)=c("date","University_world")
setDT(google.trends)[, dateid := .GRP, by = date]

google.trends2 = gtrends(c("Bachelors in Dubai"), gprop = "web", time = "all")[[1]]
google.trends2 = reshape2::dcast(google.trends2, date ~ keyword + geo, value.var = "hits")
colnames(google.trends2)=c("date","Bachelors_world")
setDT(google.trends2)[, dateid := .GRP, by = date]

google.trends3 = gtrends(c("Masters in Dubai"), gprop = "web", time = "all")[[1]]
google.trends3 = reshape2::dcast(google.trends3, date ~ keyword + geo, value.var = "hits")
colnames(google.trends3)=c("date","Masters_world")
setDT(google.trends3)[, dateid := .GRP, by = date]

google.trends4 = gtrends(c("Bachelors in Dubai"), gprop = "web", time = "all", geo = c("AE"))[[1]]
google.trends4= reshape2::dcast(google.trends4, date ~ keyword + geo, value.var = "hits", )
colnames(google.trends4)=c("date","Bachelors_UAE")
setDT(google.trends4)[, dateid := .GRP, by = date]

google.trends5 = gtrends(c("Masters in Dubai"), gprop = "web", time = "all", geo = c("AE"))[[1]]
google.trends5 = reshape2::dcast(google.trends5, date ~ keyword + geo, value.var = "hits")
colnames(google.trends5)=c("date","Masters_UAE")
setDT(google.trends5)[, dateid := .GRP, by = date]

google.trends6 = gtrends(c("Online university in Dubai"), gprop = "web", time = "all")[[1]]
google.trends6= reshape2::dcast(google.trends6, date ~ keyword + geo, value.var = "hits", )
colnames(google.trends6)=c("date","Online_University_world")
setDT(google.trends6)[, dateid := .GRP, by = date]

google.trends7 = gtrends(c("Online masters in Dubai"), gprop = "web", time = "all")[[1]]
google.trends7 = reshape2::dcast(google.trends7, date ~ keyword + geo, value.var = "hits")
colnames(google.trends7)=c("date","Online_Masters_world")
setDT(google.trends7)[, dateid := .GRP, by = date]


#Join the tables
google <- google.trends
google <- inner_join(google,google.trends2,by="dateid")
google <- inner_join(google,google.trends3,by="dateid")
google <- inner_join(google,google.trends4,by="dateid")
google <- inner_join(google,google.trends5,by="dateid")
google <- inner_join(google,google.trends6,by="dateid")
google <- inner_join(google,google.trends7,by="dateid")

#subset bs data
google = subset(google, select = -c(date.x,date.x.x,date.x.x.x,date.y,date.y.y,date.y.y.y,dateid))

#rownames(google.trends) = google.trends$date
#google.trends$date = NULL
#colnames(google.trends)=c("date","University_world")


#Utilizing skim package to provide sumary
skim(google) %>% focus(n_missing,numeric.mean,numeric.sd,numeric.p0,numeric.p100,numeric.hist) %>%
kable(caption = "Summary of google.trends for University in Dubai, Worldwide") %>%
kable_styling(full_width = F, position = "center",
                font_size = 7, latex_options = "hold_position") 

# TrendUBWD plots

p1 <- google %>% 
  mutate (Smooth_UBMW=(University_world+Bachelors_world+Masters_world)/3) %>% 
  ggplot(aes(x=date)) +
    geom_line(aes(y=University_world, color="University_world")) +
    geom_line(aes(y=Bachelors_world, color="Bachelors_world")) +
    geom_line(aes(y=Masters_world, color="Masters_world")) +
    ggtitle("Worldwide keywords search volume\n for University, Bachelors, Masters in Dubai") +
    ylab("Trend volume") +
    stat_smooth(aes(date, Smooth_UBMW, color="Smooth_UBMW")) + theme(legend.position="bottom")

p2 <- google %>% 
  mutate (Smooth_BMD=(Bachelors_UAE+Masters_UAE)/2) %>% 
  ggplot(aes(x=date)) +
   geom_line(aes(y=Bachelors_UAE, color="Bachelors_UAE")) +
   geom_line(aes(y=Masters_UAE, color="Masters_UAE")) +
   ggtitle("UAE-based keywords search volume for Bachelors,\nMasters in Dubai")+
   ylab("Trend volume") +
   stat_smooth(aes(date, Smooth_BMD, color="Smooth_BMD")) + theme(legend.position="bottom")

grid.arrange(p1, p2, nrow = 2)


#TrendOnUBWD plot

p3 <- google %>% 
  mutate (Smooth_OUMW=(Online_University_world+Online_Masters_world)/2) %>%  
  ggplot(aes(x=date)) +
   geom_line(aes(y=Online_University_world, color="Online_University_world")) +
   geom_line(aes(y=Online_Masters_world, color="Online_Masters_world")) +
   ggtitle("Worldwide keywords search volume for Online University, Masters in Dubai")+
   ylab("Trend volume") +
   stat_smooth(aes(date, Smooth_OUMW, color="Smooth_OUMW")) + theme(legend.text = element_text(size=6), legend.title = element_blank(), legend.position="bottom")
   
grid.arrange(p3, nrow = 1)

#prepare the data

google$dateY <-  as.numeric(format(google$date,'%Y'))
google <- fastDiscretization(dataSet = google, bins = list(dateY = c(2004, 2008, 2012, 2016, 2020, +Inf)))

print(table(google$dateY))

## correlogram for Google Trends Data saved as png to make pdf lighter:
svglite("myPlotTrends.svg", height = 20, width = 20)
tmp3 <- google %>% select(University_world,Bachelors_world,Masters_world,Bachelors_UAE,Masters_UAE,Online_University_world,Online_Masters_world,dateY) %>% ggpairs(title="Correlogram for Google Trends data with highlighted difference on Years", ggplot2::aes(colour=dateY)) 
print(tmp3)
dev.off()

## saving svg to png as screenshot
webshot::webshot(url="myPlotTrends.svg",file="myplotTrends.png",vwidth=1200,vheight = 1200)



## TrendsTrainTest

#University_world
# Convert the data to be officially "time-series" data
# Compute the Holt-Winters filtering for the data

ga_ts_train <- ts(google$University_world, start = c(2004,01), end = c(2009,12), frequency = 12)
ga_ts_test <- ts(google$University_world, start = c(2010,01), end = c(2011,01), frequency = 12)

#Estimate model:

nht.hw1 <- HoltWinters(ga_ts_train, gamma = FALSE)
#Obtain forecasts:
nht.forecast <- forecast(nht.hw1, h = 12)

#Check accuracy:
accuracy(nht.forecast, x = ga_ts_test)


## Google Trends Analysis Results

### Forecasting with Holt-Winters

# We will utilize *forecast* library build specifically to work with time-series.
# We will instruct the algorithm to perform forecasts for trend analysis for the next 
# 2 years and will take into all avaliable data from 2004 onwards. 


#University_world
# Convert the data to be officially "time-series" data
ga_ts <- ts(google$University_world, start = c(2004,01), end = c(2020,05), frequency = 12)
# Compute the Holt-Winters filtering for the data
forecast1 <- HoltWinters(ga_ts)
# Generate a forecast for next 24 months of the blog sessions  
#hchart(forecast(forecast1, h = 24))  
plotex <- hchart(forecast(forecast1, h = 24))
htmlwidgets::saveWidget(widget = plotex, file = "plot.html")
#setwd("~")
webshot::webshot(url = "plot.html", 
                 file = "plotUW.png", delay = 3, vheight = 800, vwidth = 800)

#Bachelors_world
# Convert the data to be officially "time-series" data
ga_ts <- ts(google$Bachelors_world, start = c(2004,01), end = c(2020,05), frequency = 12)
# Compute the Holt-Winters filtering for the data
forecast1 <- HoltWinters(ga_ts)
# Generate a forecast for next 24 months of the blog sessions  
plotex <- hchart(forecast(forecast1, h = 24))
htmlwidgets::saveWidget(widget = plotex, file = "plot.html")
#setwd("~")
webshot::webshot(url = "plot.html", 
                 file = "plotBW.png", delay = 3, vheight = 800, vwidth = 800)

#Masters_world
# Convert the data to be officially "time-series" data
ga_ts <- ts(google$Masters_world, start = c(2004,01), end = c(2020,05), frequency = 12)
# Compute the Holt-Winters filtering for the data
forecast1 <- HoltWinters(ga_ts)
# Generate a forecast for next 24 months of the blog sessions  
plotex <- hchart(forecast(forecast1, h = 24))
htmlwidgets::saveWidget(widget = plotex, file = "plot.html")
#setwd("~")
webshot::webshot(url = "plot.html", 
                 file = "plotMW.png", delay = 3, vheight = 800, vwidth = 800)

#Bachelors_UAE
# Convert the data to be officially "time-series" data
ga_ts <- ts(google$Bachelors_UAE, start = c(2004,01), end = c(2020,05), frequency = 12)
# Compute the Holt-Winters filtering for the data
forecast1 <- HoltWinters(ga_ts)
# Generate a forecast for next 24 months of the blog sessions  
plotex <- hchart(forecast(forecast1, h = 24))
htmlwidgets::saveWidget(widget = plotex, file = "plot.html")
#setwd("~")
webshot::webshot(url = "plot.html", 
                 file = "plotBD.png", delay = 3, vheight = 800, vwidth = 800)


#Masters_UAE
# Convert the data to be officially "time-series" data
ga_ts <- ts(google$Masters_UAE, start = c(2004,01), end = c(2020,05), frequency = 12)
# Compute the Holt-Winters filtering for the data
forecast1 <- HoltWinters(ga_ts)
# Generate a forecast for next 24 months of the blog sessions  
plotex <- hchart(forecast(forecast1, h = 24))
htmlwidgets::saveWidget(widget = plotex, file = "plot.html")
#setwd("~")
webshot::webshot(url = "plot.html", 
                 file = "plotMD.png", delay = 3, vheight = 800, vwidth = 800)

#Online_University_world
# Convert the data to be officially "time-series" data
ga_ts <- ts(google$Online_University_world, start = c(2004,01), end = c(2020,05), frequency = 12)
# Compute the Holt-Winters filtering for the data
forecast1 <- HoltWinters(ga_ts)
# Generate a forecast for next 24 months of the blog sessions  
plotex <- hchart(forecast(forecast1, h = 24))
htmlwidgets::saveWidget(widget = plotex, file = "plot.html")
#setwd("~")
webshot::webshot(url = "plot.html", 
                 file = "plotOUW.png", delay = 3, vheight = 800, vwidth = 800)


#Online_Masters_world
# Convert the data to be officially "time-series" data
ga_ts <- ts(google$Online_Masters_world, start = c(2004,01), end = c(2020,05), frequency = 12)
# Compute the Holt-Winters filtering for the data
forecast1 <- HoltWinters(ga_ts)
# Generate a forecast for next 24 months of the blog sessions  
plotex <- hchart(forecast(forecast1, h = 24))
htmlwidgets::saveWidget(widget = plotex, file = "plot.html")
#setwd("~")
webshot::webshot(url = "plot.html", 
                 file = "plotOMW.png", delay = 3, vheight = 800, vwidth = 800)

summary(forecast1)



# Conclusion - Final Results

#create and display table with names of variables and comments
final_results <- data.frame(
  Dataset = c("Census Data","Census Data","Census Data","Google Reviews","Google Reviews","Google Trends"),
  Algorithm = c("Multi-Linear Reg","Quantile Reg", "Random Forest", "Factorization", "Sentiment", "Holt-Winters Forecast"),
  RMSE = c("1.58","1.51","1.22","0.09","N/A","14.8"),
  MAPE = c("7.69","1.84","4.92","0.52","N/A","17.3"))

kable(final_results, caption = "Final Project Results - Algorithms") %>%
  kable_styling(position = "center",
                font_size = 11,
                full_width = FALSE, latex_options = "hold_position") %>%
  row_spec(0, bold = T, color = "white", background = "#D7261E")


## Session Info
pander(sessionInfo(), compact=TRUE) 


# Interested to collaborate on this project?
# **Get in touch via [LinkedIn](https://www.linkedin.com/in/klimpopov/) **
# or **Check for updates on the project on [GitHub](https://github.com/klim-hb/diy-project) **
 
  