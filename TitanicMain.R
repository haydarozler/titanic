# read training data
df <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)

# get brief info about dataset
summary(df)
str(df)

# some information and initail comments about each column
# PassengerId: Just an idea, doesnt mean anything else
# Survived: It shows whether passenger survived or not. 
#           0: not survived. 1: survived
#           %38 survived.
#           No missing data. 
# Pclass: Ticket class
#        1st = Upper
#        2nd = Middle
#        3rd = Lower
# Name: Name of the people
#       Wouldnt mean a lot. Can be used in case if there is missing sex data or family data.
# Sex: female or male. 
#      female: 314. male: 577
#      No missing data
# Age: Age of the people
#      177 missing data.Which is almost %20 of all passengers. Something to think about.
# SibSp: # of siblings / spouses aboard the Titanic
#        Sibling = brother, sister, stepbrother, stepsister
#        Spouse = husband, wife (mistresses and fiancés were ignored)
# Parch: # of parents / children aboard the Titanic
#        Child = daughter, son, stepdaughter, stepson
#        Some children travelled only with a nanny, therefore parch=0 for them.
# Ticket: Ticket number. 
#         No missing data.
#         Can be used to find some rfamily relationships or somebody to travel together. 
# Fare: Fare.
#       No missing value but some values are zero. Should be checked.
# Cabin: Cabin number
#        Most of the data are missing and it is already a question mark. Should be evaulated.
# Embarked: Port of Embarkation
#           C = Cherbourg, Q = Queenstown, S = Southampton
#           Only 2 data are missing


# Now time to dive into details of each attribute

# Since we need some visualizations lets import library ggplot2
library(ggplot2)

# Since we need some tabulizations lets import library ztable
library(dplyr)
library(ztable)

# Then turn 4 attributes into factor attributes
# Bunu yapmazsan visualisation da fill dediğin yerler hiç bir işe yaramıyor. Bu önemli.
df$Pclass <- as.factor(df$Pclass)
df$Survived <- as.factor(df$Survived)
df$Sex <- as.factor(df$Sex)
df$Embarked <- as.factor(df$Embarked)

# First survived bar plot
ggplot(df, aes(x = Survived)) +
  theme_bw() +  
  geom_bar() + 
  labs(y = 'Passenger Count', title = 'Titanic Survival Rates')

dfr_prop <- df %>% 
  count(Survived) %>%                 # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n)*100)    # prop = n/sum(n) works too

dfr_prop

ztable(as.data.frame(dfr_prop), digits = 2,
       caption = 'Survival Rates')

# It tells us 342 people are survived and 549 are not survived



# Pclass distribution
ggplot(df, aes(x = Pclass)) +
  theme_bw() +  
  geom_bar() + 
  labs(y = 'Passenger Count', title = 'Ticket Class Distribution')

dfr_prop <- df %>% 
  count(Pclass) %>%                 # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n)*100)    # prop = n/sum(n) works too

dfr_prop

ztable(as.data.frame(dfr_prop), digits = 2,
       caption = 'Pclass Table')


# Draw the bar plot for pclass by filling survived values.
ggplot(df, aes(x = Pclass, fill = Survived)) +
  theme_bw() +
  geom_bar() + 
  labs(y = 'Passenger Count', title = 'Titanic Survival Rates by Pclass')
# It shows that there is obvious correlation between pclass and survival. 
# 1st class passengers survived more.
# It tells us that pclass is a strong candidate for being one of the independetn variables

# Lets create a model just for Pclass and prove that it has a strong correlation with Survived

model <- glm (Survived ~ Pclass, data = df, family = binomial)
summary(model)

# Now Pclass looks very meaningful to use. Lets keep it in mind.

# Pclass and Fare can be correlated
# Lets evaluate fare data
# Another suspicious one because there is Pclass which can serve much better
sum(df$Fare==0)
# 15 tane 0 olan kayıt var. Çok değil. 

ggplot(subset(df, df$Survived == 1), aes(x = "", y = Fare)) + 
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0, 800, 50)) +
  labs(title="Boxplot for fare", y="Fare")

ggplot(subset(df, df$Survived == 0), aes(x = "", y = Fare)) + 
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0, 800, 50)) +
  labs(title="Boxplot for fare", y="Fare")

ggplot(df, aes(x = "", y = Fare)) + 
  geom_boxplot() +
  scale_y_continuous(breaks=seq(0, 800, 5)) +
  labs(title="Boxplot for fare", y="Fare")

boxplot.stats(df$Fare)

summary(subset(df, df$Survived == 1))
summary(subset(df, df$Survived == 0))

ggplot(df, aes(x=Fare, y=Survived)) + 
  geom_point(aes(col=Survived), size=3) +  
  labs(title="Fare vs Survived", y="Survived", x="Fare")

ggplot(df, aes(x=Fare, y=Pclass)) + 
  geom_point(aes(col=Survived), size=3) +  
  labs(title="Fare vs Pclass", y="Survived", x="Pclass")

# It looks not meaningful but lets create a model and check correlations
model <- glm (Survived ~ Pclass + Fare, data = df, family = binomial)
summary(model)


# With addition of fare, pclass2 is broken.
# Lets make it only with fare and evaluate
model <- glm (Survived ~ Fare, data = df, family = binomial)
summary(model)


# Lets update outliers with 3rd quadrant value and then try again
sum(df$Fare>=100)
df$Fare[df$Fare >= 100] <- 100

# Bu deneme mahiyetindeki outlier temizliğinden sonra bir daha modelleme yaptım 
# Fare açık ara survived ile daha iyi etkileşim gösterir hale geldi
# Pclass'ın önüne geçti. Harbiden ilginç, sorgulamak lazım bana hala çok anlamlı gelemiyor. 

# It looks correlated with survived but at the same time we have to make the model either fare or pclass
# We will try with both of them seperately

# Lets continue with sex
# Sex distribution
ggplot(df, aes(x = Sex)) +
  theme_bw() +  
  geom_bar() + 
  labs(y = 'Passenger Count', title = 'Sex Distribution')

dfr_prop <- df %>% 
  count(Sex) %>%                 # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n)*100)    # prop = n/sum(n) works too

dfr_prop

ztable(as.data.frame(dfr_prop), digits = 2,
       caption = 'Sex Table')


# Draw the bar plot for pclass by filling survived values.
ggplot(df, aes(x = Sex, fill = Survived)) +
  theme_bw() +
  geom_bar() + 
  labs(y = 'Passenger Count', title = 'Titanic Survival Rates by Sex')
# It shows that there is obvious correlation between sex and survival. 
# female passengers survived more.
# It tells us that sex is another strong candidate for being one of the independetn variables

# Lets group according to Pclass and check Sex for survival
ggplot(df, aes(x = Sex, fill = Survived)) +
  theme_bw() +
  facet_wrap(~ Pclass) +
  geom_bar() + 
  labs(y = 'Passenger Count', title = 'Titanic Survival Rates for Sex grouped by Pclass')

# Lets find the survival rates grouped by Sex and Pclass and tabulate them

dfr_prop <- df %>% 
  count(Pclass, Sex, Survived) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n)*100)    # prop = n/sum(n) works too

ztable(as.data.frame(dfr_prop), digits = 2,
               caption = 'A table grouped by survived, pclass and sex')

# Lets create a model for sex only and sex with pclass and sex with fare seperately
model <- glm (Survived ~ Sex, data = df, family = binomial)
summary(model)
# Sex looks strognly correlated

model <- glm (Survived ~ Sex + Pclass, data = df, family = binomial)
summary(model)

model <- glm (Survived ~ Sex + Fare, data = df, family = binomial)
summary(model)

model <- glm (Survived ~ Sex + Fare + Pclass, data = df, family = binomial)
summary(model)

# As a result difficult to say which one is better fare or pclass.
# But that is for sure they shouldnt be together



# Lets create the histogram for age
ggplot(df, aes(df$Age, fill = Survived)) + 
  geom_histogram(binwidth = 5)

# Obviously kids survived more
# adults rates dont change too much
# as a result age can be one of the independent values we may think in the model

# lets investigate outliers

temp <- subset(df, !is.na(df$Age))
sd(temp$Age)
mean(temp$Age)
median(temp$Age)

ggplot(temp, aes(x = "", y = temp$Age)) + 
  geom_boxplot()

ggplot(temp, aes(x = Survived, y = temp$Age)) + 
  geom_boxplot()

# I am for removing one data which is age=80. Even if it shows they are outliers. 
# Because I think this data is still meaningful

# Lets talk about what we can do with empty data in age. 
# There are lots of empty data. 
# Age histogram shows us that percantage of adults are almost the same for different age groups.
# So we can easily fill them with mean.

temp <- subset(df, !is.na(df$Age))

# An idea from an observation
# Age and sex are different independet variables
# At the same time it is obvious that adult survival rates are close to each other but different from children survival rates
# It can be worthy to evaluate may be with dimensiolaty reduction it can be male, female, child 
# if there is no difference between girls and boys survival rates.

# Lets visualize it

ggplot(temp, aes(x=Age, y=Sex)) + 
  geom_point(aes(col=Survived), size=3) +  
  scale_x_continuous(breaks=seq(0, 80, 2)) +
  labs(title="Age vs Sex", subtitle="With coloring according to survival", y="Sex", x="Age")

# It is kind of prooving the idea
# Lets see the ones below 10

temp <- subset(df, df$Age <= 10)
ggplot(temp, aes(x=Age, y=Sex)) + 
  geom_point(aes(col=Survived), size=3) +  
  labs(title="Age vs Sex for kids", subtitle="With coloring according to survival", y="Sex", x="Age")

# It seems there is no seperation between kids according to by being girl or boy
# We can assume children are below 13 according to the visualizations

#Lets make a model for only Age first
model <- glm (Survived ~ Age, data = df, family = binomial)
summary(model)

# We cant talk about a strong correlation
# Lets try with sex
model <- glm (Survived ~ Age + Sex, data = df, family = binomial)
summary(model)

# When you add sex, then correlation is ruined much more
# Age is a big question mark now
# The only thing is it is obvious that below a certaion age survival rate is higher
# we can make some feature engineering and make sex data as child, male, female and delete age data



# Lets check embarked data

# Draw the bar plot for embarked by filling survived values.
ggplot(df, aes(x = Embarked, fill = Survived)) +
  theme_bw() +
  geom_bar() + 
  labs(y = 'Passenger Count', title = 'Titanic Survival Rates by Embarked')
# It tells that survival rates of people from Southampton are lower than others
# 2 data are missing they can be filled with S easily because people took the ship mostly from S
# It is not a field with a big effect on survival
# We can check it with ticket class and if we can see a correlation there we can even omit this one

ggplot(df, aes(x = Embarked, fill = Pclass)) +
  theme_bw() +
  geom_bar() + 
  labs(y = 'Passenger Count', title = 'Titanic Ticket Classes by Embarked')

# It is a suspicious field because as we predicted survival depends on more pclass not embarked. 

# Lets make a model alone and with pclass
model <- glm (Survived ~ Embarked, data = df, family = binomial)
summary(model)
# It looks so bad and uncorrelated

model <- glm (Survived ~ Embarked + Pclass, data = df, family = binomial)
summary(model)

# With these results it looks we shouldnt take it into the model

# With the information we collected till now, we can try this model which makes sense
model <- glm (Survived ~ Sex + Age + Pclass, data = df, family = binomial)
summary(model)


# Lest visualize sibsp and parch data

ggplot(df, aes(x = SibSp, fill = Survived)) +
  theme_bw() +
  geom_bar() + 
  labs(y = 'Passenger Count', title = 'Titanic Survival Rates by SibSp')

ggplot(df, aes(x = Parch, fill = Survived)) +
  theme_bw() +
  geom_bar() + 
  labs(y = 'Passenger Count', title = 'Titanic Survival Rates by Parch')

# sibsp tables

dfr_prop <- df %>% 
  count(SibSp) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n)*100)    # prop = n/sum(n) works too

ztable(as.data.frame(dfr_prop), digits = 2,
       caption = 'A table grouped by sibspp')

dfr_prop <- df %>% 
  count(SibSp, Survived) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n)*100)    # prop = n/sum(n) works too

ztable(as.data.frame(dfr_prop), digits = 2,
       caption = 'A table grouped by sibsp and survived')

# Sanki böyle kaç adet olması değilde birisinin olması olmaması etkili gibi

# parch tables

dfr_prop <- df %>% 
  count(Parch) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n)*100)    # prop = n/sum(n) works too

ztable(as.data.frame(dfr_prop), digits = 2,
       caption = 'A table grouped by parch')

dfr_prop <- df %>% 
  count(Parch, Survived) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n)*100)    # prop = n/sum(n) works too

ztable(as.data.frame(dfr_prop), digits = 2,
       caption = 'A table grouped by parch and survived')

# Aynı şey parch içinde geçerli yani sanki 0 veya diğerleri şeklinde ayrılmış

# Her birinin etkisine bakalım
model <- glm (Survived ~ SibSp, data = df, family = binomial)
summary(model)

model <- glm (Survived ~ Parch, data = df, family = binomial)
summary(model)

# Parch kısmen daha iyi olmakla birlikte ikisi de survived ile çok da correlated değil
model <- glm (Survived ~ Parch + SibSp, data = df, family = binomial)
summary(model)
# beraber bakıldığında da bir düzeyde korelasyon var gibi ama çok da parlak değil
# şimdi bir de ikisini de 0 ve 1 olarak değiştirip deneyelim

df$Parch2 <- with(df, Parch)
df$SibSp2 <- with(df, SibSp)

df$Parch2[df$Parch2 > 0] <- 1
df$SibSp2[df$SibSp2 > 0] <- 1

# Her birinin etkisine bakalım
model <- glm (Survived ~ SibSp2, data = df, family = binomial)
summary(model)

model <- glm (Survived ~ Parch2, data = df, family = binomial)
summary(model)

model <- glm (Survived ~ Parch2 + SibSp2, data = df, family = binomial)
summary(model)

# Burada da şöyle oldu, ayrı ayrı baktığında gayet correlated iken ve anlamlı bir sonuç elde etmiş iken birleştirince p-values bozulmaya başladı
# İkisini birden kullanmak anlamlı olmayabilir. 
# Bir de belki ikisinin toplamına bakıabilir ama yine var yok babında
df$ParSib <- with(df, Parch + SibSp)
df$ParSib[df$ParSib > 0] <- 1
model <- glm (Survived ~ ParSib, data = df, family = binomial)
summary(model)
# En güzel sonuç işte burada yakalandı. Gayet güzel bir p score elde ettik

#Bir de en anlamlı birlikte çalışan modele sadece bu son parametreyi ekleyelim
model <- glm (Survived ~ Sex + Age + Pclass + ParSib, data = df, family = binomial)
summary(model)
# Parsib tek başına iken gayet iyi bir noktada idi ama diğerleri ile beraber kötüleşti p score





# IMPORTANT NOTES
# Outlier removal for age (may be only 1)
# Using Child, Male, Female and removing Age totally
# Filling empty age data (master are children)
# Embarked datasının ne kadar önemli olduğu tartışılır. Remove edilebilir.
# Fare should be reduced. SO many outlier and there is already pclass. 
# Fare outliers can be cleaned and then fare can be thought instead of pclass.
# Cabin should be reduced.So many empty data.
# sibsp and parch will be evaluated later

# read training data
df <- read.csv("train.csv", header = TRUE, stringsAsFactors = FALSE)

df$Age2 <- with(df, df$Age)
df$Age <- with(df, ifelse(grepl("Master",df$Name) & is.na(df$Age), 5, df$Age))
df$ParSib <- with(df, Parch + SibSp)
df$ParSib[df$ParSib > 0] <- 1
df$Age2[df$Age<=8] <- "child"
df$Age2[df$Age>8] <- "adult"

df$Age2 <- as.factor(df$Age2)
df$ParSib <- as.factor(df$ParSib)
df$Pclass <- as.factor(df$Pclass)
df$Survived <- as.factor(df$Survived)
df$Sex <- as.factor(df$Sex)
df$Embarked <- as.factor(df$Embarked)

dfagefull <- subset(df, !is.na(df$Age))
dfagena <- subset(df, is.na(df$Age))


library(caTools)
library(ggplot2)
library(dplyr)

#set.seed(88)
#split <- sample.split(df$Pclass, SplitRatio = 0.80)

#get training and test data
#dftrain <- subset(df, split == TRUE)
#dftest <- subset(df, split == FALSE)

#logistic regression model
#model <- glm (Survived ~ Pclass + Sex + Age, data = dftrain, family = binomial)
#summary(model)

#predict <- predict(model, newdata=dftest, type = 'response')

#a <- as.data.frame(predict)

#confusion matrix
#table(dftest$Survived, predict > 0.70)

#a <- mutate(a, id = rownames(a))
#colnames(a)[2] <- "PassengerId"
#b <- merge(dftest, a)

#ggplot(b, aes(b$predict, fill = Survived)) + 
#  geom_histogram(binwidth = 0.05)

# YORUMLAR
# Şunu gördük, pclass + embarked + sex + age (eğer var ise)
# ancak age bazen yanılıtabiliyor gibi geldi bana, acaba age'i child ve adult diye ayırmak işe yara mı düşüncesi geliyor bana
# bir de bazen fare plcass'dan daha anlamlı sanki, ikili bir model de uygulanabilir

set.seed(88)
split <- sample.split(dfagefull$Pclass, SplitRatio = 0.80)

dfagefulltrain <- subset(dfagefull, split == TRUE)
dfagefulltest <- subset(dfagefull, split == FALSE)

split <- sample.split(dfagena$Pclass, SplitRatio = 0.80)

dfagenatrain <- subset(dfagena, split == TRUE)
dfagenatest <- subset(dfagena, split == FALSE)


#logistic regression model age dolu olanlar için
modelagefull <- glm (Survived ~ Sex + Age2 + Pclass + Embarked, data = dfagefulltrain, family = binomial)
summary(modelagefull)

predict <- predict(modelagefull, newdata=dfagefulltest, type = 'response')

a <- as.data.frame(predict)

#confusion matrix
table(dfagefulltest$Survived, predict > 0.60)

a <- mutate(a, id = rownames(a))
colnames(a)[2] <- "PassengerId"
b <- merge(dfagefulltest, a)

ggplot(b, aes(b$predict, fill = Survived)) + 
  geom_histogram(binwidth = 0.05)



#logistic regression model age na olanlar için
modelagena <- glm (Survived ~ Pclass + Embarked + Sex, data = dfagenatrain, family = binomial)
summary(modelagena)

predict <- predict(modelagena, newdata=dfagenatest, type = 'response')

a <- as.data.frame(predict)

#confusion matrix
table(dfagenatest$Survived, predict > 0.50)

a <- mutate(a, id = rownames(a))
colnames(a)[2] <- "PassengerId"
b <- merge(dfagenatest, a)

ggplot(b, aes(b$predict, fill = Survived)) + 
  geom_histogram(binwidth = 0.05)


# FOR SUBMISSION

dfsub <- read.csv("test.csv", header = TRUE, stringsAsFactors = FALSE)
summary(dfsub)

dfsub$Age2 <- with(dfsub, dfsub$Age)
dfsub$Age <- with(dfsub, ifelse(grepl("Master",dfsub$Name) & is.na(dfsub$Age), 5, dfsub$Age))
dfsub$ParSib <- with(dfsub, Parch + SibSp)
dfsub$ParSib[dfsub$ParSib > 0] <- 1
dfsub$Age2[dfsub$Age<=8] <- "child"
dfsub$Age2[dfsub$Age>8] <- "adult"

dfsub$Age2 <- as.factor(dfsub$Age2)
dfsub$ParSib <- as.factor(dfsub$ParSib)
dfsub$Pclass <- as.factor(dfsub$Pclass)
dfsub$Sex <- as.factor(dfsub$Sex)
dfsub$Embarked <- as.factor(dfsub$Embarked)

dfsubagefull <- subset(dfsub, !is.na(dfsub$Age))
dfsubagena <- subset(dfsub, is.na(dfsub$Age))

predict <- predict(modelagefull, newdata=dfsubagefull, type = 'response')

c <- as.data.frame(predict)

c <- mutate(c, id = rownames(c))
dfsubagefull <- mutate(dfsubagefull, id = rownames(dfsubagefull))
dfsubagefull <- merge(dfsubagefull, c)

dfsubagefull$Survived <- with(dfsubagefull, ifelse(dfsubagefull$predict > 0.6, 1, 0))


predict <- predict(modelagena, newdata=dfsubagena, type = 'response')

d <- as.data.frame(predict)

d <- mutate(d, id = rownames(d))
dfsubagena <- mutate(dfsubagena, id = rownames(dfsubagena))
dfsubagena <- merge(dfsubagena, d)

dfsubagena$Survived <- with(dfsubagena, ifelse(dfsubagena$predict > 0.5, 1, 0))

dfsubtotal <- rbind(dfsubagefull, dfsubagena)

dfsubmit <- dfsubtotal[,c("PassengerId","Survived")]
dfsubmit <- dfsubmit[order(dfsubmit$PassengerId),]


write.csv(x=dfsubmit, "C:\\Users\\ho46888\\Documents\\DataScience\\Kaggle Competitions\\Titanic with R\\submit.csv", row.names = FALSE)

############################
# Decision Tree denemeleri #
############################

library(rpart)

fit <- rpart(Survived ~ Sex + Age2 + Pclass + Embarked, method="class", data=dfagefulltrain)

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

# plot tree
plot(fit, uniform=TRUE,
     main="Classification Tree for Titanic")
text(fit, use.n=TRUE, all=TRUE, cex=.8)


predict <- predict(fit, newdata=dfagefulltest, type = 'class')

a <- as.data.frame(predict)

#confusion matrix
table(predict, dfagefulltest$Survived)

a <- mutate(a, id = rownames(a))
colnames(a)[2] <- "PassengerId"
b <- merge(dfagefulltest, a)

ggplot(b, aes(b$predict, fill = Survived)) + 
  geom_histogram(binwidth = 0.05)



