#Conduct market basket analysis to identify purchasing patterns

####Install and Load Packages####
install.packages("arules")
library(arules)
require(arules) #loads all techniques within arules into environment
install.packages("arulesViz", repos = "https://cran.r-project.org/web/packages/arulesViz/index.html", dependencies = TRUE)
library(arulesViz)
require(arulesViz)
install.packages("TSP")
library(TSP)
install.packages("caTools")
library(caTools)
install.packages("whisker")
library(whisker)
install.packages("fpc")
library(fpc)
#Installing arulesViz required that installing and loading of a few more packages(TSP, caTool, whisker, and fpc)



####Upload Data####
Trans <- read.transactions("ElectronidexTransactions2017.csv", sep = ",") #Changes data into a sparse matrix (i.e.: each different product will have its own column and each transaction will have its own row. A '1' in the transaction row indicates that the product was purchased and a '0' if it wasn't.
Trans #Duplicae items not removed as if exact transactions occurred multiple times then this will aid in our analysis

####Data Analysis####
summary(Trans)
#Function above returns basic information about transaction data including what items were purchased most often
#Provides us with number of transactions per number of items purchased
#Provides 5-number plot (min, max, median, etc.) for number of items purchased per transaction
#Provides density (1 - sparsity)

#Scratch Work to understand more about summary data
9835*125*0.03506172 #43104 (out of total possible 9835*125 = 1229375) is the total number of items purchased among all 9,835 transactions
43104/125 #Equals 344.832; if each product was purchased an equal number of times, it would be purchased 344 times
344.832/43104 # Equals .008; Products with a support higher than .008 indicates a higher than average purchase rate



####Inspection####
inspect(Trans) #Veiw of all individual transactions
inspect(Trans[1:5]) #Views first five transactions
length(Trans) #Number of total transactions
size(Trans) #Number of items per transaction
LIST(Trans) #Lists the transactions from the dataset


itemLabels(Trans) #Lists all the products sold
write.table(itemLabels(Trans), "ItemsforSale.txt") #Lists first all the unique products sold in a text file
itemFrequency(Trans[,1]) #Prints support of item number listed (corresponds to the n-th item in dataset with n items); Support indicates percentage of times object appears out of total number of items purchased
itemFrequency(Trans[1,]) #Returns the first row vector of the sparse matrix('1' indicates purchase; '0' indicates otherwise)
itemFrequency(Trans[,1:10]) #Prints support for each of first 10 items (columns); items appear alphabetically
itemFrequency(Trans[1:10,]) #Lists the fraction (in decimal form) of the number of times each item was purchased in the range specified (in this case 1:10)
itemFrequencyPlot(Trans, support = .10) #Plots items that have >= support specified
itemFrequencyPlot(Trans, topN = 5) #Returns items with the five highest support


image(Trans[1:100]) #x-axis corresponds 'Item #'; y-axis corresponds to the transaction number range(as defined by code written); each dot corresponds to whether that item 'x' was purchased in transaction 'y'
image(sample(Trans, 100)) #Does the same thing as above but takes a random sample of whatever amount specified




####Notes####
#Defintion: Confidence is a measure of the proportion of transactions where the presence of an item or set of items results in the presence of another set of items
# conf{A,B} --> {C} = (support {A,B,C})/(support{A,B}); which transactions that have A and B (denominator) also have C (numerator)



####Rules Analysis####
MBRule <- apriori(Trans, parameter = list(support=0.01, confidence=.50, minlen=2)) #Returns all transactions that meet or exceed support, confidence, and minlen parameters
MBRule #Run to seee how many rules were created
summary(MBRule) #Prints out general information about rules found; lhs means left-hand side, and rhs means right-hand side


inspect(MBRule[1:19]) #Lift = confidence/support of right-hand side (rhs)
#Lift is how much more likely an item is to be purchased relative to its general purchase rate (higher lift is better)
MBRuleInspect <- inspect(MBRule[1:19]) #Created object to create table (see about 20 lines below)
MBRuleInspect


inspect(sort(MBRule, by="support")[1:4]) # Sorts by 'by' parameter and returns first four rules; can sort by lift, support, count or confidence

#Subset values based on subset rule
help(subset)
MBRuleSub <- subset(MBRule, subset = lhs %in% "HP Laptop") #Creates a subset of rules that meet subset= requirement
MBRuleSub #Returns number of rules under specified subset rule
inspect(MBRuleSub) #Prints only rules that meet information in subset = parameter


is.redundant(MBRule) #Tests to see if any of the rules created are duplicates of each other
is.redundant(MBRuleSub) #Tests to see if any of the rules created are duplicates of each other



####Plotting####
plot(MBRule, method="graph", control=list(type="p")) #Review https://www.rdocumentation.org/packages/graphics/versions/3.4.0/topics/plot
plot(MBRule, method="graph", control=list(type="l")) #Review https://www.rdocumentation.org/packages/graphics/versions/3.4.0/topics/plot
plot(MBRule, method="graph", control=list(type="b")) #Review https://www.rdocumentation.org/packages/graphics/versions/3.4.0/topics/plot
plot(MBRule, method="graph", control=list(type="s")) #Review https://www.rdocumentation.org/packages/graphics/versions/3.4.0/topics/plot

BPlot <- barplot(sort(itemFrequency(Trans[,1:3]), decreasing = FALSE)) #Prints abr plot of items with three lowest frequencies
BPlot


####Write To####
WriteTable <- write.table(MBRuleInspect, "MBRuleInspect.txt") #Need to write to table before writing to .csv file
write.csv(WriteTable, "MBRuleInspect.csv")
