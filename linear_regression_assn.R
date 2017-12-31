# 1.install required libraries and import dataset########################

install.packages("dplyr")
install.packages("tidyr")
install.packages("MASS")
install.packages("car")

library(dplyr)
library(tidyr)
library(MASS)
library(car)

car_df<-car_df_copy<-read.csv("CarPrice_Assignment.csv")

#check structure of the car dataset
str(car_df)

# 2.perform data cleaning and brief analysis#############################

#Check for duplicate values
sum(duplicated(car_df$car_ID))

#check for na values
sum(is.na(car_df$car_ID))
sum(is.na(car_df$CarName))
sum(is.na(car_df$price))

#replace porcshce with porsche in carname column
car_df$CarName<-gsub("porcshce","porsche",car_df$CarName)

#replace vw with volkswagen in carname column
car_df$CarName<-gsub("vw","volkswagen",car_df$CarName)

#replace vokswagen with volkswagen
car_df$CarName<-gsub("vokswagen","volkswagen",car_df$CarName)

#replace toyouta with toyota in carname column
car_df$CarName<-gsub("toyouta","toyota",car_df$CarName)

#replace maxda with mazda
car_df$CarName<-gsub("maxda","mazda",car_df$CarName)

#replace Nissan with nissan
car_df$CarName<-gsub("Nissan","nissan",car_df$CarName)

#convert carname column to factor again
car_df$CarName<-as.factor(car_df$CarName)

#there are values in drivewheel column as 4wd instead of fwd, convert 4wd values to fwd
car_df$drivewheel<-gsub("4","f",car_df$drivewheel)

#split carname column value by company and car model
car_df = car_df %>% mutate(company=gsub(" .*","",CarName))

#convert company column to factor
car_df$company<-as.factor(car_df$company)
car_df$drivewheel<-as.factor(car_df$drivewheel)

#3. create Dummy Variables#############################################

#since linear regression considers only numeric variable we needto convert categorical variables to numeric vlaues
#the categorical variables considered are-company,fueltype,aspiration,doornumber,carbody,drivewheel,enginelocation,enginetype,cylindernumber,fuel system.

  #a)company variable
    str(car_df$company) #22 levels ofcar company  
    summary(car_df$company)
    dummy_company<-data.frame(model.matrix( ~company,data=car_df))
    View(dummy_company)

    #remove the intercept column
    dummy_company <- dummy_company[,-1]
    #join the dummy_companydata frame with the car_df data frame
    car_df_1 <- cbind(car_df[,-3], dummy_company)
   
    
  #b)fueltype variable
    str(car_df$fueltype) #2 levels of fueltype  
    summary(car_df$fueltype)
    dummy_fueltype<-data.frame(model.matrix( ~fueltype,data=car_df))
    View(dummy_fueltype)
    
    #remove the intercept column
    dummy_fueltype <- dummy_fueltype[,-1]
    #join the dummy_fueltype data frame with the car_df data frame
    car_df_2 <- cbind(car_df_1[,-3], dummy_fueltype)    
    
    
  #c)aspiration variable
    str(car_df$aspiration) #2 levels of aspiration  
    summary(car_df$aspiration)
    dummy_asp<-data.frame(model.matrix( ~aspiration,data=car_df))
    View(dummy_asp)
    
    #remove the intercept column
    dummy_asp <- dummy_asp[,-1]
    #join the dummy_asp frame with the car_df data frame
    car_df_3 <- cbind(car_df_2[,-3], dummy_asp)    
    
  #d)doornumber variable
    str(car_df$doornumber) #2 levels ofcar company  
    summary(car_df$doornumber)
    dummy_dnum<-data.frame(model.matrix( ~doornumber,data=car_df))
    View(dummy_dnum)
    
    #remove the intercept column
    dummy_dnum <- dummy_dnum[,-1]
    #join the dummy_dnum data frame with the car_df data frame
    car_df_4 <- cbind(car_df_3[,-3], dummy_dnum)    
    
  #e)carbody variable
    str(car_df$carbody) #5 levels ofcar body  
    summary(car_df$carbody)
    dummy_cbody<-data.frame(model.matrix( ~carbody,data=car_df))
    View(dummy_cbody)
    
    #remove the intercept column
    dummy_cbody <- dummy_cbody[,-1]
    #join the dummy_cbody data frame with the car_df data frame
    car_df_5 <- cbind(car_df_4[,-3], dummy_cbody)    
    
    
    
  #f)drivewheel variable
    str(car_df$drivewheel) #2 levels of drivewheel  
    summary(car_df$drivewheel)
    dummy_dwheel<-data.frame(model.matrix( ~drivewheel,data=car_df))
    View(dummy_dwheel)
    
    #remove the intercept column
    dummy_dwheel <- dummy_dwheel[,-1]
    #join the dummy_dwheel data frame with the car_df data frame
    car_df_6 <- cbind(car_df_5[,-3], dummy_dwheel)
    

  #g)enginelocation variable
    str(car_df$enginelocation) #2 levels of engine locaiton  
    summary(car_df$enginelocation)
    dummy_eloc<-data.frame(model.matrix( ~enginelocation,data=car_df))
    View(dummy_eloc)
    
    #remove the intercept column
    dummy_eloc <- dummy_eloc[,-1]
    #join the dummy_eloc data frame with the car_df data frame
    car_df_7 <- cbind(car_df_6[,-3], dummy_eloc)
    
  #h)enginetype variable
    str(car_df$enginetype) #7 levels of enginetype  
    summary(car_df$enginetype)
    dummy_etype<-data.frame(model.matrix( ~enginetype,data=car_df))
    View(dummy_etype)
    
    #remove the intercept column
    dummy_etype <- dummy_etype[,-1]
    #join the dummy_etype data frame with the car_df data frame
    car_df_8 <- cbind(car_df_7[,-8], dummy_etype)    
    
    
  #i)cylinder number variable
    str(car_df$cylindernumber) #7 levels of cylinder number  
    summary(car_df$cylindernumber)
    dummy_cnum1<-data.frame(model.matrix( ~cylindernumber,data=car_df))
    View(dummy_cnum1)
    
    #remove the intercept column
    dummy_cnum1 <- dummy_cnum1[,-1]
    #join the dummy_companydata frame with the car_df data frame
    car_df_9 <- cbind(car_df_8[,-8], dummy_cnum1)    
    
    
    
  #j)fuelsystem variable
    str(car_df$fuelsystem) #8 levels of fuelsystem  
    summary(car_df$fuelsystem)
    dummy_fsys<-data.frame(model.matrix( ~fuelsystem,data=car_df))
    View(dummy_fsys)
    
    #remove the intercept column
    dummy_fsys <- dummy_fsys[,-1]
    #join the dummy_fsys data frame with the car_df data frame
    car_df_10 <- cbind(car_df_9[,-9], dummy_fsys)
    
  #h) create columns for the remaining type values of each variable with more than 2 levels
    
    car_df_10$companyalfaromero<-ifelse(car_df$company=="alfa-romero",1,0)
    car_df_10$carbodyconvertible<-ifelse(car_df$carbody=="convertible",1,0)
    car_df_10$enginetypedohc<-ifelse(car_df$enginetype=="dohc",1,0)
    car_df_10$cylindernumbereight<-ifelse(car_df$cylindernumber=="eight",1,0)
    car_df_10$fuelsystem1bbl<-ifelse(car_df$fuelsystem=="1bbl",1,0)
    
  #create a final variable and assign the values of car_df_10 to it
        
    car_df_final<-car-df_10
    
  #remove the company factor column to make the entire  dataset numeric  
    car_df_final<-car_df_final[,-17]
    
### 4.) Create the training and testing dataset################################
    
    #set the seed to 50, let's run it 
    set.seed(50)
    
    # randomly generate row indices for train dataset
    trainindices= sample(1:nrow(car_df_final), 0.7*nrow(car_df_final))
    
    # generate the train data set
    train = car_df_final[trainindices,]
    
    #Similarly store the rest of the observations into an object "test".
    test = car_df_final[-trainindices,]
    
    #Execute the first model_1 multilinear model in the training set. 
    model_1 <-lm(price~.,data=train)
    
    # Check the summary of model. 
    summary(model_1)

    # use the stepAIC method to eliminate all singularity variables and highly correlated independent variables
    #this helps to formulate the code for a more refined model further 
    step <- stepAIC(model_1, direction="both")

    #build model2 based on the step command
    model_2 <-lm(formula = price ~ car_ID + wheelbase + carlength + carwidth + 
                  carheight + curbweight + enginesize + boreratio + stroke + 
                  peakrpm + companyaudi + companybmw + companychevrolet + companydodge + 
                  companyisuzu + companymazda + companymercury + companymitsubishi + 
                  companynissan + companypeugeot + companyplymouth + companyrenault + 
                  companysaab + companysubaru + companytoyota + companyvolkswagen + 
                  companyvolvo + dummy_fueltype + dummy_asp + carbodyhardtop + 
                  carbodyhatchback + carbodysedan + carbodywagon + dummy_eloc + 
                  enginetypedohcv + enginetypel + enginetypeohcv + enginetyperotor + 
                  cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                  fuelsystem2bbl + fuelsystem4bbl + fuelsystemmpfi + fuelsystemspdi, 
                data = train)
   
    # Pass the model_2 in the vif function
    vif(model_2)
    
    # sort vif values to find variable with highest vifvalue and then compare them with their p-value
    sort(vif(model_2),decreasing = TRUE)
    
######Remove variables with high p-values one by one#########################
    
    #  a.)removing fuelsystemspdi variable
          model_3 <-lm(formula = price ~ car_ID + wheelbase + carlength + carwidth + 
                   carheight + curbweight + enginesize + boreratio + stroke + 
                   peakrpm + companyaudi + companybmw + companychevrolet + companydodge + 
                   companyisuzu + companymazda + companymercury + companymitsubishi + 
                   companynissan + companypeugeot + companyplymouth + companyrenault + 
                   companysaab + companysubaru + companytoyota + companyvolkswagen + 
                   companyvolvo + dummy_fueltype + dummy_asp + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon + dummy_eloc + 
                   enginetypedohcv + enginetypel + enginetypeohcv + enginetyperotor + 
                   cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                   fuelsystem2bbl + fuelsystem4bbl + fuelsystemmpfi, 
                 data = train)
            
            #summary of model
            summary(model_3)
            
            # Pass the model_3 in the vif function
            vif(model_3)
    
            # sort vif values to find variable with highest vif value and then compare them with their p-value
            sort(vif(model_3),decreasing = TRUE)
            
       # b.) now removing fuelsystem4bbl variable
            
             model_4 <-lm(formula = price ~ car_ID + wheelbase + carlength + carwidth + 
                                          carheight + curbweight + enginesize + boreratio + stroke + 
                                          peakrpm + companyaudi + companybmw + companychevrolet + companydodge + 
                                          companyisuzu + companymazda + companymercury + companymitsubishi + 
                                          companynissan + companypeugeot + companyplymouth + companyrenault + 
                                          companysaab + companysubaru + companytoyota + companyvolkswagen + 
                                          companyvolvo + dummy_fueltype + dummy_asp + carbodyhardtop + 
                                          carbodyhatchback + carbodysedan + carbodywagon + dummy_eloc + 
                                          enginetypedohcv + enginetypel + enginetypeohcv + enginetyperotor + 
                                          cylindernumberfive + cylindernumberfour + cylindernumbersix + 
                                          fuelsystem2bbl + fuelsystemmpfi, 
                                        data = train)
             
             #summary of model
             summary(model_4)
             
             # Pass the model_4 in the vif function
             vif(model_4)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_4),decreasing = TRUE)
       
        # c.) now removing cylindernumbersix variable
             
             model_5 <-lm(formula = price ~ car_ID + wheelbase + carlength + carwidth + 
                            carheight + curbweight + enginesize + boreratio + stroke + 
                            peakrpm + companyaudi + companybmw + companychevrolet + companydodge + 
                            companyisuzu + companymazda + companymercury + companymitsubishi + 
                            companynissan + companypeugeot + companyplymouth + companyrenault + 
                            companysaab + companysubaru + companytoyota + companyvolkswagen + 
                            companyvolvo + dummy_fueltype + dummy_asp + carbodyhardtop + 
                            carbodyhatchback + carbodysedan + carbodywagon + dummy_eloc + 
                            enginetypedohcv + enginetypel + enginetypeohcv + enginetyperotor + 
                            cylindernumberfive + cylindernumberfour + fuelsystem2bbl + fuelsystemmpfi, 
                          data = train)
             
             #summary of model
             summary(model_5)
             
             # Pass the model_5 in the vif function
             vif(model_5)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_5),decreasing = TRUE)
           
     # d.) now removing cylindernumberfive variable
             
             model_6 <-lm(formula = price ~ car_ID + wheelbase + carlength + carwidth + 
                            carheight + curbweight + enginesize + boreratio + stroke + 
                            peakrpm + companyaudi + companybmw + companychevrolet + companydodge + 
                            companyisuzu + companymazda + companymercury + companymitsubishi + 
                            companynissan + companypeugeot + companyplymouth + companyrenault + 
                            companysaab + companysubaru + companytoyota + companyvolkswagen + 
                            companyvolvo + dummy_fueltype + dummy_asp + carbodyhardtop + 
                            carbodyhatchback + carbodysedan + carbodywagon + dummy_eloc + 
                            enginetypedohcv + enginetypel + enginetypeohcv + enginetyperotor + 
                            cylindernumberfour + fuelsystem2bbl + fuelsystemmpfi, 
                          data = train)
             
             #summary of model
             summary(model_6)
             
             # Pass the model_5 in the vif function
             vif(model_6)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_6),decreasing = TRUE)
             
             
     # e.) now removing enginetypedohcv,carbodysedan,carbodywagon variables
             
             model_7 <-lm(formula = price ~ car_ID + wheelbase + carlength + carwidth + 
                            carheight + curbweight + enginesize + boreratio + stroke + 
                            peakrpm + companyaudi + companybmw + companychevrolet + companydodge + 
                            companyisuzu + companymazda + companymercury + companymitsubishi + 
                            companynissan + companypeugeot + companyplymouth + companyrenault + 
                            companysaab + companysubaru + companytoyota + companyvolkswagen + 
                            companyvolvo + dummy_fueltype + dummy_asp + carbodyhardtop + 
                            carbodyhatchback + dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                            cylindernumberfive + cylindernumberfour + fuelsystem2bbl + fuelsystemmpfi, 
                          data = train)
             
             #summary of model
             summary(model_7)
             
             # Pass the model_7 in the vif function
             vif(model_7)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_7),decreasing = TRUE)

                          
       # f.) now removing companysaab,companyisuzu variables
             
             model_8 <-lm(formula = price ~ car_ID + wheelbase + carlength + carwidth + 
                            carheight + curbweight + enginesize + boreratio + stroke + 
                            peakrpm + companyaudi + companybmw + companychevrolet + companydodge + 
                            companymazda + companymercury + companymitsubishi + 
                            companynissan + companypeugeot + companyplymouth + companyrenault + 
                            companysubaru + companytoyota + companyvolkswagen + 
                            companyvolvo + dummy_fueltype + dummy_asp + carbodyhardtop + 
                            carbodyhatchback + dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                            cylindernumberfive + cylindernumberfour + fuelsystem2bbl + fuelsystemmpfi, 
                          data = train)
             
             #summary of model
             summary(model_8)
             
             # Pass the model_8 in the vif function
             vif(model_8)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_8),decreasing = TRUE)
             
             
      # g.) now removing companyaudi,companymazda,companymercury variables
             
             model_9 <-lm(formula = price ~ car_ID + wheelbase + carlength + carwidth + 
                            carheight + curbweight + enginesize + boreratio + stroke + 
                            peakrpm + companybmw + companychevrolet + companydodge + 
                            companymitsubishi + 
                            companynissan + companypeugeot + companyplymouth + companyrenault + 
                            companysubaru + companytoyota + companyvolkswagen + 
                            companyvolvo + dummy_fueltype + dummy_asp + carbodyhardtop + 
                            carbodyhatchback + dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                            cylindernumberfive + cylindernumberfour + fuelsystem2bbl + fuelsystemmpfi, 
                          data = train)
             
             #summary of model
             summary(model_9)
             
             # Pass the model_9 in the vif function
             vif(model_9)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_9),decreasing = TRUE)
             

       # h.) now removing companysubaru,companynissan,companychevrolet variables
             
             model_10 <-lm(formula = price ~ car_ID + wheelbase + carlength + carwidth + 
                            carheight + curbweight + enginesize + boreratio + stroke + 
                            peakrpm + companybmw + companydodge + 
                            companymitsubishi + 
                            companypeugeot + companyplymouth + companyrenault + 
                            companytoyota + companyvolkswagen + 
                            companyvolvo + dummy_fueltype + dummy_asp + carbodyhardtop + 
                            carbodyhatchback + dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                            cylindernumberfive + cylindernumberfour + fuelsystem2bbl + fuelsystemmpfi, 
                          data = train)
             
             #summary of model
             summary(model_10)
             
             # Pass the model_10 in the vif function
             vif(model_10)
             
             
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_10),decreasing = TRUE)
             
       # i.) now removing companyrenault variable
             
             model_11 <-lm(formula = price ~ car_ID + wheelbase + carlength + carwidth + 
                             carheight + curbweight + enginesize + boreratio + stroke + 
                             peakrpm + companybmw + companydodge + 
                             companymitsubishi + 
                             companypeugeot + companyplymouth + 
                             companytoyota + companyvolkswagen + 
                             companyvolvo + dummy_fueltype + dummy_asp + carbodyhardtop + 
                             carbodyhatchback + dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                             cylindernumberfive + cylindernumberfour + fuelsystem2bbl + fuelsystemmpfi, 
                           data = train)
             
             #summary of model
             summary(model_11)
             
             # Pass the model_11 in the vif function
             vif(model_11)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_11),decreasing = TRUE)
            
             
      # j.) now removing carlength,wheelbase variables
             
             model_12 <-lm(formula = price ~ car_ID + carwidth + 
                             carheight + curbweight + enginesize + boreratio + stroke + 
                             peakrpm + companybmw + companydodge + 
                             companymitsubishi + 
                             companypeugeot + companyplymouth + 
                             companytoyota + companyvolkswagen + 
                             companyvolvo + dummy_fueltype + dummy_asp + carbodyhardtop + 
                             carbodyhatchback + dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                             cylindernumberfive + cylindernumberfour + fuelsystem2bbl + fuelsystemmpfi, 
                           data = train)
             
             #summary of model
             summary(model_12)
             
             
             # Pass the model_12 in the vif function
             vif(model_12)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_12),decreasing = TRUE)
             
             
       # k.) now removing carid,companyvolvo,carbodyhatchback variables
             
             model_13 <-lm(formula = price ~ carwidth + 
                             carheight + curbweight + enginesize + boreratio + stroke + 
                             peakrpm + companybmw + companydodge + 
                             companymitsubishi + 
                             companypeugeot + companyplymouth + 
                             companytoyota + companyvolkswagen + 
                             dummy_fueltype + dummy_asp + carbodyhardtop + 
                             dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                             cylindernumberfive + cylindernumberfour + fuelsystem2bbl + fuelsystemmpfi, 
                           data = train)
             
             #summary of model
             summary(model_13)
             
             
             # Pass the model_13 in the vif function
             vif(model_13)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_13),decreasing = TRUE)

             
       # l.) now removing carheight,companyvolkswagen,carbodyhardtop variables
             
             model_14 <-lm(formula = price ~ carwidth + 
                             curbweight + enginesize + boreratio + stroke + 
                             peakrpm + companybmw + companydodge + 
                             companymitsubishi + 
                             companypeugeot + companyplymouth + 
                             companytoyota + 
                             dummy_fueltype + dummy_asp + 
                             dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                             cylindernumberfive + cylindernumberfour + fuelsystem2bbl + fuelsystemmpfi, 
                           data = train)
             
             #summary of model
             summary(model_14)
             
             
             # Pass the model_14 in the vif function
             vif(model_14)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_14),decreasing = TRUE)
             

     # m.) now removing cylindernumberfive variable
             
             model_15 <-lm(formula = price ~ carwidth + 
                             curbweight + enginesize + boreratio + stroke + 
                             peakrpm + companybmw + companydodge + 
                             companymitsubishi + 
                             companypeugeot + companyplymouth + 
                             companytoyota + 
                             dummy_fueltype + dummy_asp + 
                             dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                             cylindernumberfour + fuelsystem2bbl + fuelsystemmpfi, 
                           data = train)
             
             #summary of model
             summary(model_15)
             
             
             # Pass the model_15 in the vif function
             vif(model_15)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_15),decreasing = TRUE)
             
             
             
       # m.) now removing fuelsystemmpfi variable
             
             model_16 <-lm(formula = price ~ carwidth + 
                             curbweight + enginesize + boreratio + stroke + 
                             peakrpm + companybmw + companydodge + 
                             companymitsubishi + 
                             companypeugeot + companyplymouth + 
                             companytoyota + 
                             dummy_fueltype + dummy_asp + 
                             dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                             cylindernumberfour + fuelsystem2bbl, 
                           data = train)
             
             #summary of model
             summary(model_16)
             
             
             # Pass the model_16 in the vif function
             vif(model_16)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_16),decreasing = TRUE)

             
         # n.) now removing dummy_fueltype variable
             
                model_17 <-lm(formula = price ~ carwidth + 
                             curbweight + enginesize + boreratio + stroke + 
                             peakrpm + companybmw + companydodge + 
                             companymitsubishi + 
                             companypeugeot + companyplymouth + 
                             companytoyota + 
                             dummy_asp + 
                             dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                             cylindernumberfour + fuelsystem2bbl, 
                           data = train)
             
             #summary of model
             summary(model_17)
             
             
             # Pass the model in the vif function
             vif(model_17)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_17),decreasing = TRUE)
             
             
             # n.) now removing fuelsystem2bbl variable
             
             model_18 <-lm(formula = price ~ carwidth + 
                             curbweight + enginesize + boreratio + stroke + 
                             peakrpm + companybmw + companydodge + 
                             companymitsubishi + 
                             companypeugeot + companyplymouth + 
                             companytoyota + 
                             dummy_asp + 
                             dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                             cylindernumberfour, 
                           data = train)
             
             #summary of model
             summary(model_18)
             
             
             # Pass the model in the vif function
             vif(model_18)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_18),decreasing = TRUE)

             
      # o.) now removing companydodge variable
             
             model_19 <-lm(formula = price ~ carwidth + 
                             curbweight + enginesize + boreratio + stroke + 
                             peakrpm + companybmw + 
                             companymitsubishi + 
                             companypeugeot + companyplymouth + 
                             companytoyota + 
                             dummy_asp + 
                             dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                             cylindernumberfour, 
                           data = train)
             
             #summary of model
             summary(model_19)
             
             
             # Pass the model in the vif function
             vif(model_19)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_19),decreasing = TRUE)
             

       # p.) now removing companytoyota,companyplymouth variables
             
             model_20 <-lm(formula = price ~ carwidth + 
                             curbweight + enginesize + boreratio + stroke + 
                             peakrpm + companybmw + 
                             companymitsubishi + 
                             companypeugeot + 
                             dummy_asp + 
                             dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                             cylindernumberfour, 
                           data = train)
             
             #summary of model
             summary(model_20)
             
             
             # Pass the model in the vif function
             vif(model_20)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_20),decreasing = TRUE)
             

       # q.) now removing companymitsubishi variables
             
             model_21 <-lm(formula = price ~ carwidth + 
                             curbweight + enginesize + boreratio + stroke + 
                             peakrpm+ companybmw+
                             companypeugeot + 
                             dummy_asp + 
                             dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                             cylindernumberfour, 
                           data = train)
             
             #summary of model
             summary(model_21)
             
             
             # Pass the model in the vif function
             vif(model_21)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_21),decreasing = TRUE)
             
             
        # r.) now removing dummyasp variable
             
             model_22 <-lm(formula = price ~ carwidth + 
                             curbweight + enginesize + boreratio + stroke + 
                             peakrpm+ companybmw+
                             companypeugeot + 
                             dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                             cylindernumberfour, 
                           data = train)
             
             #summary of model
             summary(model_22)
             
             
             # Pass the model in the vif function
             vif(model_22)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_22),decreasing = TRUE)
             
             
        # s.) now removing peakrpm variable
             
             model_23 <-lm(formula = price ~ carwidth + 
                             curbweight + enginesize + boreratio + stroke + 
                             companybmw+
                             companypeugeot + 
                             dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor + 
                             cylindernumberfour, 
                           data = train)
             
             #summary of model
             summary(model_23)
             
             
             # Pass the model in the vif function
             vif(model_23)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_23),decreasing = TRUE)
             
             
             # s.) now removing cylindernumberfour variable
             
             model_24 <-lm(formula = price ~ carwidth + 
                             curbweight + enginesize + boreratio + stroke + 
                             companybmw+
                             companypeugeot + 
                             dummy_eloc + enginetypel + enginetypeohcv + enginetyperotor, 
                           data = train)
             
             #summary of model
             summary(model_24)
             
             
             # Pass the model in the vif function
             vif(model_24)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_24),decreasing = TRUE)

             
      # t.) now removing enginetypel variable
             
             model_25 <-lm(formula = price ~ carwidth + 
                             curbweight + enginesize + boreratio + stroke + 
                             companybmw+
                             companypeugeot + 
                             dummy_eloc + enginetypeohcv + enginetyperotor, 
                           data = train)
             
             #summary of model
             summary(model_25)
             
             
             # Pass the model in the vif function
             vif(model_25)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_25),decreasing = TRUE)
             
      
       # u.) now removing companypeugeot variable
             
             model_26 <-lm(formula = price ~ carwidth + 
                             curbweight + enginesize + boreratio + stroke + 
                             companybmw+
                             dummy_eloc + enginetypeohcv + enginetyperotor, 
                           data = train)
             
             #summary of model
             summary(model_26)
             
             
             # Pass the model in the vif function
             vif(model_26)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_26),decreasing = TRUE)
             

       # u.) now removing curbweight variable
             
             model_27 <-lm(formula = price ~ carwidth + 
                             enginesize + boreratio + stroke + 
                             companybmw+
                             dummy_eloc + enginetypeohcv + enginetyperotor, 
                           data = train)
             
             #summary of model
             summary(model_27)
             
             
             # Pass the model in the vif function
             vif(model_27)
             
             # sort vif values to find variable with highest vif value and then compare them with their p-value
             sort(vif(model_27),decreasing = TRUE)

             
#hence the final model - model_27 gives a correct view of the factors affecting car prices in american car market 
#the variables representing the factors are-
#carwidth - the width of the car
#enginesize - size of the engine,
#stroke - stroke inside the engine,
#companybmw - is the car of bmw company?
#boreratio - bore ratio of the car,
#enginetypeohcv - is the engine type ohcv?,
#enginetyperotor - is the engine type rotor?,
#eloc - the location of engine-back or front?

#the r-swuare value is 0.9106 and adjusted r-square value is 0.9053 quite near to the one of the test data.
#hence this model is 90% accurately able to predict the data             
#few variables like the bore ratio and the stroke and engine type of ohcv has a negative impact on the car price (i.e. if they increase then the car price decreases and vice versa).
#while remaining variables have a positive impact             