#Restructuring BG Data + Cor matrix 

flat <- scores_export_1_[which(scores_export_1_$folderno %in% analog_fo),]
flat <- as.data.frame(subset(flat, select = c(folderno, test, score, percentile)))

flat <- reshape(flat, direction = "wide",
                timevar = "test",
                idvar = "folderno")

colnames(flat2) <- c('folderno', 
                         'score.Graphoria',                     
                         'percentile.Graphoria',               
                         'score.Ideaphoria',     
                         'percentile.Ideaphoria',
                         'score.Foresight',                      
                         'percentile.Foresight',                 
                         'score.InductiveReasoning',            
                         'percentile.InductiveReasoning',       
                         'score.AnalyticalReasoning',           
                         'percentile.AnalyticalReasoning',      
                         'score.NumericalReasoning',            
                         'percentile.NumericalReasoning',       
                         'score.NumberFacility',                
                         'percentile.NumberFacility',          
                         'score.WigglyBlock',          
                         'percentile.WigglyBlock',              
                         'score.PaperFolding',                
                         'percentile.PaperFolding',             
                         'score.StructuralVisualization',      
                         'percentile.StructuralVisualization',  
                         'score.TonalMemory',                   
                         'percentile.TonalMemory',              
                         'score.PitchDiscrimination',           
                         'percentile.PitchDiscrimination',      
                         'score.RhythmMemory',      
                         'percentile.RhythmMemory',             
                         'score.MemoryforDesign',            
                         'percentile.MemoryforDesign',         
                         'score.Silograms',
                         'percentile.Silograms', 
                         'score.NumberMemory',                
                         'percentile.NumberMemory',             
                         'score.Observation',              
                         'percentile.Observation',               
                         'score.Red-GreenVision',               
                         'percentile.Red-GreenVision',          
                         'score.ColorDiscrimination',          
                         'percentile.ColorDiscrimination',      
                         'score.FingerDexterity',                
                         'percentile.FingerDexterity',           
                         'score.TweezerDexterity',               
                         'percentile.TweezerDexterity',        
                         'score.WordAssociation',                
                         'percentile.WordAssociation',           
                         'score.EnglishVocabulary',             
                         'percentile.EnglishVocabulary',        
                         'score.VisualDesigns1',       
                         'percentile.VisualDesigns1',        
                         'score.VisualDesigns2',          
                         'percentile.VisualDesigns2',          
                         'score.GripLeft',          
                         'percentile.GripLeft',                
                         'score.GripRight',                    
                         'percentile.GripRight')

clients <- distinct(clients_export)
clients <- subset(clients_export, clients_export$folderno %in% analog_fo)
clients <- clients[c('bdate', 'folderno', 'sex')]
flat2 <- merge(flat, clients, by = "folderno")

age <- as.data.frame(str_split(flat2$bdate, "-"))
age <- as.data.frame(t(age))
age$folderno <- flat2$folderno
age <- age[-1]

age <- age %>% mutate(., Age = ifelse(V2 >23, paste("19", age$V2), paste("20", age$V2)))
age$Age <- gsub(" ", "", age$Age) %>% as.numeric()
age <- age %>% mutate(Age2 = 2022-Age)
age <- age[c("folderno", "Age2")]

analog_flat <- distinct(merge(flat2, age, by = "folderno"))
analog_flat <-  analog_flat %>% rename("Age" = "Age2") %>% select(folderno, sex, Age, everything())
analog_flat <-  select(analog_flat, -c("bdate","percentile.Red-GreenVision", "percentile.WordAssociation"))


analog_flat$sex <- as.numeric(factor(analog_flat$sex)) #M = 2
analog_flat <- analog_flat %>% mutate_at(c(2:55), as.numeric)
write.csv(analog_flat, "analog_flat.csv")






###Digital 

flat <- scores_export_1_[which(scores_export_1_$folderno %in% digital_fo),]
flat <- as.data.frame(subset(flat, select = c(folderno, test, score, percentile)))


flat <- reshape(flat, direction = "wide",
                timevar = "test",
                idvar = "folderno")

colnames(flat) <- c('folderno', 
                    'score.Graphoria',                     
                    'percentile.Graphoria',               
                    'score.Ideaphoria',     
                    'percentile.Ideaphoria',
                    'score.Foresight',                      
                    'percentile.Foresight',                 
                    'score.InductiveReasoning',            
                    'percentile.InductiveReasoning',       
                    'score.AnalyticalReasoning',           
                    'percentile.AnalyticalReasoning',      
                    'score.NumericalReasoning',            
                    'percentile.NumericalReasoning',       
                    'score.NumberFacility',                
                    'percentile.NumberFacility',          
                    'score.WigglyBlock',          
                    'percentile.WigglyBlock',              
                    'score.PaperFolding',                
                    'percentile.PaperFolding',             
                    'score.StructuralVisualization',      
                    'percentile.StructuralVisualization',  
                    'score.TonalMemory',                   
                    'percentile.TonalMemory',              
                    'score.PitchDiscrimination',           
                    'percentile.PitchDiscrimination',      
                    'score.RhythmMemory',      
                    'percentile.RhythmMemory',             
                    'score.MemoryforDesign',            
                    'percentile.MemoryforDesign',         
                    'score.Silograms',
                    'percentile.Silograms', 
                    'score.NumberMemory',                
                    'percentile.NumberMemory',             
                    'score.Observation',              
                    'percentile.Observation',               
                    'score.Red-GreenVision',               
                    'percentile.Red-GreenVision',          
                    'score.ColorDiscrimination',          
                    'percentile.ColorDiscrimination',      
                    'score.FingerDexterity',                
                    'percentile.FingerDexterity',           
                    'score.TweezerDexterity',               
                    'percentile.TweezerDexterity',        
                    'score.WordAssociation',                
                    'percentile.WordAssociation',           
                    'score.EnglishVocabulary',             
                    'percentile.EnglishVocabulary',        
                    'score.VisualDesigns1',       
                    'percentile.VisualDesigns1',        
                    'score.VisualDesigns2',          
                    'percentile.VisualDesigns2',          
                    'score.GripLeft',          
                    'percentile.GripLeft',                
                    'score.GripRight',                    
                    'percentile.GripRight')

clients <- distinct(clients_export)
clients <- subset(clients_export, clients_export$folderno %in% digital_fo)
clients <- clients[c('bdate', 'folderno', 'sex')]
flat <- merge(flat, clients, by = "folderno")


age <- as.data.frame(str_split(flat$bdate, "-"))
age <- as.data.frame(t(age))
age$folderno <- flat$folderno
age <- age[-1]

age <- age %>% mutate(., Age = ifelse(V2 >23, paste("19", age$V2), paste("20", age$V2)))
age$Age <- gsub(" ", "", age$Age) %>% as.numeric()
age <- age %>% mutate(Age2 = 2022-Age)
age <- age[c("folderno", "Age2")]

digital_flat <- distinct(merge(flat2, age, by = "folderno"))
digital_flat <-  digital_flat %>% rename("Age" = "Age2") %>% select(folderno, sex, Age, everything())
digital_flat <-  select(digital_flat, -c("bdate", "percentile.Red-GreenVision", "percentile.WordAssociation"))

digital_flat$sex <- as.numeric(factor(digital_flat$sex)) #M = 2
digital_flat <- digital_flat %>% mutate_at(c(2:55), as.numeric)
write.csv(digital_flat, "digital_flat.csv")

##Combined 
combined_flat <- full_join(digital_flat, analog_flat)
combined_flat <-  select(combined_flat, -c("bdate", "percentile.Red-GreenVision", "percentile.WordAssociation"))
combined_flat <- combined_flat %>% mutate_at(c(2:55), as.numeric)
write.csv(combined_flat, "combined_flat.csv")



##Correlate
df <- analog_flat %>% select(starts_with("score"), "sex", "Age")
analog_cor <- cor(df, method = "pearson", use = "pairwise.complete.obs")


df <- digital_flat %>% select(starts_with("score"), "sex", "Age")
digital_cor <- cor(df, method = "pearson", use = "pairwise.complete.obs")

df <- combined_flat %>% select(starts_with("score"), "sex", "Age")
combined_cor <- cor(df, method = "pearson", use = "pairwise.complete.obs")

write.xlsx(analog_cor, file="Analog_Digital_AR.xlsx", sheetName="Analog")
write.xlsx(digital_cor, file="Analog_Digital_AR.xlsx", sheetName="Digital", append=TRUE)
write.xlsx(combined_cor, file="Analog_Digital_AR.xlsx", sheetName="Combined", append=TRUE)


hist(digital_flat$Age)
hist(analog_flat$Age)
table(digital_flat$Age)
freq