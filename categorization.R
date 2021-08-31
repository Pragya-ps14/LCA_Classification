kk$SOC_NAME <- tolower(kk$SOC_NAME)

kknew<-kk %>% mutate(NewSOCNAME = ifelse(str_detect(SOC_NAME,"computer"),'Computers Related',SOC_NAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"15-11"),'Computers Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"programmer"),'Computers Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"software"),'Computers Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"software developer"),'Computers Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"//.net"),'Computers Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"database"),'Data Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"developer"),'Data Related',NewSOCNAME))


unique(kknew$NewSOCNAME)

#Data Related
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"^data"),'Data Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"datebase"),'Data Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"data analyst"),'Data Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"business intelligence"),'Data Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"business inelligence"),'Data Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"business analysts"),'Data Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"business analyst"),'Data Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"professional data warehouse"),'Data Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME," data"),'Data Related',NewSOCNAME))
unique(kknew$NewSOCNAME)
#Education
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"atmospheric, earth, marine, and space sciences tea"),'Education Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"trainers"),'Education Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"education"),'Education Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"coaches and scout"),'Education Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"coach"),'Education Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"training and development"),'Education Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"teaching"),'Education Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"instructional"),'Education Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"teach"),'Education Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"^education"),'Education Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"professor$"),'Education Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"teachers"),'Education Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"teacher"),'Education Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"tutor"),'Education Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"professor"),'Education Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"economist"),'Education Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"professor"),'Education Related',NewSOCNAME))

check<-kknew%>% select(SOC_NAME,NewSOCNAME) %>% filter(str_detect(NewSOCNAME,"logist"))
rm(check)
unique(kknew$NewSOCNAME)
#Marketing and finance related
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"fundraiser"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"^13-"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"compensation, benefits"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"real estate"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"commodities traders"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"insurance"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"tax"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"budget"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"cost estimator"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"cashier"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"sales representative"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"fraud examiner"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"treasurer"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"real estate brokers"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"financial"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"credit counselors"),'Marketing and Finance Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"fiancial"),'Marketing and Finance Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"market"),'Marketing and Finance Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"marketing"),'Marketing and Finance Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"sales agents"),'Marketing and Finance Related',NewSOCNAME))

kknew<- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"loan"),'Marketing and Finance Related',NewSOCNAME))

kknew<- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"accountant"),'Marketing and Finance Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"claims"),'Marketing and Finance Related',NewSOCNAME))
unique(kknew$NewSOCNAME)
kknew %>% filter()
#Mnagement
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"comp, benefits"),'Management and specialization Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"personnel recruiters"),'Management and specialization Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"compensation,benefit"),'Management and specialization Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"sustainability specialists"),'Management and specialization Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"arbitrators"),'Management and specialization Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"arbitrator and mediator"),'Management and specialization Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"public relations"),'Management and specialization Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"community and special services"),'Management and specialization Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"budget analyst"),'Management and specialization Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"auditor"),'Management and specialization Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"human rescouces"),'Management and specialization Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"human resource"),'Management and specialization Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"manager"),'Management and specialization Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"management"),'Management and specialization Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"mangement"),'Management and specialization Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"managment"),'Management and specialization Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"manger"),'Management and specialization Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"human resource specialist"),'Management and specialization Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"regulatory affairs"),'Management and specialization Related',NewSOCNAME))
unique(kknew$NewSOCNAME)
kknew <- kknew %>% filter(!str_detect(NewSOCNAME,"n/a"))
#MEDICAL
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"^29-"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"pharmacy"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"counselors"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"genetic"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"optometrist"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"community and social service"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"clinical"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"dental"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"veterinary"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"medical"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"fitness"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"prosthodontists"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"dental"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"orthodontists"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"rehabilitation"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"nutrition"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"podiatrist"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"veterinarians"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"pyschiatrists"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"thereapists"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"pediatrician"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"optometrists"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"urologists"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"gynecologists"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"neurologists"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"hlth diagnosing & treating pract., all other"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"acupuncturist"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"internist"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"skincare"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"epidemiologists"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"anesthesiologists"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"psychologist"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"hearing aid"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"family and general"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"^healthcare"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"pathologist"),'Medical and Healthcare Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"medicine"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"dentist"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"doctor"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"surgeon"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"medical"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"therapist"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"health"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"radiologist"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"speech and language"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"biological"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"psychiatrists"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"psychiatrist"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"physician"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"nurse"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"nurses"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"hospitalists"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"childcare"),'Medical and Healthcare Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"orthoptist"),'Medical and Healthcare Related',NewSOCNAME))


#Agriculture
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"producer"),'Agriculture and Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"farm"),'Agriculture and Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"Agriculture"),'Agriculture and Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"farmworker"),'Agriculture and Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"^Agriculture"),'Agriculture and Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"agriculture"),'Agriculture and Related',NewSOCNAME))


#clerical and Administ
unique(kknew$NewSOCNAME)
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"archivists"),'Clerical',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"librarian"),'Clerical',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"curator"),'Clerical',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"freight agents"),'Clerical',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"ticket agents"),'Clerical',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"bill and account collectors"),'Clerical',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"travel agents"),'Clerical',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"secretar"),'Clerical',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"clerks"),'Clerical',NewSOCNAME))


#Law
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"intelligence analyst"),'Law and Enforcement',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"compliance"),'Law and Enforcement',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"inspector"),'Law and Enforcement',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"lawyer"),'Law and Enforcement',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"police"),'Law and Enforcement',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"judicial"),'Law and Enforcement',NewSOCNAME))

unique(kknew$NewSOCNAME)

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"attorney"),'Law and Enforcement',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"detective"),'Law and Enforcement',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"paralegal"),'Law and Enforcement',NewSOCNAME))


#Supervisor Related
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"supervisor"),'Supervisor Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"captains, mates, and pilots"),'Supervisor Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"residential advisors"),'Supervisor Related',NewSOCNAME))
#Planning and Architectural
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"forester"),'Planning and Architectural',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"urban and regional planner"),'Planning and Architectural',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"planning aides"),'Planning and Architectural',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"event planner"),'Planning and Architectural',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"event planner"),'Planning and Architectural',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"architect"),'Planning and Architectural',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"interior designer"),'Planning and Architectural',NewSOCNAME))



#Operations and Logistics


kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"logistic"),'Operations and Logistics',NewSOCNAME))


kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"operation research"),'Operations and Logistics',NewSOCNAME))


kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"operations"),'Operations and Logistics',NewSOCNAME))

#Artists and Media
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"set and exhibit designers"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"models"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"models"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"writers"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"writers"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"set and exhibit designers"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"Models"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"film and video editors"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"fashion designers"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"fashion designers"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"designers, all other"),'Artist Related',NewSOCNAME))


kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"actor"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"photographer"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"entertainer"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"musician"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"poets"),'Artist Related',NewSOCNAME))
awe<-kknew%>%select(SOC_NAME,NewSOCNAME) %>% filter(str_detect(NewSOCNAME,"ist"))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"dancers"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"artists"),'Artist Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"choreographer"),'Artist Related',NewSOCNAME))

unique(kknew$NewSOCNAME)
#media
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"proofreader"),'Media Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"radio and television"),'Media Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"news analysts"),'Media Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"copy writer"),'Media Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"multimedia"),'Media Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"editor"),'Media Related',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"reporters"),'Media Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"technical writers"),'Media Related',NewSOCNAME))


unique(kknew$NewSOCNAME)

#leadership
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"clergy"),'Leadership Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"chief executives"),'Leadership Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"vice president"),'Leadership Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"chief officer"),'Leadership Related',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"director"),'Leadership Related',NewSOCNAME))
unique(kknew$NewSOCNAME)
#technicians and workers
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"manicurists"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"heating, air"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"travel guides"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"breeders"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"breeders"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"heating, air conditioning, and refrigeration "),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"animal trainers"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"drafters"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"installer"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"model makers, metal and plastic"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"purchasing agents"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"electrical drafter"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"mechanic"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"baker"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"operators"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"technician"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"technician"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"workers"),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,'electrician'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,'electricians'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,'electrician'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,'painters'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,'die makers'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,'repairer'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME, 'millwrights'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME, 'bartender'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME, 'cooks'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME, 'tour guide'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME, 'tailor'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME, 'welders'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME, 'machinist'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME, 'machine operators'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME, 'waiters'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME, 'fabric and apparel'),'technicians and workers',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME, 'plumbers'),'technicians and workers',NewSOCNAME))
unique(kknew$NewSOCNAME)

#Engineering
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"surveyor"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"commericial and industrial designers"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"^17-"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"geographer"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"systems analyts"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"11-"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"survey resear"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"atmospheric, earth, marine, and space sciences"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"astronomer"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"mathematical"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"systems analyst"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"web administrator"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"desktop publisher"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"technical"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"commercial pilots"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"commericial and industrial designerssales enginers"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"sales enginers"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"systems administrator"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"water and wastewater treatment plant"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"commercial and industrial designers"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"actuaries"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"acturial"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"cartographers and photogrammetrists"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"Geographer"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"acturial"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"commercial and industrial"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"chemist"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"compuer systems analysts"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"system administrator"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"orthotist"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"logist"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"epidemiologist"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"pharmacist"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"biologist"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"biologist"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"graphic designer"),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"geneticists"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"information security"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"network "),'Engineering and Science',NewSOCNAME))
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"network administrators"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"network administrator"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"enigneers"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"engineering"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"physicist"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"statistic"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"sociologist"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"scientist"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"engineers"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"engineer"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"scientists"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"social science"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"bioinformatic"),'Engineering and Science',NewSOCNAME))

unique(kknew$NewSOCNAME)
kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"engineers"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"mathematician"),'Engineering and Science',NewSOCNAME))

kknew <- kknew %>% mutate(NewSOCNAME = ifelse(str_detect(NewSOCNAME,"analyst"),'Engineering and Science',NewSOCNAME))


data.frame(colnames(kknew))
unique(kknew$NewSOCNAME)
str(kknew)

#2) PROCESSING_TIME = DECISION_DATE - CASE_SUBMITTED
kknew %>%
  group_by(CASE_SUBMITTED) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

str(kknew)
kknew$process_time <- as.Date(as.character(kknew$DECISION_DATE), format="%Y-%m-%d") -
  as.Date(as.character(kknew$CASE_SUBMITTED), format = "%Y-%m-%d")
summary(kknew)
unique(kknew$process_time)                               


#3) Employment_time = EMPLOYMENT_END_DATE - EMPLOYMENT_START_DATE
str(kknew)
kknew$Employment_Time <- as.Date(as.character(kknew$EMPLOYMENT_END_DATE), format="%Y-%m-%d") -
  as.Date(as.character(kknew$EMPLOYMENT_START_DATE), format = "%Y-%m-%d")
summary(kknew)
max(kknew$Employment_Time)



#################### CREATING DUMMY VARIABLES ######################################
library(caret)
data.frame(colnames(kknew))

kk2 <- kknew[c(2,3,4,5,10,12,13,14,15,21,22,23,24,25)]
data.frame(colnames(kk2))


l <- kk2[c(1,16,19,25,26)]
m <- kk2[-c(1,16,19,25,26)]
#
dmy <- dummyVars("~.", data = l)
trsf <- data.frame(predict(dmy, newdata = l))
trsf
#

data.frame(colnames(m))
kk3 <- cbind(trsf, m)
data.frame(colnames(kk3))

