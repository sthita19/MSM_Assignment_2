#2.a)

IAIR7DFL$feas_age= 0
IAIR7DFL$feas_age[IAIR7DFL$hw1_1>=12 & IAIR7DFL$hw1_1<=23] =1 

IAIR7DFL$pol_new [(IAIR7DFL$h8_1==1 | IAIR7DFL$h8_1==2 |IAIR7DFL$h8_1==3)] =1
IAIR7DFL$pol_newv=0
IAIR7DFL$pol_newv[IAIR7DFL$pol_new==1 & IAIR7DFL$feas_age==1]=1

prop.table(table(IAIR7DFL$pol_newv))*100
prop.table(table(IAIR7DFL$s515_1, IAIR7DFL$s190s),1)*100
#2.b)

IAIR7DFL$bcg_new=0
IAIR7DFL$bcg_new [(IAIR7DFL$h2_1==1 | IAIR7DFL$h2_1==2 |IAIR7DFL$h2_1==3)] =1
IAIR7DFL$bcg_newv=0
IAIR7DFL$bcg_newv[IAIR7DFL$bcg_new==1 & IAIR7DFL$feas_age==1]=1

IAIR7DFL$dpt_new=0
IAIR7DFL$dpt_new [(IAIR7DFL$h7_1==1 | IAIR7DFL$h7_1==2 |IAIR7DFL$h7_1==3)] =1
IAIR7DFL$dpt_newv=0
IAIR7DFL$dpt_newv[IAIR7DFL$dpt_new==1 & IAIR7DFL$feas_age==1]=1

IAIR7DFL$vita_temp= 0
IAIR7DFL$vita_temp[(IAIR7DFL$h33_1 ==1|IAIR7DFL$h33_1 ==2|IAIR7DFL$h33_1 ==3) & (IAIR7DFL$h40_1==1|IAIR7DFL$h40_1==2|IAIR7DFL$h40_1==3)]=1
IAIR7DFL$vita_newv= 0
IAIR7DFL$vita_newv [IAIR7DFL$vita_temp==1 & IAIR7DFL$feas_age==1]=1

IAIR7DFL$hepa_temp=0
IAIR7DFL$hepa_temp[(IAIR7DFL$h50_1==1|IAIR7DFL$h50_1==2|IAIR7DFL$h50_1==3) & (IAIR7DFL$h62_1==1|IAIR7DFL$h62_1==2|IAIR7DFL$h62_1==3) & (IAIR7DFL$h63_1==1|IAIR7DFL$h63_1==2|IAIR7DFL$h63_1==3)]=1
IAIR7DFL$hepa_new= 0
IAIR7DFL$hepa_new[IAIR7DFL$hepa_new==1 & IAIR7DFL$feas_age==1]=1

IAIR7DFL$q57_new=0
IAIR7DFL$q57_new [(IAIR7DFL$bcg_newv==1) & (IAIR7DFL$dpt_newv==1) & (IAIR7DFL$vita_newv==1) & (IAIR7DFL$hepa_new==1) & (IAIR7DFL$pol_newv==1)] =1

IAIR7DFL$q58_new=0
IAIR7DFL$q58_new [(IAIR7DFL$h2_1==1 | IAIR7DFL$h2_1==3) & (IAIR7DFL$h7_1==1 | IAIR7DFL$h7_1==3) & (IAIR7DFL$h33_1 ==1|IAIR7DFL$h33_1 ==3) & (IAIR7DFL$h40_1==1|IAIR7DFL$h40_1==3) & (IAIR7DFL$h50_1==1|IAIR7DFL$h50_1==3) & (IAIR7DFL$h62_1==1|IAIR7DFL$h62_1==3) & (IAIR7DFL$h63_1==1|IAIR7DFL$h63_1==3) & (IAIR7DFL$h8_1==1 | IAIR7DFL$h8_1==3)] =1

IAIR7DFL$q67new=0
IAIR7DFL$q67new [(IAIR7DFL$s515_1 > 10 & IAIR7DFL$s515_1 < 32) | (IAIR7DFL$s515a_1 > 11 & IAIR7DFL$s515a_1 < 19)] =1

IAIR7DFL$q68new=0
IAIR7DFL$q68new [(IAIR7DFL$s515_1 > 40 & IAIR7DFL$s515_1 < 53) | (IAIR7DFL$s515b_1 > 43 & IAIR7DFL$s515b_1 < 51)] =1

prop.table(table(IAIR7DFL$bcg_newv))*100

prop.table(table(IAIR7DFL$pol_newv))*100

prop.table(table(IAIR7DFL$q67new))*100

#2.c)
prop.table(table(IAIR7DFL$q57_new, IAIR7DFL$v024),1)*100
#3.a)
tb1=table(IAIR7DFL$s190s,IAIR7DFL$pol_newv)
chisq.test(tb1)
#3.b)
IAIR7DFL1 <- subset(IAIR7DFL, (IAIR7DFL$h8y_1 < 2023))
extra2 <- subset(extra1, (extra1$h8y_1 < 2023))
p_month = (IAIR7DFL1$h8m_1 + IAIR7DFL1$h8y_1*12) - (extra2$hw18_1 + extra2$hw19_1*12) + IAIR7DFL1$hw1_1
table(p_month)
hist(p_month) 
t.test(p_month,mu=3.26)
#3.c)
tempfinreg_1= lm(IAIR7DFL$q67new~IAIR7DFL$v107+ extra_1$v102)
tempfinreg_2= lm(IAIR7DFL$q67new~IAIR7DFL$v107+ extra_1$v102 + IAIR7DFL$v201)
#4.a)
Graph=IAIR7DFL %>% filter(!is.na(IAIR7DFL$s515_1))
Graph$s515_1 <- as.factor(Graph$s515_1)
ggplot(Graph,aes(x=factor(s515_1),fill=s515_1))+geom_bar(aes(fill=s515_1))+labs(x="Where receive most of his/her vaccinations?",title="Where receive most of his/her vaccinations?")
#4.b)
#Graph=IAIR7DFL %>% filter(!is.na(IAIR7DFL$s515_1))
#The above command has already been executed in question 4.a)
