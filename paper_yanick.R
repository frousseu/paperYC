library(data.table)

#############################################
### piecharts
#############################################

d<-fread("C:/Users/rouf1703/Documents/UdeS/Consultation/MBelisle/Doc/YanickPaper/calcul08décembre2009.txt",header=T,stringsAsFactors=F,verbose=FALSE)

# Nb d'individus diff?rents ? chaque abreuvoir 2007-2008-2009
E<-rep(1,12);D<-rep(2,12);C<-rep(3,7);B<-rep(4,7);A<-rep(5,7);L<-c(E,D,C,B,A);El<-c(1:12);Dl<-c(1:12);Cl<-c(1:7);Bl<-c(1:7);Al<-c(1:7);R<-c(El,Dl,Cl,Bl,Al)
nomabreuv<-c("E01","E02","E03","E04","E05","E06","E07","E08","E09","E10","E11","E12","D01","D02","D03","D04","D05","D06","D07","D08","D09","D10","D11","D12","C01","C02","C03","C04","C05","C06","C07","B01","B02","B03","B04","B05","B06","B07","A01","A02","A03","A04","A05","A06","A07")
par(mgp=c(0.1,0.1,0.1),oma=c(0,0,0,0),mar=c(1.5,1,0.5,1),bg="black")
par(col.axis="white",col.main="white",col.sub="white",col.lab="white",bg="black")
x<-subset(d,d$annee!=2006,select=c(abreuv,bague))
x<-aggregate(x[2],x[1],function(i){length(unique(i))})
l<-ifelse(substr(x$abreuv,1,1)=="E",1,ifelse(substr(x$abreuv,1,1)=="D",2,ifelse(substr(x$abreuv,1,1)=="C",3,ifelse(substr(x$abreuv,1,1)=="B",4,5))))
r<-as.numeric(substr(x$abreuv,2,3))
x<-cbind(x,l,r,stringsAsFactors=F)
plot(L,R,col="white",cex=0.1,xlim=c(0.5,5.5),ylim=c(0.5,12.5),xaxt="n",yaxt="n",xlab="",ylab="")
#!text(L,R,col="white",label=nomabreuv,cex=0.63)
points(x$l,x$r,cex=x$bague/20,col="green")
#!legend(5.5,12.5,c(10,50,90,130),text.col="white",x.intersp=2,y.intersp=2.2,bty="o",bg="black",box.col="white",pch=1,col="green",pt.cex=c(10,50,90,130)/20,xjust=1)
text(4.38,12.25,"Nb d'individus",col="white")
text(5,11.75,"20",col="white")
text(5,10.75,"50",col="white")
text(5,9.75,"90",col="white")
text(5,8.75,"130",col="white")
points(4,11.75,cex=20/20,col="white")
points(4,10.75,cex=50/20,col="white")
points(4,9.75,cex=90/20,col="white")
points(4,8.75,cex=130/20,col="white")
lines(c(3.4,5.35),c(8,8),col="white")
lines(c(3.4,5.35),c(12.5,12.5),col="white")
lines(c(3.4,3.4),c(8,12.5),col="white")
lines(c(5.35,5.35),c(8,12.5),col="white")


### Piechart

png("C:/Users/rouf1703/Documents/UdeS/Consultation/MBelisle/Doc/YanickPaper/Appendix.png",width=3,height=6,units="in",res=500,pointsize=9)

#par(mfrow=c(1,2))

par(mgp=c(0.1,0.1,0.1),oma=c(0,0,0,0),mar=c(1.5,1,0.5,1))
#par(col.axis="black",col.main="black",col.sub="black",col.lab="black")
x<-subset(d,d$annee!=2006 & d$age==1,select=c(abreuv,sexe,nbvisperso))
x<-aggregate(x[3],x[c(1,2)],sum)
#!x<-unique(subset(d,d$annee!=2006 & d$age==1,select=c(abreuv,sexe,bague)))
#!x<-aggregate(x[3],x[c(1,2)],function(i){length(unique(i))})
x<-x[order(x$abreuv,x$sexe),]
l<-ifelse(substr(x$abreuv,1,1)=="E",1,ifelse(substr(x$abreuv,1,1)=="D",2,ifelse(substr(x$abreuv,1,1)=="C",3,ifelse(substr(x$abreuv,1,1)=="B",4,5))))
r<-as.numeric(substr(x$abreuv,2,3))
x<-cbind(x,l,r,stringsAsFactors=F)
cas<-subset(x,select=c(l,r))
cas<-unique(cas[order(cas$l,cas$r),])
m<-cbind(rev(1:12),rev(13:24),c(rep(46,5),rev(25:31)),c(rep(46,5),rev(32:38)),c(rep(46,5),rev(39:45)))
nf<-layout(m,c(1,1),c(1,1),respect=T)
scrap<-0
forest<-c("A01","A02","B01","B02","C01","C02","C03","C04","D01","D02","D03","D04","D05","E01","E02","E03","E04","E05","E06")
for(i in 1:46){
	par(mar=c(0,0,0,0))
	if(i<46){
		y<-subset(x,x$l==cas$l[i] & x$r==cas$r[i])
		
		#par(new=FALSE)
		pie(y$nbvisperso,init.angle=90,labels="",col=c("black","white"),lwd=0.1)
		if(all(y$abreuv%in%forest)){rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=gray(0,0.15),border=NA)}
	}
	 
	#text(0,0,sum(y$nbvisperso),col="black",cex=1.5)
	scrap<-scrap+sum(y$nbvisperso)
	#pie(y$bague,init.angle=90,labels="",col=c("blue","red"))}
	if(i==46){
		plot(0,0,col="black",xaxt="n",yaxt="n",xlab="",ylab="",type="n",bty="n")
		text(-0.25,0.25,"Males",col="black",cex=1.8)
		points(0.75,0.25,col="black",cex=5,pch=16)
		text(-0.25,-0.25,"Females",col="black",cex=1.8)
		points(0.75,-0.25,col="black",cex=5,pch=21)
		#text(0,-0.75,paste("No. of visits=",scrap),cex=1.8,col="black")
	}
}

dev.off()

#############################################
### Fig 5
#############################################

########################  serie de modeles pour modeliser l ecart-type (ET)
d<-read.table("C:/Users/rouf1703/Documents/UdeS/Consultation/MBelisle/Doc/YanickPaper/duree.inter.18.mars.txt", header=T, stringsAsFactors=F)
#ET<-(d$var.intra.abreuv)^0.5
sexe1<-as.factor(d$sexe)
annee1<-as.factor(d$annee)
d<-cbind(d,sexe1,annee1)#,ET)
v<-subset(d, d$age==1) 
v<-subset(v,v$annee==2007 | v$annee==2008 )
names(v)
head(v)

#############################
###########################
## transformation ARCSIN
# far racine carree des valeurs avant 
#s<-(v$simpson)^0.5
#9^0.5
#[1] 3
#arc<-asin(s)
####################################
#############


library(lme4)
library(scales)

##############  avec indice theorique
i1<- (lmer(log(sd90)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdperso+ nbvisperso + theo+ temperature + pluie + nbabr + fleur + gen.mst + nbviscomp + sexe1:fleur+ sexe1:julien + sexe1:nbviscomp+ pluie:temperature+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
i2<- (lmer(log(sd90)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdperso+ nbvisperso + theo+ temperature + pluie + nbabr + fleur + gen.mst + nbviscomp+  (1|bague) + (1|abreuv), REML=F, data=v, na.action=na.omit))
i3<- (lmer(log(sd90)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdperso+ nbvisperso + theo+temperature + pluie + gen.mst + nbviscomp + nbabr+ sexe1:julien+pluie:temperature+(1|bague)+ (1|abreuv), REML=F , data=v, na.action=na.omit))
i4<- (lmer(log(sd90)~  sexe1 + masse + parasites+ annee1 + julien+ sexe1:julien + (1|bague)+ (1|abreuv) , data=v, REML=F, na.action=na.omit))
## avec variables abiotiques   Pas d interactions possible entre pluie et annee : false convergence
i5<- (lmer(log(sd90) ~  temperature+ pluie+ temperature: pluie+  julien+  annee1 +(1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
## avec variables du paysages
i6<- (lmer(log(sd90) ~  fleur +annee1+ julien+ sexe1:fleur + sexe1:julien+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
## avec variables rattachees aux mouvements et traplines
i7<- (lmer(log(sd90)~  gen.mst+ nbvisdperso+ theo+ nbvisperso+ nbabr + annee1+ julien+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
## avec variables reliees aux a la presence des competiteurs # pas d interaction avec annee ou julien possible false convergence
i8<- (lmer(log(sd90)~  nbviscomp+ annee1 + julien +(1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
i9<- (lmer(log(sd90)~ annee1+ julien+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))

############################################################################################
##cor(v$nbvisdperso,v$nbvisperso)
##[1] 0.8225252   trop correle, je l enleve


i1<- (lmer(log(sd90)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdperso+  global+ temperature + pluie + nbabr + fleur + gen.mst + nbviscomp + sexe1:fleur+ sexe1:julien + sexe1:nbviscomp+ pluie:temperature+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
i2<- (lmer(log(sd90)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdperso+  global+ temperature + pluie + nbabr + fleur + gen.mst + nbviscomp+  (1|bague) + (1|abreuv), REML=F, data=v, na.action=na.omit))
i3<- (lmer(log(sd90)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdperso+  global+temperature + pluie + gen.mst + nbviscomp + nbabr+ sexe1:julien+pluie:temperature+(1|bague)+ (1|abreuv), REML=F , data=v, na.action=na.omit))
i4<- (lmer(log(sd90)~  sexe1 + masse + parasites+ annee1 + julien+ sexe1:julien + (1|bague)+ (1|abreuv) , data=v, REML=F, na.action=na.omit))
## avec variables abiotiques   Pas d interactions possible entre pluie et annee : false convergence
i5<- (lmer(log(sd90) ~  temperature+ pluie+ temperature: pluie+  julien+  annee1 +(1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
## avec variables du paysages
i6<- (lmer(log(sd90) ~  fleur +annee1+ julien+ sexe1:fleur + sexe1:julien+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
## avec variables rattachees aux mouvements et traplines
i7<- (lmer(log(sd90)~  gen.mst+ nbvisdperso+ global+ nbabr + annee1+ julien+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
## avec variables reliees aux a la presence des competiteurs # pas d interaction avec annee ou julien possible false convergence
i8<- (lmer(log(sd90)~  nbviscomp+ annee1 + julien +(1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
i9<- (lmer(log(sd90)~ annee1+ julien+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))


#J enleve nbabr car trop correlee avec gen.mst. cor =  0.902

i1<- (lmer(log(sd90)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdperso+  global+ temperature + pluie + fleur + gen.mst + nbviscomp + sexe1:fleur+ sexe1:julien + sexe1:nbviscomp+ pluie:temperature+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
i2<- (lmer(log(sd90)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdperso+  global+ temperature + pluie + fleur + gen.mst + nbviscomp+  (1|bague) + (1|abreuv), REML=F, data=v, na.action=na.omit))
i3<- (lmer(log(sd90)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdperso+  global+temperature + pluie + gen.mst + nbviscomp + sexe1:julien+pluie:temperature+(1|bague)+ (1|abreuv), REML=F , data=v, na.action=na.omit))
i4<- (lmer(log(sd90)~  sexe1 + masse + parasites+ annee1 + julien+ sexe1:julien + (1|bague)+ (1|abreuv) , data=v, REML=F, na.action=na.omit))
## avec variables abiotiques   Pas d interactions possible entre pluie et annee : false convergence
i5<- (lmer(log(sd90) ~  temperature+ pluie+ temperature: pluie+  julien+  annee1 +(1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
## avec variables du paysages
i6<- (lmer(log(sd90) ~  fleur +annee1+ julien+ sexe1:fleur + sexe1:julien+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
## avec variables rattachees aux mouvements et traplines
i7<- (lmer(log(sd90)~  gen.mst+ nbvisdperso+ global + annee1+ julien+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
## avec variables reliees aux a la presence des competiteurs # pas d interaction avec annee ou julien possible false convergence
i8<- (lmer(log(sd90)~  nbviscomp+ annee1 + julien +(1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
i9<- (lmer(log(sd90)~ annee1+ julien+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))


#### AIC avec REML=F
#### modavg avec REML=T




##################################################################################
# Pour sortir des tableaux d AIC
# Il faut cr??r une liste appel?e "model" avec tout les mod?les
#! ex: model<-c(model1,model2,model3,...)


model<-c(i1,i2,i3,i4,i5,i6,i7,i8,i9)
# Tableau AICc pour lmer
Modele <- c(1:length(model))
Deviance<-sapply(model,function(i) deviance(i))
# indiquer le nombre de param?tres fixes dans chacun de mes mod?les
fixed.par<-sapply(model,function(i) length(fixef(i)))
# indiquer le nombre de composantes al?atoires dans chacun de mes mod?les
random.components<-sapply(model,function(i) length(ranef(i)))
K <- fixed.par+random.components
n <- length(fitted(i1))
mAIC <- Deviance+2*K
mAICc <- mAIC+2*K*(K+1)/(n-K-1)
deltai <- mAICc-min(mAICc)
wi <- round(exp(-0.5*deltai)/sum(exp(-0.5*deltai)), 3)
tableau_lmer1 <- cbind(Modele, Deviance, K, mAIC, mAICc, deltai, wi)
tableau_lmer1


##############################################
#####  courbes pour l<article avec les variables significatives
##pour une femelle en 2008  a partir du fichier txt modavg.duree.inter
# pour variables nbvisdperso,  global, temperature, sexe15: julien


### master data.frame
newdat<-data.frame(
	sexe1="4",
	masse=mean(v$masse),
	parasites=mean(v$parasites),
	annee1="2008",
	julien=mean(v$julien),
	nbvisdperso=mean(v$nbvisdperso),
	global=mean(v$global),
	temperature=mean(v$temperature),
	pluie=mean(v$pluie),
 fleur=mean(v$fleur),
	gen.mst=mean(v$gen.mst),
	nbviscomp=mean(v$nbviscomp)
)


png("C:/Users/rouf1703/Documents/UdeS/Consultation/MBelisle/Doc/YanickPaper/Fig5.png",width=6.5,height=5,units="in",res=500,pointsize=6)

par(mfrow=c(2,2))

col<-alpha("black",0.1)
#ylim<-c(0,3250)
ylim<-range(exp(log(v$sd90)),na.rm=TRUE)

### sexe15: julien
julien<- seq(142,248, by=10)
f<- 7.88650 +0.37710 -0.05772*mean(v$masse) + 0.00100*mean(v$parasites) -0.04156 -0.00094*(julien) -1.92458*mean(v$nbvisdperso) + 0.33182*mean(v$global) + 0.02039*mean(v$temperature) -0.00426*mean(v$pluie) + 0.01058*mean(v$nbabr) +  0.00014*mean(v$fleur) -0.00004*mean(v$gen.mst) + 0.00054*mean(v$nbviscomp) -0.00053*mean(v$fleur)  -0.00253*(julien)  -0.00090*mean(v$nbviscomp) +  0.00120*mean(v$temperature)*mean(v$pluie)
m<- 7.88650  -0.05772*mean(v$masse) + 0.00100*mean(v$parasites) -0.04156 -0.00094*(julien) -1.92458*mean(v$nbvisdperso) + 0.33182*mean(v$global) + 0.02039*mean(v$temperature) -0.00426*mean(v$pluie) + 0.01058*mean(v$nbabr) +  0.00014*mean(v$fleur) -0.00004*mean(v$gen.mst) + 0.00054*mean(v$nbviscomp) +  0.00120*mean(v$temperature)*mean(v$pluie)

plot(exp(f)~julien,ylim=ylim, ylab="SD.IVD (sec)", xlab="Julian date",cex.lab=1.5,cex.axis=1.5, type="n")
#lines(exp(f)~julien, lty=1, lwd=3)
#lines(exp(m)~julien, lty=3, lwd=3)
points(v$julien,exp(log(v$sd90)),col=col)
legend("topright",c("a)"),lty=c(1,3),bty="n", col=F, cex=1.5)
legend("topleft",c("Male","Female"),lty=c(1,3),lwd=2, bty="n",col=T, cex=1.25)

# modavgpred
x<-seq(142,248, by=10)
newdat2<-newdat[rep(1,length(x)),]
newdat2[,"julien"]<-x
newdat2[,"sexe1"]<-as.factor("4")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
lines(exp(p$mod.avg.pred)~x, lty=1, lwd=2)
newdat2[,"sexe1"]<-as.factor("5")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
lines(exp(p$mod.avg.pred)~x, lty=3, lwd=2)


### temperature
temperature<-seq(7,27, by=1)
yy<- 7.88650 +0.37710 -0.05772*mean(v$masse) + 0.00100*mean(v$parasites) -0.04156 -0.00094*mean(v$julien) -1.92458*mean(v$nbvisdperso) + 0.33182*mean(v$global) + 0.02039*(temperature) -0.00426*mean(v$pluie) + 0.01058*mean(v$nbabr) +  0.00014*mean(v$fleur) -0.00004*mean(v$gen.mst) + 0.00054*mean(v$nbviscomp) -0.00053*mean(v$fleur)  -0.00253*mean(v$julien)  -0.00090*mean(v$nbviscomp) +  0.00120*(temperature)*mean(v$pluie)
plot(exp(yy)~temperature,ylim=ylim, ylab="SD.IVD (sec)", xlab="Temperature (C)",cex.lab=1.5,cex.axis=1.5, type="n")
#lines(exp(yy)~temperature, lwd=3)
legend("topright",c("b)"),lty=c(1,3),bty="n", col=F, cex=1.5)
points(v$temperature,exp(log(v$sd90)),col=col)

yymax<- 7.88650 +0.37710 -0.05772*mean(v$masse) + 0.00100*mean(v$parasites) -0.04156 -0.00094*mean(v$julien) -1.92458*mean(v$nbvisdperso) + 0.33182*mean(v$global) + 0.02039*min(temperature) -0.00426*mean(v$pluie) + 0.01058*mean(v$nbabr) +  0.00014*mean(v$fleur) -0.00004*mean(v$gen.mst) + 0.00054*mean(v$nbviscomp) -0.00053*mean(v$fleur)  -0.00253*mean(v$julien)  -0.00090*mean(v$nbviscomp) +  0.00120*min(temperature)*mean(v$pluie)
yymin<- 7.88650 +0.37710 -0.05772*mean(v$masse) + 0.00100*mean(v$parasites) -0.04156 -0.00094*mean(v$julien) -1.92458*mean(v$nbvisdperso) + 0.33182*mean(v$global) + 0.02039*max(temperature) -0.00426*mean(v$pluie) + 0.01058*mean(v$nbabr) +  0.00014*mean(v$fleur) -0.00004*mean(v$gen.mst) + 0.00054*mean(v$nbviscomp) -0.00053*mean(v$fleur)  -0.00253*mean(v$julien)  -0.00090*mean(v$nbviscomp) +  0.00120*max(temperature)*mean(v$pluie)
exp(yymin)
exp(yymax)

# modavgpred
x<-seq(7,27, by=1)
newdat2<-newdat[rep(1,length(x)),]
newdat2[,"temperature"]<-x
newdat2[,"sexe1"]<-as.factor("4")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
lines(exp(p$mod.avg.pred)~x, lty=1, lwd=2)
newdat2[,"sexe1"]<-as.factor("5")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
lines(exp(p$mod.avg.pred)~x, lty=3, lwd=2)


#nbvisdperso
nbvisdperso<-seq(0,1, by=0.05)
yy<- 7.88650 +0.37710 -0.05772*mean(v$masse) + 0.00100*mean(v$parasites) -0.04156 -0.00094*mean(v$julien) -1.92458*(nbvisdperso) + 0.33182*mean(v$global) + 0.02039*mean(v$temperature) -0.00426*mean(v$pluie) + 0.01058*mean(v$nbabr) +  0.00014*mean(v$fleur) -0.00004*mean(v$gen.mst) + 0.00054*mean(v$nbviscomp) -0.00053*mean(v$fleur)  -0.00253*mean(v$julien)  -0.00090*mean(v$nbviscomp) +  0.00120*mean(v$temperature)*mean(v$pluie)
plot(exp(yy)~nbvisdperso,ylim=ylim, ylab="SD.IVD (sec)", xlab="Spatial concentration",cex.lab=1.5,cex.axis=1.5, type="n")
#lines(exp(yy)~nbvisdperso, lwd=3)
legend("topright",c("c)"),lty=c(1,3),bty="n", col=F, cex=1.5)
points(v$nbvisdperso,exp(log(v$sd90)),col=col)

yymin<- 7.88650 +0.37710 -0.05772*mean(v$masse) + 0.00100*mean(v$parasites) -0.04156 -0.00094*mean(v$julien) -1.92458*min(nbvisdperso) + 0.33182*mean(v$global) + 0.02039*mean(v$temperature) -0.00426*mean(v$pluie) + 0.01058*mean(v$nbabr) +  0.00014*mean(v$fleur) -0.00004*mean(v$gen.mst) + 0.00054*mean(v$nbviscomp) -0.00053*mean(v$fleur)  -0.00253*mean(v$julien)  -0.00090*mean(v$nbviscomp) +  0.00120*mean(v$temperature)*mean(v$pluie)
yymax<- 7.88650 +0.37710 -0.05772*mean(v$masse) + 0.00100*mean(v$parasites) -0.04156 -0.00094*mean(v$julien) -1.92458*max(nbvisdperso) + 0.33182*mean(v$global) + 0.02039*mean(v$temperature) -0.00426*mean(v$pluie) + 0.01058*mean(v$nbabr) +  0.00014*mean(v$fleur) -0.00004*mean(v$gen.mst) + 0.00054*mean(v$nbviscomp) -0.00053*mean(v$fleur)  -0.00253*mean(v$julien)  -0.00090*mean(v$nbviscomp) +  0.00120*mean(v$temperature)*mean(v$pluie)

# modavgpred
x<-seq(0,1, by=0.05)
newdat2<-newdat[rep(1,length(x)),]
newdat2[,"nbvisdperso"]<-x
newdat2[,"sexe1"]<-as.factor("4")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
lines(exp(p$mod.avg.pred)~x, lty=1, lwd=2)
newdat2[,"sexe1"]<-as.factor("5")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
lines(exp(p$mod.avg.pred)~x, lty=3, lwd=2)

## global
global<-seq(0,1, by=0.05)
yy<- 7.88650 +0.37710 -0.05772*mean(v$masse) + 0.00100*mean(v$parasites) -0.04156 -0.00094*mean(v$julien) -1.92458*mean(v$nbvisdperso) + 0.33182*(global) + 0.02039*mean(v$temperature) -0.00426*mean(v$pluie) + 0.01058*mean(v$nbabr) +  0.00014*mean(v$fleur) -0.00004*mean(v$gen.mst) + 0.00054*mean(v$nbviscomp) -0.00053*mean(v$fleur)  -0.00253*mean(v$julien)  -0.00090*mean(v$nbviscomp) +  0.00120*mean(v$temperature)*mean(v$pluie)
plot(exp(yy)~global,ylim=ylim, ylab="SD.IVD (sec)", xlab="FT", cex.lab=1.5,cex.axis=1.5, type="n")
#lines(exp(yy)~global, lwd=3)
legend("topright",c("d)"),lty=c(1,3),bty="n", col=F, cex=1.5)
points(v$global,exp(log(v$sd90)),col=col)

yymin<- 7.88650 +0.37710 -0.05772*mean(v$masse) + 0.00100*mean(v$parasites) -0.04156 -0.00094*mean(v$julien) -1.92458*mean(v$nbvisdperso) + 0.33182*min(global) + 0.02039*mean(v$temperature) -0.00426*mean(v$pluie) + 0.01058*mean(v$nbabr) +  0.00014*mean(v$fleur) -0.00004*mean(v$gen.mst) + 0.00054*mean(v$nbviscomp) -0.00053*mean(v$fleur)  -0.00253*mean(v$julien)  -0.00090*mean(v$nbviscomp) +  0.00120*mean(v$temperature)*mean(v$pluie)
yymax<- 7.88650 +0.37710 -0.05772*mean(v$masse) + 0.00100*mean(v$parasites) -0.04156 -0.00094*mean(v$julien) -1.92458*mean(v$nbvisdperso) + 0.33182*max(global) + 0.02039*mean(v$temperature) -0.00426*mean(v$pluie) + 0.01058*mean(v$nbabr) +  0.00014*mean(v$fleur) -0.00004*mean(v$gen.mst) + 0.00054*mean(v$nbviscomp) -0.00053*mean(v$fleur)  -0.00253*mean(v$julien)  -0.00090*mean(v$nbviscomp) +  0.00120*mean(v$temperature)*mean(v$pluie)
exp(yymin)
exp(yymax)

# modavgpred
x<-seq(0,1, by=0.05)
newdat2<-newdat[rep(1,length(x)),]
newdat2[,"global"]<-x
newdat2[,"sexe1"]<-as.factor("4")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
lines(exp(p$mod.avg.pred)~x, lty=1, lwd=2)
newdat2[,"sexe1"]<-as.factor("5")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
lines(exp(p$mod.avg.pred)~x, lty=3, lwd=2)


dev.off()


#############################################
### Fig 4
#############################################

##########################################################################
##  Modelisation de l indice global
#### ne converge pas avec tcompGlob, de plus,  cor(v$nbviscompGlob,v$tcompGlob)= 0.8701967  trop correle
####   dbfinal_1mars2010.txt == base de donnees final pour 2007 et 2008 
library(lme4)
v<-read.table("C:/Users/rouf1703/Documents/UdeS/Consultation/MBelisle/Doc/YanickPaper/dbfinal_1mars2010.txt", header=T, stringsAsFactors=F)
sexe1<-as.factor(v$sexe)
annee1<-as.factor(v$annee)
v<-cbind(v,sexe1,annee1)

########  Meme serie de modele pour l indice theo

t1<- (lmer(cbind(v$nb0, v$long.dia - v$nb0)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + nbabreuvTrap + moy_pond_trouee_Trap + moy_pond_arbre_Trap + moy_pond_gauli_Trap + moy_pond_fleur_Trap + gen.mst + nbviscompTrap + sexe1:moy_pond_trouee_Trap + sexe1:moy_pond_gauli_Trap + sexe1:moy_pond_arbre_Trap+ sexe1:moy_pond_fleur_Trap+ sexe1:julien+pluie:temperature+(1|bague), REML=F , data=v, na.action=na.omit,family=binomial))
t2<- (lmer(cbind(v$nb0, v$long.dia - v$nb0)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + moy_pond_trouee_Trap + moy_pond_arbre_Trap + moy_pond_gauli_Trap + moy_pond_fleur_Trap + gen.mst + nbviscompTrap +nbabreuvTrap+ sexe1:julien+pluie:temperature+(1|bague), REML=F , data=v, na.action=na.omit,family=binomial))
t3<- (lmer(cbind(v$nb0, v$long.dia - v$nb0)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + gen.mst + nbviscompTrap + sexe1:julien+nbabreuvTrap+pluie:temperature+(1|bague), REML=F , data=v, na.action=na.omit,family=binomial))
t4<- (lmer(cbind(v$nb0, v$long.dia - v$nb0)~  sexe1 + masse + parasites+ annee1 + julien + (1|bague) , data=v, REML=F, na.action=na.omit, family=binomial))
## avec variables abiotiques   Pas d interactions possible entre pluie et annee : false convergence
t5<- (lmer(cbind(v$nb0, v$long.dia - v$nb0) ~  temperature+ pluie+ pluie:temperature +  julien+  annee1 +(1|bague), REML=F , data=v, na.action=na.omit, family=binomial))
## avec variables du paysages
t6<- (lmer(cbind(v$nb0, v$long.dia - v$nb0) ~  moy_pond_trouee_Trap + moy_pond_arbre_Trap + moy_pond_gauli_Trap + moy_pond_fleur_Trap +annee1+ julien+(1|bague), REML=F , data=v, na.action=na.omit, family=binomial))
## avec variables rattachees aux mouvements et traplines  (ne converge pas avec gen.mst)
t7<- (lmer(cbind(v$nb0, v$long.dia - v$nb0) ~  nbvisdpersoMAX+ nbabreuvTrap+annee1+ julien+(1|bague), REML=F , data=v, na.action=na.omit, family=binomial))
## avec variables reliees aux a la presence des competiteurs # pas d interaction avec annee ou julien possible false convergence
t8<- (lmer(cbind(v$nb0, v$long.dia - v$nb0) ~  nbviscompTrap+ annee1 + julien +(1|bague), REML=F , data=v, na.action=na.omit, family=binomial))
t9<- (lmer(cbind(v$nb0, v$long.dia - v$nb0)~ annee1+ julien+ (1|bague), REML=F , data=v, na.action=na.omit,family=binomial))

##  Theo avec REML=T pour le model avraging ...
t1<- (lmer(cbind(v$nb0, v$long.dia - v$nb0)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + nbabreuvTrap + moy_pond_trouee_Trap + moy_pond_arbre_Trap + moy_pond_gauli_Trap + moy_pond_fleur_Trap + gen.mst + nbviscompTrap + sexe1:moy_pond_trouee_Trap + sexe1:moy_pond_gauli_Trap + sexe1:moy_pond_arbre_Trap+ sexe1:moy_pond_fleur_Trap+ sexe1:julien+pluie:temperature+(1|bague), REML=T , data=v, na.action=na.omit,family=binomial))
t2<- (lmer(cbind(v$nb0, v$long.dia - v$nb0)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + moy_pond_trouee_Trap + moy_pond_arbre_Trap + moy_pond_gauli_Trap + moy_pond_fleur_Trap + gen.mst + nbviscompTrap +nbabreuvTrap+ sexe1:julien+pluie:temperature+(1|bague), REML=T , data=v, na.action=na.omit,family=binomial))
t3<- (lmer(cbind(v$nb0, v$long.dia - v$nb0)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + gen.mst + nbviscompTrap + sexe1:julien+nbabreuvTrap+pluie:temperature+(1|bague), REML=T , data=v, na.action=na.omit,family=binomial))
t4<- (lmer(cbind(v$nb0, v$long.dia - v$nb0)~  sexe1 + masse + parasites+ annee1 + julien + (1|bague) , data=v, REML=T, na.action=na.omit, family=binomial))
## avec variables abiotiques   Pas d interactions possible entre pluie et annee : false convergence
t5<- (lmer(cbind(v$nb0, v$long.dia - v$nb0) ~  temperature+ pluie+ pluie:temperature +  julien+  annee1 +(1|bague), REML=T , data=v, na.action=na.omit, family=binomial))
## avec variables du paysages
t6<- (lmer(cbind(v$nb0, v$long.dia - v$nb0) ~  moy_pond_trouee_Trap + moy_pond_arbre_Trap + moy_pond_gauli_Trap + moy_pond_fleur_Trap +annee1+ julien+(1|bague), REML=T , data=v, na.action=na.omit, family=binomial))
## avec variables rattachees aux mouvements et traplines  (ne converge pas avec gen.mst)
t7<- (lmer(cbind(v$nb0, v$long.dia - v$nb0) ~  nbvisdpersoMAX+ nbabreuvTrap+annee1+ julien+(1|bague), REML=T , data=v, na.action=na.omit, family=binomial))
## avec variables reliees aux a la presence des competiteurs # pas d interaction avec annee ou julien possible false convergence
t8<- (lmer(cbind(v$nb0, v$long.dia - v$nb0) ~  nbviscompTrap+ annee1 + julien +(1|bague), REML=T , data=v, na.action=na.omit, family=binomial))
t9<- (lmer(cbind(v$nb0, v$long.dia - v$nb0)~ annee1+ julien+ (1|bague), REML=T , data=v, na.action=na.omit,family=binomial))


# Pour sortir des tableaux d AIC
# Il faut cr??r une liste appel?e "model" avec tout les mod?les
#! ex: model<-c(model1,model2,model3,...)


model<-c(t1,t2,t3,t4,t5,t6,t7,t8,t9)
#model<-c(g1,g2,g3,g4,g5,g6,g7,g8,g9)
#model<-c(s1,s2,s3,s4,s5)
#model<-c(c1,c2,c3,c4)
# Tableau AICc pour lmer
Modele <- c(1:length(model))
Deviance<-sapply(model,function(i) deviance(i))
# indiquer le nombre de param?tres fixes dans chacun de mes mod?les
fixed.par<-sapply(model,function(i) length(fixef(i)))
# indiquer le nombre de composantes al?atoires dans chacun de mes mod?les
random.components<-sapply(model,function(i) length(ranef(i)))
K <- fixed.par+random.components
n <- length(fitted(g1))
mAIC <- Deviance+2*K
mAICc <- mAIC+2*K*(K+1)/(n-K-1)
deltai <- mAICc-min(mAICc)
wi <- round(exp(-0.5*deltai)/sum(exp(-0.5*deltai)), 3)
tableau_lmer1 <- cbind(Modele, Deviance, K, mAIC, mAICc, deltai, wi)
tableau_lmer1

### master data.frame
newdat<-data.frame(
  sexe1=4,
  masse=mean(v$masse),
	 parasites=mean(v$parasites),
  annee1="2008",
  julien=mean(v$julien),
  nbvisdpersoMAX=mean(v$nbvisdpersoMAX),
  temperature=mean(v$temperature),
  pluie=mean(v$pluie),
  nbabreuvTrap=mean(v$nbabreuvTrap),
  moy_pond_trouee_Trap=mean(v$moy_pond_trouee_Trap),
  moy_pond_arbre_Trap=mean(v$moy_pond_arbre_Trap),
  moy_pond_gauli_Trap=mean(v$moy_pond_gauli_Trap),
  moy_pond_fleur_Trap=mean(v$moy_pond_fleur_Trap),
  nbviscompTrap=mean(v$nbviscompTrap),
  gen.mst=mean(v$gen.mst)
)



png("C:/Users/rouf1703/Documents/UdeS/Consultation/MBelisle/Doc/YanickPaper/Fig4.png",width=6.5,height=4,units="in",res=500,pointsize=6)

###### Figures illustrant l effet des covariables sur l indice global
library(boot)
par(mfrow=c(1,2),mar=c(5,6,2,2)) # pour mon seminaire 2
par(mfrow=c(2,3))
# les coefs sont ceux des femelles en 2008

cbind(v$nb0, v$long.dia - v$nb0)
val<-v$nb0/v$long.dia

####################
moyarbre<-seq(0,max(v$moy_pond_arbre_Global),1)
f<- 8.816e-01 + 2.507e+00 + mean(v$masse)*(-3.486e-01) + mean(v$parasites)*8.140e-03 + 1.312e-01+ mean(v$julien)*(-1.604e-03)+ mean(v$nbvisdpersoMAX)*-2.003e+00 + mean(v$temperature)*(-1.753e-03) + mean(v$pluie)*(-6.578e-03) + (mean(v$nbabreuvGlob)*2.351e-01) + mean(v$moy_pond_trouee_Global)*1.160e+00 + moyarbre*4.693e-02 + mean(v$moy_pond_gauli_Global)*3.034e-02 + mean(v$moy_pond_fleur_Global)*(3.496e-04) + mean(v$gen.mst)*(-1.025e-03) + mean(v$nbviscompGlob)*(-2.180e-04) -1.503e+00*mean(v$moy_pond_trouee_Global) -5.038e-02*mean(v$moy_pond_gauli_Global) -4.589e-02*moyarbre -2.559e-03*mean(v$moy_pond_fleur_Global) -1.752e-03*mean(v$julien) +2.989e-04*mean(v$temperature)*mean(v$pluie)
m<- 8.816e-01 + mean(v$masse)*(-3.486e-01) + mean(v$parasites)*8.140e-03 + 1.312e-01+ mean(v$julien)*(-1.604e-03)+ mean(v$nbvisdpersoMAX)*-2.003e+00 + mean(v$temperature)*(-1.753e-03) + mean(v$pluie)*(-6.578e-03) + (mean(v$nbabreuvGlob)*2.351e-01) + mean(v$moy_pond_trouee_Global)*1.160e+00 + moyarbre*4.693e-02 + mean(v$moy_pond_gauli_Global)*3.034e-02 + mean(v$moy_pond_fleur_Global)*(3.496e-04) + mean(v$gen.mst)*(-1.025e-03) + mean(v$nbviscompGlob)*(-2.180e-04) +2.989e-04*mean(v$temperature)*mean(v$pluie)
#plot(inv.logit(f)~moyarbre,ylim=c(0,1), ylab="indice global",xlab="densit? arbres",cex.lab=1.5,cex.axis=1.5)
plot(inv.logit(f)~moyarbre,ylim=c(0,1), ylab="FT",xlab="Tree density",cex.lab=1.8,cex.axis=2,type="n")
points(v$moy_pond_arbre_Trap,val,col=gray(0,0.05))
#lines(inv.logit(m)~moyarbre, lty=1, lwd=3.5)
#lines(inv.logit(f)~moyarbre, lty=3, lwd=3.5)
legend("topright",c("a)"),lty=c(1,3),bty="n", col=F, cex=2)
legend("topleft",c("Male","Female"),lty=c(1,3),lwd=2, bty="n",col=T, cex=2)

### modavgpred
x<-seq(0,max(v$moy_pond_arbre_Trap),1)
newdat2<-newdat[rep(1,length(x)),]
newdat2[,"moy_pond_arbre_Trap"]<-x
newdat2[,"sexe1"]<-as.factor("4")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
p$mod.avg.pred<-inv.logit(p$mod.avg.pred)
lines(p$mod.avg.pred~x, lty=1, lwd=2)

newdat2[,"sexe1"]<-as.factor("5")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
p$mod.avg.pred<-inv.logit(p$mod.avg.pred)
lines(p$mod.avg.pred~x, lty=3, lwd=2)


##################
moytrouee<-seq(0,1,0.05)
f<- 8.816e-01 + 2.507e+00 + mean(v$masse)*(-3.486e-01) + mean(v$parasites)*8.140e-03 + 1.312e-01+ mean(v$julien)*(-1.604e-03)+ mean(v$nbvisdpersoMAX)*-2.003e+00 + mean(v$temperature)*(-1.753e-03) + mean(v$pluie)*(-6.578e-03) + (mean(v$nbabreuvGlob)*2.351e-01) + moytrouee*1.160e+00 + mean(v$moy_pond_arbre_Global)*4.693e-02 + mean(v$moy_pond_gauli_Global)*3.034e-02 + mean(v$moy_pond_fleur_Global)*(3.496e-04) + mean(v$gen.mst)*(-1.025e-03) + mean(v$nbviscompGlob)*(-2.180e-04) -1.503e+00*moytrouee -5.038e-02*mean(v$moy_pond_gauli_Global) -4.589e-02*mean(v$moy_pond_arbre_Global) -2.559e-03*mean(v$moy_pond_fleur_Global) -1.752e-03*mean(v$julien) +2.989e-04*mean(v$temperature)*mean(v$pluie)
m<- 8.816e-01 + mean(v$masse)*(-3.486e-01) + mean(v$parasites)*8.140e-03 + 1.312e-01+ mean(v$julien)*(-1.604e-03)+ mean(v$nbvisdpersoMAX)*-2.003e+00 + mean(v$temperature)*(-1.753e-03) + mean(v$pluie)*(-6.578e-03) + (mean(v$nbabreuvGlob)*2.351e-01) + moytrouee*1.160e+00 + mean(v$moy_pond_arbre_Global)*4.693e-02 + mean(v$moy_pond_gauli_Global)*3.034e-02 + mean(v$moy_pond_fleur_Global)*(3.496e-04) + mean(v$gen.mst)*(-1.025e-03) + mean(v$nbviscompGlob)*(-2.180e-04) +2.989e-04*mean(v$temperature)*mean(v$pluie)
#plot(inv.logit(f)~moytrouee,ylim=c(0,1), ylab="indice global",xlab="ouverture canop?e",cex.lab=1.5,cex.axis=1.5)
plot(inv.logit(f)~moytrouee,ylim=c(0,1), ylab="FT",type="n",xlab="Forest gap",cex.lab=1.8,cex.axis=2)
points(v$moy_pond_trouee_Trap,val,col=gray(0,0.05))
#lines(inv.logit(m)~moytrouee, lty=1, lwd=3.5)
#lines(inv.logit(f)~moytrouee, lty=3, lwd=3.5)
legend("topright",c("b)"),lty=c(1,3),bty="n", col=F, cex=2)
#legend(0.3,0.9,c("Male","Female"),lty=c(1,3),lwd=3, bty="n",col=T, cex=2)

### modavgpred
x<-seq(0,max(v$moy_pond_trouee_Trap),0.05)
newdat2<-newdat[rep(1,length(x)),]
newdat2[,"moy_pond_trouee_Trap"]<-x
newdat2[,"sexe1"]<-as.factor("4")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
p$mod.avg.pred<-inv.logit(p$mod.avg.pred)
lines(p$mod.avg.pred~x, lty=1, lwd=2)

newdat2[,"sexe1"]<-as.factor("5")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
p$mod.avg.pred<-inv.logit(p$mod.avg.pred)
lines(p$mod.avg.pred~x, lty=3, lwd=2)


##################
moyfleur<-seq(0,max(v$moy_pond_fleur_Glob),10)
f<- 8.816e-01 + 2.507e+00 + mean(v$masse)*(-3.486e-01) + mean(v$parasites)*8.140e-03 + 1.312e-01+ mean(v$julien)*(-1.604e-03)+ mean(v$nbvisdpersoMAX)*-2.003e+00 + mean(v$temperature)*(-1.753e-03) + mean(v$pluie)*(-6.578e-03) + (mean(v$nbabreuvGlob)*2.351e-01) + mean(v$moy_pond_trouee_Global)*1.160e+00 + mean(v$moy_pond_arbre_Global)*4.693e-02 + mean(v$moy_pond_gauli_Global)*3.034e-02 + moyfleur*(3.496e-04) + mean(v$gen.mst)*(-1.025e-03) + mean(v$nbviscompGlob)*(-2.180e-04) -1.503e+00*mean(v$moy_pond_trouee_Global) -5.038e-02*mean(v$moy_pond_gauli_Global) -4.589e-02*mean(v$moy_pond_arbre_Global) -2.559e-03*moyfleur -1.752e-03*mean(v$julien) +2.989e-04*mean(v$temperature)*mean(v$pluie)
m<- 8.816e-01 + mean(v$masse)*(-3.486e-01) + mean(v$parasites)*8.140e-03 + 1.312e-01+ mean(v$julien)*(-1.604e-03)+ mean(v$nbvisdpersoMAX)*-2.003e+00 + mean(v$temperature)*(-1.753e-03) + mean(v$pluie)*(-6.578e-03) + (mean(v$nbabreuvGlob)*2.351e-01) + mean(v$moy_pond_trouee_Global)*1.160e+00 + mean(v$moy_pond_arbre_Global)*4.693e-02 + mean(v$moy_pond_gauli_Global)*3.034e-02 + moyfleur*(3.496e-04) + mean(v$gen.mst)*(-1.025e-03) + mean(v$nbviscompGlob)*(-2.180e-04) +2.989e-04*mean(v$temperature)*mean(v$pluie)
#plot(inv.logit(f)~moyfleur,ylim=c(0,1), ylab="indice global", xlab="densit? fleurs",cex.lab=1.5,cex.axis=1.5)
plot(inv.logit(f)~moyfleur,ylim=c(0,1), ylab="FT", xlab="Flower density (nb/m^2)",cex.lab=1.8,cex.axis=2,type="n")
points(v$moy_pond_fleur_Trap,val,col=gray(0,0.05))
#lines(inv.logit(m)~moyfleur, lty=1, lwd=3.5)
#lines(inv.logit(f)~moyfleur, lty=3, lwd=3.5)
legend("topright",c("c)"),lty=c(1,3),bty="n", col=F, cex=2)
#legend(100,0.9,c("Male","Female"),lty=c(1,3),lwd=3, bty="n",col=T, cex=2)        #,

### modavgpred
x<-seq(0,max(v$moy_pond_fleur_Trap),10)
newdat2<-newdat[rep(1,length(x)),]
newdat2[,"moy_pond_fleur_Trap"]<-x
newdat2[,"sexe1"]<-as.factor("4")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
p$mod.avg.pred<-inv.logit(p$mod.avg.pred)
lines(p$mod.avg.pred~x, lty=1, lwd=2)

newdat2[,"sexe1"]<-as.factor("5")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
p$mod.avg.pred<-inv.logit(p$mod.avg.pred)
lines(p$mod.avg.pred~x, lty=3, lwd=2)



########################
nbvisdpersoMAX<-seq(0,1,0.05)
xx<- 8.816e-01 + 2.507e+00 + mean(v$masse)*(-3.486e-01) + mean(v$parasites)*8.140e-03 + 1.312e-01+ mean(v$julien)*(-1.604e-03)+ (nbvisdpersoMAX*-2.003e+00)+ mean(v$temperature)*(-1.753e-03) + mean(v$pluie)*(-6.578e-03) + (mean(v$nbabreuvGlob)*2.351e-01) + mean(v$moy_pond_trouee_Global)*1.160e+00 + mean(v$moy_pond_arbre_Global)*4.693e-02 + mean(v$moy_pond_gauli_Global)*3.034e-02 + mean(v$moy_pond_fleur_Global)*(3.496e-04) + mean(v$gen.mst)*(-1.025e-03) + mean(v$nbviscompGlob)*(-2.180e-04) -1.503e+00*mean(v$moy_pond_trouee_Global) -5.038e-02*mean(v$moy_pond_gauli_Global) -4.589e-02*mean(v$moy_pond_arbre_Global) -2.559e-03*mean(v$moy_pond_fleur_Global) -1.752e-03*mean(v$julien) +2.989e-04*mean(v$temperature)*mean(v$pluie)
plot(inv.logit(xx)~nbvisdpersoMAX,ylim=c(0,1),ylab="FT", xlab="Spatial concentration",cex.lab=1.8,cex.axis=2,type="n")
#plot(inv.logit(xx)~nbvisdpersoMAX,ylim=c(0,1), ylab="indice global",cex.lab=1.5,cex.axis=1.5)
points(v$nbvisdpersoMAX,val,col=gray(0,0.05))
#lines(inv.logit(xx)~nbvisdpersoMAX, lwd=3.5)
legend("topright",c("d)"),lty=c(1,3),bty="n", col=F, cex=2)

### modavgpred
x<-seq(0,1,0.05)
newdat2<-newdat[rep(1,length(x)),]
newdat2[,"nbvisdpersoMAX"]<-x
newdat2[,"sexe1"]<-as.factor("4")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
p$mod.avg.pred<-inv.logit(p$mod.avg.pred)
lines(p$mod.avg.pred~x, lty=1, lwd=2)

newdat2[,"sexe1"]<-as.factor("5")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
p$mod.avg.pred<-inv.logit(p$mod.avg.pred)
lines(p$mod.avg.pred~x, lty=3, lwd=2)





############################
nbabreuv<-c(2:9)
xx<- 8.816e-01 + 2.507e+00 + mean(v$masse)*(-3.486e-01) + mean(v$parasites)*8.140e-03 + 1.312e-01+ mean(v$julien)*(-1.604e-03)+ (mean(v$nbvisdpersoMAX)*-2.003e+00)+ mean(v$temperature)*(-1.753e-03) + mean(v$pluie)*(-6.578e-03) + (nbabreuv*2.351e-01) + mean(v$moy_pond_trouee_Global)*1.160e+00 + mean(v$moy_pond_arbre_Global)*4.693e-02 + mean(v$moy_pond_gauli_Global)*3.034e-02 + mean(v$moy_pond_fleur_Global)*(3.496e-04) + mean(v$gen.mst)*(-1.025e-03) + mean(v$nbviscompGlob)*(-2.180e-04) -1.503e+00*mean(v$moy_pond_trouee_Global) -5.038e-02*mean(v$moy_pond_gauli_Global) -4.589e-02*mean(v$moy_pond_arbre_Global) -2.559e-03*mean(v$moy_pond_fleur_Global) -1.752e-03*mean(v$julien) +2.989e-04*mean(v$temperature)*mean(v$pluie)
plot(inv.logit(xx)~nbabreuv,ylim=c(0,1),ylab="FT",xlab="Number of feeders",cex.lab=1.8,cex.axis=2, type="n")
points(v$nbabreuvTrap,val,col=gray(0,0.05))
#lines(inv.logit(xx)~nbabreuv, lwd=3.5)
legend("topright",c("e)"),lty=c(1,3),bty="n", col=F, cex=2)

### modavgpred
x<-2:4
newdat2<-newdat[rep(1,length(x)),]
newdat2[,"nbabreuvTrap"]<-x
newdat2[,"sexe1"]<-as.factor("4")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
p$mod.avg.pred<-inv.logit(p$mod.avg.pred)
lines(p$mod.avg.pred~x, lty=1, lwd=2)

newdat2[,"sexe1"]<-as.factor("5")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
p$mod.avg.pred<-inv.logit(p$mod.avg.pred)
lines(p$mod.avg.pred~x, lty=3, lwd=2)



##############################
mst<-seq(0,max(v$gen.mst),100)
y<- 8.816e-01 + 2.507e+00 + mean(v$masse)*(-3.486e-01) + mean(v$parasites)*8.140e-03 + 1.312e-01+ mean(v$julien)*(-1.604e-03)+ mean(v$nbvisdpersoMAX)*-2.003e+00 + mean(v$temperature)*(-1.753e-03) + mean(v$pluie)*(-6.578e-03) + (mean(v$nbabreuvGlob)*2.351e-01) + mean(v$moy_pond_trouee_Global)*1.160e+00 + mean(v$moy_pond_arbre_Global)*4.693e-02 + mean(v$moy_pond_gauli_Global)*3.034e-02 + mean(v$moy_pond_fleur_Global)*(3.496e-04) + mst*(-1.025e-03) + mean(v$nbviscompGlob)*(-2.180e-04) -1.503e+00*mean(v$moy_pond_trouee_Global) -5.038e-02*mean(v$moy_pond_gauli_Global) -4.589e-02*mean(v$moy_pond_arbre_Global) -2.559e-03*mean(v$moy_pond_fleur_Global) -1.752e-03*mean(v$julien) +2.989e-04*mean(v$temperature)*mean(v$pluie)
#plot(inv.logit(y)~mst,ylim=c(0,1), ylab="indice global",cex.lab=1.5,cex.axis=1.5)
plot(inv.logit(y)~mst,ylim=c(0,1), ylab="FT", xlab="MST (m)",cex.lab=1.8,cex.axis=2, type="n")
points(v$gen.mst,val,col=gray(0,0.05))
#lines(inv.logit(y)~mst, lwd=3.5)
legend("topright",c("f)"),lty=c(1,3),bty="n", col=F, cex=2)

### modavgpred
x<-seq(min(v$gen.mst),max(v$gen.mst),100)
newdat2<-newdat[rep(1,length(x)),]
newdat2[,"gen.mst"]<-x
newdat2[,"sexe1"]<-as.factor("4")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
p$mod.avg.pred<-inv.logit(p$mod.avg.pred)
lines(p$mod.avg.pred~x, lty=1, lwd=2)

newdat2[,"sexe1"]<-as.factor("5")
p<-as.data.frame(modavgPred(model,newdat=newdat2,type="response")$matrix.output)
p$mod.avg.pred<-inv.logit(p$mod.avg.pred)
lines(p$mod.avg.pred~x, lty=3, lwd=2)


dev.off()





