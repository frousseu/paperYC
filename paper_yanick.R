library(data.table)
library(lme4)
library(AICcmodavg)
library(MuMIn)
library(lqmm)
library(quantreg)
library(TeachingDemos)
library(scales)
library(htmlTable)

#############################################
### piecharts
#############################################

d<-read.table("C:/Users/rouf1703/Documents/UdeS/Consultation/MBelisle/Doc/YanickPaper/calcul08décembre2009.txt",header=T,stringsAsFactors=F)

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
		if(all(y$abreuv%in%forest)){rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=gray(0,0.25),border=NA,density=10)}
		par(new=TRUE)
		pie(y$nbvisperso,init.angle=90,labels="",col=c("black","white"),lwd=0.1)
	}
	 
	#text(0,0,sum(y$nbvisperso),col="black",cex=1.5)
	scrap<-scrap+sum(y$nbvisperso)
	#pie(y$bague,init.angle=90,labels="",col=c("blue","red"))}
	if(i==46){
		plot(0,0,col="black",xaxt="n",yaxt="n",xlab="",ylab="",type="n",bty="n")
		text(-0.25,0.50,"Males",col="black",cex=1.8)
		points(0.75,0.50,col="black",cex=5,pch=16)
		text(-0.25,0.25,"Females",col="black",cex=1.8)
		points(0.75,0.25,col="black",cex=5,pch=21)
		
		text(-0.25,-0.25,"Fields/Fallows",col="black",cex=1.8)
		points(0.75,-0.25,col="black",cex=5,pch=22)
		text(-0.25,-0.50,"Forests",cex=1.8)
		points(0.75,-0.50,col="black",cex=5,pch=22)#,density=22)
		rect(0.65,-0.44,0.85,-0.56,col=gray(0,0.25),border=NA,density=20)
		#text(0,-0.75,paste("No. of visits=",scrap),cex=1.8,col="black")
	}
}

dev.off()

#############################################
### Fig 5
#############################################

subplotlim<-function(p=0.3,inset=c(0.04,0.05)){
  l<-par("usr")
  sx<-(l[2]-l[1])*p
  sy<-(l[4]-l[3])*p
  x<-c(l[2]-sx,l[2])-(sx/p)*p*inset[1]
  y<-c(l[4]-sy,l[4])-(sy/p)*p*inset[2]
  list(x=x,y=y)
}

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

### data for scaled explanatory variables
v2<-v
v2[]<-lapply(v,function(i){if(is.numeric(i)){scale(i)}else{i}})

### control to improve convergence
controlglmer<-glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=200000))

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

############################################################################################
##cor(v$nbvisdperso,v$nbvisperso)
##[1] 0.8225252   trop correle, je l enleve

#J enleve nbabr car trop correlee avec gen.mst. cor =  0.902

i1<- (lmer(log(v$sd90)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdperso+  global+ temperature + pluie + fleur + gen.mst + nbviscomp + sexe1:fleur+ sexe1:julien + sexe1:nbviscomp+ pluie:temperature+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
i2<- (lmer(log(v$sd90)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdperso+  global+ temperature + pluie + fleur + gen.mst + nbviscomp+  (1|bague) + (1|abreuv), REML=F, data=v, na.action=na.omit))
i3<- (lmer(log(v$sd90)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdperso+  global+temperature + pluie + gen.mst + nbviscomp + sexe1:julien+pluie:temperature+(1|bague)+ (1|abreuv), REML=F , data=v, na.action=na.omit))
i4<- (lmer(log(v$sd90)~  sexe1 + masse + parasites+ annee1 + julien+ sexe1:julien + (1|bague)+ (1|abreuv) , data=v, REML=F, na.action=na.omit))
## avec variables abiotiques   Pas d interactions possible entre pluie et annee : false convergence
i5<- (lmer(log(v$sd90) ~  temperature+ pluie+ temperature: pluie+  julien+  annee1 +(1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
## avec variables du paysages
i6<- (lmer(log(v$sd90) ~  fleur +annee1+ julien+ sexe1:fleur + sexe1:julien+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
## avec variables rattachees aux mouvements et traplines
i7<- (lmer(log(v$sd90)~  gen.mst+ nbvisdperso+ global + annee1+ julien+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
## avec variables reliees aux a la presence des competiteurs # pas d interaction avec annee ou julien possible false convergence
i8<- (lmer(log(v$sd90)~  nbviscomp+ annee1 + julien +(1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))
i9<- (lmer(log(v$sd90)~ annee1+ julien+ (1|bague)+ (1|abreuv), REML=F, data=v, na.action=na.omit))


#### AIC avec REML=F
#### modavg avec REML=T

#q1<- lqmm(log(sd90)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdperso+  global+ temperature + pluie + fleur + gen.mst + nbviscomp + sexe1:fleur+ sexe1:julien + sexe1:nbviscomp+ pluie:temperature,random=~1,group=bague, data=v,tau=0.9,na.action=na.omit)

q1<- rq(log(sd90)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdperso+  global+ temperature + pluie + fleur + gen.mst + nbviscomp + sexe1:fleur+ sexe1:julien + sexe1:nbviscomp+ pluie:temperature,data=v,tau=0.9,na.action=na.omit)

q2<- rq(log(sd90)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdperso+  global+ temperature + pluie + fleur + gen.mst + nbviscomp + sexe1:fleur+ sexe1:julien + sexe1:nbviscomp+ pluie:temperature,data=v,tau=0.1,na.action=na.omit)


##################################################################################
# Pour sortir des tableaux d AIC
# Il faut cr??r une liste appel?e "model" avec tout les mod?les
#! ex: model<-c(model1,model2,model3,...)


modeli<-list(i1=i1,i2=i2,i3=i3,i4=i4,i5=i5,i6=i6,i7=i7,i8=i8,i9=i9)
maSD<-aictab(modeli)
ma<-model.avg(modeli)
co<-t(ma$coefficients)
ci<-confint(ma,full=TRUE)
paramSD<-cbind(co,ci)



##############################################
#####  courbes pour l<article avec les variables significatives
##pour une femelle en 2008  a partir du fichier txt modavg.duree.inter
# pour variables nbvisdperso,  global, temperature, sexe15: julien


### master data.frame
newdat<-with(v,data.frame(
	sexe1="4",
	masse=mean(masse),
	parasites=mean(parasites),
	annee1="2008",
	julien=mean(julien),
	nbvisdperso=mean(nbvisdperso),
	global=mean(global),
	temperature=mean(temperature),
	pluie=mean(pluie),
  fleur=mean(fleur),
	gen.mst=mean(gen.mst),
	nbviscomp=mean(nbviscomp)
))



png("C:/Users/rouf1703/Documents/UdeS/Consultation/MBelisle/Doc/YanickPaper/Fig5_scaled.png",width=6.5,height=5,units="in",res=500,pointsize=5)

par(mfrow=c(2,2),oma=c(0,0,0,0),mar=c(4.5,5.5,1,1))

colm<-gray(0,0.50)
colf<-gray(0,0.20)
colp<-gray(0,0.10)
colp2<-gray(0,0.10/7)
ylim<-c(0,5000)

col<-alpha("black",0.1)
ylim2<-range(exp(log(v$sd90)),na.rm=TRUE)

### sexe15: julien
plot(v$julien,exp(log(v$sd90)),ylim=ylim, ylab="SD.IVD (sec x 1000)", xlab="Julian date",cex.lab=2,cex.axis=2, type="n",yaxt="n")
points(v$julien,exp(log(v$sd90)),col=colp)
legend("topleft",c("a)"),lty=c(1,3),bty="n", col=F, cex=2,inset=c(-0.1,0))
legend("topleft",c("Male","Female"),inset=c(0.2,0),fill=c(colm,colf),border=NA, bty="n",col=T, cex=2)
axis(2,at=pretty(ylim),label=pretty(ylim)/1000,las=2,cex.axis=2)

# modavgpred
x<-seq(142,248, by=10)
newdat2<-newdat[rep(1,length(x)),]
newdat2[,"julien"]<-x
newdat2[,"sexe1"]<-as.factor("4")
p1<-as.data.frame(modavgPred(modeli,newdat=newdat2,type="response")$matrix.output)
polygon(c(x,rev(x)),exp(c(p1$lower,rev(p1$upper))),border=NA,col=colm)
newdat2[,"sexe1"]<-as.factor("5")
p2<-as.data.frame(modavgPred(modeli,newdat=newdat2,type="response")$matrix.output)
polygon(c(x,rev(x)),exp(c(p2$lower,rev(p2$upper))),border=NA,col=colf)
rect(subplotlim()$x[1],subplotlim()$y[1],subplotlim()$x[2],subplotlim()$y[2],col="white")
subplot({
  par(mgp=c(2,1,0))
  plot(exp(f)~julien,ylim=ylim2,ylab="SD.IVD (sec x 1000)       ",xlab="Julian date",cex.lab=1,cex.axis=1,type="n",yaxt="n")
  points(v$julien,exp(log(v$sd90)),col=colp2,cex=0.7)
  polygon(c(x,rev(x)),exp(c(p1$lower,rev(p1$upper))),border=NA,col=colm)
  polygon(c(x,rev(x)),exp(c(p2$lower,rev(p2$upper))),border=NA,col=colf)
  lab<-pretty(ylim2)/1000#;lab[length(lab)]<-""
  axis(2,at=pretty(ylim2),label=lab,las=2,cex.axis=1)
  par(mgp=c(3,1,0))
},x=subplotlim()$x,y=subplotlim()$y,type="plt")


### temperature
plot(v$temperature,exp(log(v$sd90)),ylim=ylim, ylab="SD.IVD (sec x 1000)", xlab="Temperature (\u00B0C)",cex.lab=2,cex.axis=2, type="n",yaxt="n")
legend("topleft",c("b)"),lty=c(1,3),bty="n", col=F, cex=2,inset=c(-0.1,0))
points(v$temperature,exp(log(v$sd90)),col=colp)
axis(2,at=pretty(ylim),label=pretty(ylim)/1000,las=2,cex.axis=2)


# modavgpred
x<-seq(7,27, by=1)
newdat2<-newdat[rep(1,length(x)),]
newdat2[,"temperature"]<-x
newdat2[,"sexe1"]<-as.factor("4")
p1<-as.data.frame(modavgPred(modeli,newdat=newdat2,type="response")$matrix.output)
polygon(c(x,rev(x)),exp(c(p1$lower,rev(p1$upper))),border=NA,col=colm)
newdat2[,"sexe1"]<-as.factor("5")
p2<-as.data.frame(modavgPred(modeli,newdat=newdat2,type="response")$matrix.output)
polygon(c(x,rev(x)),exp(c(p2$lower,rev(p2$upper))),border=NA,col=colf)
rect(subplotlim()$x[1],subplotlim()$y[1],subplotlim()$x[2],subplotlim()$y[2],col="white")
subplot({
  par(mgp=c(2,1,0))
  plot(exp(yy)~temperature,ylim=ylim2,ylab="SD.IVD (sec x 1000)       ",xlab="Temperature (\u00B0C)",cex.lab=1,cex.axis=1,type="n",yaxt="n")
  points(v$temperature,exp(log(v$sd90)),col=colp2,cex=0.7)
  polygon(c(x,rev(x)),exp(c(p1$lower,rev(p1$upper))),border=NA,col=colm)
  polygon(c(x,rev(x)),exp(c(p2$lower,rev(p2$upper))),border=NA,col=colf)
  lab<-pretty(ylim2)/1000#;lab[length(lab)]<-""
  axis(2,at=pretty(ylim2),label=lab,las=2,cex.axis=1)
  par(mgp=c(3,1,0))
},x=subplotlim()$x,y=subplotlim()$y,type="plt")


#nbvisdperso
plot(v$nbvisdperso,exp(log(v$sd90)),ylim=ylim, ylab="SD.IVD (sec x 1000)", xlab="Spatial concentration",cex.lab=2,cex.axis=2, type="n",yaxt="n")
legend("topleft",c("c)"),lty=c(1,3),bty="n", col=F, cex=2,inset=c(-0.1,0))
points(v$nbvisdperso,exp(log(v$sd90)),col=colp)
axis(2,at=pretty(ylim),label=pretty(ylim)/1000,las=2,cex.axis=2)

# modavgpred
x<-seq(0,1, by=0.05)
newdat2<-newdat[rep(1,length(x)),]
newdat2[,"nbvisdperso"]<-x
newdat2[,"sexe1"]<-as.factor("4")
p1<-as.data.frame(modavgPred(modeli,newdat=newdat2,type="response")$matrix.output)
polygon(c(x,rev(x)),exp(c(p1$lower,rev(p1$upper))),border=NA,col=colm)
newdat2[,"sexe1"]<-as.factor("5")
p2<-as.data.frame(modavgPred(modeli,newdat=newdat2,type="response")$matrix.output)
polygon(c(x,rev(x)),exp(c(p2$lower,rev(p2$upper))),border=NA,col=colf)
rect(subplotlim()$x[1],subplotlim()$y[1],subplotlim()$x[2],subplotlim()$y[2],col="white")
subplot({
  par(mgp=c(2,1,0))
  plot(exp(yy)~nbvisdperso,ylim=ylim2,ylab="SD.IVD (sec x 1000)       ",xlab="Spatial concentration",cex.lab=1,cex.axis=1,type="n",yaxt="n")
  points(v$nbvisdperso,exp(log(v$sd90)),col=colp2,cex=0.7)
  polygon(c(x,rev(x)),exp(c(p1$lower,rev(p1$upper))),border=NA,col=colm)
  polygon(c(x,rev(x)),exp(c(p2$lower,rev(p2$upper))),border=NA,col=colf)
  lab<-pretty(ylim2)/1000#;lab[length(lab)]<-""
  axis(2,at=pretty(ylim2),label=lab,las=2,cex.axis=1)
  par(mgp=c(3,1,0))
},x=subplotlim()$x,y=subplotlim()$y,type="plt")

## global
plot(v$global,exp(log(v$sd90)),ylim=ylim, ylab="SD.IVD (sec x 1000)", xlab="FT", cex.lab=2,cex.axis=2, type="n",yaxt="n")
legend("topleft",c("d)"),lty=c(1,3),bty="n", col=F, cex=2,inset=c(-0.1,0))
points(v$global,exp(log(v$sd90)),col=colp)
axis(2,at=pretty(ylim),label=pretty(ylim)/1000,las=2,cex.axis=2)

# modavgpred
x<-seq(0,1, by=0.05)
newdat2<-newdat[rep(1,length(x)),]
newdat2[,"global"]<-x
newdat2[,"sexe1"]<-as.factor("4")
p1<-as.data.frame(modavgPred(modeli,newdat=newdat2,type="response")$matrix.output)
polygon(c(x,rev(x)),exp(c(p1$lower,rev(p1$upper))),border=NA,col=colm)
newdat2[,"sexe1"]<-as.factor("5")
p2<-as.data.frame(modavgPred(modeli,newdat=newdat2,type="response")$matrix.output)
polygon(c(x,rev(x)),exp(c(p2$lower,rev(p2$upper))),border=NA,col=colf)
rect(subplotlim()$x[1],subplotlim()$y[1],subplotlim()$x[2],subplotlim()$y[2],col="white")
subplot({
  par(mgp=c(2,1,0))
  plot(exp(yy)~global,ylim=ylim2,ylab="SD.IVD (sec x 1000)       ",xlab="FT",cex.lab=1,cex.axis=1,type="n",yaxt="n",bg="blue")
  points(v$global,exp(log(v$sd90)),col=colp2,cex=0.7)
  polygon(c(x,rev(x)),exp(c(p1$lower,rev(p1$upper))),border=NA,col=colm)
  polygon(c(x,rev(x)),exp(c(p2$lower,rev(p2$upper))),border=NA,col=colf)
  lab<-pretty(ylim2)/1000#;lab[length(lab)]<-""
  axis(2,at=pretty(ylim2),label=lab,las=2,cex.axis=1)
  par(mgp=c(3,1,0))
},x=subplotlim()$x,y=subplotlim()$y,type="plt")

dev.off()


#############################################
### Fig 4
#############################################

### THERE USE TO BE AN ERROR IN MAzerolle package when type="response" was not backtransformed to the original scale, error not present anymore in current version 2.1-0 (2017-02-17)

##########################################################################
##  Modelisation de l indice global
#### ne converge pas avec tcompGlob, de plus,  cor(v$nbviscompGlob,v$tcompGlob)= 0.8701967  trop correle
####   dbfinal_1mars2010.txt == base de donnees final pour 2007 et 2008 
library(lme4)
v<-read.table("C:/Users/rouf1703/Documents/UdeS/Consultation/MBelisle/Doc/YanickPaper/dbfinal_1mars2010.txt", header=T, stringsAsFactors=F)
sexe1<-as.factor(v$sexe)
annee1<-as.factor(v$annee)
v<-cbind(v,sexe1,annee1)

v2<-v
v2[]<-lapply(v,function(i){if(is.numeric(i)){scale(i)}else{i}})

########  Meme serie de modele pour l indice theo

t1<- (glmer(cbind(v$nb0, v$long.dia - v$nb0)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + nbabreuvTrap + moy_pond_trouee_Trap + moy_pond_arbre_Trap + moy_pond_gauli_Trap + moy_pond_fleur_Trap + gen.mst + nbviscompTrap + sexe1:moy_pond_trouee_Trap + sexe1:moy_pond_gauli_Trap + sexe1:moy_pond_arbre_Trap+ sexe1:moy_pond_fleur_Trap+ sexe1:julien+pluie:temperature+(1|bague), data=v, na.action=na.omit,family=binomial,control=controlglmer))
t2<- (glmer(cbind(v$nb0, v$long.dia - v$nb0)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + moy_pond_trouee_Trap + moy_pond_arbre_Trap + moy_pond_gauli_Trap + moy_pond_fleur_Trap + gen.mst + nbviscompTrap +nbabreuvTrap+ sexe1:julien+pluie:temperature+(1|bague), data=v, na.action=na.omit,family=binomial,control=controlglmer))
t3<- (glmer(cbind(v$nb0, v$long.dia - v$nb0)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + gen.mst + nbviscompTrap + sexe1:julien+nbabreuvTrap+pluie:temperature+(1|bague), data=v, na.action=na.omit,family=binomial,control=controlglmer))
t4<- (glmer(cbind(v$nb0, v$long.dia - v$nb0)~  sexe1 + masse + parasites+ annee1 + julien + (1|bague) , data=v, na.action=na.omit, family=binomial,control=controlglmer))
## avec variables abiotiques   Pas d interactions possible entre pluie et annee : false convergence
t5<- (glmer(cbind(v$nb0, v$long.dia - v$nb0) ~  temperature+ pluie+ pluie:temperature +  julien+  annee1 +(1|bague),data=v, na.action=na.omit, family=binomial,control=controlglmer))
## avec variables du paysages
t6<- (glmer(cbind(v$nb0, v$long.dia - v$nb0) ~  moy_pond_trouee_Trap + moy_pond_arbre_Trap + moy_pond_gauli_Trap + moy_pond_fleur_Trap +annee1+ julien+(1|bague), data=v, na.action=na.omit, family=binomial,control=controlglmer))
## avec variables rattachees aux mouvements et traplines  (ne converge pas avec gen.mst)
t7<- (glmer(cbind(v$nb0, v$long.dia - v$nb0) ~  nbvisdpersoMAX+ nbabreuvTrap+annee1+ julien+(1|bague),data=v, na.action=na.omit, family=binomial,control=controlglmer))
## avec variables reliees aux a la presence des competiteurs # pas d interaction avec annee ou julien possible false convergence
t8<- (glmer(cbind(v$nb0, v$long.dia - v$nb0) ~  nbviscompTrap+ annee1 + julien +(1|bague),data=v, na.action=na.omit, family=binomial,control=controlglmer))
t9<- (glmer(cbind(v$nb0, v$long.dia - v$nb0)~ annee1+ julien+ (1|bague),data=v, na.action=na.omit,family=binomial,control=controlglmer))

##  Theo avec REML=T pour le model avraging ...
t1<- (glmer(cbind(v$nb0, v$long.dia - v$nb0)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + nbabreuvTrap + moy_pond_trouee_Trap + moy_pond_arbre_Trap + moy_pond_gauli_Trap + moy_pond_fleur_Trap + gen.mst + nbviscompTrap + sexe1:moy_pond_trouee_Trap + sexe1:moy_pond_gauli_Trap + sexe1:moy_pond_arbre_Trap+ sexe1:moy_pond_fleur_Trap+ sexe1:julien+pluie:temperature+(1|bague),data=v, na.action=na.omit,family=binomial,control=controlglmer))
t2<- (glmer(cbind(v$nb0, v$long.dia - v$nb0)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + moy_pond_trouee_Trap + moy_pond_arbre_Trap + moy_pond_gauli_Trap + moy_pond_fleur_Trap + gen.mst + nbviscompTrap +nbabreuvTrap+ sexe1:julien+pluie:temperature+(1|bague), data=v, na.action=na.omit,family=binomial,control=controlglmer))
t3<- (glmer(cbind(v$nb0, v$long.dia - v$nb0)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + gen.mst + nbviscompTrap + sexe1:julien+nbabreuvTrap+pluie:temperature+(1|bague),data=v, na.action=na.omit,family=binomial,control=controlglmer))
t4<- (glmer(cbind(v$nb0, v$long.dia - v$nb0)~  sexe1 + masse + parasites+ annee1 + julien + (1|bague) , data=v,na.action=na.omit, family=binomial,control=controlglmer))
## avec variables abiotiques   Pas d interactions possible entre pluie et annee : false convergence
t5<- (glmer(cbind(v$nb0, v$long.dia - v$nb0) ~  temperature+ pluie+ pluie:temperature +  julien+  annee1 +(1|bague),data=v, na.action=na.omit, family=binomial,control=controlglmer))
## avec variables du paysages
t6<- (glmer(cbind(v$nb0, v$long.dia - v$nb0) ~  moy_pond_trouee_Trap + moy_pond_arbre_Trap + moy_pond_gauli_Trap + moy_pond_fleur_Trap +annee1+ julien+(1|bague),data=v, na.action=na.omit, family=binomial,control=controlglmer))
## avec variables rattachees aux mouvements et traplines  (ne converge pas avec gen.mst)
t7<- (glmer(cbind(v$nb0, v$long.dia - v$nb0) ~  nbvisdpersoMAX+ nbabreuvTrap+annee1+ julien+(1|bague),data=v, na.action=na.omit, family=binomial,control=controlglmer))
## avec variables reliees aux a la presence des competiteurs # pas d interaction avec annee ou julien possible false convergence
t8<- (glmer(cbind(v$nb0, v$long.dia - v$nb0) ~  nbviscompTrap+ annee1 + julien +(1|bague),data=v, na.action=na.omit, family=binomial,control=controlglmer))
t9<- (glmer(cbind(v$nb0, v$long.dia - v$nb0)~ annee1+ julien+ (1|bague),data=v, na.action=na.omit,family=binomial,control=controlglmer))


# Pour sortir des tableaux d AIC
# Il faut cr??r une liste appel?e "model" avec tout les mod?les
#! ex: model<-c(model1,model2,model3,...)


modelt<-c(t1=t1,t2=t2,t3=t3,t4=t4,t5=t5,t6=t6,t7=t7,t8=t8,t9=t9)
maST<-aictab(modelt)
ma<-model.avg(modelt)
co<-t(ma$coefficients)
ci<-confint(ma,full=TRUE)
paramST<-cbind(co,ci)



### master data.frame
newdat<-with(v,data.frame(
  sexe1=4,
  masse=mean(masse),
	 parasites=mean(parasites),
  annee1="2008",
  julien=mean(julien),
  nbvisdpersoMAX=mean(nbvisdpersoMAX),
  temperature=mean(temperature),
  pluie=mean(pluie),
  nbabreuvTrap=mean(nbabreuvTrap),
  moy_pond_trouee_Trap=mean(moy_pond_trouee_Trap),
  moy_pond_arbre_Trap=mean(moy_pond_arbre_Trap),
  moy_pond_gauli_Trap=mean(moy_pond_gauli_Trap),
  moy_pond_fleur_Trap=mean(moy_pond_fleur_Trap),
  nbviscompTrap=mean(nbviscompTrap),
  gen.mst=mean(gen.mst)
))

################################################
### ST pred
################################################
colm<-gray(0,0.50)
colf<-gray(0,0.20)
colp<-gray(0,0.15)

val<-v$nb0/v$long.dia

png("C:/Users/rouf1703/Documents/UdeS/Consultation/MBelisle/Doc/YanickPaper/Fig_extra_scaled.png",width=6.5,height=4,units="in",res=500,pointsize=5)
par(mfrow=c(1,2),mar=c(5,6,2,2)) # pour mon seminaire 2
par(mfrow=c(2,3))

va<-c("moy_pond_arbre_Trap","moy_pond_trouee_Trap","moy_pond_fleur_Trap","nbvisdpersoMAX","nbabreuvTrap","gen.mst")
valab<-c("Tree density","Forest gap","Flower density (nb/m\U00B2)","Spatial concentration","Number of feeders","MST (m)")

for(i in seq_along(va)){
  if(any(grep("abreuv",va[i]))){
    x<-seq(min(v[,va[i]]),max(v[,va[i]]),by=1)
  }else{
    x<-seq(min(v[,va[i]]),max(v[,va[i]]),length.out=5)
  }
  plot(x,seq_along(x),ylim=c(0,1), ylab="ST",xlab=valab[i],cex.lab=1.8,cex.axis=2,type="n")
  points(v[,va[i]],val,col=colp)
  legend("topright",paste0(letters[i],")"),lty=c(1,3),bty="n", col=F, cex=2)
  if(i==1){legend("topleft",c("Male","Female"),fill=c(colm,colf),border=NA, bty="n",col=T, cex=2)}
  ### modavgpred
  newdat2<-newdat[rep(1,length(x)),]
  newdat2[,va[i]]<-x
  newdat2[,"sexe1"]<-as.factor("4")
  p<-as.data.frame(modavgPred(modelt,newdat=newdat2,type="response")$matrix.output)
  polygon(c(x,rev(x)),c(p$lower,rev(p$upper)),border=NA,col=colm)
  newdat2[,"sexe1"]<-as.factor("5")
  p<-as.data.frame(modavgPred(modelt,newdat=newdat2,type="response")$matrix.output)
  polygon(c(x,rev(x)),c(p$lower,rev(p$upper)),border=NA,col=colf)
}

dev.off()



#################################################
### FT pred
#################################################

library(lme4)
v<-read.table("C:/Users/rouf1703/Documents/UdeS/Consultation/MBelisle/Doc/YanickPaper/dbfinal_1mars2010.txt", header=T, stringsAsFactors=F)
sexe1<-as.factor(v$sexe)
annee1<-as.factor(v$annee)
v<-cbind(v,sexe1,annee1)

# scaled version of numeric variables
v2<-v
v2[]<-lapply(v,function(i){if(is.numeric(i)){scale(i)}else{i}})


g1<- (glmer(cbind(v$nbvisINtrap,v$nbvispersotot - v$nbvisINtrap)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + nbabreuvGlob + moy_pond_trouee_Global + moy_pond_arbre_Global + moy_pond_gauli_Global + moy_pond_fleur_Global + gen.mst + nbviscompGlob + sexe1:moy_pond_trouee_Global + sexe1:moy_pond_gauli_Global + sexe1:moy_pond_arbre_Global+ sexe1:moy_pond_fleur_Global+ sexe1:julien+ pluie:temperature+ (1|bague),data=v, na.action=na.omit,family=binomial,control=controlglmer))
g2<- (glmer(cbind(v$nbvisINtrap,v$nbvispersotot - v$nbvisINtrap)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + nbabreuvGlob + moy_pond_trouee_Global + moy_pond_arbre_Global + moy_pond_gauli_Global + moy_pond_fleur_Global + gen.mst + nbviscompGlob+ sexe1:julien + pluie:temperature+(1|bague),data=v, na.action=na.omit,family=binomial,control=controlglmer))
g3<- (glmer(cbind(v$nbvisINtrap,v$nbvispersotot - v$nbvisINtrap)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + gen.mst + nbviscompGlob + nbabreuvGlob+ sexe1:julien+pluie:temperature+(1|bague),data=v, na.action=na.omit,family=binomial,control=controlglmer))
g4<- (glmer(cbind(v$nbvisINtrap,v$nbvispersotot - v$nbvisINtrap)~  sexe1 + masse + parasites+ annee1 + julien+ sexe1:julien + (1|bague) , data=v,na.action=na.omit, family=binomial,control=controlglmer))
## avec variables abiotiques   Pas d interactions possible entre pluie et annee : false convergence
g5<- (glmer(cbind(v$nbvisINtrap,v$nbvispersotot - v$nbvisINtrap) ~  temperature+ pluie+  pluie:temperature+  julien+  annee1 +(1|bague),data=v, na.action=na.omit, family=binomial,control=controlglmer))
## avec variables du paysages
g6<- (glmer(cbind(v$nbvisINtrap,v$nbvispersotot - v$nbvisINtrap) ~  moy_pond_trouee_Global + moy_pond_arbre_Global + moy_pond_gauli_Global + moy_pond_fleur_Global +annee1+ julien+(1|bague),data=v, na.action=na.omit, family=binomial,control=controlglmer))
## avec variables rattachees aux mouvements et traplines
g7<- (glmer(cbind(v$nbvisINtrap,v$nbvispersotot - v$nbvisINtrap) ~  gen.mst+ nbvisdpersoMAX+ nbabreuvGlob + annee1+ julien+ (1|bague),data=v, na.action=na.omit, family=binomial,control=controlglmer))
## avec variables reliees aux a la presence des competiteurs # pas d interaction avec annee ou julien possible false convergence
g8<- (glmer(cbind(v$nbvisINtrap,v$nbvispersotot - v$nbvisINtrap) ~  nbviscompGlob+ annee1 + julien +(1|bague),data=v, na.action=na.omit, family=binomial,control=controlglmer))
g9<- (glmer(cbind(v$nbvisINtrap,v$nbvispersotot - v$nbvisINtrap)~ annee1+ julien+ (1|bague),data=v, na.action=na.omit,family=binomial,control=controlglmer))

# avec REML =T pour avoir les bons coef
gg<- (glmer(cbind(v$nbvisINtrap,v$nbvispersotot - v$nbvisINtrap)~ sexe1 + masse+ parasites+ annee1+ julien+ nbvisdpersoMAX+ temperature + pluie + nbabreuvGlob + moy_pond_trouee_Global + moy_pond_arbre_Global + moy_pond_gauli_Global + moy_pond_fleur_Global + gen.mst + nbviscompGlob + sexe1:moy_pond_trouee_Global + sexe1:moy_pond_gauli_Global + sexe1:moy_pond_arbre_Global+ sexe1:moy_pond_fleur_Global+ sexe1:julien+ pluie:temperature+ (1|bague),data=v,na.action=na.omit,family=binomial,control=controlglmer))

modelg<-c(g1=g1,g2=g2,g3=g3,g4=g4,g5=g5,g6=g6,g7=g7,g8=g8,g9=g9)
maFT<-aictab(modelg)
ma<-model.avg(modelg)
co<-t(ma$coefficients)
ci<-confint(ma,full=TRUE)
paramFT<-cbind(co,ci)


### master data.frame
newdat<-with(v,data.frame(
  sexe1=4,
  masse=mean(masse),
  parasites=mean(parasites),
  annee1="2008",
  julien=mean(julien),
  nbvisdpersoMAX=mean(nbvisdpersoMAX),
  temperature=mean(temperature),
  pluie=mean(pluie),
  nbabreuvGlob=mean(nbabreuvGlob),
  moy_pond_trouee_Global=mean(moy_pond_trouee_Global),
  moy_pond_arbre_Global=mean(moy_pond_arbre_Global),
  moy_pond_gauli_Global=mean(moy_pond_gauli_Global),
  moy_pond_fleur_Global=mean(moy_pond_fleur_Global),
  nbviscompGlob=mean(nbviscompGlob),
  gen.mst=mean(gen.mst)
))


###### Figures illustrant l effet des covariables sur l indice global

colm<-gray(0,0.50)
colf<-gray(0,0.20)
colp<-gray(0,0.15)

val<-v$nbvisINtrap/v$nbvispersotot


png("C:/Users/rouf1703/Documents/UdeS/Consultation/MBelisle/Doc/YanickPaper/Fig4_scaled.png",width=6.5,height=4,units="in",res=500,pointsize=5)
par(mfrow=c(1,2),mar=c(5,6,2,2)) # pour mon seminaire 2
par(mfrow=c(2,3))

va<-c("moy_pond_arbre_Global","moy_pond_trouee_Global","moy_pond_fleur_Global","nbvisdpersoMAX","nbabreuvGlob","gen.mst")
valab<-c("Tree density","Forest gap","Flower density (nb/m\U00B2)","Spatial concentration","Number of feeders","MST (m)")

for(i in seq_along(va)){
  if(any(grep("abreuv",va[i]))){
    x<-seq(min(v[,va[i]]),max(v[,va[i]]),by=1)
  }else{
    x<-seq(min(v[,va[i]]),max(v[,va[i]]),length.out=5)
  }
  plot(x,seq_along(x),ylim=c(0,1), ylab="FT",xlab=valab[i],cex.lab=1.8,cex.axis=2,type="n")
  points(v[,va[i]],val,col=colp)
  legend("topright",paste0(letters[i],")"),lty=c(1,3),bty="n", col=F, cex=2)
  if(i==1){legend("topleft",c("Male","Female"),fill=c(colm,colf),border=NA, bty="n",col=T, cex=2)}
  ### modavgpred
  newdat2<-newdat[rep(1,length(x)),]
  newdat2[,va[i]]<-x
  newdat2[,"sexe1"]<-as.factor("4")
  p<-as.data.frame(modavgPred(modelg,newdat=newdat2,type="response")$matrix.output)
  polygon(c(x,rev(x)),c(p$lower,rev(p$upper)),border=NA,col=colm)
  newdat2[,"sexe1"]<-as.factor("5")
  p<-as.data.frame(modavgPred(modelg,newdat=newdat2,type="response")$matrix.output)
  polygon(c(x,rev(x)),c(p$lower,rev(p$upper)),border=NA,col=colf)
}

dev.off()

#####################################
### tables productions
#####################################

### function for viewing as html table to copy paste in word
print.htmlTable<- function(x, useViewer = TRUE, ...){
  # Don't use viewer if in knitr
  if (useViewer &&
      !"package:knitr" %in% search()){
    
    htmlFile <- tempfile(fileext=".html")
    htmlPage <- paste("<html>",
                      "<head>",
                      "<meta http-equiv=\"Content-type\" content=\"text/html;charset=UTF-8\">",
                      "</head>",
                      "<body>",
                      "<div style=\"margin: 0 auto; display: table; margin-top: 1em;\">",
                      x,
                      "</div>",
                      "</body>",
                      "</html>", sep="\n")
    cat(htmlPage, file=htmlFile)
    
    viewer <- getOption("viewer")
    if (!is.null(viewer) &&
        is.function(viewer)){
      # (code to write some content to the file)
      viewer(htmlFile)
    }else{
      utils::browseURL(htmlFile)
    }
  }else{
    cat(x)
  }
}

fix_formula<-function(x){
  strsplit(as.character(lme4:::nobars(formula(x)))," ~ ")[[3]]  
}

dec<-function(i,col=1:ncol(i),...){
  for(j in col){
    i[j]<-format(round(i[j],5),...)  
  }
  i
}

options(scipen=20)
STFT<-as.data.frame(cbind(paramST,paramFT))
#STFT<-apply(STFT,2,format,digits=1,nsmall=1,width=2,trim=FALSE)
print.htmlTable(htmlTable(dec(STFT,digits=1,nsmall=2,width=2)))

SD<-apply(paramSD,2,format,digits=1,nsmall=1,width=2,trim=FALSE)
print.htmlTable(htmlTable(SD))

tabST<-cbind(as.data.frame(maST)[,c(1,2,4,6)],model=sapply(as.integer(row.names(maST)),function(i){fix_formula(modeli[[i]])}))[,c(1,5,2,3,4)]
tabFT<-cbind(as.data.frame(maFT)[,c(1,2,4,6)],model=sapply(as.integer(row.names(maFT)),function(i){fix_formula(modeli[[i]])}))[,c(1,5,2,3,4)]
tabSD<-cbind(as.data.frame(maSD)[,c(1,2,4,6)],model=sapply(as.integer(row.names(maSD)),function(i){fix_formula(modeli[[i]])}))[,c(1,5,2,3,4)]

print.htmlTable(htmlTable(dec(tabST,col=4:5,digits=1,width=2,nsmall=2),rnames=FALSE))
print.htmlTable(htmlTable(dec(tabFT,col=4:5,digits=1,width=2,nsmall=2),rnames=FALSE))
print.htmlTable(htmlTable(dec(tabSD,col=4:5,digits=1,width=2,nsmall=2),rnames=FALSE))






