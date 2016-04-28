### criando meu namespace

pos <- which(grepl(".ocmisc",search()))
if(length(pos) >0) detach(pos=pos)
rm(pos)
.ocmisc_env <- new.env()



## EDA modifcada ha muito tempo!!! baseada na funcao eda do splus 3.4
## mod OGC 2012

.ocmisc_env[['eda']]  <- function(x,nome="")
{
    # require(ctest)
    if (!is.numeric(x) || !is.vector(x))  {
        stop("argumento nao eŽ vetor ou nao eŽ numerico ")
        
    }
    
    
    par(mfrow = c(2, 2))
    par(oma = c(1, 1, 2, 1))
    
    densi  <- density(x)
    xli  <- range(densi$x)
    yli  <- range(densi$y)
    
    if(nome =="") nome <- deparse(substitute(x))
    
    hist(x,col="red",probability = T,xlim = xli, ylim = yli,main=paste("Histograma de ",nome))
    lines(densi,lwd=2)
    
    
    boxplot(x,col="limegreen")
    points(1, mean(x), pch = 16,cex=1,col="blue")
    title("Boxplot")
    
    qqnorm(x, xlab="", sub="",main="qq-plot Normal")
    qqline(x,col="orange",lwd=2)
    
    plot(seq(1:50),seq(1:50),axes=F,type="n",ylab="",xlab="")
    
    # text(24,58,paste("N =",length(x)," Observacoes"))
    lugarnomes <- list(x=c(24,24,24,24,24,24),y=c(50,42,34,26,18,10))
    text(lugarnomes,c("Minimo =","1o Quartil =","Mediana =","Media =","3o Quartil =","Maximo ="),adj=1) 
    lugx=c(25,25,25,25,25,25)
    lugy=c(50,42,34,26,18,10)
    text(lugx,lugy,summary(x),adj=0) 
    if (length(x) >= 3 & length(x) <= 5000) {
        s1 <- shapiro.test(x)
        text(18,2,"Shapiro test ",adj=1 )
        text(18,2, paste("w=",round(s1$statistic,4),"p=",round(s1$p.value,4)),adj=0)
    }
    else { 
        text (6,2, "* Teste Shapiro nao efetuado",adj=0)
    }
    title(paste("Sumario ",length(x)," Observacoes"))
    
}


#### Modificada dos pacote psych
### out 2010 OGC

.ocmisc_env[['panel.lm']] <-
    function (x, y, col = par("col"), bg = NA, pch = par("pch"),
              cex = 1, col.lm = "red",  ...)
    {   ymin <- min(y)
    ymax <- max(y)
    xmin <- min(x)
    xmax <- max(x)
    ylim <- c(min(ymin,xmin),max(ymax,xmax))
    xlim <- ylim
    points(x, y, pch = pch, col = par("col"), bg = bg, cex = cex,ylim = ylim, xlim= xlim)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        abline(lm(y[ok]~ x[ok]),
               col = col.lm, ...)
    }

.ocmisc_env[['panel.lm.ellipse']] <-
    function (x, y, col = par("col"), bg = NA, pch = par("pch"),
              cex = 1, col.lm = "red",  ...)
    {   ymin <- min(y)
    ymax <- max(y)
    xmin <- min(x)
    xmax <- max(x)
    ylim <- c(min(ymin,xmin),max(ymax,xmax))
    xlim <- ylim
    points(x, y, pch = pch, col = par("col"), bg = bg, cex = cex,ylim = ylim, xlim= xlim)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        abline(lm(y[ok]~ x[ok]),
               col = col.lm, ...)
    xm <- mean(x,na.rm=TRUE)
    ym <- mean(y,na.rm=TRUE)
    xs <- sd(x,na.rm=TRUE)
    ys <- sd(y,na.rm=TRUE)
    r = cor(x, y,use="pairwise")
    p.ellipse(xm,ym,xs,ys,r,col=col.lm,...)
    
    }

.ocmisc_env[['panel.hist.density']] <-
    function(x, ...)
    {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks; nB <- length(breaks)
        y <- h$counts; y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
        d <- density(x,na.rm=TRUE,bw="nrd",adjust=1.2)
        d$y <- d$y/max(d$y)
        lines(d)
    }

.ocmisc_env[['panel.hist']] <-
    function(x, ...)
    {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5) )
        h <- hist(x, plot = FALSE)
        breaks <- h$breaks; nB <- length(breaks)
        y <- h$counts; y <- y/max(y)
        rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
    }


.ocmisc_env[['panel.cor']] <-
    function(x, y, digits=2, prefix="", cex.cor)
    {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r = (cor(x, y,use="pairwise"))
        txt <- format(c(round(r,digits), 0.123456789), digits=digits)[1]
        txt <- paste(prefix, txt, sep="")
        if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
        text(0.5, 0.5, txt, cex = cex )
    }

#baseda na funcao de eilse do John Fox
.ocmisc_env[['p.ellipse']] <- function(x=0,y=0,xs=1,ys=1,r=0,add=TRUE,segments=51, col = par("col"),col.pt = par("col"),...) {
    
    angles <- (0:segments) * 2 * pi/segments
    unit.circle <- cbind(cos(angles), sin(angles))
    # shape <- diag(c(1+r,1-r)) %*% matrix(c(r,sqrt(1-r^2),-sqrt(1-r^2),r),ncol=2,byrow=TRUE)
    if (abs(r)>0 )theta <- sign(r)/sqrt(2) else theta=1/sqrt(2)
    
    shape <- diag(c(sqrt(1+r),sqrt(1-r))) %*% matrix(c(theta,theta,-theta,theta),ncol=2,byrow=TRUE)
    ellipse <- unit.circle %*% shape
    ellipse[,1] <- ellipse[,1]*xs + x
    ellipse[,2] <- ellipse[,2]*ys + y
    
    points(x,y,pch=19,cex=1.5,col=col.pt )
    lines(ellipse,col=col, ...)
}


.ocmisc_env[['panel.ellipse']] <-
    function (x, y, col = par("col"), bg = NA, pch = par("pch"),
              cex = 1, col.smooth = "red",col.pt = par("col"), ...)
    {xm <- mean(x,na.rm=TRUE)
    ym <- mean(y,na.rm=TRUE)
    xs <- sd(x,na.rm=TRUE)
    ys <- sd(y,na.rm=TRUE)
    r = (cor(x, y,use="pairwise"))
    points(x, y, pch = pch, col = col.pt, bg = bg, cex = cex)
    p.ellipse(xm,ym,xs,ys,r,col=col.smooth,...)
    }


.ocmisc_env[['panel.smoothie']] <-
    function (x, y, col = par("col"), bg = NA, pch = par("pch"),
              cex = 1, col.smooth = "red", span = 2/3, iter = 3,col.pt = par("col"),...)
    {
        
        xm <- mean(x,na.rm=TRUE)
        ym <- mean(y,na.rm=TRUE)
        xs <- sd(x,na.rm=TRUE)
        ys <- sd(y,na.rm=TRUE)
        r = cor(x, y,use="pairwise")
        
        points(x, y, pch = pch, col = col.pt , bg = bg, cex = cex)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok))
            lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
                  col = col.smooth, ...)
        
        p.ellipse(xm,ym,xs,ys,r,col=col.smooth,...)
    }

.ocmisc_env[['panel.jiggle']] <-
    function (x, y, col = cor, bg = NA, pch = par("pch"),
              cex = 1, col.smooth = "red", span = 2/3, iter = 3,col.pt = par("col"), ...)
    {
        
        xm <- mean(x,na.rm=TRUE)
        ym <- mean(y,na.rm=TRUE)
        xs <- sd(x,na.rm=TRUE)
        ys <- sd(y,na.rm=TRUE)
        r = cor(x, y,use="pairwise")
        x <- jitter(x,factor=2)
        y <- jitter(y,factor=2)
        points(x, y, pch = pch, col = col.pt, bg = bg, cex = cex)
        ok <- is.finite(x) & is.finite(y)
        if (any(ok))
            lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
                  col = col.smooth, ...)
        
        p.ellipse(xm,ym,xs,ys,r,col=col.smooth,...)
    }

.ocmisc_env[['panel.cor.scale']] <-
    function(x, y, digits=2, prefix="", cex.cor)
    {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        r = (cor(x, y,use="pairwise"))
        txt <- format(c(r, 0.123456789), digits=digits)[1]
        txt <- paste(prefix, txt, sep="")
        if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
        cex1 <- cex  * abs(r)
        if(cex1 < .25) cex1 <- .25 #otherwise they just vanish
        text(0.5, 0.5, txt, cex = cex1)
    }





.ocmisc_env[['eda.pairs']] <- function(x) pairs(x,upper.panel=panel.cor.scale,diag.panel=panel.hist.density,lower.panel=panel.smooth)



### eda heat

.ocmisc_env[['eda.cor']] <- function(x,metodo="complete") {
    if(length(dev.list()) >0) dev.off()
    split.screen(c(1,2)) 
    screen(1)
    .hm(cor(x),numbers=T,main="Correlacao")
    screen(2)
    plot(hclust(dist(cor(x)),method=metodo),sub="",ylab="",ann=F)
    title("Dendograma variaveis",cex.main=0.8,sub="")
    
}


## adaptada da funcao plot.cor pacote psych

.ocmisc_env[['.hm']]<- function (r, numbers = FALSE, colors = TRUE, n = 51, main = NULL, 
                                 zlim = c(-1, 1), show.legend = FALSE, labels = NULL, n.legend = 10, 
                                 keep.par = TRUE, ...) 
{
    if (keep.par) 
        op <- par(no.readonly = TRUE)
    if (is.null(main)) {
        main <- ""
    }
    if (!is.matrix(r) & (!is.data.frame(r))) {
        if ((length(class(r)) > 1) & (class(r)[1] == "psych")) {
            if (class(r)[2] == "omega") {
                r <- r$schmid$sl
                nff <- ncol(r)
                r <- r[, 1:(nff - 2)]
            }
            else {
                r <- r$loadings
            }
        }
    }
    r <- as.matrix(r)
    if (min(dim(r)) < 2) {
        stop("minimo matriz 2 x 2")
    }
    r <- t(r)
    if (is.null(n)) {
        n <- dim(r)[2]
    }
    nf <- dim(r)[2]
    nvar <- dim(r)[1]
    if (is.null(labels)) {
        if (is.null(rownames(r))) 
            rownames(r) <- paste("V", 1:nvar)
        if (is.null(colnames(r))) 
            colnames(r) <- paste("V", 1:nf)
    }
    else {
        rownames(r) <- colnames(r) <- labels
    }
    max.len <- max(nchar(rownames(r)))/6
    if (is.null(zlim)) {
        zlim <- range(r)
    }
    if (colors) {
        gr <- colorRampPalette(c("blue", "white", "red"))
        colramp <- gr(n)
    }
    else {
        colramp <- grey((n:0)/n)
    }
    ord1 <- seq(nvar, 1, -1)
    if (nvar != nf) {
        r <- t(r)
    }
    r <- r[, ord1]
    MAR <- 5
    par(mar = c(MAR + max.len, MAR + max.len, 4, 0.5))
    if (show.legend) {
        layout(matrix(c(1, 2), nrow = 1), widths = c(0.9, 0.1), 
               heights = c(1, 1))
    }
    image(r, col = colramp, axes = FALSE, main = main, zlim = zlim,cex.main=0.8)
    box()
    at1 <- (0:(nf - 1))/(nf - 1)
    at2 <- (0:(nvar - 1))/(nvar - 1)
    if (max.len > 0.5) {
        axis(2, at = at2, labels = colnames(r), las = 1, ...)
        axis(1, at = at1, labels = rownames(r), las = 2, ...)
    }
    else {
        axis(2, at = at2, labels = colnames(r), ...)
        axis(1, at = at1, labels = rownames(r), las = 1, ...)
    }
    at1 <- (0:(nf - 1))/(nf - 1)
    if (numbers) {
        rx <- rep(at1, ncol(r))
        ry <- rep(at2, each = ncol(r))
        text(rx, ry, round(r,2))
    }
    if (show.legend) {
        leg <- matrix(seq(from = zlim[1], to = zlim[2], by = (zlim[2] - 
                                                                  zlim[1])/n), nrow = 1)
        par(mar = c(MAR, 0, 4, 3))
        image(leg, col = colramp, axes = FALSE, zlim = zlim)
        at2 <- seq(0, 1, 1/n.legend)
        labels = seq(zlim[1], zlim[2], (zlim[2] - zlim[1])/(length(at2) - 
                                                                1))
        axis(4, at = at2, labels = labels, las = 2, ...)
    }
    if (keep.par) 
        par(op)
}


## funcoes genericas
#adaptada de jonh fox

.ocmisc_env[['pacote']] <- function(pkg, dep=TRUE,lib=Sys.getenv("R_LIBS_USER"), ...){
    
    pkg <- as.character(substitute(pkg))
    if (!(pkg %in% .packages(all.available=TRUE)))
        install.packages(pkg, dependencie=dep,lib=lib,
                         repos="http://cran.fiocruz.br",
        )
    library(pkg, character.only=TRUE, ...)     
}



# modificada 20/10/2011 OGC
.ocmisc_env[['ls.obj']] <- function (pos = 1, pattern, mode = "any", type = "any",sort.size=FALSE,kb=TRUE){
    Object.Name <- ls(pos = pos, envir = as.environment(pos), pattern = pattern)
    if(length(Object.Name) == 0 ) stop("Nao ha nenhum objeto a ser listado") 
    Object.Mode <- rep("",length(Object.Name))
    Object.Type <- rep("",length(Object.Name))
    Variables <- rep("-",length(Object.Name))
    Observations <- rep("-",length(Object.Name))
    for (i in 1:length(Object.Name)){
        Object.Mode[[i]] <- mode(get(Object.Name[[i]]))
        if(is.list(get(Object.Name[[i]]))){
            if(is.null(class(get(Object.Name[[i]]))))
                Object.Type[[i]] <- c("unknown")
            else {
                
                Object.Attrib <- attributes(get(Object.Name[[i]]))
                
                #cat (paste("Nome=",Object.Name[[i]],"Classe=",Object.Attrib$class,
                #          "tipo=",is.null(Object.Attrib$class), "int=",i,"\n"))
                
                if(is.null(Object.Attrib$class))
                    Object.Type[[i]] <- c("unknown")
                else
                    Object.Type[[i]] <- strtrim(paste(Object.Attrib$class,collapse="|"),14)
                
                
                if(Object.Type[[i]]=="data.frame"){
                    Variables[[i]] <- as.character(length(Object.Attrib$names))
                    Observations[[i]] <- as.character(length(Object.Attrib$row.names))
                }
            }
        }
        if(is.matrix(get(Object.Name[[i]]))){
            Object.Attrib <- dim(get(Object.Name[[i]]))
            Object.Type[[i]] <- c("matrix")
            Variables[[i]] <- as.character(Object.Attrib[2])
            Observations[[i]] <- as.character(Object.Attrib[1])
        }
        if(is.vector(get(Object.Name[[i]])) && (Object.Mode[[i]]=="character" ||
                                                Object.Mode[[i]]=="numeric")){
            Object.Type[[i]] <- c("vector")
            Variables[[i]] <- c("1")
            Observations[[i]] <- as.character(length(get(Object.Name[[i]])))
        }
        if(is.factor(get(Object.Name[[i]]))){
            Object.Type[[i]] <- c("factor")
            Variables[[i]] <- c("1")
            Observations[[i]] <- as.character(length(get(Object.Name[[i]])))
        }
        
        if(is.ts(get(Object.Name[[i]]))){
            Object.Type[[i]] <- c("ts")
            Variables[[i]] <- c("1")
            Observations[[i]] <- as.character(length(get(Object.Name[[i]])))
        }
        
        if(is.function(get(Object.Name[[i]]))) Object.Type[[i]] <- c("function")
        
        # test the class for NULL Types
        if( is.null(Object.Type[[i]])  || Object.Type[[i]] == "")  
            Object.Type[[i]] <- strtrim(paste(class(get(Object.Name[[i]])),collapse="|"),14)
        
        # para debug
        # paste(cat ("valor de i ",i,"\n"))
    }
    
    
    Object.Size <- sapply(Object.Name, function(x) object.size(get(x)))
    
    names(Object.Size) <- NULL
    objList <-
        data.frame(Object.Name,Object.Mode,Object.Type,Object.Size,Observations,Variables)
    if(mode != "any") objList <- objList[objList[["Object.Mode"]] == mode,]
    if(type != "any") objList <- objList[objList[["Object.Type"]] == type,]
    
    if (kb) objList$Object.Size <-  round(objList$Object.Size/1024,2)
    if ( sort.size) return(objList[order(objList$Object.Size),])  
    else
        return(objList)
}

.ocmisc_env[['eda.cor2']] <- function(obj) {
    pacote("reshape2")
    pacote("ggplot2")
    
    
    dat <- cor(obj)
    diag(dat) <- NA
    cor_dat <- melt(dat)
    cor_dat$Var1 <- factor(cor_dat$Var1, levels=colnames(obj))
    cor_dat$Var2 <- factor(cor_dat$Var2, levels=rev(colnames(obj)))
    cor_dat$pctile <- rank(cor_dat$value, na.last="keep")/sum(!is.na(cor_dat$value))
    
    ggplot(data =  cor_dat, aes(x = Var1, y = Var2)) +
        geom_tile(aes(fill = pctile), colour = "white") +
        geom_text(aes(label = sprintf("%1.2f",value)), vjust = 1) +
        scale_fill_gradientn(colours=c("blue","blue","white","red","red"),
                             values=c(0,0.05,0.5,0.95,1),
                             guide = "none", na.value = "white") +
        coord_equal() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.background = element_blank())
    
}


## soundexBR fonte lista do R 
.ocmisc_env[['soundexBR']]  <- function(termo) {
    
    termo <- toupper(termo)
    # 1. Retire toda pontuação da palavra
    termo <- gsub("[ÃÁÀÂÄ]","A",termo)
    termo <- gsub("[ÉÈÊ???Ë]","E",termo)
    termo <- gsub("[ÍÌÏÎI]","I",termo)
    termo <- gsub("[ÓÕÒÔÖ]","O",termo)
    termo <- gsub("[ÚÙÛÜU]","U",termo)
    termo <- gsub("Ç","C",termo)
    #1.a Substituir todas as letras duplas por uma única letra
    #http://www.archives.gov/genealogy/census/soundex.html
    
    termo <- gsub("([A-Z])\\1", "\\1", termo)
    # 2. Fique com a primeira letra;
    N <- nchar(termo)
    # Modificação PT-BR (Coeli e Camargo)
    termo <- ifelse(substr(termo,1,2)=="WA",sub("W","V",termo),termo)
    termo <- ifelse(substr(termo,1,1)=="H",substr(termo,2,N),termo)
    termo <- ifelse(substr(termo,1,2)=="KA"|substr(termo,1,2)=="KO"|substr(termo,1,2)=="KU",sub("K","C",termo),termo)
    termo <- ifelse(substr(termo,1,1)=="Y",sub("Y","I",termo),termo)
    termo <- ifelse(substr(termo,1,2)=="CE"|substr(termo,1,2)=="CI",sub("C","S",termo),termo)
    termo <- ifelse(substr(termo,1,2)=="GE"|substr(termo,1,2)=="GI",sub("G","J",termo),termo)      
    ############## Segunda Parte #######################   
    termo.1<-substr(termo,1,1)
    termo<-substr(termo,2,N)
    # 3. Mude todas as ocorrências das letras a seguir por '0' (zero): 'A','E','I','O','U','H','W','Y';
    termo<-gsub("[A,E,I,O,U,H,W,Y]",0,termo)
    # 4. Mude as letras restantes de acordo com a tabela abaixo: 
    # Número      Letra 
    # 1                   'B','F','P','V'
    termo<-gsub("[B,F,P,V]",1,termo)
    # 2                   'C','G','J','K', 'Q','S','X','Z'
    termo<-gsub("[C,G,J,K,Q,S,X,Z]",2,termo)
    # 3                   'D','T'
    termo<-gsub("[D,T]",3,termo) 
    # 4                   'L' 
    termo<-gsub("L",4,termo)
    # 5                   'M','N' 
    termo<-gsub("[M,N]",5,termo)
    # 6                   'R'
    termo<-gsub("R",6,termo)
    # 5. Remova todos os zeros da string resultante;
    termo<-gsub(0,"",termo)
    # Eliminar dois numeros iguais consecutivos por um único numero 
    #http://www.archives.gov/genealogy/census/soundex.html
    termo<-gsub("([0-9])\\1", "\\1", termo) 
    # Remontar 
    termo<-paste(termo.1,termo,sep="")
    
    # 6. Preencha a string resultante com zeros à direita e retorne desta forma:  
    #<letra maiuscula><digito><digito><digito>
    termo<-paste(termo,"0000",sep="")
    termo<-substr(termo,1,4)
    return(termo)
}

.ocmisc_env[['levenshtein']] <- function (string1, string2, case = TRUE, map = NULL) 
{
    if (!is.null(map)) {
        m <- matrix(map, ncol = 2, byrow = TRUE)
        s <- c(ifelse(case, string1, tolower(string1)), ifelse(case, 
                                                               string2, tolower(string2)))
        for (i in 1:dim(m)[1]) s <- gsub(m[i, 1], m[i, 2], s)
        string1 <- s[1]
        string2 <- s[2]
    }
    if (ifelse(case, string1, tolower(string1)) == ifelse(case, 
                                                          string2, tolower(string2))) 
        return(0)
    s1 <- strsplit(paste(" ", ifelse(case, string1, tolower(string1)), 
                         sep = ""), NULL)[[1]]
    s2 <- strsplit(paste(" ", ifelse(case, string2, tolower(string2)), 
                         sep = ""), NULL)[[1]]
    l1 <- length(s1)
    l2 <- length(s2)
    if (l1 == 1) 
        return(l2 - 1)
    if (l2 == 1) 
        return(l1 - 1)
    d <- matrix(nrow = l1, ncol = l2)
    for (i in 1:l1) d[i, 1] <- i - 1
    for (i in 1:l2) d[1, i] <- i - 1
    for (i in 2:l1) for (j in 2:l2) d[i, j] <- min((d[i - 1, 
                                                      j] + 1), (d[i, j - 1] + 1), (d[i - 1, j - 1] + ifelse(s1[i] == 
                                                                                                                s2[j], 0, 1)))
    d[l1, l2]
}


###
#
# FUNÇOES P/ CALCULO E GRAFICOS DE OR EM OBJETOS GLM,GLMM E LME
#
# POR : Oswaldo G. Cruz (oswaldo@fiocruz.br)
#       PROCC / FIOCRUZ
#
# Versao: 10-09-2006 rev Nov 2008
#
###


#####
#
# Exemplo:
#
#   >library(car)
#   >data(Mroz)
#   >modelo <- glm(lfp ~ ., family=binomial, data=Mroz)
#   >resp<-or(modelo)
#   >resp
#   >plot(resp)    # em vermelho int. risco >1 ;em azul < 1 ;em preto risco inclui 1
#   >plot(resp,filtro=-1) #remove o intercepto
#
#####





# funcao generica que declara a classe S3 p/ OR
.ocmisc_env[['or']] <- function (x,...) {
    UseMethod("or")
}


#
# funcao p/ calculo de OR familia glm
#
# Argumento ic = intervalo de confiança
#
.ocmisc_env[['or.glm']] <- function(x, ic=0.95)
{
    
    if (!all(class(x)==c("glm" ,"lm"))) stop("Essa função só pode ser usada em objetos das classes glm e lm ")
    nomes <- names(x$coefficients)
    x<-summary(x)
    coeficiente <- x$coefficients[,1]
    # nomes <- names(x$coefficients)
    erro <- x$coefficients[,2]
    odds <- exp(as.numeric(x$coefficients[,1]))
    # confint da um warning confing
    icinf <-exp(coeficiente - (erro*qnorm(1-((1-ic)/2))))
    icsup <-exp(coeficiente + (erro*qnorm(1-((1-ic)/2))))
    pvalor <-  x$coefficients[,4]
    
    resp <- list(modelo=x$call,familia=x$family,nome=nomes,coeficiente=coeficiente,Erro=erro,OR=odds,
                 ICSup=icsup,ICInf=icinf,Pvalor=pvalor)
    
    class(resp) <- "or"
    return(resp)
}

#
# funcao p/ calculo de OR familia  lme e glmmPQL
#
# Argumento ic = intervalo de confiança
#
.ocmisc_env[['or.lme']] <- function(x, ic=0.95)
{
    #Calcula o odds ratio e faz um grafico com os intervalos de confiança
    if (!all(class(x)==c("glmmPQL","lme")))stop("Essa função so pode ser usada em objetos da classe lme")
    x<-summary(x)
    nomes <-dimnames(x$tTable)[[1]]
    coeficiente <- x$tTable[,1]
    erro <- x$tTable[,2]
    odds <- exp(coeficiente)
    icinf <-exp(coeficiente-(erro*qnorm(1-((1-ic)/2))))
    icsup <-exp(coeficiente+(erro*qnorm(1-((1-ic)/2))))
    pvalor <-  x$tTable[,5]
    resp <- list(modelo=x$call,familia=x$family,nome=nomes,coeficiente=coeficiente,Erro=erro,OR=odds,
                 ICSup=icsup,ICInf=icinf,Pvalor=pvalor)
    
    class(resp) <- "or"
    return(resp)
    
}

.ocmisc_env[['or.glmmPQL']] <- function(x, ic=0.95)
{
    #Calcula o odds ratio e faz um grafico com os intervalos de confiança
    if (!all(class(x)==c("glmmPQL" ,"lme")))stop("Essa função so pode ser usada em objetos da classe lme")
    x<-summary(x)
    nomes <-dimnames(x$tTable)[[1]]
    coeficiente <- x$tTable[,1]
    erro <- x$tTable[,2]
    odds <- exp(coeficiente)
    icinf <-exp(coeficiente-(erro*qnorm(1-((1-ic)/2))))
    icsup <-exp(coeficiente+(erro*qnorm(1-((1-ic)/2))))
    pvalor <-  x$tTable[,5]
    resp <- list(modelo=x$call,familia=x$family,nome=nomes,coeficiente=coeficiente,Erro=erro,OR=odds,
                 ICSup=icsup,ICInf=icinf,Pvalor=pvalor)
    
    class(resp) <- "or"
    return(resp)
    
}


.ocmisc_env[['or.gam']] <- function(x, ic=0.95)
{
    
    if (!all(class(x)==c("gam" ,"glm","lm"))) stop("Essa função so pode ser usada em objetos das classes glm e lm e gam ")
    
    x<-summary(x)
    coeficiente <- x$p.table[,1]
    # nomes <- names(x$coefficients)
    nomes <- rownames(x$p.table)
    erro <- x$p.table[,2]
    odds <- exp(as.numeric(x$p.table[,1]))
    # confint da um warning confing
    icinf <-exp(coeficiente - (erro*qnorm(1-((1-ic)/2))))
    icsup <-exp(coeficiente + (erro*qnorm(1-((1-ic)/2))))
    pvalor <-  x$p.table[,4]
    
    resp <- list(modelo=x$call,familia=x$family,nome=nomes,coeficiente=coeficiente,Erro=erro,OR=odds,
                 ICSup=icsup,ICInf=icinf,Pvalor=pvalor)
    
    class(resp) <- "or"
    return(resp)
}

.ocmisc_env[['or.matrix']] <- function(x, alpha = 0.05){
    #
    #  Compute the odds ratio between two binary variables, x and y,
    #  as defined by the four numbers nij:
    #
    #    x[1,1] = number of cases where x = 0 and y = 0
    #    x[1,2] = number of cases where x = 0 and y = 1
    #    x[2,1] = number of cases where x = 1 and y = 0
    #    x[2,2] = number of cases where x = 1 and y = 1
    #
    
    if(ncol(x) > 2 | ncol(x) != nrow(x)) stop("Somente para matrizes e tabelas 2x2")
    
    OR <- (x[1,1] * x[2,2])/(x[1,2] * x[2,1])
    #
    #  Compute the Wald confidence intervals:
    #
    siglog <- sqrt((1/x[1,1]) + (1/x[1,2]) + (1/x[2,1]) + (1/x[2,2]))
    zalph <- qnorm(1 - alpha/2)
    logOR <- log(OR)
    loglo <- logOR - zalph * siglog
    loghi <- logOR + zalph * siglog
    #
    ORlo <- exp(loglo)
    ORhi <- exp(loghi)
    #
    oframe <- list(matriz=x,result=data.frame( OR = OR, LowerIC=ORlo , UpperCI = ORhi, alpha = alpha))
    oframe
}

.ocmisc_env[['or.table']] <- function(x, alpha = 0.05){
    
    or.matrix(as.matrix(x))
    
}


#
# funcao p/ imprimir objeto da classe OR
#
# Argumento digitos = numero de decimais na tabela
#

.ocmisc_env[['print.or']] <- function(x,digitos=4,...) {
    
    cat("\nModelo: \n", deparse(x$modelo), "\n")
    
    if (x$familia[[1]]=="binomial") cat("\nFamilia: ",x$familia[[1]],"\t Link: ",x$familia[[2]],"\n\n")
    else cat("\nFamilia: ",x$familia[[1]],"\n\nOBS:  OddsRatio em geral so e valido para familia Binomial \n\n")
    
    
    signf <- cut(x$Pvalor,br=c(0.00000,0.0001,0.01,0.05,0.1,1),lab=c("***","**","*","."," "))
    resp <- data.frame(Coeficiente=x$coeficiente,Erro=x$Erro,OR=x$OR,ICSup=x$ICSup,
                       ICInf=x$ICInf,Pvalor=x$Pvalor,Sig=signf,row.names=x$nome)
    
    print(resp, digits = digitos, print.gap = 2, quote = FALSE)
    
    cat("-- \nSig. :  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 \n\n")
    
}



#
# funcao p/ graficos dos OR e seus intervalos
#
# Argumentos
#            cor = usar cor, vermelho p/ risco, azul p/ protecao e preto
#                   valores que incluem 1
#
#             um = ajuste da escala se TRUE 1 sempre aparece no grafico
#
#             filtro = selecao de colunas.. use -1 para nao ver intercepto
#
#             linha = largura de linha

.ocmisc_env[['plot.or']] <- function (xls,cor=TRUE,um=TRUE,filtro=-1,linha=2,...) {
    cores <- 1
    x <- data.frame(OR=xls$OR,ICInf=xls$ICInf,ICSup=xls$ICSup,nome=xls$nome)
    if (all(as.logical(filtro))) {x <- x[filtro,] }
    n <- length(x$OR)
    
    if(um) ylim <- range(x$ICInf,1,x$ICSup)
    else ylim <- range(x$ICInf,x$ICSup)
    
    
    xlim <- c(0.5,max(n)*1.05)
    plot(x$OR, ylim=ylim, xlim=xlim, xlab = '', ylab = "OR", axes = F,type="n")
    box()
    axis(2)
    axis(1, at=1:n, labels = as.character(x$nome) , las = 3, cex.axis = .8)
    
    cores <- 1
    if (cor) {
        for (i in 1:n) {
            cores[i] <- ifelse(all(range(x$ICInf[i],x$ICSup[i]) >1),2,1)
            cores[i] <- ifelse(all(range(x$ICInf[i],x$ICSup[i]) <1),4,cores[i])
        }
    }
    points(x$OR,pch=19,lwd=linha+1,col=cores)
    arrows(1:n,x$ICInf,1:n,x$ICSup,angle=90,code=3,col=cores,len=0.2,lwd=linha)
    abline(h=1,lty=2,col="grey",lwd=linha)
    
}

.ocmisc_env[['plotOR']] <- function(x, title = NULL,ylab="OR",nomes=NULL,ordem="OR"){
    if(all(class(x)!= "or")) stop ("classe deve ser or")
    require(ggplot2)
    tmp<-data.frame(x$OR, x$ICInf,x$ICSup)
    odds<-tmp[-1,]
    names(odds) <- c("OR", "lower", "upper")
    
    if (is.null(nomes)) odds$vars<-ordered(row.names(odds)) else odds$vars<-ordered(nomes)
    if (ordem=='parm') odds$vars <- reorder(odds$vars,length(odds$vars):1)
    if (ordem=='OR') odds$vars <- reorder(odds$vars, odds$OR) 
    ticks<-c(seq(.1, 1, by =.1), seq(2, 10, by =1), seq(20, 100, by =10))
    
    ggplot(odds, aes(y= OR, x = vars)) +
        geom_point() +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
        scale_y_log10(breaks=ticks, labels = ticks) +
        geom_hline(yintercept = 1, linetype=2,col="red") +
        coord_flip() +
        labs(title = title, x = "Variables", y = ylab) + 
        theme_bw()
}


.ocmisc_env[['plotRR']] <- function(x, title = NULL,ylab="RR",nomes=NULL,ordem=""){
    if(all(class(x)!= "or")) stop ("classe deve ser or")
    require(ggplot2)
    tmp<-data.frame(x$OR, x$ICInf,x$ICSup)
    odds<-tmp[-1,]
    names(odds) <- c("OR", "lower", "upper")
    if (is.null(nomes)) odds$vars<-ordered(row.names(odds)) else odds$vars<-ordered(nomes)
    if (ordem=='parm') odds$vars <- reorder(odds$vars,length(odds$vars):1)
    if (ordem=='OR') odds$vars <- reorder(odds$vars, odds$OR) 
    ticks<-c(seq(.1, 1, by =.1), seq(2, 10, by =1), seq(20, 100, by =10))
    
    ggplot(odds, aes(y= OR, x = vars)) +
        geom_point() +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
        scale_y_log10(breaks=ticks, labels = ticks) +
        geom_hline(yintercept = 1, linetype=2,col="red") +
        coord_flip() +
        labs(title = title, x = "Variables", y = ylab) + 
        theme_bw()
}

### Semana Epidemiologica

.ocmisc_env[['episem']] <- function(x,separa='W') {
    
    # semana epi 1 de 2000 02/01/2000
    
    if (class(x)!= "Date") {
        
        x <- as.Date(x)
        #warning("Precisa ser do tipo Date - Convertendo de texto")
    }
    
    ##  funcoes auxiliares - poderia usar a lubridate mas achei assim mais simples
    
    year  <- function(dt) {as.numeric(format(dt,"%Y"))}  ## retorna ano
    wday <- function(dt) {as.numeric(format(dt,"%w"))}   ## retorna dia sendo  0 = domingo a 6= sabado
    passado <- function(dt,diff=1) {as.Date(paste(as.numeric(format(dt,"%Y"))-diff,format(dt,"%m-%d"),sep="-"))} ## ano - x
    
    ## Inicio 
    
    ano <- year(x) # extrai ano
    dia1 <- as.Date(paste(ano,'01','01',sep='-')) # primeiro do ano 
    
    diasem <- wday(dia1)  #descobre o dia da semana do dia1 
    
    fwd <- ifelse (diasem <=3, dia1 - diasem , dia1 + (7 - diasem) ) #se for menor ou igua a 3 (quarta) 
    fwd <- as.Date(fwd,origin = '1970-01-01') # reformata em data pois ela perde a formatacao 
    
    
    ## caso a data seja menor que a da 1o semana do ano (fwd)
    if (x < fwd) {
        
        dia1 <- passado(dia1)  # ano -1 
        diasem <- wday(dia1)  #dia da semana 
        fwd <- ifelse (diasem <=3, dia1 - diasem , dia1 + (7 - diasem) )
        fwd <- as.Date(fwd,origin = '1970-01-01')
        
    }
    
    diafim <- as.Date(paste(ano,'12','31',sep='-')) #Ultimo dia do ano
    diasem <- wday(diafim)                          #dia semana do ultimo dia
    
    ewd <- ifelse (diasem < 3, diafim - diasem , diafim + 6 - diasem) 
    ewd <- as.Date(ewd,origin = '1970-01-01') # ultima semana epi do ano
    
    if (x >= ewd) fwd <- ewd + 1 #caso a data (x) seja maior ou igual a ultiam semaan do ano
    
    
    epiweek <- floor(as.numeric(x - fwd) / 7 ) + 1 #numero de semanas e a diff da data e da primeira semana div por 7
    
    if(epiweek==0) epiweek <- 1 ## gatilho se for 0 vira semana 1
    
    epiyear <- year(fwd + 180) ## ano epidemiologico
    
    sprintf("%4d%s%02d",epiyear,separa,epiweek)  ## formata string com separador
    
}


## trancando e atachando namespace
lockEnvironment(.ocmisc_env, bindings=TRUE)

attach(.ocmisc_env)
