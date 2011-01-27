#!/usr/bin/env Rscript

#load packages, functions and data
source('setup.R')

cat('Second Year Model using the', dataset, ' dataset\n\t',
    ns,'States\n\t',
    q, 'Years Back\n\t',
    r, 'Years Ahead\n\t',
    m, 'Year Running Mean\n')
flush.console()

paleo   <- switch(dataset, meko = meko,   woodhouse = wood,   nino3 = cook)
paleo.b <- switch(dataset, meko = meko.b, woodhouse = wood.b, nino3 = cook.b)

hist <-   switch(dataset, meko = lees,   woodhouse = lees,   nino3 = nino3)
hist.b <- switch(dataset, meko = lees.b, woodhouse = lees.b, nino3 = nino3.b)

n <- length(paleo)
nh <- length(hist)

    #all state tansitions of paleo
paleo.qr <- laglead(paleo,q,r)
paleo.bqr <- laglead(paleo.b,q,r)
hist.bqr <- laglead(hist.b,q,r)
cat('Building Historical State Transitions...\n')
flush.console()
tr.hist <- trprob( hist.bqr$lag, hist.bqr$lead )

    # block transition probabilities 
    # ns^q from states and 
    # ns^p to states
tr.base <- paste("tr_ns",ns,"_q",q,"_r",r,"_m",m,'_',dataset,sep='')
tr.file <- file.path("data/",paste(tr.base,".Rdata",sep=""))
if(file.exists(tr.file) & cache){
  load(tr.file)
}else{
  cat('Building Paleo State Transitions...\n')
  flush.console()
  tr.paleo <- trprob( paleo.bqr$lag, paleo.bqr$lead )
  save(tr.paleo,file=tr.file)
}

    #all binary combinations of length q
b.i <- permutations(ns,q,0:(ns-1),repeats.allowed=T)

    #number of states
nfrom <- nrow(unique(tr.paleo$from))
nto <- nrow(unique(tr.paleo$to))

    #Quantiles of paleo and historical
if(hist.cdf == 'ecdf'){
    Qp <- ecdf(paleo)(paleo)
}else if(hist.cdf == 'gamma'){
    require(MASS)
    par <- fitdistr(paleo,'gamma',lower = 0.001)
    shape.paleo <- par$estimate[1]
    rate.paleo <- par$estimate[2]
    Qp <- pgamma(paleo,shape=shape.paleo,rate=rate.paleo)
}    
Qp.qr <- laglead(Qp,q,r)
paleo.qr <- laglead(paleo,q,r)

    # All of the trasitions from a particular state
    # in the paleo and the associated states that 
    # came after, also the knn weight functions
  # for each beginning state, get the quantile pools to and from    
pools.from.Q <- build.pools.from(tr.paleo,Qp.qr)
pools.to.Q <- build.pools.to(tr.paleo,Qp.qr)
pools.from.paleo <- build.pools.from(tr.paleo,paleo.qr)
pools.to.paleo <- build.pools.to(tr.paleo,paleo.qr)
    

    # allocate matricies to store simualations
    # simulate each historical year except the ones 
    # where there is not q past years
    # We can always forecast one more year thatn the data we have
sim <- list()
models <- pdfs <- vector('list',nfrom)

for( s in 1:r ) sim[[s]] <- matrix(NA,nsims,nh-q+1)

no.analog <-logical(length((q+1):(nh+1)))

cat('Forecasting...\n');flush.console()

pb <- txtProgressBar(q+1,nh+1,style=3)
    #y is the year to forecast FROM
for(y in (q+1):(nh+r+ifelse(q==1 & r > 1,-1,0))){
  
  #setTxtProgressBar(pb,y)
    sy <- if(is.na(time(hist)[y])) time(hist)[y-1] + 1 else time(hist)[y]
    fcy <- if(is.na(time(hist)[y])) time(hist)[y-1]+1:r else time(hist)[y]+1:r
    cat('Forecasting From Year',sy,'for',fcy,'\n')
    
        # The historical quantiles removing the current year
    #this.hist <- hist[-y]
    if(hist.cdf=='ecdf'){
        Qh <- ecdf(hist)(hist)
    }else{
        par.hist <- fitdistr(hist,'gamma',lower = 0.001)
        shape.hist <- par.hist$estimate[1]
        rate.hist <- par.hist$estimate[2]
        Qh <- pgamma(hist,shape=shape.hist,rate=rate.hist)
        
        par.paleo <- fitdistr(paleo,'gamma',lower = 0.001)
        shape.paleo <- par.paleo$estimate[1]
        rate.paleo <- par.paleo$estimate[2]
    }
    Qh.qr <- laglead(Qh,q,r)

        # current binary state
    state <- hist.b[(y-q):(y-1)]
      # current state in terms of historical quantiles
    Q.state <- Qh[(y-q):(y-1)]
      # current state in terms of paleo flow by applying 
      # the *historical* quantiles
    paleo.flow.state <- quantile(paleo,Q.state)

        # all the transtition probabilities coming from the current state
    which.from <- rows.equal(b.i,state)
    this.p <- tr.paleo$p[which.from,]   

    cat('\tThe Current State is', state,'\n')
    cat('\tThe Quantile State is', sprintf('%4.2f',Q.state),'\n')
    #cat('\tThe Magnitude State is', sprintf('%6.3f',flow.state),'\n')

    this.sim <- matrix(NA,nsims,r)

        # find which state are we coming from so we know which 
        # transition pool to choose from
    pool.from.Q <- pools.from.Q[[ which.from ]]
    pool.from.paleo <- pools.from.paleo[[ which.from ]]
    these.pool.to.Q <- pools.to.Q[[ which.from ]]
    these.pool.to.paleo <- pools.to.paleo[[ which.from ]]

    # check if there are no years to sample from 
    if(nrow(rbind(pool.from.paleo))==0){
      cat('\tWarning: No analog year found!\n')
      no.analog[y-q] <- TRUE
    }
    
    for(i in 1:nsims){
            
            # 1. Simulate a transition from the current state 
            # 2. Select the corresponding pool
            # 3. Knn from the pool 
            # 4. Repeat 
            
            # 1. Simulate a transition from the current state 
        rand1 <- runif(1) 
        which.to <- rank(c(rand1,cumsum(this.p)))[1]
        state.to <- tr.paleo$to[which.to,]
            
            # 2. corresponding pool
        this.pool.to.paleo <- rbind(these.pool.to.paleo[[which.to]])
        this.pool.to.Q <- these.pool.to.Q[[which.to]]
        those.to <- tr.paleo$pools$to[[ which.from ]][[which.to]]
        this.pool.from.paleo <- 
            if(q ==1) 
                cbind(rbind(pool.from.paleo)[those.to,]) 
            else 
                rbind(rbind(pool.from.paleo)[those.to,])
        this.pool.from.Q <- rbind(pool.from.Q)[those.to,]
        
        if(transition == 'locfit' & is.na(tr.paleo$cor[which.from,which.to]) & 
            nrow(this.pool.from.paleo) > 1)
        {

                
            #xx <- apply(this.pool.from.paleo,1,mean)
            #yy <- apply(this.pool.to.paleo,1,mean)
            xx <- this.pool.from.paleo[,q]
            yy <- this.pool.to.paleo[,1]
            co <- cor(xx,yy)

            if(is.null(models[[which.from]])) 
                models[[which.from]] <- vector('list',nto)
                
            if(is.null(models[[which.from]][[which.to]])){
                
                bp <- best.par(xx,yy,f=gcvplot)
                models[[which.from]][[which.to]] <- 
                    locfit.raw(xx,yy,alpha=bp$a,deg=bp$p,scale=T,kern="bisq")
                    
                fit <- summary(lm(this.pool.to.paleo[,1]~this.pool.from.paleo))
                pval <- pf(fit$fstatistic[1L], fit$fstatistic[2L], fit$fstatistic[3L], 
                lower.tail = FALSE)

                tr.paleo$pts[which.from,which.to] <- length(this.pool.to.paleo[,1])
                tr.paleo$cor[which.from,which.to] <- co
                tr.paleo$pval[which.from,which.to] <- pval
                tr.paleo$sig[which.from,which.to] <- 
                    ifelse(abs(co) > sigcor(tr.paleo$pts[which.from,which.to]),T,F) 
                tr.paleo$gcv[which.from,which.to] <- bp$gcv
                
            }            
        } else if(transition == 'gamma'){
                
            if(is.null(pdfs[[which.from]])) 
                pdfs[[which.from]] <- vector('list',nto)

            if(is.null(pdfs[[which.from]][[which.to]])){
                pdfs[[which.from]][[which.to]] <- 
                    if(length(this.pool.to.paleo[,1]) > 1)
                        fitdistr(this.pool.to.paleo,'gamma',lower = 0.001)
                    else
                        NA
            }
            if(!is.na(pdfs[[which.from]][[which.to]][1])){
                this.shape <- pdfs[[which.from]][[which.to]]$estimate[1]
                this.rate <- pdfs[[which.from]][[which.to]]$estimate[2]

                this.paleo.sim <- rgamma(1,shape=this.shape,rate=this.rate)
            }else{
                this.paleo.sim <- this.pool.to.paleo
            }
            this.quant <- pgamma(this.paleo.sim,shape=shape.paleo,rate=rate.paleo)

        } else { # knn
            #cat('From:',state,'  To: ',state.to,'  ',co,' n = ',length(xx),'\n')

                # Weight function and nn for the current state 
                # weight function dependent on number of points
            k <- floor(sqrt(nrow(this.pool.from.paleo)))
            w <- cumsum(oneoverk(k))

                # 3. Knn 

            if(nrow(this.pool.from.paleo) == 0 ){

                # then there is no analog year in the paleo record :(
              this.sim[i,] <- NA

            }
            else{
                
                if(back.weight == 'equal'){

                    neighbors <- 
                    find.matches(rbind(paleo.flow.state),
                        this.pool.from.paleo, 
                        tol = rep(max(this.pool.from.paleo),q),
                        maxmatch = length(paleo))$matches[1:k]

                }else if(back.weight == 'flow'){

                        # using magnitudes and a higher weight on near values
                    #d <- sqrt(apply((this.pool.from.paleo - paleo.flow.state)^2*(1/oneoverk(q)),1,sum))
                    d <- apply(abs(this.pool.from.paleo - paleo.flow.state)*(1/oneoverk(q)),1,sum)
                    #browser()
                        # find nearest quantile neighbors 
                        # (rbind to make sure that it is a row vector)
                    #d <- sqrt(apply((rbind(this.pool.from.Q) - Q.state)^2,1,sum))

                    neighbors <- order(d)[1:k]
                }

                    # a less drastic weight function
                # nn <- 1/sort(neighbors)
                # w <- cumsum(nn/sum(nn))

                rand2 <- runif(1)
                this.neighbor <- rank(c(rand2,w))[1]


                    # need cbind to make sure we're using matricies, 
                    # could use two cases, but this is less lines of code.
                this.quant <- cbind(cbind(this.pool.to.Q)[neighbors,])[this.neighbor,]
                #this.quant <- this.pool.to[sample(1:nrow(this.pool.to),1),]
            }
        }
        
            # the simulated quantile from paleo
        this.sim[i,] <- 
        if(transition == 'knn' || transition == 'gamma' || 
            tr.paleo$sig[which.from,which.to] <= 10) 
                this.quant 
        else if (transition == 'locfit')
            predict(models[[which.from]][[which.to]],this.quant)
            
        if(transition == 'locfit' & this.sim[i,] > 1) this.sim[i,] <- 1
        if(transition == 'locfit' & this.sim[i,] < 0) this.sim[i,] <- 0
        
        #cat("Chose",state.to, 'and', quantile(hist,this.quant),'from',state,'and',
        # quantile(hist,Q.state),'real',hist[(y+1):(y+r)],'\n')
    }
    #browser()

    for(s in 1:r)
      sim[[s]][,y-q] <- 
        if(all(is.na(this.sim[,s])))
            rep(NA,nsims) else 
                if(hist.cdf == "ecdf") quantile(hist,this.sim[,s]) else
                    qgamma(this.sim[,s],shape=shape.hist,rate=rate.hist)

}
close(pb)

plot.sy.sims(sims,hist,q,r,fn=file.path(plotdir,paste(tr.base,'.pdf',sep='')))
hist.plot <- plot.sy.sims(sims,hist,q,r)

rpss <- RPSS(hist.plot[[s]],sim[[1]][,-ncol(sim[[1]])])
#cat('Median RPSS:',median(rpss[rpss>-3.5]),'\n')
cat(median(rpss[!no.analog[-length(no.analog)]],na.rm=T),'\n')

if(q > 1){
    comp.pos <- cbind(hist.bqr$lag[rpss>0,],0)
    comp.neg <- cbind(hist.bqr$lag[rpss<0,],0)
    for(i in 1:nrow(comp.neg))
        if(abs(comp.neg[i,1]-comp.neg[i,2])>1) comp.neg[i,ncol(comp.neg)] <- 1
    for(i in 1:nrow(comp.pos))
        if(abs(comp.pos[i,1]-comp.pos[i,2])>1) comp.pos[i,ncol(comp.pos)] <- 1
}

save(sim, hist.plot, hist ,file='data/fc.Rdata')