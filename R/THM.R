

#Discount factor
NPV <- function(amount, payments, rate){
        NPV <- -amount
        for (t in 1:length(payments)){
                NPV <- NPV + payments[t]/(1+rate)^(t/12)
        }
        return(NPV)
}

#Internal rate of return of payments
IRR <- function(amount, payments){
        return(uniroot(NPV, interval = c(0, 1), amount = amount, payments = payments, tol = 0.0001)$root)
}

#Annuity calculation
Instalment <- function(amount, rate, tenor){
    r <- rate/12
    t <- tenor
    d  <- 1/r - 1/(r*(1+r)^t)
    return(amount/d)
}


#Credit Card APR (Hungarian regulation)
THM_CC <- function(limit = 100000, mindue = 0.05,                   
                   rate = 0.33, f_monthly = 200,                  
                   f_yearly = 0, mode = 'value'){       
        res <- matrix(0, nrow = 12, ncol = 4)        
        res[1, 1] = limit - f_yearly 
        #Repayment table
        for (i in 1:12){                
                res[i, 2] <- res[i, 1]*rate/12              
                res[i, 3] <- res[i, 1] + res[i, 2] + f_monthly                
                if(i == 12){                       
                        res[i, 4] <- res[i, 3]                       
                }               
                else{                      
                        res[i, 4] <- res[i, 3]*mindue                     
                        res[i + 1, 1] <- res[i, 3] - res[i, 4]                    
                }}    
        colnames(res) <- c('Start_amount', 'Interest', 'End_amount', 'Mindue') 
        
        #THM calculation
        THM <- IRR(res[1,1], res[,4])
        
        #Return values
        return(if(mode == 'table'){return(res)}else{return(round(THM, 4))})      
}

#Personal loan APR (Hungarian regulation)
THM_PL <- function(amount = 500000, rate = .173, 
                    tenor = 36, f_monthly = 0, f_disburs = 0.01,
                    mode = 'value'){
        res <- matrix(0, nrow = tenor, ncol = 4)
        res[1, 1] <- amount
        payment <- Instalment(amount, rate, tenor)
        #Repayment table
        for (i in 1:tenor){
                res[i, 2] <- res[i, 1]*(1+rate/12) + f_monthly
                res[i, 3] <- (payment + f_monthly)
                if(i != tenor){
                res[i + 1, 1] <- res[i, 2] - res[i, 3]
                res[i, 4] <- res[i, 1] - res[i + 1, 1]}
        }
        res[tenor, 4] <- invisible(NA)
        colnames(res) <- c('Start_amount', 'End_amount', 'Payment', 'Capital_repayment')
        
        #THM calculation
        THM <- IRR((1-f_disburs)*amount, res[,3])
        if(mode == 'table'){return(res)}else{return(round(THM, 4))}
}

#Sales Finance APR (Hungarian regulation)
THM_SF <- function(amount = 500000, rate = .173, 
                   tenor = 12, f_monthly = 0, f_disburs = 20000,
                   mode = 'value'){
        return(THM_PL(amount - f_disburs, rate, tenor, f_monthly, f_disburs = 0, mode))
}

#Current Account Overdraft APR (Hungarian regulation)
THM_OD <- function(amount = 375000, rate = 0.2145, 
                   f_monthly = 200, f_disburs = 0.01, mode = 'value'){
        res <- matrix(0, nrow = 12, ncol = 3)
        res[1, 1] <- amount
        for(i in 1:12){
                res[i, 2] <- res[i, 1]*(1 + rate/12) + f_monthly
                if(i == 12){res[i, 3] <- res[i, 2]}
                else{
                        res[i + 1, 1] <- res[i, 2]
                        res[i, 3] <- 0
                }
        }
        res[12, 3] <- res[12, 2]
        THM <- IRR(amount, res[,3])
        if(mode == 'table'){return(res)}else{return(THM)}
}