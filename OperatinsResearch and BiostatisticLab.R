#===============================================================================
#============================= Simplex Method ==================================
#===============================================================================

library(lpSolve)
obj<-c(50,70,60)
const<-matrix(c(
                -4,-5,-3,
                -6,-3,-5,
                -2,-3,-4), nrow=3, byrow=TRUE
)
rhs<-c(-6,-9,-4)
#simplex method hole chinho always "<=" korte hobe "-" dara gun kore

dir<-c("<=","<=","<=")
sol<-lp(
        direction= "min",
        objective.in = obj,
        const.mat= const,
        const.dir= dir,
        const.rhs=rhs
)
cat("The optimal values are:
     \nx1=",sol$solution[1], 
    "\nx2=",sol$solution[2],
    "\nx3=",sol$solution[3],
    "\nMinimum cost for the food mixture=",sol$objval)


#===============================================================================
#============================= Two-Phase Method ==================================
#===============================================================================

#s,e,a

# "<=" -- s
# "=" -- a
# ">=" -- -e,+a

obj1<-c(0,0,0,0,1,1)
const1<-matrix(c(
  2,4,-1,0,1,0,
  1,7,0,-1,0,1
),nrow=2,byrow=TRUE)

#Two-Phase method hole chinho always "=" 
dir1<-c("=","=")
rhs1<-c(4,7)

phase1<-lp(
  direction="min",
  objective.in =obj1,
  const.mat=const1,
  const.rhs= rhs1,
  const.dir= dir1
)
cat("the optimal values are:
    \na1=", phase1$solution[5],
    "\na2=",phase1$solution[6],
    "\nThe minimize value of Z is=",phase1$objval)

# zodi Z,a er man "0" pawa jy tahole Phase-II te jabo . 

#Since the optimal value and all the artificial variables are zero in Phase-I, the original LP problem has a feasible solution.Now, we can proceed to Phase-II


# zodi Z,a er man "0" na peye onno positive value pawa jy tahole Phase-II te jabo na .

#Since the optimal value and all the artificial variables are not zero in Phase-I, the original LP problem has no feasible solution.


obj2<-c(1,1,0,0)
const2<-matrix(c(
  2,4,-1,0,
  1,7,0,-1
),nrow=2,byrow=TRUE)

dir2<-c("=","=")
rhs2<-c(4,7)

phase2<-lp(
  direction="min",
  objective.in =obj2,
  const.mat=const2,
  const.rhs= rhs2,
  const.dir= dir2
)
cat("the optimal values are:
    \nx1=", phase2$solution[1],
    "\nx2=",phase2$solution[2],
    "\nThe minimize value of Z is=",phase2$objval)


#===============================================================================
#============================= Big-M Method ==================================
#===============================================================================

# s,e,a
# 
# "<=" -- s
# "=" -- a
# ">=" -- -e,a

M<-1e7
obj<-c( 5,3,0,0,M,M)
const<-matrix(c(
  2,4,1,0,0,0,
  2,2,0,0,1,0,
  5,2,0,-1,0,1
),nrow=3, byrow=TRUE)
dir<-c("=","=","=")
rhs<-c(12,10,10)

sol<-lp(
  objective.in=obj,
  direction="min",
  const.mat=const,
  const.dir=dir,
  const.rhs=rhs
  
)
cat("the optimal values are:
    \nx1=", sol$solution[1],
    "\nx2=",sol$solution[2],
    "\nThe minimize value of Z is=",sol$objval)





#===============================================================================
#============================= Transportation Problem ==================================
#===============================================================================

library(lpSolve)
cost <- matrix(c(4, 8, 8, 
                 16, 24, 16, 
                 8, 16, 24), nrow = 3, byrow=TRUE)
cost
# rsig: Row signs 
# shob somoy "<" hobe
rsig <- rep("<", 3)

# rrhs: Row right-hand side
rrhs <- c(76, 82, 77)

# csig: Column signs
# shob somoy ">" hobe
csig <- rep(">", 3)

# crhs: Column right-hand side
crhs <- c(72, 102, 41)

sol <- lp.transport(
  cost.mat=cost, 
  direction="min", 
  row.sign=rsig,
  row.rhs= rrhs, 
  col.sign=csig,
  col.rhs= crhs)
sol
sol$solution


















