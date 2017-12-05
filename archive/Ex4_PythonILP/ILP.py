# main.py
import numpy as np
#import networkx as nx
from scipy import stats 

# parse files
def parsefile(x):
    f=open(x)
    mat=[rows.split() for rows in f.read().splitlines()]
    f.close()
    return mat
    

#f=open('lung_expressed_genes.txt')
#lung_genes=f.read().splitlines()
#f.close() 
#
options=parsefile("ilp_options.txt")
for i in range(len(options)):
    if options[i][0]=="mipgap":
        mipgap=options[i][1]
    if options[i][0]=="number_solutions":
        number_solutions=options[i][1]
    if options[i][0]=="timelimit":
        timelimit=options[i][1]
    if options[i][0]=="inputs_file":
        inputs_file=options[i][1]
    if options[i][0]=="network_file":
        net_file=options[i][1]
    if options[i][0]=="measurements_file":
        measurements_file=options[i][1]

inputs=parsefile(inputs_file)
measurements=parsefile(measurements_file)
measurements[1]=[int(measurements[1][i]) for i in range(len(measurements[1]))]
net=parsefile(net_file)

spc=set()
for i in range(len(net)):
    spc.add(net[i][0])
    spc.add(net[i][2])

spc=list(spc)
nspc=len(spc)
nreactions=len(net)
nexp=len(measurements)-1
nsignals=len(measurements[0])
ninputs=len(inputs[0])

# create network structures
dact=dict((spc[i],list()) for i in range(nspc))
[dact[net[i][0]].append(i) for i in range(nreactions) if net[i][1]=="1"]
dinh=dict((spc[i],list()) for i in range(nspc))
[dinh[net[i][0]].append(i) for i in range(nreactions) if net[i][1]=="-1"]
upact=dict((spc[i],list()) for i in range(nspc))
[upact[net[i][2]].append(i) for i in range(nreactions) if net[i][1]=="1"]
upinh=dict((spc[i],list()) for i in range(nspc))
[upinh[net[i][2]].append(i) for i in range(nreactions) if net[i][1]=="-1"]
dstream=dict((spc[i],list()) for i in range(nspc))
[dstream[net[i][0]].append(i) for i in range(nreactions) ]
ustream=dict((spc[i],list()) for i in range(nspc))
[ustream[net[i][2]].append(i) for i in range(nreactions) ]

# setup cplex
import cplex

# Define Model
m = cplex.Cplex()

# setting the objective weights
obj_x=20.0
obj_a=100

# add variables
ub=[]
names=[]
lb=[]
vtypes=[]
obj=[]

def x(j,k): return "x_{%s,%d}" % (j, k)
def fx(j,k): return "fx_{%s,%d}" % (j, k)
def up(j,k): return "u+_{%d,%d}" % (j, k)
def um(j,k): return "u-_{%d,%d}" % (j, k)
def xp(j,k): return "x+_{%s,%d}" % (j, k)
def xm(j,k): return "x-_{%s,%d}" % (j, k)
def y(i): return "y_{%d}" % i
def yinv(i): return "yinv_{%d}" % i
def a(j,k): return "abs_{%s,%d}" % (j, k)
def d(j,k): return "dist_{%s,%d}" % (j, k)
def B(j,k): return "B_{%s,%d}" % (j, k)
def fz(j,k): return "fz_{%s,%d}" % (j, k)

for i in range(nexp):
    counter=(i)*(6*nspc+2*nreactions)
    # add x(j,k)
    ub.extend([1 for j in range(len(spc))])
    lb.extend([-1 for j in range(len(spc))])
    vtypes.extend(["I" for j in range(len(spc))])
    obj.extend([0 for j in range(len(spc))])
    names.extend([x(spc[j],i) for j in range(len(spc))])
    xdic=dict((x(spc[j],i),counter+j) for j in range(len(spc)))
    xid=dict((x(spc[j],i),j) for j in range(len(spc)))
    # add B(j,k)
    ub.extend([1 for j in range(len(spc))])
    lb.extend([-1 for j in range(len(spc))])
    vtypes.extend(["I" for j in range(len(spc))])
    obj.extend([0 for j in range(len(spc))])
    names.extend([B(spc[j],i) for j in range(len(spc))])
    Bdic=dict((B(spc[j],i),counter+nspc+j) for j in range(len(spc)))
    # add abs(j,k)
    ub.extend([2 for j in range(len(spc))])
    lb.extend([0 for j in range(len(spc))])
    vtypes.extend(["I" for j in range(len(spc))])
    obj.extend([obj_a for j in range(len(spc))])
    names.extend([a(spc[j],i) for j in range(len(spc))])
    adic=dict((a(spc[j],i),counter+2*nspc+j) for j in range(len(spc)))
    # add x+(j,k)
    ub.extend([1 for j in range(len(spc))])
    lb.extend([0 for j in range(len(spc))])
    vtypes.extend(["B" for j in range(len(spc))])
    obj.extend([obj_x  for j in range(len(spc))])
    names.extend([xp(spc[j],i) for j in range(len(spc))])
    xpdic=dict((xp(spc[j],i),counter+3*nspc+j) for j in range(len(spc)))
    # add x-(j,k)
    ub.extend([1 for j in range(len(spc))])
    lb.extend([0 for j in range(len(spc))])
    vtypes.extend(["B" for j in range(len(spc))])
    obj.extend([obj_x for j in range(len(spc))])
    names.extend([xm(spc[j],i) for j in range(len(spc))])
    xmdic=dict((xm(spc[j],i),counter+4*nspc+j) for j in range(len(spc)))
    # add u+(j,k)
    ub.extend([1 for j in range(len(net))])
    lb.extend([0 for j in range(len(net))])
    vtypes.extend(["B" for j in range(len(net))])
    obj.extend([0 for j in range(len(net))])
    names.extend([up(j,i) for j in range(len(net))])
    updic=dict((up(j,i),counter+5*nspc+j) for j in range(len(net)))
    upid=dict((up(j,i),j) for j in range(len(net)))
    # add u-(j,k)
    ub.extend([1 for j in range(len(net))])
    lb.extend([0 for j in range(len(net))])
    vtypes.extend(["B" for j in range(len(net))])
    obj.extend([0 for j in range(len(net))])
    names.extend([um(j,i) for j in range(len(net))])
    umdic=dict((um(j,i),counter+5*nspc+nreactions+j) for j in range(len(net)))
    umid=dict((um(j,i),j) for j in range(len(net)))
    # add dist(j,k)
    ub.extend([100 for j in range(len(spc))])
    lb.extend([0 for j in range(len(spc))])
    vtypes.extend(["C" for j in range(len(spc))])
    obj.extend([0 for j in range(len(spc))])
    names.extend([d(spc[j],i) for j in range(len(spc))])
    ddic=dict((d(spc[j],i),counter+5*nspc+2*nreactions+j) for j in range(len(spc)))

m.variables.add(obj=obj, lb=lb, ub=ub, types=vtypes, names=names)

# add constraints

# constraints for up, um

##   up \geq sigma * x 
rows=[[[updic[up(i,k)], xdic[x(net[i][0],k)]], [1, -1]] for i in range(nreactions) if net[i][1]=="1" for k in range(nexp)]
rhs=[0 for i in range(nreactions) if net[i][1]=="1" for k in range(nexp)]
sens=['G' for i in range(nreactions) if net[i][1]=="1" for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)

rows=[[[updic[up(i,k)], xdic[x(net[i][0],k)]], [1, 1]] for i in range(nreactions) if net[i][1]=="-1" for k in range(nexp)]
rhs=[0 for i in range(nreactions) if net[i][1]=="-1" for k in range(nexp)]
sens=['G' for i in range(nreactions) if net[i][1]=="-1" for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)

##   um \geq -sigma * x 
rows=[[[umdic[um(i,k)], xdic[x(net[i][0],k)]], [1, 1]] for i in range(nreactions) if net[i][1]=="1" for k in range(nexp)]
rhs=[0 for i in range(nreactions) if net[i][1]=="1" for k in range(nexp)]
sens=['G' for i in range(nreactions) if net[i][1]=="1" for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)

rows=[[[umdic[um(i,k)], xdic[x(net[i][0],k)]], [1, -1]] for i in range(nreactions) if net[i][1]=="-1" for k in range(nexp)]
rhs=[0 for i in range(nreactions) if net[i][1]=="-1" for k in range(nexp)]
sens=['G' for i in range(nreactions) if net[i][1]=="-1" for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)

## up \leq 1 - um
rows=[[[updic[up(i,k)], umdic[um(i,k)]], [1, 1]] for i in range(nreactions) for k in range(nexp)]
rhs=[1 for i in range(nreactions) for k in range(nexp)]
sens=['L' for i in range(nreactions) for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)

##   up \leq sigma * x + um
rows=[[[updic[up(i,k)], xdic[x(net[i][0],k)], umdic[um(i,k)]], [1, -1, -1]] for i in range(nreactions) if net[i][1]=="1" for k in range(nexp)]
rhs=[0 for i in range(nreactions) if net[i][1]=="1" for k in range(nexp)]
sens=['L' for i in range(nreactions) if net[i][1]=="1" for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)

rows=[[[updic[up(i,k)], xdic[x(net[i][0],k)], umdic[um(i,k)]], [1, 1, -1]] for i in range(nreactions) if net[i][1]=="-1" for k in range(nexp)]
rhs=[0 for i in range(nreactions) if net[i][1]=="-1" for k in range(nexp)]
sens=['L' for i in range(nreactions) if net[i][1]=="-1" for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)

##   um \leq -sigma * x + up
rows=[[[umdic[um(i,k)], xdic[x(net[i][0],k)], updic[up(i,k)]], [1, 1, -1]] for i in range(nreactions) if net[i][1]=="1" for k in range(nexp)]
rhs=[0 for i in range(nreactions) if net[i][1]=="1" for k in range(nexp)]
sens=['L' for i in range(nreactions) if net[i][1]=="1" for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)

rows=[[[umdic[um(i,k)], xdic[x(net[i][0],k)], updic[up(i,k)]], [1, -1, -1]] for i in range(nreactions) if net[i][1]=="-1" for k in range(nexp)]
rhs=[0 for i in range(nreactions) if net[i][1]=="-1" for k in range(nexp)]
sens=['L' for i in range(nreactions) if net[i][1]=="-1" for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)


# constraints for xp, xm

###   xp \geq up - y(i) 
#rows=[[[xpdic[xp(net[i][2],k)], updic[up(i,k)], ydic[y(i)]], [1, -1, 1]] for i in range(nreactions) for k in range(nexp)]
#rhs=[0 for i in range(nreactions)  for k in range(nexp)]
#sens=['G' for i in range(nreactions)  for k in range(nexp)]
#m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)
#
###  xm \geq um - y(i) 
#rows=[[[xmdic[xm(net[i][2],k)], umdic[um(i,k)], ydic[y(i)]], [1, -1, 1]] for i in range(nreactions) for k in range(nexp)]
#rhs=[0 for i in range(nreactions)  for k in range(nexp)]
#sens=['G' for i in range(nreactions)  for k in range(nexp)]
#m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)

# xp \leq sum(up) 
rows= [[[xpdic[xp(spc[j],k)]] + [updic[up(i, k)] for i in ustream[spc[j]]] , [1] + [-1 for i in ustream[spc[j]]] ]  for j in range(nspc) for k in range(nexp)]
rhs=[0 for i in range(nspc) for k in range(nexp)]
sens=['L' for i in range(nspc) for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)   

# xm \leq sum(um) 
rows= [[[xmdic[xm(spc[j],k)]] + [umdic[um(i, k)] for i in ustream[spc[j]]] , [1] + [-1 for i in ustream[spc[j]]]  ] for j in range(nspc) for k in range(nexp)]
rhs=[0 for i in range(nspc) for k in range(nexp)]
sens=['L' for i in range(nspc) for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)   


# constraints for x and fitness error

##   x \eq xp - xm + B
rows=[[[xdic[x(spc[i],k)], xpdic[xp(spc[i],k)], xmdic[xm(spc[i],k)], Bdic[B(spc[i], k)]], [1, -1, 1, -1]] for i in range(nspc) for k in range(nexp)]
rhs=[0 for i in range(nspc) for k in range(nexp)]
sens=['E' for i in range(nspc) for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)

##   abs \geq x - m
rows=[[[adic[a(spc[i],k)], xdic[x(spc[i],k)]], [1, -1]] for i in range(nspc) if (spc[i] in measurements[0])  for k in range(nexp)]
rhs=[-int(measurements[k+1][measurements[0].index(spc[i])]) for i in range(nspc) if (spc[i] in measurements[0])  for k in range(nexp)]
sens=['G' for i in range(nspc) if (spc[i] in measurements[0])  for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)

##   abs \geq m - x
rows=[[[adic[a(spc[i],k)], xdic[x(spc[i],k)]], [1, 1]] for i in range(nspc) if (spc[i] in measurements[0])  for k in range(nexp)]
rhs=[int(measurements[k+1][measurements[0].index(spc[i])]) for i in range(nspc) if (spc[i] in measurements[0])  for k in range(nexp)]
sens=['G' for i in range(nspc) if (spc[i] in measurements[0])  for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)


# constraints for distances

# d_P \ge d_R - 100 + 101*z
rows= [[[ddic[d(net[i][2], k)], ddic[d(net[i][0],k)] , updic[up(i,k)]], [1, -1, -101]] for i in range(nreactions) if net[i][2] not in inputs[0] for k in range(nexp)]
rhs=[-100 for i in range(nreactions) if net[i][2] not in inputs[0]  for k in range(nexp)]
sens=['G' for i in range(nreactions) if net[i][2] not in inputs[0]  for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)

rows= [[[ddic[d(net[i][2], k)], ddic[d(net[i][0],k)] , umdic[um(i,k)]], [1, -1, -101]] for i in range(nreactions) if net[i][2] not in inputs[0] for k in range(nexp)]
rhs=[-100 for i in range(nreactions)  if net[i][2] not in inputs[0] for k in range(nexp)]
sens=['G' for i in range(nreactions) if net[i][2] not in inputs[0]  for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)

# x = Inp for all input nodes 
rows=[[[xdic[x(spc[i],k)]], [1]] for i in range(nspc) if (spc[i] in inputs[0]) and inputs[k+1][inputs[0].index(spc[i])]!="NaN"  for k in range(nexp)]
rhs=[int(inputs[k+1][inputs[0].index(spc[i])]) for i in range(nspc) if (spc[i] in inputs[0]) and inputs[k+1][inputs[0].index(spc[i])]!="NaN"  for k in range(nexp)]
sens=['E' for i in range(nspc) if (spc[i] in inputs[0]) and inputs[k+1][inputs[0].index(spc[i])]!="NaN"  for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)

# x = B for all nodes without upstream reactions 
rows=[[[xdic[x(spc[i],k)], Bdic[B(spc[i], k)]], [1, -1]] for i in range(nspc) if ((not ustream[spc[i]]))  for k in range(nexp)]
rhs=[0 for i in range(nspc) if ((not ustream[spc[i]]))  for k in range(nexp)]
sens=['E' for i in range(nspc) if ((not ustream[spc[i]]))  for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)

# B = 0 for all non input nodes 
rows=[[[Bdic[B(spc[i], k)]], [1]] for i in range(nspc) if (spc[i] not in inputs[0])  for k in range(nexp)]
rhs=[0 for i in range(nspc) if (spc[i] not in inputs[0])  for k in range(nexp)]
sens=['E' for i in range(nspc) if (spc[i] not in inputs[0])  for k in range(nexp)]
m.linear_constraints.add(lin_expr=rows, senses=sens, rhs=rhs)


# Set Model Parameters
m.parameters.threads.set(4)
m.parameters.mip.limits.populate.set(int(number_solutions))
m.parameters.mip.pool.absgap.set(0.0)
m.parameters.mip.pool.intensity=1
m.parameters.mip.pool.replace=0
m.parameters.mip.tolerances.mipgap.set(float(mipgap))
m.parameters.timelimit.set(float(timelimit))

m.write("main.lp")

## Solve Problem
m.solve()
m.populate_solution_pool()

# parse the solution
nsol=m.solution.pool.get_num()

#get values
sol=[ m.solution.pool.get_values(i) for i in range(nsol)]
xs=[ [sol[i][j] for j in list(xdic.itervalues()) ] for i in range(nsol) ]
#ys=[ [sol[i][j] for j in list(ydic.itervalues()) ] for i in range(nsol) ]
Bs=[ [sol[i][j] for j in list(Bdic.itervalues()) ] for i in range(nsol) ]
ups=[ [sol[i][j] for j in list(updic.itervalues()) ] for i in range(nsol) ]
ums=[ [sol[i][j] for j in list(umdic.itervalues()) ] for i in range(nsol) ]

xnames=list(xdic.iterkeys())
#ynames=list(ydic.iterkeys())
Bnames=list(Bdic.iterkeys())
upnames=list(updic.iterkeys())
umnames=list(umdic.iterkeys())


#f=open("output.txt", 'w')
#for i in range(len(xs[0])):
#    f.write("%s\t" %xnames[i])
#    for j in range(nsol):
#        f.write("%f\t" %xs[j][i])
#    f.write("\n")
#
#for i in range(len(ys[0])):
#    f.write("%s\t" %ynames[i])
#    for j in range(nsol):
#        f.write("%f\t" %ys[j][i])
#    f.write("\n")
#
#for i in range(len(Bs[0])):
#    f.write("%s\t" %Bnames[i])
#    for j in range(nsol):
#        f.write("%f\t" %Bs[j][i])
#    f.write("\n")
#    
#for i in range(len(ups[0])):
#    f.write("%s\t" %upnames[i])
#    for j in range(nsol):
#        f.write("%f\t" %ups[j][i])
#    f.write("\n")
#
#for i in range(len(ums[0])):
#    f.write("%s\t" %umnames[i])
#    for j in range(nsol):
#        f.write("%f\t" %ums[j][i])
#    f.write("\n")
#
#f.close()

# calculate average xs and us

avx=[ float(sum( [ xs[i][j] for i in range(len(xs)) ]))/len(xs) for j in range(len(xs[0]))  ]
avup=[ float(sum( [ ups[i][j] for i in range(len(ups)) ]))/len(ups) for j in range(len(ups[0]))  ]
avum=[ float(sum( [ ums[i][j] for i in range(len(ums)) ]))/len(ums) for j in range(len(ums[0]))  ]


#spcd=dict((key, value) for key, value in [(spc[i], i) for i in range(len(spc))])

f=open("xs.txt", "w")
[ f.write("%s\t%f\n" % (xnames[i], avx[i])) for i in range(len(xnames)) if avx[i]!=0.0 ]
f.close()

active_species=set([ spc[xid[xnames[i]]] for i in range(len(xnames)) if avx[i]!=0.0 ])
up_species=set([ spc[xid[xnames[i]]] for i in range(len(xnames)) if avx[i]>0.0 ])
down_species=set([ spc[xid[xnames[i]]] for i in range(len(xnames)) if avx[i]<0.0 ])

f=open("us.txt", "w")
[ f.write("%s->%s\t%f\n" % (net[upid[upnames[i]]][0], net[upid[upnames[i]]][2], avup[i])) for i in range(len(avup)) if net[upid[upnames[i]]][2] in up_species and avup[i]>0 ]
[ f.write("%s->%s\t-%f\n" % (net[umid[umnames[i]]][0], net[umid[umnames[i]]][2], avum[i])) for i in range(len(avum)) if net[umid[umnames[i]]][2] in down_species and avum[i]>0 ]
f.close()


#create graphviz file

f=open("network_out.dot", "w")
f.write("digraph {\n\n")
[ f.write("\"%s\"->\"%s\"\t[penwidth=%f, color=%s]\n" % (net[upid[upnames[i]]][0], net[upid[upnames[i]]][2], avup[i], "black")) for i in range(len(avup)) if net[upid[upnames[i]]][2] in up_species and avup[i]>0 ]
[ f.write("\"%s\"->\"%s\"\t[penwidth=%f, color=%s]\n" % (net[umid[umnames[i]]][0], net[umid[umnames[i]]][2], avum[i], "red")) for i in range(len(avum)) if net[umid[umnames[i]]][2] in down_species and avum[i]>0 ]
f.write("\n\n}")
f.close()

###########

# calculate importance of nodes in the solution

##########
#
#count_all_sol_up=list()
#count_all_sol_down=list()
#for k in range(nsol):
#    active_species=[ spc[xid[xnames[i]]] for i in range(len(xnames)) if xs[k][i]!=0.0 ]
#    up_species=[ spc[xid[xnames[i]]] for i in range(len(xnames)) if xs[k][i]>0.0 ]
#    down_species=[ spc[xid[xnames[i]]] for i in range(len(xnames)) if xs[k][i]<0.0 ]
#    g=nx.DiGraph()
#    [ g.add_edge(net[upid[upnames[i]]][0], net[upid[upnames[i]]][2]) for i in range(len(ups[k])) if ups[k][i]>=1 and net[upid[upnames[i]]][2] in up_species ]
#    [ g.add_edge(net[umid[umnames[i]]][0], net[umid[umnames[i]]][2]) for i in range(len(ums[k])) if ums[k][i]>=1 and net[umid[umnames[i]]][2] in down_species ]
#    spcopt_up=list()
#    spcopt_down=list()
#    [ spcopt_up.append(net[upid[upnames[i]]][0])   for i in range(len(ups[k])) if ups[k][i]>=1 and net[upid[upnames[i]]][2] in up_species and net[upid[upnames[i]]][0] not in spcopt_up and net[upid[upnames[i]]][0] in up_species ]
#    [ spcopt_up.append(net[upid[upnames[i]]][2])   for i in range(len(ups[k])) if ups[k][i]>=1 and net[upid[upnames[i]]][2] in up_species and net[upid[upnames[i]]][2] not in spcopt_up and net[upid[upnames[i]]][2] in up_species ]
#    [ spcopt_up.append(net[umid[umnames[i]]][0])   for i in range(len(ums[k])) if ums[k][i]>=1 and net[umid[umnames[i]]][2] in down_species and net[umid[umnames[i]]][0] not in spcopt_up and net[umid[umnames[i]]][0] in up_species ]
#    [ spcopt_down.append(net[umid[umnames[i]]][0])   for i in range(len(ums[k])) if ums[k][i]>=1 and net[umid[umnames[i]]][2] in down_species and net[umid[umnames[i]]][0] not in spcopt_down and net[umid[umnames[i]]][0] in down_species ]
#    [ spcopt_down.append(net[umid[umnames[i]]][2])   for i in range(len(ums[k])) if ums[k][i]>=1 and net[umid[umnames[i]]][2] in down_species and net[umid[umnames[i]]][2] not in spcopt_down and net[umid[umnames[i]]][2] in down_species ]
#    [ spcopt_down.append(net[upid[upnames[i]]][0])   for i in range(len(ups[k])) if ups[k][i]>=1 and net[upid[upnames[i]]][2] in up_species and net[upid[upnames[i]]][0] not in spcopt_down and net[upid[upnames[i]]][0] in down_species ]
##    spcopt=g.nodes()    
#    count_up=[0]*len(spc)
#    count_down=[0]*len(spc)
#    for j in range(len(spcopt_up)):
#        count_up[spc.index(spcopt_up[j])]=0
#        for m in range(len(measurements[0])):
#            try:
#                if nx.algorithms.shortest_paths.has_path(g, spcopt_up[j], measurements[0][m]):
#                    count_up[spc.index(spcopt_up[j])]=count_up[spc.index(spcopt_up[j])]+1
#            except:
#                pass
#    count_all_sol_up.append(count_up)
#    for j in range(len(spcopt_down)):
#        count_down[spc.index(spcopt_down[j])]=0
#        for m in range(len(measurements[0])):
#            try:
#                if nx.algorithms.shortest_paths.has_path(g, spcopt_down[j], measurements[0][m]):
#                    count_down[spc.index(spcopt_down[j])]=count_down[spc.index(spcopt_down[j])]+1
#            except:
#                pass
#    count_all_sol_down.append(count_down)
#
#f=open("Fxs_up.txt", 'w')
#for i in range(len(count_all_sol_up[0])):
#    f.write("%s\t" %spc[i])
#    for j in range(nsol):
#        f.write("%f\t" %count_all_sol_up[j][i])
#    f.write("\n")
#
#f.close()
#
#f=open("Fxs_down.txt", 'w')
#for i in range(len(count_all_sol_down[0])):
#    f.write("%s\t" %spc[i])
#    for j in range(nsol):
#        f.write("%f\t" %count_all_sol_down[j][i])
#    f.write("\n")
#
#f.close()
#
#
#avfx_up=[ float(sum( [ count_all_sol_up[i][j] for i in range(len(count_all_sol_up)) ]))/len(count_all_sol_up) for j in range(len(count_all_sol_up[0]))  ]
#avfx_down=[ float(sum( [ count_all_sol_down[i][j] for i in range(len(count_all_sol_down)) ]))/len(count_all_sol_down) for j in range(len(count_all_sol_down[0]))  ]
#
#
##spcd=dict((key, value) for key, value in [(spc[i], i) for i in range(len(spc))])
#
#f=open("avFxs_up.txt", 'w')
#[ f.write("%s\t%f\n" % (spc[i], avfx_up[i])) for i in range(len(spc)) if avfx_up[i]!=0.0 ]
#f.close()
#f=open("avFxs_down.txt", 'w')
#[ f.write("%s\t%f\n" % (spc[i], avfx_down[i])) for i in range(len(spc)) if avfx_down[i]!=0.0 ]
#f.close()
#
#
## calculate predicted and measured drug effects
#'''
#drugs=[sensitivities[0][i] for i in range(len(sensitivities[0]))]
#targets=[[targets[i][0]] + targets[i][1].split(",") for i in range(len(targets))]
#meas_score=[ [targets[j][0]] +  [sensitivities[1][sensitivities[0].index(targets[j][0])] ] for j in range(len(targets)) if targets[j][0] in sensitivities[0] ]    
#pred_score=[ [targets[j][0]] + [sum( [ avFx[i] for i in [ k for k in range(len(spc)) for targ in targets[j][1:] if spc[k][1]==targ  ] ] ) ] for j in range(len(targets))  ]
#
#
#
################### need to edit here #############################
#
#null=[float(meas_score[i][1]) for i in range(len(meas_score)) if meas_score[i][1]!="NaN" and meas_score[i][0] not in [ targets[j][0] for j in range(len(targets)) if targets[j][1]=="nan" ] and meas_score[i][0] not in [pred_score[j][0] for j in range(len(pred_score)) if pred_score[j][1] > 1.0 and pred_score[j][0] not in excl] ]
#
#hits=[float(meas_score[i][1]) for i in range(len(meas_score)) if meas_score[i][0] in  [pred_score[j][0] for j in range(len(pred_score)) if pred_score[j][1] > 1.0 and pred_score[j][0] not in excl] ]
#
#ttest=stats.ttest_ind(hits, null)
#f=open("ttest.res","w")
#f.write("t-test statistic:\t%f\n" % ttest[0])
#f.write("p-value for rejecting null hypothesis:\t%f" % ttest[1])
#f.close()
#
#'''
#'''
## do regression
#
#if len([ pred_score[i][1] for i in range(len(pred_score)) if pred_score[i][1]>0.0 ]) > 1:
#
#    regmat=[[meas_score[i][0], float(meas_score[i][1]), pred_score[j][1]] for i in range(len(meas_score)) for j in range(len(pred_score)) if (meas_score[i][0]==pred_score[j][0]) and (pred_score[j][1]>0.0) and (meas_score[i][1]!="NaN") and (meas_score[i][0] not in excl) ]
#    regmat_vals=[ [regmat[i][1], regmat[i][2] ] for i in range(len(regmat)) ]
#    reg=stats.linregress(np.array(regmat_vals)[:,0], np.array(regmat_vals)[:,1])
#    f=open("validation.txt", "w")
#    f.write("slope\t%f\n" % reg[0])
#    f.write("intercept\t%f\n" % reg[1])
#    f.write("correlation_coef\t%f\n" % reg[2])
#    f.write("p-value\t%f\n" % reg[3])
#    f.write("stderr\t%f\n" % reg[4])
#    f.close()
#
#''' 
#    
#    
#    
