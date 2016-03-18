m.employ=lm(Xt2~Xt1,data= cbindata)
summary(m.employ)
employ.res = m.employ$res