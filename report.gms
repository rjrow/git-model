* Store file in a parameter structure using 5 dimensions




* -- results are in E+9 Mio US Doloar, convert to E+6 Mio Us Dollar to have
*    units as in GTAP data base
*
$setlocal scale 1000


   set  PQV /P "Price", Q "Quantity", V "Value", T "tax rate", G "Tax income","Meta" /;
   parameter p_results(*,pqv,*,*,*);


   set samRows / set.i,set.f /;
   set samCols / set.i,hou,gov,intTrs,set.s,inv /;
   set oris    / dom,imp,tot /;


* -------------------------------------------------------------------------------
*
*   Meta information on model structure and solve
*
* -------------------------------------------------------------------------------
*
   p_results("Wor","Meta","# of sectors","tots","dom")   = card(i);
   p_results("Wor","Meta","# of factors","tots","dom")   = card(f);
   p_results("Wor","Meta","# of regions","tots","dom")   = card(r);

   acronym MCP,CNS;
$iftheni %modeltype% == MCP

   p_results("Wor","Meta","Model type","tots","dom")   = MCP;
$else

   p_results("Wor","Meta","Model type","tots","dom")   = CNS;
$endif

$iftheni NOT %modeltye% == NONE


   p_results("Wor","Meta","# of equations","tots","dom") = %model%.numequ;
   p_results("Wor","Meta","# of variables","tots","dom") = %model%.numvar;
   p_results("Wor","Meta","SumInfes","tots","dom")       = %model%.sumInfes $ (%model%.sumInfes ne inf);
   p_results("Wor","Meta","NumInfes","tots","dom")       = %model%.numInfes $ (%model%.numInfes ne inf);
   p_results("Wor","Meta","# Iterations","tots","dom")                    = %model%.iterusd;
   p_results("Wor","Meta","# seconds solution time","tots","dom")         = %model%.resusd;

   acronym sluggish, mobile,fixed priced;


   p_results("Wor","Meta","Lab Market"      ,"tots","dom")         = %labmarket%    ;
   p_results("Wor","Meta","skl Market"      ,"tots","dom")         = %sklMarket%    ;
   p_results("Wor","Meta","capital Market"  ,"tots","dom")         = %capitalMarket%;
   p_results("Wor","Meta","res Market"      ,"tots","dom")         = %resMarket%    ;
   p_results("Wor","Meta","lnd Market"      ,"tots","dom")         = %lndMarket%    ;

$endif



* -------------------------------------------------------------------------------
*
*   First block: products
*
* -------------------------------------------------------------------------------
*
*  --- final demand by households
*
    p_results(r,"Q",i,"hou","dom")        = ddpm.l(i,r) * %scale%;
    p_results(r,"P",i,"hou","dom")        = p_dc.l(i,r);
    p_results(r,"T",i,"hou","dom")        = rtpd0(i,r);

    p_results(r,"Q",i,"hou","imp")        = dipm.l(i,r) * %scale%;
    p_results(r,"P",i,"hou","imp")        = p_ic.l(i,r);
    p_results(r,"T",i,"hou","imp")        = rtpi(i,r);
*
*  --- final demand by government
*
    p_results(r,"Q",i,"gov","dom")        = ddgm.l(i,r) * %scale%;
    p_results(r,"P",i,"gov","dom")        = p_dg.l(i,r);

    p_results(r,"Q",i,"gov","imp")        = digm.l(i,r) * %scale%;
    p_results(r,"P",i,"gov","imp")        = p_ig.l(i,r);
*
*  --- intermediate demand
*
    p_results(r,"Q",i,j,"dom")            = ddfm.l(i,j,r) * %scale%;
    p_results(r,"P",i,j,"dom")            = p_d.l(i,j,r);
    p_results(r,"T",i,j,"dom")            = rtfd0(i,j,r);

    p_results(r,"Q",i,j,"imp")            = difm.l(i,j,r) * %scale%;
    p_results(r,"P",i,j,"imp")            = p_i.l(i,j,r);
    p_results(r,"T",i,j,"imp")            = rtfi0(i,j,r);

*
*  --- final demand for international transport
*
    p_results(r,"Q",i,"intTrs","dom")     = dst.l(i,r) * %scale%;
*
*  --- investment demand
*
    p_results(r,"Q",i,"inv","dom")        = vdim(i,r) * %scale%;
    p_results(r,"P",i,"inv","dom")        = py.l(i,r);
*
*  --- exports
*
    p_results(r,"Q",i,s,"dom")            = dxmd.l(i,r,s) * %scale%;
    p_results(r,"P",i,s,"dom")            = py.l(i,r) * ( 1 + rtxs(i,s,r));
    p_results(r,"T",i,s,"dom")            = rtxs(i,r,s);
*
*  --- imports
*
    p_results(r,"Q",s,i,"imp")            = dxmd.l(i,s,r) * %scale%;
    p_results(r,"P",s,i,"imp")            = pyt_m.l(i,s,r);
    p_results(r,"T",s,i,"imp")            = rtms(i,s,r);
*
*   --- deliveries
*
    p_results(r,"Q",i,"dom","dom")        = y.l(i,r) * vom(i,r) * %scale%;
    p_results(r,"Q",i,"imp","imp")        = m.l(i,r) * vim(i,r) * %scale%;

* -------------------------------------------------------------------------------
*
*   Second block: Factors
*
* -------------------------------------------------------------------------------

*
*  --- primary factor demand
*
    p_results(r,"Q",f,j,"dom")         = dfm.l(f,j,r) * %scale%;
    p_results(r,"P",f,j,"dom")         = (pf.l(f,r) $ (mf(f) or fpf(f)) + ps.l(f,j,r) $ sf(f));
    p_results(r,"T",f,j,"dom")         = rtf(f,j,r);
*
*  --- bilateral trade margins for exports
*
    p_results(r,"P",j,i,s)            = vtwr(j,i,r,s) * (1 + dtwr.l(j,i,r,s));

* -------------------------------------------------------------------------------
*
*   Third block: Sectors
*
* -------------------------------------------------------------------------------

*
*  --- total output
*
    p_results(r,"Q","out",j,"dom") = p_results(r,"Q",j,"dom","dom");
    p_results(r,"P","out",j,"dom") = py.l(j,r);
    p_results(r,"T","out",j,"dom") = rto(j,r);

* -------------------------------------------------------------------------------
*
*   Fourth block: Goverment, Household, regional agent
*
* -------------------------------------------------------------------------------
*
*  --- factor income
*
    p_results(r,"V","ra","totf","dom") = sum( (f,j), dfm.l(f,j,r)*pf.l(f,r));
    p_results(r,"V","ra","vb","dom")   = sum(rnum, pc.l(rnum)*vb(r));
    p_results(r,"V","ra","o","dom")    =    revto.l(r);
    p_results(r,"V","ra","fd","dom")   =    revtfd.l(r);
    p_results(r,"V","ra","fi","dom")   =    revtfi.l(r);
    p_results(r,"V","ra","f","dom")    =    revtf.l(r);
    p_results(r,"V","ra","pd","dom")   =    revtpd.l(r);
    p_results(r,"V","ra","pi","dom")   =    revtpi.l(r);
    p_results(r,"V","ra","gd","dom")   =    revtgd.l(r);
    p_results(r,"V","ra","gi","dom")   =    revtgi.l(r);
    p_results(r,"V","ra","xs","dom")   =    revtxs.l(r);
    p_results(r,"V","ra","ms","dom")   =    revtms.l(r);

    set incPos / totf,vb,o,fd,fi,f,pd,pi,gd,gi,xs,ms /;

    set taxPos / o,fd,fi,f,pd,pi,gd,gi,xs,ms /;

    p_results(r,"V","ra",incPos,"dom")  = p_results(r,"V","ra",incPos,"dom") * %scale%;

    p_results(r,"V","ra","tax","dom")  = sum(taxPos, p_results(r,"V","ra",taxPos,"dom"));
    p_results(r,"V","ra","inc","dom")  = sum(incPos, p_results(r,"V","ra",incPos,"dom"));
    p_results(r,"V","ra","c","dom")    = c.l(r) * vpm(r) * %scale%;
    p_results(r,"V","ra","pc","dom")   = pc.l(r);
    p_results(r,"V","ra","g","dom")    = g.l(r) * vgm(r) * %scale%;
    p_results(r,"V","ra","cg","dom")   = p_results(r,"V","ra","c","dom") + p_results(r,"V","ra","g","dom");

    set addPos / tax,inc,g,c,cg /;
    incPos(addPos) = yes;


* -------------------------------------------------------------------------------
*
*   Calculate values and tax income
*
* -------------------------------------------------------------------------------

    samRows("out") = yes;
    samRows(s)     = yes;
    samCols("imp") = yes;
    samCols("dom") = yes;

*
*   --- calculate values (Q*P)
*
    p_results(r,"V",samRows,SamCols,oris)
        = p_results(r,"Q",samRows,SamCols,oris) * p_results(r,"P",samRows,SamCols,oris);
*
*   --- calculate prices if missing as V/Q, that happens for
*
    p_results(r,"P",samRows,SamCols,oris) $ ( p_results(r,"Q",samRows,SamCols,oris)
                                             $ (not p_results(r,"P",samRows,SamCols,oris)))
        = p_results(r,"V",samRows,SamCols,oris) / p_results(r,"Q",samRows,SamCols,oris);

*
*   --- calculate tax income from value and ad-valorem rate
*
    p_results(r,"G",samRows,SamCols,oris)
        = p_results(r,"V",samRows,SamCols,oris) * p_results(r,"T",samRows,SamCols,oris);

*
*   --- aggregate over origins (domestic and imported) to totals
*
    set agg(PQV) / Q,V,G /;

    p_results(r,AGG,samRows,SamCols,"tot")
        = p_results(r,AGG,samRows,SamCols,"dom") + p_results(r,AGG,samRows,SamCols,"imp");

    p_results(r,"P",samRows,SamCols,"tot") $ p_results(r,"Q",samRows,SamCols,"tot")
     = p_results(r,"V",samRows,SamCols,"tot") / p_results(r,"Q",samRows,SamCols,"tot");

    p_results(r,"T",samRows,SamCols,"tot") $ p_results(r,"V",samRows,SamCols,"tot")
     = p_results(r,"G",samRows,SamCols,"tot") / p_results(r,"V",samRows,SamCols,"tot");

* -------------------------------------------------------------------------------
*
*   Aggregations of items
*
* -------------------------------------------------------------------------------

*
*   --- aggregate to total use
*

    set useCols / set.i,hou,gov,intTrs,set.s,inv /;

    p_results(r,agg,i,"use",oris)         = sum(useCols, p_results(r,agg,i,useCols,oris));

    p_results(r,"P",i,"use",oris) $ p_results(r,"Q",i,"use",oris)
       = p_results(r,"V",i,"use",oris) / p_results(r,"Q",i,"use",oris);

    p_results(r,"T",i,"use",oris) $ p_results(r,"V",i,"use",oris)
       = p_results(r,"G",i,"use",oris) / p_results(r,"V",i,"use",oris);
*
*   --- aggregate intermediate demand by productm, by summing up
*       over sectors
*
    p_results(r,agg,i,"int",oris)         = sum(j, p_results(r,agg,i,j,oris));

    p_results(r,"P",i,"int",oris) $ p_results(r,"Q",i,"int",oris)
       = p_results(r,"V",i,"int",oris) / p_results(r,"Q",i,"int",oris);

    p_results(r,"T",i,"int",oris) $ p_results(r,"V",i,"int",oris)
       = p_results(r,"G",i,"int",oris) / p_results(r,"V",i,"int",oris);

*
*   --- aggregate factor demand  by sectors  to "totf"
*
    p_results(r,agg,"totf",j,oris)         = sum(f, p_results(r,agg,f,j,oris));

    p_results(r,"P","totf",j,oris) $ p_results(r,"Q","totf",j,oris)
       = p_results(r,"V","totf",j,oris) / p_results(r,"Q","totf",j,oris);

    p_results(r,"T","totf",j,oris) $ p_results(r,"V","totf",j,oris)
       = p_results(r,"G","totf",j,oris) / p_results(r,"V","totf",j,oris);
*
*   --- aggregate intermediate demand  by sectors
*
    p_results(r,agg,"int",j,oris)         = sum(i, p_results(r,agg,i,j,oris));

    p_results(r,"Q","OutTax",j,oris)      = p_results(r,"G","out",j,oris);
    p_results(r,"V","OutTax",j,oris)      = p_results(r,"G","out",j,oris);

    p_results(r,"Q","facTax",j,oris)      = sum(f, p_results(r,"G",f,j,oris));
    p_results(r,"V","facTax",j,oris)      = p_results(r,"Q","facTax",j,oris);

    p_results(r,"Q","intTax",j,oris)      = p_results(r,"G","int",j,oris);
    p_results(r,"V","intTax",j,oris)      = p_results(r,"Q","intTax",j,oris);

    p_results(r,"P","int",j,oris) $ p_results(r,"Q","int",j,oris)
       = p_results(r,"V","int",j,oris) / p_results(r,"Q","int",j,oris);

    p_results(r,"T","int",j,oris) $ p_results(r,"V","int",j,oris)
       = p_results(r,"G","int",j,oris) / p_results(r,"V","int",j,oris);

*
*   --- aggregate exports from single destinations s
*
    p_results(r,agg,i,"exp",oris)         = sum(s, p_results(r,agg,i,s,oris));

    p_results(r,"P",i,"exp",oris) $ p_results(r,"Q",i,"exp",oris)
       = p_results(r,"V",i,"exp",oris) / p_results(r,"Q",i,"exp",oris);

    p_results(r,"T",i,"exp",oris) $ p_results(r,"V",i,"exp",oris)
       = p_results(r,"G",i,"exp",oris) / p_results(r,"V",i,"exp",oris);
*
*   --- aggregate import
*
    p_results(r,agg,"imp",i,oris)         = sum(s, p_results(r,agg,i,s,oris));

    p_results(r,"P","imp",i,oris) $ p_results(r,"Q","imp",i,oris)
       = p_results(r,"V","imp",i,oris) / p_results(r,"Q","imp",i,oris);

    p_results(r,"T","imp",i,oris) $ p_results(r,"V","imp",i,oris)
       = p_results(r,"G","imp",i,oris) / p_results(r,"V","imp",i,oris);

* -------------------------------------------------------------------------------
*
*   Aggregation across products to "totP"
*
* -------------------------------------------------------------------------------


    samRows("outTax") = Yes;
    samRows("facTax") = Yes;
    samRows("intTax") = Yes;
    samRows("int") = Yes;
    samRows("totf") = Yes;

    set aggItemsC(*);
    aggItemsC(samCols) = yes;
    aggItemsC("int")   = Yes;
    aggItemsC("exp")   = Yes;
    aggItemsC("del")   = Yes;
    aggItemsC("imp")   = Yes;
    aggItemsC("dom")   = Yes;
    aggItemsC("use")   = Yes;
    aggItemsC(r)       = Yes;

    p_results(r,agg,"totp",aggItemsC,oris) = sum(i,p_results(r,agg,i,aggItemsC,oris));

    p_results(r,"P","totp",aggItemsC,oris) $ p_results(r,"Q","totp",aggItemsC,oris)
      = p_results(r,"V","totp",aggItemsC,oris) / p_results(r,"Q","totp",aggItemsC,oris);

    p_results(r,"T","totp",aggItemsC,oris) $ p_results(r,"V","totp",aggItemsC,oris)
      = p_results(r,"G","totp",aggItemsC,oris) / p_results(r,"V","totp",aggItemsC,oris);


* -------------------------------------------------------------------------------
*
*   Aggregation across sectors "totS"
*
* -------------------------------------------------------------------------------

    set aggItemsR(*);
    aggItemsR(samRows) = yes;
    aggItemsR(r)       = Yes;
    aggItemsR("totP")  = Yes;

    p_results(r,agg,aggItemsR,"totS",oris) = sum(j,p_results(r,agg,aggItemsR,j,oris));

    p_results(r,"P",aggItemsR,"totS",oris) $ p_results(r,"Q",aggItemsR,"totS",oris)
      = p_results(r,"V",aggItemsR,"totS",oris) / p_results(r,"Q",aggItemsR,"totS",oris);

    p_results(r,"T",aggItemsR,"totS",oris) $ p_results(r,"V",aggItemsR,"totS",oris)
      = p_results(r,"G",aggItemsR,"totS",oris) / p_results(r,"V",aggItemsR,"totS",oris);


* -------------------------------------------------------------------------------
*
*   Aggregation to world
*
* -------------------------------------------------------------------------------

    aggItemsC("totS") = YES;

    p_results("wor",agg,aggItemsR,aggItemsC,oris) = sum(r,p_results(r,agg,aggItemsR,aggItemsC,oris));

    p_results("wor","P",aggItemsR,aggItemsC,oris) $ p_results("wor","Q",aggItemsR,aggItemsC,oris)
      = p_results("wor","V",aggItemsR,aggItemsC,oris) / p_results("wor","Q",aggItemsR,aggItemsC,oris);

    p_results("wor","T",aggItemsR,aggItemsC,oris) $ p_results("wor","V",aggItemsR,aggItemsC,oris)
      = p_results("wor","G",aggItemsR,aggItemsC,oris) / p_results("wor","V",aggItemsR,aggItemsC,oris);

    p_results("wor","V","ra",incPos,"dom") = sum(r, p_results(r,"V","ra",incPos,"dom"));

* -------------------------------------------------------------------------------
*
*   Output to GDX
*
* -------------------------------------------------------------------------------

   execute_unload "%resdirR%\%shock%.gdx";




