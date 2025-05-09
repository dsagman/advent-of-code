:- initialization(main).
:- table eval/2.
eval(a,A) :- eval(lx,A).
eval(b,19138).
eval(c,0).
eval(d,D) :- eval(b,B), D is B >> 2.
eval(e,E) :- eval(b,B), E is B >> 3.
eval(f,F) :- eval(b,B), F is B >> 5.
eval(g,G) :- eval(e,E), eval(f,F), G is E \/ F.
eval(h,H) :- eval(e,E), eval(f,F), H is E /\ F.
eval(i,I) :- eval(h,H), I is \H.
eval(j,J) :- eval(g,G), eval(i,I), J is G /\ I.
eval(k,K) :- eval(d,D), eval(j,J), K is D \/ J.
eval(l,L) :- eval(d,D), eval(j,J), L is D /\ J.
eval(m,M) :- eval(l,L), M is \L.
eval(n,N) :- eval(k,K), eval(m,M), N is K /\ M.
eval(o,O) :- eval(b,B), eval(n,N), O is B \/ N.
eval(p,P) :- eval(b,B), eval(n,N), P is B /\ N.
eval(q,Q) :- eval(p,P), Q is \P.
eval(r,R) :- eval(o,O), eval(q,Q), R is O /\ Q.
eval(s,S) :- eval(r,R), S is 1 /\ R.
eval(t,T) :- eval(c,C), T is C << 1.
eval(u,U) :- eval(t,T), eval(s,S), U is T \/ S.
eval(v,V) :- eval(b,B), V is B >> 1.
eval(w,W) :- eval(s,S), W is S << 15.
eval(x,X) :- eval(v,V), eval(w,W), X is V \/ W.
eval(y,Y) :- eval(x,X), Y is X >> 2.
eval(z,Z) :- eval(x,X), Z is X >> 3.
eval(aa,AA) :- eval(x,X), AA is X >> 5.
eval(ab,AB) :- eval(z,Z), eval(aa,AA), AB is Z \/ AA.
eval(ac,AC) :- eval(z,Z), eval(aa,AA), AC is Z /\ AA.
eval(ad,AD) :- eval(ac,AC), AD is \AC.
eval(ae,AE) :- eval(ab,AB), eval(ad,AD), AE is AB /\ AD.
eval(af,AF) :- eval(y,Y), eval(ae,AE), AF is Y \/ AE.
eval(ag,AG) :- eval(y,Y), eval(ae,AE), AG is Y /\ AE.
eval(ah,AH) :- eval(ag,AG), AH is \AG.
eval(ai,AI) :- eval(af,AF), eval(ah,AH), AI is AF /\ AH.
eval(aj,AJ) :- eval(x,X), eval(ai,AI), AJ is X \/ AI.
eval(ak,AK) :- eval(x,X), eval(ai,AI), AK is X /\ AI.
eval(al,AL) :- eval(ak,AK), AL is \AK.
eval(am,AM) :- eval(aj,AJ), eval(al,AL), AM is AJ /\ AL.
eval(an,AN) :- eval(am,AM), AN is 1 /\ AM.
eval(ao,AO) :- eval(u,U), AO is U << 1.
eval(ap,AP) :- eval(ao,AO), eval(an,AN), AP is AO \/ AN.
eval(aq,AQ) :- eval(x,X), AQ is X >> 1.
eval(ar,AR) :- eval(an,AN), AR is AN << 15.
eval(as,AS) :- eval(aq,AQ), eval(ar,AR), AS is AQ \/ AR.
eval(at,AT) :- eval(as,AS), AT is AS >> 2.
eval(au,AU) :- eval(as,AS), AU is AS >> 3.
eval(av,AV) :- eval(as,AS), AV is AS >> 5.
eval(aw,AW) :- eval(au,AU), eval(av,AV), AW is AU \/ AV.
eval(ax,AX) :- eval(au,AU), eval(av,AV), AX is AU /\ AV.
eval(ay,AY) :- eval(ax,AX), AY is \AX.
eval(az,AZ) :- eval(aw,AW), eval(ay,AY), AZ is AW /\ AY.
eval(ba,BA) :- eval(at,AT), eval(az,AZ), BA is AT \/ AZ.
eval(bb,BB) :- eval(at,AT), eval(az,AZ), BB is AT /\ AZ.
eval(bc,BC) :- eval(bb,BB), BC is \BB.
eval(bd,BD) :- eval(ba,BA), eval(bc,BC), BD is BA /\ BC.
eval(be,BE) :- eval(as,AS), eval(bd,BD), BE is AS \/ BD.
eval(bf,BF) :- eval(as,AS), eval(bd,BD), BF is AS /\ BD.
eval(bg,BG) :- eval(bf,BF), BG is \BF.
eval(bh,BH) :- eval(be,BE), eval(bg,BG), BH is BE /\ BG.
eval(bi,BI) :- eval(bh,BH), BI is 1 /\ BH.
eval(bj,BJ) :- eval(ap,AP), BJ is AP << 1.
eval(bk,BK) :- eval(bj,BJ), eval(bi,BI), BK is BJ \/ BI.
eval(bl,BL) :- eval(as,AS), BL is AS >> 1.
eval(bm,BM) :- eval(bi,BI), BM is BI << 15.
eval(bn,BN) :- eval(bl,BL), eval(bm,BM), BN is BL \/ BM.
eval(bo,BO) :- eval(bn,BN), BO is BN >> 2.
eval(bp,BP) :- eval(bn,BN), BP is BN >> 3.
eval(bq,BQ) :- eval(bn,BN), BQ is BN >> 5.
eval(br,BR) :- eval(bp,BP), eval(bq,BQ), BR is BP \/ BQ.
eval(bs,BS) :- eval(bp,BP), eval(bq,BQ), BS is BP /\ BQ.
eval(bt,BT) :- eval(bs,BS), BT is \BS.
eval(bu,BU) :- eval(br,BR), eval(bt,BT), BU is BR /\ BT.
eval(bv,BV) :- eval(bo,BO), eval(bu,BU), BV is BO \/ BU.
eval(bw,BW) :- eval(bo,BO), eval(bu,BU), BW is BO /\ BU.
eval(bx,BX) :- eval(bw,BW), BX is \BW.
eval(by,BY) :- eval(bv,BV), eval(bx,BX), BY is BV /\ BX.
eval(bz,BZ) :- eval(bn,BN), eval(by,BY), BZ is BN \/ BY.
eval(ca,CA) :- eval(bn,BN), eval(by,BY), CA is BN /\ BY.
eval(cb,CB) :- eval(ca,CA), CB is \CA.
eval(cc,CC) :- eval(bz,BZ), eval(cb,CB), CC is BZ /\ CB.
eval(cd,CD) :- eval(cc,CC), CD is 1 /\ CC.
eval(ce,CE) :- eval(bk,BK), CE is BK << 1.
eval(cf,CF) :- eval(ce,CE), eval(cd,CD), CF is CE \/ CD.
eval(cg,CG) :- eval(bn,BN), CG is BN >> 1.
eval(ch,CH) :- eval(cd,CD), CH is CD << 15.
eval(ci,CI) :- eval(cg,CG), eval(ch,CH), CI is CG \/ CH.
eval(cj,CJ) :- eval(ci,CI), CJ is CI >> 2.
eval(ck,CK) :- eval(ci,CI), CK is CI >> 3.
eval(cl,CL) :- eval(ci,CI), CL is CI >> 5.
eval(cm,CM) :- eval(ck,CK), eval(cl,CL), CM is CK \/ CL.
eval(cn,CN) :- eval(ck,CK), eval(cl,CL), CN is CK /\ CL.
eval(co,CO) :- eval(cn,CN), CO is \CN.
eval(cp,CP) :- eval(cm,CM), eval(co,CO), CP is CM /\ CO.
eval(cq,CQ) :- eval(cj,CJ), eval(cp,CP), CQ is CJ \/ CP.
eval(cr,CR) :- eval(cj,CJ), eval(cp,CP), CR is CJ /\ CP.
eval(cs,CS) :- eval(cr,CR), CS is \CR.
eval(ct,CT) :- eval(cq,CQ), eval(cs,CS), CT is CQ /\ CS.
eval(cu,CU) :- eval(ci,CI), eval(ct,CT), CU is CI \/ CT.
eval(cv,CV) :- eval(ci,CI), eval(ct,CT), CV is CI /\ CT.
eval(cw,CW) :- eval(cv,CV), CW is \CV.
eval(cx,CX) :- eval(cu,CU), eval(cw,CW), CX is CU /\ CW.
eval(cy,CY) :- eval(cx,CX), CY is 1 /\ CX.
eval(cz,CZ) :- eval(cf,CF), CZ is CF << 1.
eval(da,DA) :- eval(cz,CZ), eval(cy,CY), DA is CZ \/ CY.
eval(db,DB) :- eval(ci,CI), DB is CI >> 1.
eval(dc,DC) :- eval(cy,CY), DC is CY << 15.
eval(dd,DD) :- eval(db,DB), eval(dc,DC), DD is DB \/ DC.
eval(de,DE) :- eval(dd,DD), DE is DD >> 2.
eval(df,DF) :- eval(dd,DD), DF is DD >> 3.
eval(dg,DG) :- eval(dd,DD), DG is DD >> 5.
eval(dh,DH) :- eval(df,DF), eval(dg,DG), DH is DF \/ DG.
eval(di,DI) :- eval(df,DF), eval(dg,DG), DI is DF /\ DG.
eval(dj,DJ) :- eval(di,DI), DJ is \DI.
eval(dk,DK) :- eval(dh,DH), eval(dj,DJ), DK is DH /\ DJ.
eval(dl,DL) :- eval(de,DE), eval(dk,DK), DL is DE \/ DK.
eval(dm,DM) :- eval(de,DE), eval(dk,DK), DM is DE /\ DK.
eval(dn,DN) :- eval(dm,DM), DN is \DM.
eval(do,DO) :- eval(dl,DL), eval(dn,DN), DO is DL /\ DN.
eval(dp,DP) :- eval(dd,DD), eval(do,DO), DP is DD \/ DO.
eval(dq,DQ) :- eval(dd,DD), eval(do,DO), DQ is DD /\ DO.
eval(dr,DR) :- eval(dq,DQ), DR is \DQ.
eval(ds,DS) :- eval(dp,DP), eval(dr,DR), DS is DP /\ DR.
eval(dt,DT) :- eval(ds,DS), DT is 1 /\ DS.
eval(du,DU) :- eval(da,DA), DU is DA << 1.
eval(dv,DV) :- eval(du,DU), eval(dt,DT), DV is DU \/ DT.
eval(dw,DW) :- eval(dd,DD), DW is DD >> 1.
eval(dx,DX) :- eval(dt,DT), DX is DT << 15.
eval(dy,DY) :- eval(dw,DW), eval(dx,DX), DY is DW \/ DX.
eval(dz,DZ) :- eval(dy,DY), DZ is DY >> 2.
eval(ea,EA) :- eval(dy,DY), EA is DY >> 3.
eval(eb,EB) :- eval(dy,DY), EB is DY >> 5.
eval(ec,EC) :- eval(ea,EA), eval(eb,EB), EC is EA \/ EB.
eval(ed,ED) :- eval(ea,EA), eval(eb,EB), ED is EA /\ EB.
eval(ee,EE) :- eval(ed,ED), EE is \ED.
eval(ef,EF) :- eval(ec,EC), eval(ee,EE), EF is EC /\ EE.
eval(eg,EG) :- eval(dz,DZ), eval(ef,EF), EG is DZ \/ EF.
eval(eh,EH) :- eval(dz,DZ), eval(ef,EF), EH is DZ /\ EF.
eval(ei,EI) :- eval(eh,EH), EI is \EH.
eval(ej,EJ) :- eval(eg,EG), eval(ei,EI), EJ is EG /\ EI.
eval(ek,EK) :- eval(dy,DY), eval(ej,EJ), EK is DY \/ EJ.
eval(el,EL) :- eval(dy,DY), eval(ej,EJ), EL is DY /\ EJ.
eval(em,EM) :- eval(el,EL), EM is \EL.
eval(en,EN) :- eval(ek,EK), eval(em,EM), EN is EK /\ EM.
eval(eo,EO) :- eval(en,EN), EO is 1 /\ EN.
eval(ep,EP) :- eval(dv,DV), EP is DV << 1.
eval(eq,EQ) :- eval(ep,EP), eval(eo,EO), EQ is EP \/ EO.
eval(er,ER) :- eval(dy,DY), ER is DY >> 1.
eval(es,ES) :- eval(eo,EO), ES is EO << 15.
eval(et,ET) :- eval(er,ER), eval(es,ES), ET is ER \/ ES.
eval(eu,EU) :- eval(et,ET), EU is ET >> 2.
eval(ev,EV) :- eval(et,ET), EV is ET >> 3.
eval(ew,EW) :- eval(et,ET), EW is ET >> 5.
eval(ex,EX) :- eval(ev,EV), eval(ew,EW), EX is EV \/ EW.
eval(ey,EY) :- eval(ev,EV), eval(ew,EW), EY is EV /\ EW.
eval(ez,EZ) :- eval(ey,EY), EZ is \EY.
eval(fa,FA) :- eval(ex,EX), eval(ez,EZ), FA is EX /\ EZ.
eval(fb,FB) :- eval(eu,EU), eval(fa,FA), FB is EU \/ FA.
eval(fc,FC) :- eval(eu,EU), eval(fa,FA), FC is EU /\ FA.
eval(fd,FD) :- eval(fc,FC), FD is \FC.
eval(fe,FE) :- eval(fb,FB), eval(fd,FD), FE is FB /\ FD.
eval(ff,FF) :- eval(et,ET), eval(fe,FE), FF is ET \/ FE.
eval(fg,FG) :- eval(et,ET), eval(fe,FE), FG is ET /\ FE.
eval(fh,FH) :- eval(fg,FG), FH is \FG.
eval(fi,FI) :- eval(ff,FF), eval(fh,FH), FI is FF /\ FH.
eval(fj,FJ) :- eval(fi,FI), FJ is 1 /\ FI.
eval(fk,FK) :- eval(eq,EQ), FK is EQ << 1.
eval(fl,FL) :- eval(fk,FK), eval(fj,FJ), FL is FK \/ FJ.
eval(fm,FM) :- eval(et,ET), FM is ET >> 1.
eval(fn,FN) :- eval(fj,FJ), FN is FJ << 15.
eval(fo,FO) :- eval(fm,FM), eval(fn,FN), FO is FM \/ FN.
eval(fp,FP) :- eval(fo,FO), FP is FO >> 2.
eval(fq,FQ) :- eval(fo,FO), FQ is FO >> 3.
eval(fr,FR) :- eval(fo,FO), FR is FO >> 5.
eval(fs,FS) :- eval(fq,FQ), eval(fr,FR), FS is FQ \/ FR.
eval(ft,FT) :- eval(fq,FQ), eval(fr,FR), FT is FQ /\ FR.
eval(fu,FU) :- eval(ft,FT), FU is \FT.
eval(fv,FV) :- eval(fs,FS), eval(fu,FU), FV is FS /\ FU.
eval(fw,FW) :- eval(fp,FP), eval(fv,FV), FW is FP \/ FV.
eval(fx,FX) :- eval(fp,FP), eval(fv,FV), FX is FP /\ FV.
eval(fy,FY) :- eval(fx,FX), FY is \FX.
eval(fz,FZ) :- eval(fw,FW), eval(fy,FY), FZ is FW /\ FY.
eval(ga,GA) :- eval(fo,FO), eval(fz,FZ), GA is FO \/ FZ.
eval(gb,GB) :- eval(fo,FO), eval(fz,FZ), GB is FO /\ FZ.
eval(gc,GC) :- eval(gb,GB), GC is \GB.
eval(gd,GD) :- eval(ga,GA), eval(gc,GC), GD is GA /\ GC.
eval(ge,GE) :- eval(gd,GD), GE is 1 /\ GD.
eval(gf,GF) :- eval(fl,FL), GF is FL << 1.
eval(gg,GG) :- eval(gf,GF), eval(ge,GE), GG is GF \/ GE.
eval(gh,GH) :- eval(fo,FO), GH is FO >> 1.
eval(gi,GI) :- eval(ge,GE), GI is GE << 15.
eval(gj,GJ) :- eval(gh,GH), eval(gi,GI), GJ is GH \/ GI.
eval(gk,GK) :- eval(gj,GJ), GK is GJ >> 2.
eval(gl,GL) :- eval(gj,GJ), GL is GJ >> 3.
eval(gm,GM) :- eval(gj,GJ), GM is GJ >> 5.
eval(gn,GN) :- eval(gl,GL), eval(gm,GM), GN is GL \/ GM.
eval(go,GO) :- eval(gl,GL), eval(gm,GM), GO is GL /\ GM.
eval(gp,GP) :- eval(go,GO), GP is \GO.
eval(gq,GQ) :- eval(gn,GN), eval(gp,GP), GQ is GN /\ GP.
eval(gr,GR) :- eval(gk,GK), eval(gq,GQ), GR is GK \/ GQ.
eval(gs,GS) :- eval(gk,GK), eval(gq,GQ), GS is GK /\ GQ.
eval(gt,GT) :- eval(gs,GS), GT is \GS.
eval(gu,GU) :- eval(gr,GR), eval(gt,GT), GU is GR /\ GT.
eval(gv,GV) :- eval(gj,GJ), eval(gu,GU), GV is GJ \/ GU.
eval(gw,GW) :- eval(gj,GJ), eval(gu,GU), GW is GJ /\ GU.
eval(gx,GX) :- eval(gw,GW), GX is \GW.
eval(gy,GY) :- eval(gv,GV), eval(gx,GX), GY is GV /\ GX.
eval(gz,GZ) :- eval(gy,GY), GZ is 1 /\ GY.
eval(ha,HA) :- eval(gg,GG), HA is GG << 1.
eval(hb,HB) :- eval(ha,HA), eval(gz,GZ), HB is HA \/ GZ.
eval(hc,HC) :- eval(gj,GJ), HC is GJ >> 1.
eval(hd,HD) :- eval(gz,GZ), HD is GZ << 15.
eval(he,HE) :- eval(hc,HC), eval(hd,HD), HE is HC \/ HD.
eval(hf,HF) :- eval(he,HE), HF is HE >> 2.
eval(hg,HG) :- eval(he,HE), HG is HE >> 3.
eval(hh,HH) :- eval(he,HE), HH is HE >> 5.
eval(hi,HI) :- eval(hg,HG), eval(hh,HH), HI is HG \/ HH.
eval(hj,HJ) :- eval(hg,HG), eval(hh,HH), HJ is HG /\ HH.
eval(hk,HK) :- eval(hj,HJ), HK is \HJ.
eval(hl,HL) :- eval(hi,HI), eval(hk,HK), HL is HI /\ HK.
eval(hm,HM) :- eval(hf,HF), eval(hl,HL), HM is HF \/ HL.
eval(hn,HN) :- eval(hf,HF), eval(hl,HL), HN is HF /\ HL.
eval(ho,HO) :- eval(hn,HN), HO is \HN.
eval(hp,HP) :- eval(hm,HM), eval(ho,HO), HP is HM /\ HO.
eval(hq,HQ) :- eval(he,HE), eval(hp,HP), HQ is HE \/ HP.
eval(hr,HR) :- eval(he,HE), eval(hp,HP), HR is HE /\ HP.
eval(hs,HS) :- eval(hr,HR), HS is \HR.
eval(ht,HT) :- eval(hq,HQ), eval(hs,HS), HT is HQ /\ HS.
eval(hu,HU) :- eval(ht,HT), HU is 1 /\ HT.
eval(hv,HV) :- eval(hb,HB), HV is HB << 1.
eval(hw,HW) :- eval(hv,HV), eval(hu,HU), HW is HV \/ HU.
eval(hx,HX) :- eval(he,HE), HX is HE >> 1.
eval(hy,HY) :- eval(hu,HU), HY is HU << 15.
eval(hz,HZ) :- eval(hx,HX), eval(hy,HY), HZ is HX \/ HY.
eval(ia,IA) :- eval(hz,HZ), IA is HZ >> 2.
eval(ib,IB) :- eval(hz,HZ), IB is HZ >> 3.
eval(ic,IC) :- eval(hz,HZ), IC is HZ >> 5.
eval(id,ID) :- eval(ib,IB), eval(ic,IC), ID is IB \/ IC.
eval(ie,IE) :- eval(ib,IB), eval(ic,IC), IE is IB /\ IC.
eval(if,IF) :- eval(ie,IE), IF is \IE.
eval(ig,IG) :- eval(id,ID), eval(if,IF), IG is ID /\ IF.
eval(ih,IH) :- eval(ia,IA), eval(ig,IG), IH is IA \/ IG.
eval(ii,II) :- eval(ia,IA), eval(ig,IG), II is IA /\ IG.
eval(ij,IJ) :- eval(ii,II), IJ is \II.
eval(ik,IK) :- eval(ih,IH), eval(ij,IJ), IK is IH /\ IJ.
eval(il,IL) :- eval(hz,HZ), eval(ik,IK), IL is HZ \/ IK.
eval(im,IM) :- eval(hz,HZ), eval(ik,IK), IM is HZ /\ IK.
eval(in,IN) :- eval(im,IM), IN is \IM.
eval(io,IO) :- eval(il,IL), eval(in,IN), IO is IL /\ IN.
eval(ip,IP) :- eval(io,IO), IP is 1 /\ IO.
eval(iq,IQ) :- eval(hw,HW), IQ is HW << 1.
eval(ir,IR) :- eval(iq,IQ), eval(ip,IP), IR is IQ \/ IP.
eval(is,IS) :- eval(hz,HZ), IS is HZ >> 1.
eval(it,IT) :- eval(ip,IP), IT is IP << 15.
eval(iu,IU) :- eval(is,IS), eval(it,IT), IU is IS \/ IT.
eval(iv,IV) :- eval(iu,IU), IV is IU >> 2.
eval(iw,IW) :- eval(iu,IU), IW is IU >> 3.
eval(ix,IX) :- eval(iu,IU), IX is IU >> 5.
eval(iy,IY) :- eval(iw,IW), eval(ix,IX), IY is IW \/ IX.
eval(iz,IZ) :- eval(iw,IW), eval(ix,IX), IZ is IW /\ IX.
eval(ja,JA) :- eval(iz,IZ), JA is \IZ.
eval(jb,JB) :- eval(iy,IY), eval(ja,JA), JB is IY /\ JA.
eval(jc,JC) :- eval(iv,IV), eval(jb,JB), JC is IV \/ JB.
eval(jd,JD) :- eval(iv,IV), eval(jb,JB), JD is IV /\ JB.
eval(je,JE) :- eval(jd,JD), JE is \JD.
eval(jf,JF) :- eval(jc,JC), eval(je,JE), JF is JC /\ JE.
eval(jg,JG) :- eval(iu,IU), eval(jf,JF), JG is IU \/ JF.
eval(jh,JH) :- eval(iu,IU), eval(jf,JF), JH is IU /\ JF.
eval(ji,JI) :- eval(jh,JH), JI is \JH.
eval(jj,JJ) :- eval(jg,JG), eval(ji,JI), JJ is JG /\ JI.
eval(jk,JK) :- eval(jj,JJ), JK is 1 /\ JJ.
eval(jl,JL) :- eval(ir,IR), JL is IR << 1.
eval(jm,JM) :- eval(jl,JL), eval(jk,JK), JM is JL \/ JK.
eval(jn,JN) :- eval(iu,IU), JN is IU >> 1.
eval(jo,JO) :- eval(jk,JK), JO is JK << 15.
eval(jp,JP) :- eval(jn,JN), eval(jo,JO), JP is JN \/ JO.
eval(jq,JQ) :- eval(jp,JP), JQ is JP >> 2.
eval(jr,JR) :- eval(jp,JP), JR is JP >> 3.
eval(js,JS) :- eval(jp,JP), JS is JP >> 5.
eval(jt,JT) :- eval(jr,JR), eval(js,JS), JT is JR \/ JS.
eval(ju,JU) :- eval(jr,JR), eval(js,JS), JU is JR /\ JS.
eval(jv,JV) :- eval(ju,JU), JV is \JU.
eval(jw,JW) :- eval(jt,JT), eval(jv,JV), JW is JT /\ JV.
eval(jx,JX) :- eval(jq,JQ), eval(jw,JW), JX is JQ \/ JW.
eval(jy,JY) :- eval(jq,JQ), eval(jw,JW), JY is JQ /\ JW.
eval(jz,JZ) :- eval(jy,JY), JZ is \JY.
eval(ka,KA) :- eval(jx,JX), eval(jz,JZ), KA is JX /\ JZ.
eval(kb,KB) :- eval(jp,JP), eval(ka,KA), KB is JP \/ KA.
eval(kc,KC) :- eval(jp,JP), eval(ka,KA), KC is JP /\ KA.
eval(kd,KD) :- eval(kc,KC), KD is \KC.
eval(ke,KE) :- eval(kb,KB), eval(kd,KD), KE is KB /\ KD.
eval(kf,KF) :- eval(ke,KE), KF is 1 /\ KE.
eval(kg,KG) :- eval(jm,JM), KG is JM << 1.
eval(kh,KH) :- eval(kg,KG), eval(kf,KF), KH is KG \/ KF.
eval(ki,KI) :- eval(jp,JP), KI is JP >> 1.
eval(kj,KJ) :- eval(kf,KF), KJ is KF << 15.
eval(kk,KK) :- eval(ki,KI), eval(kj,KJ), KK is KI \/ KJ.
eval(kl,KL) :- eval(kk,KK), KL is KK >> 2.
eval(km,KM) :- eval(kk,KK), KM is KK >> 3.
eval(kn,KN) :- eval(kk,KK), KN is KK >> 5.
eval(ko,KO) :- eval(km,KM), eval(kn,KN), KO is KM \/ KN.
eval(kp,KP) :- eval(km,KM), eval(kn,KN), KP is KM /\ KN.
eval(kq,KQ) :- eval(kp,KP), KQ is \KP.
eval(kr,KR) :- eval(ko,KO), eval(kq,KQ), KR is KO /\ KQ.
eval(ks,KS) :- eval(kl,KL), eval(kr,KR), KS is KL \/ KR.
eval(kt,KT) :- eval(kl,KL), eval(kr,KR), KT is KL /\ KR.
eval(ku,KU) :- eval(kt,KT), KU is \KT.
eval(kv,KV) :- eval(ks,KS), eval(ku,KU), KV is KS /\ KU.
eval(kw,KW) :- eval(kk,KK), eval(kv,KV), KW is KK \/ KV.
eval(kx,KX) :- eval(kk,KK), eval(kv,KV), KX is KK /\ KV.
eval(ky,KY) :- eval(kx,KX), KY is \KX.
eval(kz,KZ) :- eval(kw,KW), eval(ky,KY), KZ is KW /\ KY.
eval(la,LA) :- eval(kz,KZ), LA is 1 /\ KZ.
eval(lb,LB) :- eval(kh,KH), LB is KH << 1.
eval(lc,LC) :- eval(lb,LB), eval(la,LA), LC is LB \/ LA.
eval(ld,LD) :- eval(kk,KK), LD is KK >> 1.
eval(le,LE) :- eval(la,LA), LE is LA << 15.
eval(lf,LF) :- eval(ld,LD), eval(le,LE), LF is LD \/ LE.
eval(lg,LG) :- eval(lf,LF), LG is LF >> 2.
eval(lh,LH) :- eval(lf,LF), LH is LF >> 3.
eval(li,LI) :- eval(lf,LF), LI is LF >> 5.
eval(lj,LJ) :- eval(lh,LH), eval(li,LI), LJ is LH \/ LI.
eval(lk,LK) :- eval(lh,LH), eval(li,LI), LK is LH /\ LI.
eval(ll,LL) :- eval(lk,LK), LL is \LK.
eval(lm,LM) :- eval(lj,LJ), eval(ll,LL), LM is LJ /\ LL.
eval(ln,LN) :- eval(lg,LG), eval(lm,LM), LN is LG \/ LM.
eval(lo,LO) :- eval(lg,LG), eval(lm,LM), LO is LG /\ LM.
eval(lp,LP) :- eval(lo,LO), LP is \LO.
eval(lq,LQ) :- eval(ln,LN), eval(lp,LP), LQ is LN /\ LP.
eval(lr,LR) :- eval(lf,LF), eval(lq,LQ), LR is LF \/ LQ.
eval(ls,LS) :- eval(lf,LF), eval(lq,LQ), LS is LF /\ LQ.
eval(lt,LT) :- eval(ls,LS), LT is \LS.
eval(lu,LU) :- eval(lr,LR), eval(lt,LT), LU is LR /\ LT.
eval(lv,LV) :- eval(lu,LU), LV is 1 /\ LU.
eval(lw,LW) :- eval(lc,LC), LW is LC << 1.
eval(lx,LX) :- eval(lw,LW), eval(lv,LV), LX is LW \/ LV.
eval(ly,LY) :- eval(lf,LF), LY is LF >> 1.
eval(lz,LZ) :- eval(lv,LV), LZ is LV << 15.
eval(ma,MA) :- eval(ly,LY), eval(lz,LZ), MA is LY \/ LZ.

main :- 
     eval(a,A),
     writeln(A).