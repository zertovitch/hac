--  Solution to Advent of Code 2022, Day 21
-------------------------------------------
--  Monkey Math
--
--  https://adventofcode.com/2022/day/21
--  Copy of questions in: aoc_2022_21_questions.txt

--  !!! Only part 1 so far !!!

--  For building this program with "full Ada",
--  files hat*.ad* are in ../../../src
--  See also the GNAT project file aoc_2020.gpr .
with HAT;

--  Interfaces is needed for compiling on both
--  HAC and GNAT (64-bit integer: Integer_64):
with Interfaces;

procedure AoC_2022_21 is
  use HAT, Interfaces;

  verbose : constant Natural := 1;

  type Monkey_Mini is
    (root_m,
     dbpl,
     cczh,
     zczc,
     ptdq,
     dvpt,
     lfqf,
     humn_m,
     ljgn,
     sjmn,
     sllz,
     pppw,
     lgvd,
     drzm,
     hmdt);
    
  type Monkey is
    (rmmz,
     vdjz,
     rqbd,
     zvqw,
     sshm,
     dtzl,
     gnzq,
     zlpv,
     nqgr,
     tftg,
     fhjc,
     fwbm,
     wtlq,
     nzhf,
     nqpq,
     jqzv,
     crrj,
     glpw,
     cnbg,
     bdcm,
     bcww,
     wrbf,
     hpvb,
     qffb,
     mfmc,
     cnzj,
     wbnh,
     swds,
     twtb,
     dlgc,
     bvzn,
     gwpm,
     rtnf,
     sdmb,
     mtjq,
     dpmg,
     drct,
     zrpj,
     gwlm,
     msdr,
     fqbl,
     zwqw,
     lbvn,
     dggg,
     bjwg,
     bnzl,
     qtmp,
     llpf,
     zwqn,
     trpt,
     lzgm,
     swhh,
     tpnf,
     rgrp,
     zswl,
     gzbv,
     ppfs,
     nfhf,
     pgrc,
     jjwr,
     fvsf,
     qmvq,
     twjq,
     twsj,
     mwrc,
     sbpb,
     cbjq,
     mgsl,
     vzgd,
     qrll,
     slpm,
     rffd,
     jjbm,
     pjds,
     btfl,
     sfcv,
     vtnq,
     fzdp,
     cwjp,
     mpnj,
     wnbb,
     nfgv,
     pszs,
     chpt,
     bhpw,
     dfps,
     pzqv,
     fjzz,
     ffqr,
     bqvg,
     qlmg,
     lfhb,
     pqqj,
     qhtv,
     wchl,
     vcwt,
     nfbp,
     bbcn,
     smld,
     mvmj,
     mqph,
     brmz,
     ttdb,
     bcsc,
     frjj,
     jcjf,
     pmfh,
     drrf,
     lmht,
     sstg,
     swhz,
     rtpz,
     qrzq,
     rtmt,
     pmds,
     vfzw,
     smhv,
     vdzv,
     btqb,
     wclq,
     vzrz,
     ztct,
     zvvd,
     mfrc,
     sdrn,
     jfvq,
     qjrs,
     tnnt,
     fqlp,
     nwhm,
     rlcw,
     rwjc,
     tfqh,
     crqg,
     nrps,
     pmng,
     gmzt,
     fhdz,
     fwzf,
     tzls,
     pvtb,
     gpdf,
     zcgs,
     clbn,
     wghf,
     vffj,
     gfbw,
     tzth,
     vwhb,
     scwp,
     rpbm,
     zdhf,
     fdmf,
     lqdf,
     rndt,
     jnzr,
     qpqs,
     tmhg,
     dtfz,
     pnvc,
     mfnh,
     vjgb,
     czvm,
     qhnd,
     dhfw,
     nqpv,
     rhcv,
     lqqh,
     mspl,
     ngps,
     bcmp,
     nlsw,
     mhlj,
     bqnn,
     fbqs,
     cnln,
     vjbz,
     vhhz,
     crmb,
     wbgs,
     qhvn,
     lsnd,
     hhcv,
     vmjn,
     fbtc,
     jgnj,
     jmcp,
     mfhg,
     ztdn,
     zttf,
     tjwt,
     lzzh,
     rnns,
     ffnn,
     wpvg,
     wjds,
     nnmh,
     jswz,
     qjqw,
     bndd,
     mdpg,
     wtwb,
     nbfg,
     nmts,
     nvjf,
     gqgh,
     btqt,
     cblz,
     nvpd,
     vldc,
     drhw,
     bpjf,
     dpgl,
     ggnd,
     qzpn,
     ttvz,
     qwqc,
     zhmb,
     nzdj,
     rrnp,
     mbvg,
     mlbw,
     wvfb,
     tzhp,
     zjcn,
     wbbf,
     mfsz,
     bfpq,
     vpvl,
     pnrv,
     lbgc,
     pbdd,
     jsrb,
     bbzt,
     mhdn,
     jpcd,
     ldzb,
     nmlg,
     pnpc,
     rbqt,
     dsqb,
     zhqg,
     dphz,
     gbzd,
     cldm,
     ftbq,
     rzff,
     tzcd,
     gzwh,
     qgnb,
     jzvg,
     qqwz,
     jbtg,
     rrgh,
     bvfr,
     mwdl,
     chfs,
     gwmr,
     qcml,
     zbhj,
     zshp,
     pvcs,
     sszh,
     jdsc,
     ndnm,
     lrlz,
     wpbs,
     bcpn,
     glts,
     qnld,
     hrtv,
     zbbf,
     vvbz,
     vvjg,
     lrlg,
     zpht,
     ndtz,
     stcv,
     jjrh,
     hsbr,
     wtrh,
     jfqs,
     hjgd,
     hgnr,
     brqb,
     ndsm,
     fjgh,
     tptf,
     njcj,
     jjvz,
     grww,
     jvqf,
     sgvm,
     zscd,
     mgjl,
     bfgp,
     mpst,
     wjzd,
     wqdj,
     csdf,
     cgwj,
     rccr,
     vsgw,
     pvwn,
     mfql,
     dspn,
     llcn,
     dcph,
     npcs,
     wgvj,
     bblh,
     dmrn,
     qbzv,
     lrdn,
     wzfr,
     bwnc,
     dswn,
     bnws,
     cdnp,
     fddw,
     nbbd,
     lbzc,
     sntb,
     vzts,
     jvfw,
     mqcv,
     jhzw,
     lhpf,
     znjr,
     wrvl,
     cdwt,
     vtjg,
     dvnh,
     pcgq,
     nhjt,
     pmqh,
     zrwb,
     zphs,
     nmvn,
     sbvl,
     bcdf,
     jhwn,
     ftjv,
     ncnm,
     bgwj,
     hrbr,
     qljb,
     vdsz,
     nhpf,
     gvfn,
     ppts,
     fvhm,
     rrqr,
     lnqm,
     qwnz,
     cgvr,
     lvrc,
     vcfm,
     zhgt,
     nvdh,
     llfg,
     wjgf,
     nztj,
     shpt,
     gzmv,
     tqzv,
     pdmb,
     fpcs,
     mwmp,
     hcfs,
     bshp,
     gppj,
     pzpp,
     ltnz,
     wzdj,
     frst,
     scdn,
     fpsh,
     gzwm,
     jmbs,
     rqsw,
     bnvg,
     tfwb,
     snwm,
     zvrq,
     vnpq,
     cfbh,
     ntnh,
     jnwv,
     npvl,
     tpwq,
     flhs,
     pnvd,
     zvvp,
     fcql,
     jrvp,
     wbvt,
     vwht,
     pzvr,
     qncp,
     bcgz,
     dcbt,
     csgr,
     rsdv,
     fnqf,
     hzrc,
     cdvw,
     bsvc,
     tlwr,
     hjmr,
     spjg,
     mqgl,
     mzzd,
     dqnv,
     lqcm,
     sqzp,
     drvt,
     ghvn,
     hfbh,
     wtlg,
     dsgt,
     vvzh,
     sclr,
     jqqs,
     zprq,
     dczh,
     lpzb,
     dqsv,
     hgzs,
     vrpp,
     snbm,
     tdrq,
     tbwl,
     jdmt,
     fzgj,
     vnvr,
     mcht,
     ztrt,
     bncr,
     nqsl,
     cclc,
     ztpd,
     lfrb,
     pvfv,
     swpl,
     csnf,
     ptdz,
     nvhn,
     rdhg,
     jclb,
     cscm,
     fcgj,
     hgwm,
     rqbq,
     wjtc,
     dpvl,
     nwdv,
     zcgn,
     jvrt,
     zmdq,
     rtsd,
     nqzz,
     dshq,
     mddc,
     rpwz,
     pmlg,
     blmp,
     hqbb,
     chtt,
     cchg,
     bntz,
     gvgp,
     vvhm,
     nlcv,
     rwcd,
     rfcq,
     zgdb,
     mggl,
     mwjg,
     rbbh,
     mqlt,
     ddtd,
     hpmm,
     cjlm,
     cpdf,
     gqrp,
     zmtg,
     vwdl,
     ggpn,
     mtfr,
     nsfm,
     cftm,
     wcdm,
     jcch,
     zqjp,
     wjmq,
     cbgj,
     jmnj,
     tlnz,
     fqjd,
     ngzp,
     trdj,
     qrch,
     hmrz,
     rvnh,
     humn,
     jstb,
     vqlt,
     vlqd,
     vjjw,
     mnlp,
     jtcf,
     lwlc,
     jdgq,
     vznc,
     dcfw,
     fqwz,
     hhbn,
     wrpp,
     mmnw,
     rdpb,
     hcsc,
     nfvj,
     qznt,
     flvw,
     wphq,
     bpwd,
     cbfh,
     bcbw,
     dvwj,
     tnbt,
     qlzt,
     rjfh,
     cprt,
     qgwg,
     zpbt,
     drln,
     jdcd,
     jzcd,
     sbpp,
     ttwm,
     rdbs,
     pfpt,
     rfbw,
     bqld,
     rrmg,
     rbsf,
     mlgj,
     jfcd,
     sjrh,
     djjd,
     jhgd,
     nlts,
     ppwc,
     gptt,
     bvvl,
     ssnq,
     gmhz,
     nnsc,
     zbzf,
     jhvm,
     mrrt,
     pmrt,
     rddj,
     wszm,
     pbgr,
     hzlh,
     qnbn,
     hrcp,
     dnwd,
     rvlz,
     stcr,
     ptvh,
     vmnc,
     bmwf,
     jzdn,
     djrh,
     lwlb,
     bqht,
     brfj,
     rlqm,
     mdht,
     lbbj,
     blnq,
     chfz,
     sqsc,
     mwrt,
     nvrb,
     lgrj,
     pvcn,
     fbgc,
     mqnl,
     bdtp,
     wdwf,
     hhjc,
     jzlv,
     smwb,
     bhvg,
     zmmd,
     mdld,
     qlqt,
     dgbr,
     rwsm,
     jnll,
     dbht,
     bhmz,
     mrlq,
     fzdg,
     jbsh,
     svzv,
     zjmw,
     tcgm,
     nzcv,
     rmrj,
     bjnj,
     qsml,
     mfts,
     ffqs,
     dbcw,
     wpcs,
     rvhs,
     qmcb,
     hlms,
     vwpq,
     wslz,
     jpvr,
     hcdz,
     svvq,
     lgpg,
     mlwr,
     hbqm,
     fgbm,
     cjlh,
     tlmq,
     zzqd,
     drdl,
     sdjb,
     ntmt,
     rwth,
     jjgt,
     mpvz,
     qbmm,
     zdrh,
     trft,
     dqjz,
     gfrq,
     vvgt,
     tsft,
     gzpf,
     tfpw,
     cddt,
     rlbp,
     ndph,
     blbc,
     cvhn,
     zncl,
     rthb,
     jfld,
     gdgd,
     lrsc,
     gzgm,
     bhrb,
     bsmm,
     qnds,
     bhzq,
     vwvc,
     qmgt,
     gfml,
     tjqj,
     vlmc,
     qlnr,
     fvph,
     jgzl,
     wnjc,
     ffbb,
     hzmn,
     rpbj,
     sqmd,
     ndcd,
     mmbs,
     mhbr,
     nsqq,
     wgvv,
     rvmp,
     dwnz,
     jvtz,
     nzzv,
     cqcg,
     nrvw,
     qqtm,
     fvng,
     phrv,
     tlzh,
     twjs,
     lhgv,
     rctr,
     snlb,
     dfgh,
     tvtj,
     flcb,
     nqpr,
     jmbp,
     rcmc,
     mnpl,
     twcj,
     nlth,
     drrq,
     fszd,
     mlrz,
     jvgd,
     jtvw,
     ftvm,
     cgcz,
     dfzw,
     tdhw,
     qwnt,
     mspb,
     fhmz,
     dqzt,
     brfc,
     hstj,
     rzjm,
     jbcl,
     zhpq,
     hmrs,
     mtdh,
     lwhp,
     wnvf,
     qqdn,
     wrmw,
     gzrs,
     nzwc,
     wflc,
     gtwp,
     trbs,
     zsns,
     vvvb,
     cstb,
     gcmv,
     vrjj,
     rrll,
     wgsb,
     scdt,
     wqqv,
     szhq,
     tqzf,
     wpqd,
     vmzc,
     pjlz,
     mdjm,
     qgjf,
     qnhq,
     hfbg,
     jfsr,
     nfhw,
     cstm,
     ghvl,
     tzcl,
     pzzg,
     cdqv,
     wcpr,
     jdrg,
     gtqh,
     hlmn,
     hnqc,
     sphc,
     bnlr,
     dbfd,
     hpzh,
     ghqp,
     dfql,
     hbzt,
     chsm,
     lpjq,
     plfd,
     vbvt,
     rdpg,
     vhds,
     rmfv,
     smft,
     wlpd,
     cvvw,
     bsqn,
     svcp,
     qflq,
     bwwc,
     wqfg,
     lbqn,
     zbhw,
     jlnf,
     nlst,
     zfjw,
     wfms,
     fjps,
     djpm,
     snsc,
     hnlj,
     pvfm,
     cwhc,
     blgg,
     jmwl,
     wbsb,
     jwwv,
     jdsd,
     rsnp,
     rrnt,
     nfnw,
     mpfm,
     mnzz,
     lfjp,
     qvfg,
     mjmv,
     lclg,
     pcvv,
     wsml,
     gvbp,
     mpjj,
     vcqw,
     lhvt,
     qpdv,
     mrlc,
     bdmw,
     vntv,
     cfdl,
     gvww,
     dqhh,
     mtnb,
     wlfp,
     zslv,
     lqrr,
     qfzb,
     ltzm,
     qcnn,
     bhvr,
     wnzr,
     zglw,
     svdw,
     pfml,
     zpqd,
     fjnh,
     rhqj,
     szmm,
     ztwb,
     npnf,
     wltv,
     bwff,
     slnh,
     ppwz,
     hgls,
     dbvt,
     wrst,
     rzzg,
     prbr,
     bvdb,
     jlmb,
     rbpq,
     jwjj,
     dhts,
     dcpm,
     qvpv,
     bbhp,
     sjfs,
     wqpv,
     sdhj,
     wfbz,
     mdnp,
     ctjq,
     mbqs,
     wcfp,
     ftzm,
     nwbt,
     bmsf,
     cmnt,
     wqfz,
     jrgs,
     gmzh,
     pnvs,
     gcvn,
     qbvr,
     phbh,
     shtz,
     smsm,
     gznh,
     tjch,
     bpnt,
     lljf,
     qfsz,
     dgvh,
     lvbp,
     cdvv,
     qlzg,
     rwbg,
     snff,
     flrw,
     tdnf,
     nzlr,
     ftzc,
     pznf,
     zjvz,
     nvjl,
     qtvv,
     hdff,
     swmh,
     ghfr,
     ghtd,
     mfmt,
     pgtq,
     frjp,
     bnfz,
     qfqz,
     djmr,
     crcm,
     ngbg,
     sbqm,
     wlqd,
     jfrn,
     jbqf,
     zhrs,
     lvtv,
     vclb,
     qtgd,
     hqbd,
     qqnm,
     nvjh,
     jcgh,
     bgzz,
     slds,
     lwsn,
     bhgh,
     bjbr,
     tgvp,
     dbfm,
     msmm,
     llqs,
     qqpl,
     pfdc,
     qlvg,
     wzdv,
     glqr,
     vdgt,
     qrnj,
     fbrf,
     gvmj,
     nrjc,
     sdsl,
     mqvg,
     chvq,
     lrzn,
     crrm,
     pgbv,
     brqv,
     gmln,
     wcmz,
     hbhz,
     vrcl,
     hqdv,
     nchp,
     bfvd,
     nnhm,
     thpg,
     jpzw,
     vrmt,
     hnbn,
     tsbf,
     nbpq,
     csml,
     cnmq,
     qbwf,
     jbrp,
     bbzc,
     pgfr,
     fmwn,
     lrvc,
     stqc,
     ngzt,
     mdrm,
     mrtq,
     lpvs,
     lqfq,
     rnnq,
     jdfj,
     tqst,
     dnzc,
     lwld,
     sgjm,
     bfzd,
     dchw,
     rlrb,
     pchr,
     lvhm,
     mswh,
     sdgs,
     snfj,
     sdwj,
     qmqt,
     przh,
     qrpv,
     pjsp,
     hpmw,
     jhlw,
     ntqb,
     qpct,
     rvvn,
     lmcj,
     bvjd,
     fngd,
     fnws,
     frqq,
     ltsv,
     cmnv,
     wdqj,
     cjbz,
     rqpj,
     lbtn,
     dbgp,
     qbjm,
     ljzj,
     mrvr,
     nght,
     djff,
     jmqh,
     vcpt,
     vsqt,
     tfjw,
     hwvf,
     qgtf,
     mmtj,
     blgf,
     mvmz,
     svhz,
     wfds,
     pnmc,
     szvb,
     bnwj,
     dnvm,
     fwrh,
     cspp,
     lltp,
     smrq,
     rgzs,
     crmw,
     dncw,
     rvjf,
     wlmv,
     fqbj,
     fwjg,
     gnsc,
     gtwd,
     qdnl,
     pjjj,
     ntsl,
     glqw,
     wmzt,
     vjzr,
     ncgd,
     bngr,
     qtfp,
     bdqg,
     hzpr,
     tctd,
     jlqg,
     qhlj,
     jtvm,
     szcv,
     sfmn,
     lfhp,
     root,
     jbwd,
     zrnr,
     hrmz,
     rfjw,
     hpdh,
     jqrl,
     vgsm,
     mvgg,
     hcqw,
     cdtq,
     spgw,
     wfjn,
     hrtz,
     rzsn,
     fbcd,
     vhwz,
     lvcn,
     grdf,
     pdbc,
     qssh,
     wnjl,
     btwt,
     hplt,
     mwwr,
     smqg,
     dqcj,
     ptwr,
     mttr,
     pzcc,
     jscq,
     hzcm,
     tsjq,
     fsgb,
     plvw,
     tcqz,
     mqzp,
     tldn,
     rwmq,
     vnft,
     ztqg,
     fjvg,
     nzdz,
     qrgn,
     tbqq,
     tdjh,
     qwqw,
     pdbt,
     bnmb,
     lslq,
     twwv,
     rzcm,
     jtjd,
     htgm,
     pjss,
     mrwz,
     djqq,
     jmcf,
     hmqr,
     lqdc,
     gpzn,
     pgdw,
     mwgb,
     wggq,
     mwqc,
     bnfp,
     pwlt,
     hlzp,
     qplw,
     vhqz,
     zptr,
     gtbs,
     zwwb,
     hmfb,
     mffc,
     lqgr,
     bcqm,
     cbdr,
     bssd,
     dmsz,
     fpwz,
     rbmm,
     rvhn,
     hqjt,
     bftg,
     ppjl,
     nzbp,
     rwzt,
     zwgg,
     frlv,
     vmgn,
     tvrd,
     wfld,
     pnwz,
     ndmd,
     fmbj,
     jqcf,
     jqsn,
     wzsh,
     hjqq,
     cvqf,
     wrll,
     vtns,
     wsnf,
     qfjg,
     rsqz,
     dddm,
     ftmn,
     bcnd,
     qgtc,
     vthd,
     mvnc,
     ndpf,
     pwlh,
     cfzf,
     qldj,
     ftqd,
     lhml,
     mrmm,
     crnb,
     qpbz,
     qspw,
     jtrg,
     nwwb,
     rgpt,
     spcf,
     brqh,
     qfrg,
     ddmr,
     lpqs,
     zvqs,
     dfdl,
     vnbv,
     bwqm,
     zjpc,
     btbn,
     bpgp,
     jbsn,
     tdlw,
     fsrm,
     rjrt,
     wpdn,
     dzvg,
     rdfn,
     zrnz,
     djgb,
     pwln,
     vfgt,
     tbdh,
     jlwb,
     cshj,
     lzhf,
     hncq,
     nqms,
     lhfr,
     ltdq,
     zssf,
     dqld,
     grcv,
     zfbq,
     lmjg,
     lncn,
     qmjq,
     rnbs,
     mstm,
     pnwh,
     ccfw,
     tsqc,
     gqpj,
     tnfz,
     rhwm,
     bcnp,
     fvrg,
     vtgj,
     wwtj,
     slmm,
     mjmt,
     zzpd,
     wttp,
     qdfp,
     psfr,
     rlqh,
     mgsz,
     cjvp,
     hrjj,
     fjrq,
     qmpm,
     lddq,
     snbl,
     bghp,
     zdnh,
     cbqn,
     pbzb,
     rmzw,
     qpqb,
     jffq,
     rscq,
     wjzv,
     ngbz,
     hdrj,
     slpc,
     bmzw,
     cmtm,
     pwqg,
     dgpr,
     trpv,
     qnvn,
     gcqh,
     bdgd,
     dbcs,
     nrqv,
     cnvb,
     cvgt,
     tqlp,
     sgpj,
     swsv,
     hndh,
     smwv,
     dwdd,
     jszj,
     ncrt,
     rpbg,
     jvsz,
     ffvs,
     mnhf,
     hbwh,
     tpmp,
     nttr,
     ldbg,
     ntpj,
     lbbv,
     vrct,
     lccn,
     nrsd,
     fvzw,
     vgjq,
     dptm,
     ggbp,
     qdrg,
     hcpf,
     zrbc,
     tpjw,
     jnqv,
     fzrd,
     cbjl,
     bnjt,
     czpd,
     fjld,
     sbwd,
     rqfq,
     pwpn,
     ztzq,
     ltqr,
     nlgg,
     ltsl,
     zgzs,
     mqrm,
     fhtw,
     zdfg,
     dzwg,
     czst,
     ncsp,
     bsnq,
     sflt,
     vsmt,
     vszg,
     tqfz,
     fhzm,
     wgcm,
     zsvh,
     hjfj,
     pqsr,
     lrbp,
     twdf,
     jdbp,
     smdh,
     cjhz,
     psfp,
     cqmq,
     nvlv,
     lrqh,
     wqlh,
     qrsg,
     zzcv,
     sgzh,
     vrbh,
     drlt,
     shrh,
     qrln,
     hslj,
     mtsv,
     hmfd,
     grcn,
     fctt,
     lhmd,
     rtgr,
     zmvz,
     jdjq,
     vhzf,
     fmvc,
     dzlc,
     fbjn,
     ppcl,
     jjjj,
     wfnm,
     dflc,
     ffnt,
     zbnt,
     dqnh,
     slww,
     djjs,
     sqml,
     lghm,
     bsbd,
     gwhs,
     zqpw,
     drgv,
     rswn,
     gnjq,
     sjcd,
     fzml,
     mthn,
     cgnr,
     ldbp,
     lqrf,
     gjbv,
     ljnf,
     hjmp,
     mrnr,
     zspm,
     qlvz,
     tfdv,
     vtvn,
     qlmw,
     hsmp,
     wcfj,
     brst,
     fnrh,
     qhzd,
     mmps,
     lpfz,
     bjcs,
     tbqc,
     qhwm,
     hzvm,
     nqhg,
     bzdh,
     gbtc,
     wfnl,
     ndlf,
     nmlt,
     nhfw,
     rtbm,
     fbpz,
     rjtc,
     vspf,
     dhqh,
     ddln,
     lbzn,
     vhrn,
     cgcc,
     cgvs,
     jqtj,
     wqnp,
     dmtd,
     rlvd,
     cvvv,
     rjdp,
     hmvq,
     vjtf,
     gbgs,
     wgbc,
     vdbp,
     vtdp,
     cgvv,
     vfhg,
     pcwz,
     zngg,
     scrz,
     tvrz,
     mqzw,
     zfmv,
     gdmv,
     cgnv,
     qlvw,
     fjzn,
     vzgb,
     dsng,
     pbvh,
     qbqm,
     vwdt,
     lhsw,
     hzjj,
     jplv,
     zmlh,
     zwbh,
     mhnm,
     wmdp,
     pbhg,
     qmzt,
     dlws,
     rbzl,
     tpjp,
     vlgn,
     phwc,
     bcng,
     cvnr,
     bmlj,
     llhb,
     nbcb,
     ffmp,
     nnzp,
     szlh,
     gztl,
     mtwg,
     mpws,
     fqgl,
     qqpt,
     bqtp,
     nttd,
     svvh,
     drnb,
     ptnv,
     wtnp,
     ndgh,
     lrgv,
     nrqr,
     hfvj,
     ctqb,
     nqlh,
     nhnp,
     tcjb,
     mnhc,
     jnnp,
     zbqq,
     gjzv,
     shwr,
     gmpl,
     lfpz,
     tmhl,
     wglc,
     ppll,
     ccbp,
     jflm,
     mwms,
     svwg,
     qfss,
     tjtl,
     vwmr,
     cllb,
     nssr,
     qcfn,
     rmvq,
     rbtb,
     jcdb,
     cdpw,
     zdsr,
     plzd,
     phjj,
     bnmf,
     twtq,
     qwcl,
     ccjf,
     mrjc,
     thlz,
     thcc,
     fjcg,
     flhd,
     gnrj,
     hnlf,
     tprm,
     vlhc,
     tvgm,
     qvpw,
     vfwd,
     pmwl,
     rzbj,
     rncj,
     znfq,
     rjbz,
     hhws,
     qfln,
     ltmj,
     csln,
     snvw,
     ltgf,
     wfbp,
     hjlz,
     zvst,
     qvvc,
     bjfh,
     mzfn,
     dqnp,
     stlr,
     cvfw,
     rtmh,
     rqss,
     jddd,
     gggh,
     gssz,
     tsms,
     lrcj,
     pnfb,
     rtjb,
     zvmv,
     dfrb,
     mcfr,
     pjrt,
     ltds,
     fvsb,
     nldv,
     cdnm,
     jwqg,
     jvqz,
     sqdd,
     cwvl,
     qcnv,
     fhjz,
     svst,
     qdrc,
     cfmw,
     jgld,
     bppp,
     wwqp,
     cprm,
     fcgs,
     swfp,
     frjl,
     nwqz,
     mldc,
     mgmw,
     mnrr,
     hvtv,
     tpdj,
     znlh,
     qbgj,
     cnhr,
     djgh,
     lccp,
     frrn,
     bbjs,
     gdpg,
     pnwv,
     vhtl,
     vjwz,
     qwqq,
     rdnj,
     nltq,
     fpvs,
     shcg,
     stzl,
     rfcv,
     snhd,
     gjmt,
     btzg,
     vfrh,
     lpjr,
     nzbz,
     dhpp,
     hvsd,
     ldtj,
     chlt,
     rhtv,
     wpgm,
     csfh,
     mhzn,
     wtbs,
     cpqv,
     dngq,
     vszd,
     pgvq,
     smnv,
     zrfw,
     jbvq,
     rmqt,
     pfvz,
     fmvw,
     jqhr,
     szwj,
     tqfr,
     rmvt,
     qllm,
     gcmz,
     sqcb,
     chvn,
     fzvh,
     sbtd,
     hnst,
     ljtp,
     lqml,
     dpqz,
     pwgj,
     qvbn,
     ppsl,
     jfmb,
     bzqw,
     nhcr,
     llnl,
     fzwr,
     mfst,
     flhf,
     whbb,
     rllb,
     rfcm,
     lqft,
     snmq,
     fwmw,
     ngfs,
     mqfd,
     lflj,
     vzcr);

  part : Positive;
  
  human, left, right : Integer_64 := 0;

  function Compute_Mini (m : Monkey_Mini) return Integer_64 is
  begin
    case m is
      when root_m =>
        if part = 1 then
          return Compute_Mini (pppw) + Compute_Mini (sjmn);
        else
          left  := Compute_Mini (pppw);
          right := Compute_Mini (sjmn);
          return left - right;
        end if;
      when dbpl   => return 5;
      when cczh   => return Compute_Mini (sllz) + Compute_Mini (lgvd);
      when zczc   => return 2;
      when ptdq   => return Compute_Mini (humn_m) - Compute_Mini (dvpt);
      when dvpt   => return 3;
      when lfqf   => return 4;
      when humn_m =>
        if part = 1 then
          return 5;
        else
          return human;
        end if;
      when ljgn   => return 2;
      when sjmn   => return Compute_Mini (drzm) * Compute_Mini (dbpl);
      when sllz   => return 4;
      when pppw   => return Compute_Mini (cczh) / Compute_Mini (lfqf);
      when lgvd   => return Compute_Mini (ljgn) * Compute_Mini (ptdq);
      when drzm   => return Compute_Mini (hmdt) - Compute_Mini (zczc);
      when hmdt   => return 32;
    end case;
  end;
  
  function Compute (m : Monkey) return Integer_64 is
  begin
    case m is
      when rmmz => return 2; 
      when vdjz => return 4; 
      when rqbd => return 3; 
      when zvqw => return 4; 
      when sshm => return Compute (qhnd) * Compute (lrcj); 
      when dtzl => return Compute (rnbs) + Compute (gppj); 
      when gnzq => return 5; 
      when zlpv => return Compute (pmng) * Compute (wgvj); 
      when nqgr => return 4; 
      when tftg => return 1; 
      when fhjc => return Compute (sbpb) / Compute (nhcr); 
      when fwbm => return Compute (jcch) * Compute (qhlj); 
      when wtlq => return 3; 
      when nzhf => return Compute (fjzn) * Compute (bnws); 
      when nqpq => return 2; 
      when jqzv => return 2; 
      when crrj => return Compute (vnbv) / Compute (grww); 
      when glpw => return 8; 
      when cnbg => return Compute (jmbp) * Compute (jnqv); 
      when bdcm => return 2; 
      when bcww => return 11; 
      when wrbf => return 2; 
      when hpvb => return Compute (zswl) * Compute (qpbz); 
      when qffb => return Compute (jvqz) + Compute (qgtf); 
      when mfmc => return Compute (hnbn) * Compute (twwv); 
      when cnzj => return Compute (qqdn) * Compute (pvtb); 
      when wbnh => return Compute (cshj) * Compute (snvw); 
      when swds => return Compute (bmlj) + Compute (scdn); 
      when twtb => return 3; 
      when dlgc => return Compute (gfbw) + Compute (djgh); 
      when bvzn => return 2; 
      when gwpm => return Compute (rrll) + Compute (zshp); 
      when rtnf => return Compute (qbwf) + Compute (bqnn); 
      when sdmb => return 15; 
      when mtjq => return 2; 
      when dpmg => return 3; 
      when drct => return Compute (bvfr) + Compute (rtmh); 
      when zrpj => return Compute (gfml) * Compute (tsms); 
      when gwlm => return 2; 
      when msdr => return 3; 
      when fqbl => return Compute (wbbf) + Compute (pbgr); 
      when zwqw => return Compute (lnqm) - Compute (nzdj); 
      when lbvn => return Compute (zwbh) + Compute (btbn); 
      when dggg => return Compute (bnmf) / Compute (trpv); 
      when bjwg => return Compute (trdj) + Compute (lpqs); 
      when bnzl => return Compute (vfhg) * Compute (pjds); 
      when qtmp => return 2; 
      when llpf => return 2; 
      when zwqn => return Compute (lfhb) + Compute (spcf); 
      when trpt => return Compute (mdrm) * Compute (qdrc); 
      when lzgm => return Compute (jzvg) + Compute (qlzg); 
      when swhh => return 2; 
      when tpnf => return 15; 
      when rgrp => return 7; 
      when zswl => return 3; 
      when gzbv => return Compute (djgb) + Compute (dvwj); 
      when ppfs => return 5; 
      when nfhf => return 5; 
      when pgrc => return Compute (pdmb) * Compute (qgwg); 
      when jjwr => return 6; 
      when fvsf => return 2; 
      when qmvq => return Compute (hlzp) * Compute (zvvd); 
      when twjq => return 2; 
      when twsj => return 5; 
      when mwrc => return 2; 
      when sbpb => return Compute (djpm) * Compute (pfpt); 
      when cbjq => return Compute (ztqg) - Compute (tzls); 
      when mgsl => return 2; 
      when vzgd => return Compute (qmcb) + Compute (wfbz); 
      when qrll => return 2; 
      when slpm => return Compute (fbpz) * Compute (jjwr); 
      when rffd => return Compute (mfmc) / Compute (csdf); 
      when jjbm => return Compute (slpc) * Compute (bncr); 
      when pjds => return Compute (cpqv) + Compute (ldtj); 
      when btfl => return Compute (qvfg) + Compute (lqdf); 
      when sfcv => return 7; 
      when vtnq => return 5; 
      when fzdp => return Compute (fzrd) * Compute (slww); 
      when cwjp => return 9; 
      when mpnj => return 2; 
      when wnbb => return Compute (zssf) + Compute (qnld); 
      when nfgv => return Compute (lzhf) + Compute (mqzw); 
      when pszs => return 4; 
      when chpt => return 2; 
      when bhpw => return Compute (cnbg) + Compute (zrnz); 
      when dfps => return 5; 
      when pzqv => return 3; 
      when fjzz => return 8; 
      when ffqr => return 16; 
      when bqvg => return Compute (lrqh) + Compute (wpdn); 
      when qlmg => return Compute (mpvz) + Compute (wpvg); 
      when lfhb => return Compute (jscq) + Compute (bpnt); 
      when pqqj => return 2; 
      when qhtv => return 5; 
      when wchl => return Compute (ffqr) + Compute (mdnp); 
      when vcwt => return Compute (llfg) + Compute (wjtc); 
      when nfbp => return 2; 
      when bbcn => return 8; 
      when smld => return 2; 
      when mvmj => return Compute (lqdc) * Compute (bhmz); 
      when mqph => return Compute (jjgt) + Compute (vtjg); 
      when brmz => return Compute (mpfm) + Compute (zcgs); 
      when ttdb => return Compute (zncl) + Compute (nqpv); 
      when bcsc => return 6; 
      when frjj => return 9; 
      when jcjf => return Compute (lgpg) + Compute (qmvq); 
      when pmfh => return 3; 
      when drrf => return 9; 
      when lmht => return 7; 
      when sstg => return Compute (vrct) + Compute (wfjn); 
      when swhz => return Compute (zvvp) * Compute (dbvt); 
      when rtpz => return Compute (nnsc) + Compute (zmlh); 
      when qrzq => return 2; 
      when rtmt => return 12; 
      when pmds => return 3; 
      when vfzw => return Compute (jhlw) * Compute (vmzc); 
      when smhv => return Compute (ffnt) * Compute (gtwp); 
      when vdzv => return 5; 
      when btqb => return 3; 
      when wclq => return Compute (rwth) + Compute (drln); 
      when vzrz => return Compute (bsvc) + Compute (vmjn); 
      when ztct => return Compute (zscd) / Compute (lhml); 
      when zvvd => return Compute (tcqz) - Compute (cmnv); 
      when mfrc => return Compute (jmcp) + Compute (lpvs); 
      when sdrn => return 9; 
      when jfvq => return Compute (pgdw) + Compute (vbvt); 
      when qjrs => return Compute (qvpw) + Compute (cpdf); 
      when tnnt => return Compute (psfr) + Compute (hjlz); 
      when fqlp => return Compute (bblh) + Compute (dbfm); 
      when nwhm => return 3; 
      when rlcw => return 2; 
      when rwjc => return Compute (ljzj) + Compute (zdrh); 
      when tfqh => return 5; 
      when crqg => return 3; 
      when nrps => return Compute (lrdn) + Compute (hmfb); 
      when pmng => return Compute (gzrs) + Compute (vtns); 
      when gmzt => return Compute (tlmq) * Compute (bfzd); 
      when fhdz => return 6; 
      when fwzf => return 4; 
      when tzls => return Compute (fnqf) + Compute (fpcs); 
      when pvtb => return 2; 
      when gpdf => return Compute (zslv) * Compute (qwqc); 
      when zcgs => return Compute (jlmb) * Compute (crnb); 
      when clbn => return 2; 
      when wghf => return Compute (fwbm) - Compute (jcjf); 
      when vffj => return Compute (frjp) + Compute (mrtq); 
      when gfbw => return Compute (csfh) + Compute (rlvd); 
      when tzth => return Compute (tvtj) * Compute (bnvg); 
      when vwhb => return 2; 
      when scwp => return 3; 
      when rpbm => return 3; 
      when zdhf => return 2; 
      when fdmf => return 3; 
      when lqdf => return 2; 
      when rndt => return Compute (wtbs) + Compute (lrsc); 
      when jnzr => return 5; 
      when qpqs => return Compute (bcng) / Compute (nwdv); 
      when tmhg => return Compute (vtvn) - Compute (nzbp); 
      when dtfz => return Compute (bdgd) * Compute (dfdl); 
      when pnvc => return Compute (brqb) * Compute (drrq); 
      when mfnh => return Compute (rdhg) * Compute (rmvq); 
      when vjgb => return 3; 
      when czvm => return 18; 
      when qhnd => return 3; 
      when dhfw => return Compute (jmcf) + Compute (vjjw); 
      when nqpv => return Compute (bhvg) + Compute (zmvz); 
      when rhcv => return Compute (ddtd) / Compute (jqzv); 
      when lqqh => return 5; 
      when mspl => return 5; 
      when ngps => return 11; 
      when bcmp => return 2; 
      when nlsw => return Compute (gnjq) * Compute (vzrz); 
      when mhlj => return 4; 
      when bqnn => return Compute (pgrc) - Compute (bwff); 
      when fbqs => return 11; 
      when cnln => return Compute (sqml) * Compute (wlqd); 
      when vjbz => return 5; 
      when vhhz => return Compute (pwpn) * Compute (dsng); 
      when crmb => return 5; 
      when wbgs => return 3; 
      when qhvn => return 3; 
      when lsnd => return 1; 
      when hhcv => return 2; 
      when vmjn => return 12; 
      when fbtc => return 2; 
      when jgnj => return 4; 
      when jmcp => return 2; 
      when mfhg => return 2; 
      when ztdn => return 2; 
      when zttf => return 3; 
      when tjwt => return 16; 
      when lzzh => return 11; 
      when rnns => return Compute (frrn) * Compute (pnmc); 
      when ffnn => return 3; 
      when wpvg => return Compute (qvpv) * Compute (frqq); 
      when wjds => return 3; 
      when nnmh => return 5; 
      when jswz => return Compute (vthd) - Compute (jjrh); 
      when qjqw => return 2; 
      when bndd => return 7; 
      when mdpg => return 2; 
      when wtwb => return 2; 
      when nbfg => return Compute (vwvc) * Compute (rbtb); 
      when nmts => return Compute (pfml) * Compute (jqqs); 
      when nvjf => return Compute (mddc) * Compute (lrbp); 
      when gqgh => return Compute (wzsh) * Compute (tqzv); 
      when btqt => return Compute (qbzv) + Compute (wtrh); 
      when cblz => return 5; 
      when nvpd => return 3; 
      when vldc => return 2; 
      when drhw => return Compute (dngq) * Compute (nmlt); 
      when bpjf => return Compute (ndlf) + Compute (tjwt); 
      when dpgl => return 2; 
      when ggnd => return 3; 
      when qzpn => return 2; 
      when ttvz => return 2; 
      when qwqc => return 3; 
      when zhmb => return 15; 
      when nzdj => return Compute (svvh) + Compute (qplw); 
      when rrnp => return Compute (qrnj) + Compute (hmvq); 
      when mbvg => return Compute (sgzh) / Compute (mdpg); 
      when mlbw => return Compute (jwwv) * Compute (jhzw); 
      when wvfb => return Compute (mvmj) + Compute (phjj); 
      when tzhp => return 15; 
      when zjcn => return Compute (pgfr) * Compute (dgvh); 
      when wbbf => return Compute (ghvn) * Compute (dfrb); 
      when mfsz => return Compute (fpvs) * Compute (lzgm); 
      when bfpq => return Compute (tmhg) * Compute (nvlv); 
      when vpvl => return Compute (blnq) * Compute (jfsr); 
      when pnrv => return Compute (lfrb) - Compute (qwnt); 
      when lbgc => return Compute (bzdh) * Compute (wszm); 
      when pbdd => return Compute (dcpm) * Compute (nrsd); 
      when jsrb => return 13; 
      when bbzt => return 8; 
      when mhdn => return Compute (bsmm) * Compute (rzzg); 
      when jpcd => return 2; 
      when ldzb => return Compute (nqpr) * Compute (qnds); 
      when nmlg => return 2; 
      when pnpc => return 5; 
      when rbqt => return 5; 
      when dsqb => return Compute (mrjc) / Compute (ftqd); 
      when zhqg => return Compute (lpjr) * Compute (zttf); 
      when dphz => return 3; 
      when gbzd => return Compute (ttvz) * Compute (tsjq); 
      when cldm => return 4; 
      when ftbq => return 5; 
      when rzff => return Compute (gdpg) + Compute (cgvs); 
      when tzcd => return 1; 
      when gzwh => return Compute (bjfh) * Compute (qtfp); 
      when qgnb => return Compute (vdgt) + Compute (zwwb); 
      when jzvg => return 5; 
      when qqwz => return Compute (pnwh) * Compute (wlmv); 
      when jbtg => return 9; 
      when rrgh => return 2; 
      when bvfr => return Compute (jzcd) + Compute (gjbv); 
      when mwdl => return Compute (rlcw) * Compute (jffq); 
      when chfs => return Compute (mrlq) - Compute (cgvv); 
      when gwmr => return Compute (hbwh) * Compute (wzdj); 
      when qcml => return 3; 
      when zbhj => return Compute (ctqb) * Compute (hfbg); 
      when zshp => return Compute (wphq) * Compute (hzpr); 
      when pvcs => return 4; 
      when sszh => return 2; 
      when jdsc => return 15; 
      when ndnm => return 8; 
      when lrlz => return 1; 
      when wpbs => return 2; 
      when bcpn => return 7; 
      when glts => return Compute (gpdf) - Compute (vznc); 
      when qnld => return Compute (jrvp) * Compute (smnv); 
      when hrtv => return Compute (bdcm) * Compute (cgnr); 
      when zbbf => return Compute (shtz) + Compute (fvsb); 
      when vvbz => return Compute (bqht) * Compute (zwqn); 
      when vvjg => return Compute (zgzs) * Compute (sbpp); 
      when lrlg => return 10; 
      when zpht => return 10; 
      when ndtz => return Compute (cnzj) + Compute (qsml); 
      when stcv => return 3; 
      when jjrh => return Compute (mlwr) * Compute (dqjz); 
      when hsbr => return Compute (dbcs) / Compute (zngg); 
      when wtrh => return Compute (sclr) + Compute (chsm); 
      when jfqs => return 15; 
      when hjgd => return 12; 
      when hgnr => return 9; 
      when brqb => return 4; 
      when ndsm => return Compute (tmhl) + Compute (ddln); 
      when fjgh => return Compute (cbjl) + Compute (pchr); 
      when tptf => return 3; 
      when njcj => return Compute (ppll) * Compute (psfp); 
      when jjvz => return Compute (vhqz) + Compute (fmvw); 
      when grww => return 2; 
      when jvqf => return 2; 
      when sgvm => return Compute (vhwz) * Compute (gzgm); 
      when zscd => return Compute (dshq) * Compute (tbwl); 
      when mgjl => return 3; 
      when bfgp => return Compute (jbvq) * Compute (ppcl); 
      when mpst => return Compute (zzpd) * Compute (qcnn); 
      when wjzd => return Compute (vwht) * Compute (pzcc); 
      when wqdj => return Compute (zvmv) + Compute (sntb); 
      when csdf => return 2; 
      when cgwj => return 3; 
      when rccr => return 2; 
      when vsgw => return 3; 
      when pvwn => return Compute (fpwz) * Compute (wnjc); 
      when mfql => return 3; 
      when dspn => return Compute (qlvz) + Compute (bcsc); 
      when llcn => return 4; 
      when dcph => return 4; 
      when npcs => return Compute (lrvc) + Compute (mcfr); 
      when wgvj => return 4; 
      when bblh => return 1; 
      when dmrn => return 9; 
      when qbzv => return 1; 
      when lrdn => return 3; 
      when wzfr => return Compute (nltq) + Compute (lrlz); 
      when bwnc => return 2; 
      when dswn => return 11; 
      when bnws => return 2; 
      when cdnp => return Compute (zvst) * Compute (cwjp); 
      when fddw => return Compute (zhgt) * Compute (mfst); 
      when nbbd => return Compute (flvw) * Compute (thlz); 
      when lbzc => return Compute (jfrn) * Compute (gdgd); 
      when sntb => return Compute (bhrb) * Compute (lhgv); 
      when vzts => return 6; 
      when jvfw => return Compute (hlms) + Compute (lwlc); 
      when mqcv => return 2; 
      when jhzw => return 5; 
      when lhpf => return Compute (dqzt) * Compute (fjld); 
      when znjr => return 2; 
      when wrvl => return Compute (pdbt) * Compute (qlvg); 
      when cdwt => return 6; 
      when vtjg => return Compute (stcr) * Compute (bvzn); 
      when dvnh => return Compute (brqv) * Compute (jdmt); 
      when pcgq => return Compute (jtcf) * Compute (qssh); 
      when nhjt => return 2; 
      when pmqh => return Compute (dflc) * Compute (rzsn); 
      when zrwb => return Compute (gtqh) * Compute (fwjg); 
      when zphs => return Compute (bcww) + Compute (sbtd); 
      when nmvn => return Compute (vrpp) * Compute (mdht); 
      when sbvl => return 3; 
      when bcdf => return Compute (rhwm) * Compute (zbhj); 
      when jhwn => return Compute (mgjl) * Compute (tprm); 
      when ftjv => return 3; 
      when ncnm => return Compute (vzcr) * Compute (wqfg); 
      when bgwj => return Compute (qlqt) / Compute (dnwd); 
      when hrbr => return Compute (gvmj) + Compute (vspf); 
      when qljb => return 3; 
      when vdsz => return 3; 
      when nhpf => return Compute (rdpg) + Compute (qrll); 
      when gvfn => return Compute (drnb) / Compute (fhmz); 
      when ppts => return Compute (zhrs) * Compute (lccp); 
      when fvhm => return 7; 
      when rrqr => return 2; 
      when lnqm => return Compute (rwjc) + Compute (vnvr); 
      when qwnz => return Compute (hfbh) + Compute (vdbp); 
      when cgvr => return 3; 
      when lvrc => return 4; 
      when vcfm => return 5; 
      when zhgt => return 3; 
      when nvdh => return 3; 
      when llfg => return Compute (cgcc) * Compute (nqhg); 
      when wjgf => return 2; 
      when nztj => return Compute (wqfz) + Compute (tdlw); 
      when shpt => return 2; 
      when gzmv => return 3; 
      when tqzv => return Compute (rtpz) + Compute (glqr); 
      when pdmb => return 2; 
      when fpcs => return 5; 
      when mwmp => return 6; 
      when hcfs => return 2; 
      when bshp => return Compute (chvq) + Compute (pdbc); 
      when gppj => return 1; 
      when pzpp => return 5; 
      when ltnz => return Compute (dswn) * Compute (tsqc); 
      when wzdj => return 3; 
      when frst => return 5; 
      when scdn => return 6; 
      when fpsh => return 2; 
      when gzwm => return 2; 
      when jmbs => return Compute (lwlb) * Compute (sqcb); 
      when rqsw => return 5; 
      when bnvg => return 3; 
      when tfwb => return 2; 
      when snwm => return 6; 
      when zvrq => return 5; 
      when vnpq => return Compute (djjs) + Compute (rmrj); 
      when cfbh => return 4; 
      when ntnh => return Compute (jvfw) * Compute (jzdn); 
      when jnwv => return Compute (tlnz) * Compute (zqpw); 
      when npvl => return Compute (sbqm) * Compute (zmmd); 
      when tpwq => return Compute (jnll) / Compute (flhf); 
      when flhs => return Compute (qcml) + Compute (zmdq); 
      when pnvd => return Compute (cwvl) + Compute (dgpr); 
      when zvvp => return 3; 
      when fcql => return 20; 
      when jrvp => return 17; 
      when wbvt => return Compute (bftg) + Compute (tlzh); 
      when vwht => return 3; 
      when pzvr => return 3; 
      when qncp => return 3; 
      when bcgz => return Compute (tpjp) / Compute (bngr); 
      when dcbt => return Compute (rctr) + Compute (wltv); 
      when csgr => return Compute (wrvl) / Compute (znjr); 
      when rsdv => return Compute (vcwt) * Compute (mqcv); 
      when fnqf => return 2; 
      when hzrc => return Compute (jdbp) + Compute (jnwv); 
      when cdvw => return Compute (sjfs) + Compute (fzvh); 
      when bsvc => return Compute (pwln) / Compute (nrjc); 
      when tlwr => return 3; 
      when hjmr => return 3; 
      when spjg => return 5; 
      when mqgl => return Compute (mqlt) * Compute (jlnf); 
      when mzzd => return 3; 
      when dqnv => return Compute (mhnm) * Compute (zhqg); 
      when lqcm => return Compute (ffqs) / Compute (zvrq); 
      when sqzp => return 5; 
      when drvt => return Compute (hhws) * Compute (llpf); 
      when ghvn => return Compute (vrjj) + Compute (vfrh); 
      when hfbh => return 16; 
      when wtlg => return 19; 
      when dsgt => return Compute (hhcv) * Compute (npcs); 
      when vvzh => return 6; 
      when sclr => return Compute (dddm) * Compute (nttr); 
      when jqqs => return Compute (znlh) + Compute (ffnn); 
      when zprq => return Compute (jmnj) * Compute (mwqc); 
      when dczh => return Compute (vvvb) * Compute (wcpr); 
      when lpzb => return Compute (bqvg) + Compute (mlgj); 
      when dqsv => return Compute (pmfh) * Compute (zrbc); 
      when hgzs => return 3; 
      when vrpp => return 10; 
      when snbm => return 4; 
      when tdrq => return Compute (hqdv) * Compute (csln); 
      when tbwl => return Compute (sdmb) * Compute (zrfw); 
      when jdmt => return Compute (btwt) - Compute (gwpm); 
      when fzgj => return 4; 
      when vnvr => return Compute (pmqh) * Compute (pwlt); 
      when mcht => return 2; 
      when ztrt => return Compute (vlhc) * Compute (swds); 
      when bncr => return Compute (wpcs) - Compute (pcgq); 
      when nqsl => return Compute (qcfn) + Compute (fsgb); 
      when cclc => return Compute (hjfj) + Compute (cfdl); 
      when ztpd => return Compute (dzlc) + Compute (zcgn); 
      when lfrb => return Compute (rbsf) * Compute (hzjj); 
      when pvfv => return 2; 
      when swpl => return 2; 
      when csnf => return 1; 
      when ptdz => return 2; 
      when nvhn => return 2; 
      when rdhg => return Compute (nnhm) * Compute (wpbs); 
      when jclb => return 2; 
      when cscm => return Compute (lclg) * Compute (mrwz); 
      when fcgj => return Compute (hmrz) * Compute (nwbt); 
      when hgwm => return Compute (mspl) * Compute (rmfv); 
      when rqbq => return 2; 
      when wjtc => return Compute (fszd) + Compute (tpmp); 
      when dpvl => return 8; 
      when nwdv => return 3; 
      when zcgn => return Compute (lzzh) * Compute (brfc); 
      when jvrt => return Compute (bhzq) / Compute (rqsw); 
      when zmdq => return Compute (sdwj) + Compute (mwdl); 
      when rtsd => return Compute (rcmc) + Compute (jtjd); 
      when nqzz => return 11; 
      when dshq => return Compute (rtnf) * Compute (chvn); 
      when mddc => return 2; 
      when rpwz => return 3; 
      when pmlg => return 8; 
      when blmp => return Compute (vwmr) * Compute (ndpf); 
      when hqbb => return 2; 
      when chtt => return 2; 
      when cchg => return 2; 
      when bntz => return Compute (tvrd) * Compute (ncsp); 
      when gvgp => return 3; 
      when vvhm => return Compute (mspb) * Compute (dpgl); 
      when nlcv => return 5; 
      when rwcd => return Compute (qffb) + Compute (nzcv); 
      when rfcq => return 6; 
      when zgdb => return 2; 
      when mggl => return Compute (cchg) * Compute (crcm); 
      when mwjg => return Compute (vjwz) * Compute (vsmt); 
      when rbbh => return 13; 
      when mqlt => return Compute (gwlm) * Compute (mqvg); 
      when ddtd => return Compute (qcnv) + Compute (wjzv); 
      when hpmm => return 3; 
      when cjlm => return 6; 
      when cpdf => return Compute (jjbm) * Compute (mtnb); 
      when gqrp => return Compute (tdrq) + Compute (djrh); 
      when zmtg => return 8; 
      when vwdl => return Compute (hzmn) * Compute (nqms); 
      when ggpn => return 11; 
      when mtfr => return Compute (jlqg) * Compute (dlgc); 
      when nsfm => return 4; 
      when cftm => return 18; 
      when wcdm => return Compute (tsbf) * Compute (wgvv); 
      when jcch => return 2; 
      when zqjp => return Compute (tzhp) - Compute (nldv); 
      when wjmq => return 6; 
      when cbgj => return Compute (mdjm) * Compute (tqfr); 
      when jmnj => return Compute (ppts) + Compute (fqbl); 
      when tlnz => return Compute (vfgt) * Compute (vdjz); 
      when fqjd => return 2; 
      when ngzp => return 11; 
      when trdj => return 10; 
      when qrch => return Compute (nfhf) + Compute (vhtl); 
      when hmrz => return Compute (qjrs) * Compute (rtsd); 
      when rvnh => return 3; 
      when humn =>
        if part = 1 then
          return 4481;
        else
          return human;
        end if;
      when jstb => return 5; 
      when vqlt => return 2; 
      when vlqd => return Compute (ndgh) * Compute (zptr); 
      when vjjw => return Compute (mrlc) / Compute (pvfv); 
      when mnlp => return Compute (svdw) + Compute (mtjq); 
      when jtcf => return 4; 
      when lwlc => return Compute (qqtm) * Compute (cnvb); 
      when jdgq => return 2; 
      when vznc => return 2; 
      when dcfw => return Compute (qvvc) * Compute (wqdj); 
      when fqwz => return Compute (twtb) + Compute (vtnq); 
      when hhbn => return 4; 
      when wrpp => return 14; 
      when mmnw => return 3; 
      when rdpb => return 10; 
      when hcsc => return 5; 
      when nfvj => return 4; 
      when qznt => return Compute (zfjw) * Compute (wfld); 
      when flvw => return 5; 
      when wphq => return 5; 
      when bpwd => return 2; 
      when cbfh => return 2; 
      when bcbw => return Compute (jtvm) * Compute (cdnm); 
      when dvwj => return Compute (zwgg) * Compute (dspn); 
      when tnbt => return Compute (qdfp) * Compute (bpwd); 
      when qlzt => return 12; 
      when rjfh => return 5; 
      when cprt => return 3; 
      when qgwg => return Compute (jrgs) + Compute (sphc); 
      when zpbt => return Compute (swfp) / Compute (mldc); 
      when drln => return 6; 
      when jdcd => return 3; 
      when jzcd => return Compute (nvjl) - Compute (swpl); 
      when sbpp => return 17; 
      when ttwm => return 5; 
      when rdbs => return Compute (wsml) * Compute (drrf); 
      when pfpt => return Compute (pmrt) * Compute (fvsf); 
      when rfbw => return Compute (ntpj) + Compute (vwpq); 
      when bqld => return Compute (zqjp) + Compute (rbzl); 
      when rrmg => return 19; 
      when rbsf => return 3; 
      when mlgj => return Compute (vvgt) + Compute (dmsz); 
      when jfcd => return Compute (bsqn) / Compute (blgf); 
      when sjrh => return 7; 
      when djjd => return Compute (fddw) + Compute (svvq); 
      when jhgd => return Compute (fjnh) * Compute (bbzt); 
      when nlts => return Compute (fjgh) - Compute (cnln); 
      when ppwc => return Compute (lhpf) - Compute (zprq); 
      when gptt => return 2; 
      when bvvl => return 20; 
      when ssnq => return Compute (cfmw) + Compute (wfnm); 
      when gmhz => return 4; 
      when nnsc => return Compute (cvqf) + Compute (vpvl); 
      when zbzf => return Compute (nlth) * Compute (dhfw); 
      when jhvm => return 16; 
      when mrrt => return 7; 
      when pmrt => return Compute (mwrc) * Compute (mnhc); 
      when rddj => return Compute (fbcd) / Compute (tfwb); 
      when wszm => return Compute (hsbr) + Compute (rscq); 
      when pbgr => return Compute (glqw) * Compute (qqnm); 
      when hzlh => return 2; 
      when qnbn => return Compute (jpvr) - Compute (bgzz); 
      when hrcp => return Compute (fjvg) * Compute (vjbz); 
      when dnwd => return Compute (lqrf) * Compute (flhd); 
      when rvlz => return 3; 
      when stcr => return Compute (cblz) + Compute (nfnw); 
      when ptvh => return 3; 
      when vmnc => return 5; 
      when bmwf => return Compute (rpbm) * Compute (cvgt); 
      when jzdn => return 2; 
      when djrh => return Compute (ccjf) * Compute (fqgl); 
      when lwlb => return 5; 
      when bqht => return Compute (cldm) * Compute (btqb); 
      when brfj => return Compute (prbr) + Compute (qqwz); 
      when rlqm => return 2; 
      when mdht => return Compute (jflm) - Compute (trpt); 
      when lbbj => return 2; 
      when blnq => return 6; 
      when chfz => return 3; 
      when sqsc => return 5; 
      when mwrt => return Compute (slnh) + Compute (thcc); 
      when nvrb => return 3; 
      when lgrj => return Compute (qnvn) * Compute (tnbt); 
      when pvcn => return Compute (qhwm) * Compute (smld); 
      when fbgc => return 2; 
      when mqnl => return Compute (drlt) * Compute (bbhp); 
      when bdtp => return Compute (gzpf) * Compute (qdnl); 
      when wdwf => return Compute (fhjc) - Compute (zpbt); 
      when hhjc => return Compute (qbvr) + Compute (fmwn); 
      when jzlv => return Compute (cftm) * Compute (rhtv); 
      when smwb => return Compute (dgbr) + Compute (rbbh); 
      when bhvg => return Compute (lpfz) + Compute (bjnj); 
      when zmmd => return Compute (smsm) + Compute (jfcd); 
      when mdld => return Compute (zzqd) + Compute (bdmw); 
      when qlqt => return Compute (zfbq) + Compute (ntqb); 
      when dgbr => return Compute (sbwd) * Compute (cgvr); 
      when rwsm => return 10; 
      when jnll => return Compute (nztj) * Compute (vzgb); 
      when dbht => return Compute (jdcd) * Compute (rccr); 
      when bhmz => return 3; 
      when mrlq => return Compute (bbjs) + Compute (drdl); 
      when fzdg => return 2; 
      when jbsh => return 2; 
      when svzv => return 2; 
      when zjmw => return 3; 
      when tcgm => return 2; 
      when nzcv => return 6; 
      when rmrj => return Compute (mtsv) + Compute (snlb); 
      when bjnj => return 2; 
      when qsml => return 18; 
      when mfts => return Compute (mwjg) * Compute (pjsp); 
      when ffqs => return Compute (rhcv) - Compute (rrnt); 
      when dbcw => return Compute (gtbs) * Compute (mbvg); 
      when wpcs => return Compute (phbh) * Compute (mnhf); 
      when rvhs => return 2; 
      when qmcb => return 17; 
      when hlms => return 11; 
      when vwpq => return 2; 
      when wslz => return Compute (szhq) / Compute (hstj); 
      when jpvr => return Compute (sflt) + Compute (drvt); 
      when hcdz => return Compute (nfhw) * Compute (qgjf); 
      when svvq => return Compute (twdf) * Compute (rhqj); 
      when lgpg => return Compute (wglc) + Compute (gwmr); 
      when mlwr => return Compute (zfmv) + Compute (hbqm); 
      when hbqm => return 1; 
      when fgbm => return Compute (lltp) + Compute (fwmw); 
      when cjlh => return Compute (btfl) * Compute (mpnj); 
      when tlmq => return Compute (fqwz) * Compute (lwhp); 
      when zzqd => return Compute (vnpq) / Compute (rlqm); 
      when drdl => return Compute (hzlh) * Compute (btqt); 
      when sdjb => return Compute (ssnq) * Compute (gzwm); 
      when ntmt => return 3; 
      when rwth => return Compute (tdjh) + Compute (cstb); 
      when jjgt => return Compute (crqg) * Compute (dmrn); 
      when mpvz => return Compute (ppwc) * Compute (chpt); 
      when qbmm => return Compute (rqpj) + Compute (rfcm); 
      when zdrh => return Compute (csml) + Compute (cvvv); 
      when trft => return Compute (lbbv) * Compute (nlgg); 
      when dqjz => return 7; 
      when gfrq => return 4; 
      when vvgt => return Compute (zrwb) / Compute (rfcq); 
      when tsft => return Compute (sstg) + Compute (ngfs); 
      when gzpf => return 2; 
      when tfpw => return Compute (gmln) * Compute (hqbd); 
      when cddt => return 6; 
      when rlbp => return 1; 
      when ndph => return Compute (ztzq) * Compute (jgzl); 
      when blbc => return Compute (jfvq) * Compute (cprt); 
      when cvhn => return Compute (jdgq) * Compute (llcn); 
      when zncl => return Compute (mwrt) + Compute (dbgp); 
      when rthb => return 2; 
      when jfld => return 4; 
      when gdgd => return 7; 
      when lrsc => return Compute (vdzv) * Compute (czst); 
      when gzgm => return 4; 
      when bhrb => return Compute (pnvc) / Compute (smdh); 
      when bsmm => return 2; 
      when qnds => return Compute (sbvl) * Compute (szmm); 
      when bhzq => return Compute (zhmb) * Compute (qfss); 
      when vwvc => return 17; 
      when qmgt => return 3; 
      when gfml => return 4; 
      when tjqj => return Compute (glpw) * Compute (zsns); 
      when vlmc => return 4; 
      when qlnr => return Compute (ndph) + Compute (twtq); 
      when fvph => return 3; 
      when jgzl => return Compute (drct) + Compute (vwdt); 
      when wnjc => return 2; 
      when ffbb => return 2; 
      when hzmn => return Compute (pgbv) + Compute (rwbg); 
      when rpbj => return Compute (shrh) * Compute (jvqf); 
      when sqmd => return 2; 
      when ndcd => return Compute (sdgs) * Compute (spjg); 
      when mmbs => return 3; 
      when mhbr => return Compute (wggq) + Compute (grcv); 
      when nsqq => return Compute (cvfw) * Compute (ffmp); 
      when wgvv => return 2; 
      when rvmp => return Compute (nnzp) * Compute (dbcw); 
      when dwnz => return 2; 
      when jvtz => return Compute (fctt) + Compute (bfpq); 
      when nzzv => return Compute (jbcl) * Compute (llnl); 
      when cqcg => return 3; 
      when nrvw => return 2; 
      when qqtm => return 3; 
      when fvng => return 19; 
      when phrv => return 3; 
      when tlzh => return 11; 
      when twjs => return Compute (ltnz) - Compute (ltsv); 
      when lhgv => return Compute (nwqz) + Compute (qflq); 
      when rctr => return Compute (mhdn) * Compute (mvnc); 
      when snlb => return 4; 
      when dfgh => return 7; 
      when tvtj => return 13; 
      when flcb => return 2; 
      when nqpr => return 3; 
      when jmbp => return 4; 
      when rcmc => return Compute (bpjf) / Compute (qrzq); 
      when mnpl => return Compute (pbdd) - Compute (qpct); 
      when twcj => return 3; 
      when nlth => return 2; 
      when drrq => return 13; 
      when fszd => return Compute (bmwf) + Compute (qspw); 
      when mlrz => return 2; 
      when jvgd => return Compute (dvnh) - Compute (mpjj); 
      when jtvw => return Compute (nzhf) - Compute (wfnl); 
      when ftvm => return 3; 
      when cgcz => return Compute (njcj) - Compute (bjbr); 
      when dfzw => return 2; 
      when tdhw => return Compute (rzjm) * Compute (qrgn); 
      when qwnt => return 2; 
      when mspb => return Compute (dggg) + Compute (ghtd); 
      when fhmz => return 2; 
      when dqzt => return 5; 
      when brfc => return Compute (dczh) - Compute (tfpw); 
      when hstj => return 2; 
      when rzjm => return 5; 
      when jbcl => return 3; 
      when zhpq => return Compute (gpzn) * Compute (cnmq); 
      when hmrs => return Compute (ltqr) * Compute (hslj); 
      when mtdh => return 8; 
      when lwhp => return 5; 
      when wnvf => return 8; 
      when qqdn => return 3; 
      when wrmw => return Compute (fwrh) * Compute (rbmm); 
      when gzrs => return Compute (jclb) * Compute (zjpc); 
      when nzwc => return 3; 
      when wflc => return Compute (cbgj) / Compute (qllm); 
      when gtwp => return 3; 
      when trbs => return 2; 
      when zsns => return 2; 
      when vvvb => return Compute (wrpp) - Compute (qrsg); 
      when cstb => return Compute (dfps) * Compute (svwg); 
      when gcmv => return Compute (whbb) * Compute (mfnh); 
      when vrjj => return Compute (lbbj) * Compute (jgnj); 
      when rrll => return Compute (scdt) * Compute (zjmw); 
      when wgsb => return Compute (jqrl) * Compute (jlwb); 
      when scdt => return Compute (vlmc) * Compute (qfln); 
      when wqqv => return Compute (nrps) * Compute (pnwz); 
      when szhq => return Compute (nchp) * Compute (ndtz); 
      when tqzf => return 3; 
      when wpqd => return Compute (jqsn) * Compute (przh); 
      when vmzc => return Compute (nhpf) * Compute (gcqh); 
      when pjlz => return 2; 
      when mdjm => return 3; 
      when qgjf => return Compute (pgvq) + Compute (spgw); 
      when qnhq => return Compute (rncj) * Compute (rllb); 
      when hfbg => return 2; 
      when jfsr => return 8; 
      when nfhw => return 2; 
      when cstm => return Compute (rgzs) * Compute (nbcb); 
      when ghvl => return Compute (dbfd) + Compute (qbmm); 
      when tzcl => return 2; 
      when pzzg => return Compute (ltmj) - Compute (mfts); 
      when cdqv => return Compute (dfgh) + Compute (mnlp); 
      when wcpr => return 5; 
      when jdrg => return Compute (qmpm) + Compute (hlmn); 
      when gtqh => return 8; 
      when hlmn => return Compute (rdnj) + Compute (sjcd); 
      when hnqc => return Compute (mfsz) / Compute (hbzt); 
      when sphc => return Compute (vrcl) * Compute (nhfw); 
      when bnlr => return Compute (qhvn) * Compute (vvzh); 
      when dbfd => return Compute (bcnp) / Compute (ffvs); 
      when hpzh => return Compute (snwm) + Compute (rrnp); 
      when ghqp => return 5; 
      when dfql => return Compute (dzwg) * Compute (snbl); 
      when hbzt => return Compute (shwr) + Compute (rthb); 
      when chsm => return Compute (crmb) * Compute (scwp); 
      when lpjq => return Compute (wlpd) * Compute (fcgs); 
      when plfd => return Compute (sqsc) * Compute (hvsd); 
      when vbvt => return Compute (ghqp) * Compute (qmjq); 
      when rdpg => return 7; 
      when vhds => return 12; 
      when rmfv => return 5; 
      when smft => return Compute (crmw) * Compute (msdr); 
      when wlpd => return 2; 
      when cvvw => return 2; 
      when bsqn => return Compute (wlfp) + Compute (sdjb); 
      when svcp => return Compute (jfmb) + Compute (rwsm); 
      when qflq => return Compute (scrz) * Compute (dtfz); 
      when bwwc => return 3; 
      when wqfg => return Compute (qmzt) * Compute (chfz); 
      when lbqn => return 5; 
      when zbhw => return Compute (wcdm) + Compute (hhjc); 
      when jlnf => return 3; 
      when nlst => return Compute (wmzt) + Compute (twjs); 
      when zfjw => return Compute (tbdh) + Compute (rmmz); 
      when wfms => return 2; 
      when fjps => return 5; 
      when djpm => return Compute (pznf) * Compute (qlnr); 
      when snsc => return 2; 
      when hnlj => return 5; 
      when pvfm => return 2; 
      when cwhc => return 3; 
      when blgg => return 15; 
      when jmwl => return Compute (rdpb) * Compute (zpht); 
      when wbsb => return Compute (jcdb) * Compute (ngbz); 
      when jwwv => return 2; 
      when jdsd => return 5; 
      when rsnp => return 3; 
      when rrnt => return Compute (jnnp) * Compute (dmtd); 
      when nfnw => return 2; 
      when mpfm => return Compute (mfrc) * Compute (qrch); 
      when mnzz => return Compute (lwld) + Compute (jbwd); 
      when lfjp => return 3; 
      when qvfg => return Compute (fdmf) * Compute (svst); 
      when mjmv => return Compute (cdwt) + Compute (pzvr); 
      when lclg => return 8; 
      when pcvv => return Compute (rvmp) + Compute (hmrs); 
      when wsml => return 3; 
      when gvbp => return 3; 
      when mpjj => return 10; 
      when vcqw => return Compute (jjjj) * Compute (brmz); 
      when lhvt => return Compute (pmds) * Compute (tqzf); 
      when qpdv => return 15; 
      when mrlc => return Compute (zbhw) * Compute (svzv); 
      when bdmw => return Compute (fpsh) * Compute (hwvf); 
      when vntv => return Compute (npvl) * Compute (mbqs); 
      when cfdl => return Compute (mfhg) * Compute (gzbv); 
      when gvww => return 2; 
      when dqhh => return Compute (gzwh) * Compute (thpg); 
      when mtnb => return Compute (mqph) * Compute (bnfp); 
      when wlfp => return Compute (ntsl) + Compute (lfhp); 
      when zslv => return 3; 
      when lqrr => return Compute (hhbn) * Compute (gmzh); 
      when qfzb => return Compute (nhjt) + Compute (mjmt); 
      when ltzm => return 12; 
      when qcnn => return Compute (mnpl) - Compute (wsnf); 
      when bhvr => return Compute (lflj) + Compute (rfjw); 
      when wnzr => return Compute (jbrp) * Compute (nnmh); 
      when zglw => return Compute (flcb) * Compute (cqmq); 
      when svdw => return 4; 
      when pfml => return Compute (bnlr) + Compute (ndcd); 
      when zpqd => return 2; 
      when fjnh => return 5; 
      when rhqj => return Compute (qrpv) + Compute (ngzp); 
      when szmm => return 3; 
      when ztwb => return 9; 
      when npnf => return 1; 
      when wltv => return Compute (rqfq) - Compute (hncq); 
      when bwff => return Compute (dwdd) + Compute (qhzd); 
      when slnh => return Compute (ljnf) * Compute (tnfz); 
      when ppwz => return Compute (hqbb) * Compute (nlts); 
      when hgls => return Compute (wnzr) / Compute (wwtj); 
      when dbvt => return 3; 
      when wrst => return Compute (qdrg) + Compute (bnfz); 
      when rzzg => return Compute (rpbj) + Compute (fsrm); 
      when prbr => return Compute (hpzh) - Compute (zmtg); 
      when bvdb => return 2; 
      when jlmb => return Compute (cmtm) - Compute (qbjm); 
      when rbpq => return Compute (wnbb) * Compute (glts); 
      when jwjj => return 15; 
      when dhts => return 8; 
      when dcpm => return 3; 
      when qvpv => return Compute (grcn) * Compute (cgwj); 
      when bbhp => return 2; 
      when sjfs => return Compute (ztpd) - Compute (bnzl); 
      when wqpv => return Compute (fzgj) * Compute (rvvn); 
      when sdhj => return 2; 
      when wfbz => return 2; 
      when mdnp => return Compute (gvfn) * Compute (tfqh); 
      when ctjq => return Compute (tgvp) * Compute (wjzd); 
      when mbqs => return Compute (rrmg) * Compute (rnnq); 
      when wcfp => return 3; 
      when ftzm => return 3; 
      when nwbt => return Compute (bhpw) * Compute (bhgh); 
      when bmsf => return Compute (fnws) / Compute (bcmp); 
      when cmnt => return Compute (bvjd) + Compute (lccn); 
      when wqfz => return 4; 
      when jrgs => return Compute (hjgd) + Compute (mpst); 
      when gmzh => return Compute (drhw) / Compute (sdsl); 
      when pnvs => return Compute (bmsf) * Compute (qljb); 
      when gcvn => return Compute (wqqv) + Compute (dqnv); 
      when qbvr => return Compute (ltds) - Compute (vsgw); 
      when phbh => return Compute (snsc) * Compute (flhs); 
      when shtz => return Compute (pnrv) + Compute (pvcs); 
      when smsm => return 4; 
      when gznh => return 7; 
      when tjch => return Compute (ncrt) * Compute (rrgh); 
      when bpnt => return Compute (ccfw) + Compute (dfql); 
      when lljf => return 3; 
      when qfsz => return 2; 
      when dgvh => return 3; 
      when lvbp => return Compute (hdrj) + Compute (trbs); 
      when cdvv => return 4; 
      when qlzg => return 2; 
      when rwbg => return 2; 
      when snff => return 4; 
      when flrw => return 3; 
      when tdnf => return 4; 
      when nzlr => return 2; 
      when ftzc => return 2; 
      when pznf => return Compute (cllb) * Compute (zzcv); 
      when zjvz => return Compute (znfq) * Compute (chlt); 
      when nvjl => return Compute (ngzt) * Compute (wbgs); 
      when qtvv => return 2; 
      when hdff => return 4; 
      when swmh => return Compute (vrbh) + Compute (jdjq); 
      when ghfr => return 2; 
      when ghtd => return Compute (cfzf) * Compute (gnsc); 
      when mfmt => return Compute (cdqv) * Compute (chfs); 
      when pgtq => return Compute (phwc) * Compute (gbgs); 
      when frjp => return 17; 
      when bnfz => return 2; 
      when qfqz => return Compute (mswh) * Compute (slds); 
      when djmr => return 2; 
      when crcm => return Compute (lsnd) + Compute (zbqq); 
      when ngbg => return Compute (jgld) * Compute (lhfr); 
      when sbqm => return 3; 
      when wlqd => return 2; 
      when jfrn => return 3; 
      when jbqf => return Compute (cdvw) * Compute (dnvm); 
      when zhrs => return Compute (pjjj) + Compute (fngd); 
      when lvtv => return Compute (nvjf) / Compute (nmlg); 
      when vclb => return Compute (bbcn) + Compute (jqhr); 
      when qtgd => return 2; 
      when hqbd => return 4; 
      when qqnm => return Compute (nsqq) - Compute (rfbw); 
      when nvjh => return 6; 
      when jcgh => return Compute (pmwl) * Compute (hcqw); 
      when bgzz => return Compute (swsv) * Compute (qldj); 
      when slds => return 2; 
      when lwsn => return Compute (hcfs) + Compute (rswn); 
      when bhgh => return Compute (hcsc) * Compute (mzzd); 
      when bjbr => return 3; 
      when tgvp => return Compute (smft) / Compute (cnhr); 
      when dbfm => return 9; 
      when msmm => return Compute (djmr) * Compute (bshp); 
      when llqs => return Compute (mmps) + Compute (pwqg); 
      when qqpl => return 5; 
      when pfdc => return Compute (mwmp) + Compute (zvqs); 
      when qlvg => return 2; 
      when wzdv => return 3; 
      when glqr => return Compute (qqpl) * Compute (blgg); 
      when vdgt => return 5; 
      when qrnj => return Compute (fjrq) * Compute (nwhm); 
      when fbrf => return 3; 
      when gvmj => return Compute (ngbg) / Compute (vsqt); 
      when nrjc => return 2; 
      when sdsl => return 2; 
      when mqvg => return Compute (tbqq) - Compute (bzqw); 
      when chvq => return Compute (jdsd) + Compute (cgcz); 
      when lrzn => return Compute (gfrq) + Compute (drgv); 
      when crrm => return 2; 
      when pgbv => return Compute (swhz) + Compute (dqld); 
      when brqv => return 2; 
      when gmln => return 2; 
      when wcmz => return 3; 
      when hbhz => return 18; 
      when vrcl => return 5; 
      when hqdv => return 5; 
      when nchp => return Compute (dtzl) + Compute (qpdv); 
      when bfvd => return 7; 
      when nnhm => return Compute (pzpp) * Compute (dwnz); 
      when thpg => return 4; 
      when jpzw => return 3; 
      when vrmt => return Compute (lqrr) - Compute (smrq); 
      when hnbn => return 2; 
      when tsbf => return Compute (lljf) + Compute (mqnl); 
      when nbpq => return 2; 
      when csml => return Compute (lbtn) * Compute (zspm); 
      when cnmq => return 8; 
      when qbwf => return Compute (vvhm) - Compute (dsqb); 
      when jbrp => return 11; 
      when bbzc => return Compute (ffbb) * Compute (lqft); 
      when pgfr => return 3; 
      when fmwn => return 2; 
      when lrvc => return 10; 
      when stqc => return 5; 
      when ngzt => return Compute (rlbp) + Compute (fbjn); 
      when mdrm => return Compute (jhwn) + Compute (lmht); 
      when mrtq => return Compute (rzcm) * Compute (nqgr); 
      when lpvs => return 9; 
      when lqfq => return 3; 
      when rnnq => return 3; 
      when jdfj => return 15; 
      when tqst => return Compute (cjbz) + Compute (rvhn); 
      when dnzc => return Compute (mmbs) * Compute (qlmw); 
      when lwld => return 1; 
      when sgjm => return 8; 
      when bfzd => return 17; 
      when dchw => return 4; 
      when rlrb => return Compute (tftg) + Compute (qbqm); 
      when pchr => return Compute (trft) + Compute (bcbw); 
      when lvhm => return Compute (dhqh) * Compute (dchw); 
      when mswh => return 5; 
      when sdgs => return 5; 
      when snfj => return 1; 
      when sdwj => return 2; 
      when qmqt => return Compute (wttp) / Compute (cgnv); 
      when przh => return Compute (wqlh) + Compute (nqzz); 
      when qrpv => return Compute (sqmd) * Compute (tpdj); 
      when pjsp => return 5; 
      when hpmw => return Compute (zdsr) * Compute (rlrb); 
      when jhlw => return 9; 
      when ntqb => return Compute (gzmv) * Compute (pzzg); 
      when qpct => return 3; 
      when rvvn => return Compute (rrqr) * Compute (wflc); 
      when lmcj => return Compute (szcv) + Compute (lqcm); 
      when bvjd => return 2; 
      when fngd => return Compute (wjds) * Compute (nrvw); 
      when fnws => return Compute (jmwl) + Compute (rtgr); 
      when frqq => return 4; 
      when ltsv => return 3; 
      when cmnv => return 7; 
      when wdqj => return Compute (vhrn) * Compute (hpmw); 
      when cjbz => return Compute (qlzt) * Compute (fqjd); 
      when rqpj => return Compute (tqst) * Compute (ptdz); 
      when lbtn => return 8; 
      when dbgp => return Compute (lbzn) * Compute (jqtj); 
      when qbjm => return 12; 
      when ljzj => return 15; 
      when mrvr => return 3; 
      when nght => return 2; 
      when djff => return 12; 
      when jmqh => return 14; 
      when vcpt => return Compute (ppwz) * Compute (zvqw); 
      when vsqt => return 5; 
      when tfjw => return Compute (bhvr) * Compute (vjgb); 
      when hwvf => return Compute (vszg) + Compute (lvrc); 
      when qgtf => return 6; 
      when mmtj => return 6; 
      when blgf => return 3; 
      when mvmz => return Compute (qpqb) * Compute (cvhn); 
      when svhz => return Compute (jqcf) * Compute (nzzv); 
      when wfds => return 3; 
      when pnmc => return 13; 
      when szvb => return Compute (mdld) + Compute (bfgp); 
      when bnwj => return 2; 
      when dnvm => return Compute (msmm) - Compute (tqfz); 
      when fwrh => return Compute (wdqj) - Compute (dqnp); 
      when cspp => return Compute (mggl) + Compute (rndt); 
      when lltp => return Compute (rwcd) * Compute (wfds); 
      when smrq => return Compute (lrlg) + Compute (bwqm); 
      when rgzs => return 3; 
      when crmw => return Compute (ntmt) + Compute (cjlh); 
      when dncw => return 5; 
      when rvjf => return Compute (mttr) * Compute (frlv); 
      when wlmv => return 4; 
      when fqbj => return Compute (pfdc) * Compute (rqbd); 
      when fwjg => return Compute (vdsz) * Compute (brfj); 
      when gnsc => return 2; 
      when gtwd => return 2; 
      when qdnl => return Compute (nmvn) - Compute (wpqd); 
      when pjjj => return Compute (bnjt) + Compute (fhdz); 
      when ntsl => return 1; 
      when glqw => return 3; 
      when wmzt => return 3; 
      when vjzr => return Compute (swmh) + Compute (svhz); 
      when ncgd => return 3; 
      when bngr => return 5; 
      when qtfp => return 2; 
      when bdqg => return 2; 
      when hzpr => return 5; 
      when tctd => return 13; 
      when jlqg => return 4; 
      when qhlj => return Compute (gcvn) + Compute (bntz); 
      when jtvm => return 9; 
      when szcv => return Compute (jswz) + Compute (lbvn); 
      when sfmn => return Compute (wchl) + Compute (cscm); 
      when lfhp => return Compute (nght) * Compute (wcfp); 
      when root =>
        if part = 1 then
          return Compute (bsbd) + Compute (fcgj); 
        else
          left  := Compute (bsbd);
          right := Compute (fcgj);
          return left - right;
        end if;
      when jbwd => return 6; 
      when zrnr => return 2; 
      when hrmz => return Compute (mthn) * Compute (dncw); 
      when rfjw => return 13; 
      when hpdh => return 2; 
      when jqrl => return Compute (pvwn) + Compute (hjmr); 
      when vgsm => return 5; 
      when mvgg => return Compute (nbfg) + Compute (mtfr); 
      when hcqw => return 14; 
      when cdtq => return Compute (rjrt) / Compute (mffc); 
      when spgw => return Compute (wbvt) * Compute (clbn); 
      when wfjn => return 5; 
      when hrtz => return Compute (llqs) * Compute (rvlz); 
      when rzsn => return 5; 
      when fbcd => return Compute (mfmt) + Compute (humn); 
      when vhwz => return Compute (mtwg) + Compute (fbgc); 
      when lvcn => return 4; 
      when grdf => return Compute (dbht) + Compute (sgvm); 
      when pdbc => return Compute (rsnp) * Compute (djqq); 
      when qssh => return 5; 
      when wnjl => return 9; 
      when btwt => return Compute (tzth) * Compute (hrtv); 
      when hplt => return 11; 
      when mwwr => return Compute (bsnq) * Compute (crrm); 
      when smqg => return 14; 
      when dqcj => return Compute (blbc) + Compute (bghp); 
      when ptwr => return 7; 
      when mttr => return Compute (hnqc) * Compute (wfms); 
      when pzcc => return 5; 
      when jscq => return 1; 
      when hzcm => return 4; 
      when tsjq => return Compute (nsfm) * Compute (hpdh); 
      when fsgb => return Compute (vhhz) + Compute (jhgd); 
      when plvw => return Compute (zpqd) * Compute (rffd); 
      when tcqz => return Compute (dqhh) + Compute (mlrz); 
      when mqzp => return 2; 
      when tldn => return Compute (rjfh) * Compute (zwqw); 
      when rwmq => return Compute (tdnf) * Compute (jdrg); 
      when vnft => return Compute (mqrm) + Compute (tjch); 
      when ztqg => return Compute (wtwb) * Compute (bqld); 
      when fjvg => return Compute (snfj) + Compute (gbzd); 
      when nzdz => return Compute (sfcv) + Compute (qtgd); 
      when qrgn => return 5; 
      when tbqq => return Compute (jplv) * Compute (rmzw); 
      when tdjh => return Compute (wrst) * Compute (wnjl); 
      when qwqw => return 5; 
      when pdbt => return 11; 
      when bnmb => return Compute (cvnr) * Compute (nvrb); 
      when lslq => return 1; 
      when twwv => return 19; 
      when rzcm => return 6; 
      when jtjd => return 6; 
      when htgm => return 3; 
      when pjss => return 2; 
      when mrwz => return 12; 
      when djqq => return 4; 
      when jmcf => return 12; 
      when hmqr => return Compute (zrpj) - Compute (nttd); 
      when lqdc => return 5; 
      when gpzn => return Compute (hpvb) + Compute (tpwq); 
      when pgdw => return 4; 
      when mwgb => return Compute (nvdh) * Compute (mrrt); 
      when wggq => return Compute (bgwj) + Compute (tldn); 
      when mwqc => return 2; 
      when bnfp => return 13; 
      when pwlt => return 5; 
      when hlzp => return Compute (phrv) + Compute (hrmz); 
      when qplw => return Compute (ncgd) * Compute (cmnt); 
      when vhqz => return Compute (pgtq) - Compute (ncnm); 
      when zptr => return 3; 
      when gtbs => return 13; 
      when zwwb => return Compute (pqqj) * Compute (dpqz); 
      when hmfb => return 4; 
      when mffc => return 3; 
      when lqgr => return 9; 
      when bcqm => return Compute (vjzr) + Compute (jzlv); 
      when cbdr => return Compute (wbsb) + Compute (cspp); 
      when bssd => return 2; 
      when dmsz => return Compute (vwdl) + Compute (rmqt); 
      when fpwz => return Compute (ltsl) * Compute (fbqs); 
      when rbmm => return 2; 
      when rvhn => return Compute (ttwm) * Compute (tctd); 
      when hqjt => return 7; 
      when bftg => return Compute (ftjv) * Compute (lghm); 
      when ppjl => return Compute (lfpz) / Compute (nlcv); 
      when nzbp => return Compute (chtt) * Compute (hnst); 
      when rwzt => return Compute (ltgf) * Compute (qwqq); 
      when zwgg => return 5; 
      when frlv => return 3; 
      when vmgn => return 1; 
      when tvrd => return Compute (qnhq) + Compute (ldzb); 
      when wfld => return 8; 
      when pnwz => return Compute (stlr) / Compute (fbrf); 
      when ndmd => return 2; 
      when fmbj => return Compute (slmm) * Compute (twsj); 
      when jqcf => return 3; 
      when jqsn => return 15; 
      when wzsh => return 3; 
      when hjqq => return 11; 
      when cvqf => return Compute (mpws) * Compute (sdrn); 
      when wrll => return 2; 
      when vtns => return Compute (jvrt) / Compute (cjvp); 
      when wsnf => return 4; 
      when qfjg => return 1; 
      when rsqz => return Compute (bbzc) - Compute (vfwd); 
      when dddm => return 2; 
      when ftmn => return Compute (wcmz) * Compute (hrtz); 
      when bcnd => return 13; 
      when qgtc => return 3; 
      when vthd => return Compute (zbzf) * Compute (mmnw); 
      when mvnc => return 2; 
      when ndpf => return 10; 
      when pwlh => return Compute (hbhz) + Compute (rtmt); 
      when cfzf => return Compute (rlqh) + Compute (qfrg); 
      when qldj => return 2; 
      when ftqd => return Compute (nssr) * Compute (mqzp); 
      when lhml => return 7; 
      when mrmm => return Compute (pszs) * Compute (cjlm); 
      when crnb => return Compute (ndnm) + Compute (lslq); 
      when qpbz => return 3; 
      when qspw => return 4; 
      when jtrg => return 7; 
      when nwwb => return Compute (hfvj) * Compute (snmq); 
      when rgpt => return 4; 
      when spcf => return Compute (cdvv) * Compute (wpgm); 
      when brqh => return Compute (bndd) + Compute (nbbd); 
      when qfrg => return Compute (hdff) * Compute (qmgt); 
      when ddmr => return Compute (fhzm) + Compute (qvbn); 
      when lpqs => return 1; 
      when zvqs => return 11; 
      when dfdl => return 3; 
      when vnbv => return Compute (ldbp) * Compute (frjl); 
      when bwqm => return 19; 
      when zjpc => return 4; 
      when btbn => return Compute (cvvw) * Compute (hnlf); 
      when bpgp => return Compute (nqlh) - Compute (gqgh); 
      when jbsn => return 3; 
      when tdlw => return 3; 
      when fsrm => return Compute (pzqv) * Compute (zjcn); 
      when rjrt => return Compute (nzwc) * Compute (gcmz); 
      when wpdn => return Compute (rsdv) * Compute (wcfj); 
      when dzvg => return 2; 
      when rdfn => return Compute (rbqt) * Compute (btzg); 
      when zrnz => return Compute (cbjq) * Compute (jmbs); 
      when djgb => return Compute (mrnr) + Compute (ttdb); 
      when pwln => return Compute (vqlt) * Compute (mnrr); 
      when vfgt => return 7; 
      when tbdh => return Compute (dzvg) * Compute (stcv); 
      when jlwb => return 3; 
      when cshj => return Compute (pwgj) + Compute (rpbg); 
      when lzhf => return 5; 
      when hncq => return 11; 
      when nqms => return 5; 
      when lhfr => return Compute (fgbm) / Compute (mstm); 
      when ltdq => return 4; 
      when zssf => return 5; 
      when dqld => return Compute (sjrh) * Compute (zdhf); 
      when grcv => return Compute (pnvs) * Compute (gvgp); 
      when zfbq => return Compute (ggbp) * Compute (plfd); 
      when lmjg => return 4; 
      when lncn => return 1; 
      when qmjq => return 5; 
      when rnbs => return Compute (vtgj) + Compute (dpmg); 
      when mstm => return 2; 
      when pnwh => return 2; 
      when ccfw => return 8; 
      when tsqc => return 2; 
      when gqpj => return Compute (hqjt) + Compute (fwzf); 
      when tnfz => return Compute (qrln) * Compute (nvpd); 
      when rhwm => return 4; 
      when bcnp => return Compute (dcfw) / Compute (nzlr); 
      when fvrg => return 3; 
      when vtgj => return 5; 
      when wwtj => return 5; 
      when slmm => return Compute (crrj) + Compute (bcgz); 
      when mjmt => return 4; 
      when zzpd => return 2; 
      when wttp => return Compute (lbgc) - Compute (gnrj); 
      when qdfp => return Compute (qlvw) * Compute (hmfd); 
      when psfr => return Compute (wfbp) * Compute (wzdv); 
      when rlqh => return Compute (ptwr) * Compute (lvbp); 
      when mgsz => return Compute (qqpt) * Compute (hgls); 
      when cjvp => return 2; 
      when hrjj => return 2; 
      when fjrq => return 6; 
      when qmpm => return Compute (bwwc) * Compute (hcdz); 
      when lddq => return Compute (rjbz) + Compute (ppsl); 
      when snbl => return Compute (vwhb) * Compute (szwj); 
      when bghp => return Compute (wgcm) * Compute (tfdv); 
      when zdnh => return 11; 
      when cbqn => return Compute (ppfs) * Compute (fjps); 
      when pbzb => return 2; 
      when rmzw => return Compute (ftvm) + Compute (mhlj); 
      when qpqb => return Compute (ptnv) + Compute (vhds); 
      when jffq => return 3; 
      when rscq => return Compute (bcdf) + Compute (tfjw); 
      when wjzv => return Compute (vcqw) / Compute (nqpq); 
      when ngbz => return 2; 
      when hdrj => return 5; 
      when slpc => return Compute (jstb) + Compute (lncn); 
      when bmzw => return 5; 
      when cmtm => return Compute (twcj) * Compute (tjqj); 
      when pwqg => return 17; 
      when dgpr => return Compute (bpgp) / Compute (lmjg); 
      when trpv => return 7; 
      when qnvn => return 2; 
      when gcqh => return 3; 
      when bdgd => return 2; 
      when dbcs => return Compute (nrqv) - Compute (zlpv); 
      when nrqv => return Compute (fvph) * Compute (hzrc); 
      when cnvb => return Compute (hrjj) * Compute (lbqn); 
      when cvgt => return Compute (qhtv) * Compute (vgsm); 
      when tqlp => return 2; 
      when sgpj => return 11; 
      when swsv => return 7; 
      when hndh => return 2; 
      when smwv => return Compute (gssz) + Compute (rgrp); 
      when dwdd => return Compute (fzdp) / Compute (gtwd); 
      when jszj => return Compute (gqrp) + Compute (tpnf); 
      when ncrt => return Compute (ntnh) + Compute (mwgb); 
      when rpbg => return 1; 
      when jvsz => return Compute (ndmd) * Compute (hrbr); 
      when ffvs => return 2; 
      when mnhf => return 3; 
      when hbwh => return Compute (ggpn) + Compute (pbzb); 
      when tpmp => return Compute (qjqw) * Compute (hzvm); 
      when nttr => return 3; 
      when ldbg => return Compute (cprm) * Compute (wrbf); 
      when ntpj => return 4; 
      when lbbv => return 3; 
      when vrct => return 2; 
      when lccn => return 5; 
      when nrsd => return 10; 
      when fvzw => return Compute (mwwr) + Compute (vmgn); 
      when vgjq => return 2; 
      when dptm => return 6; 
      when ggbp => return Compute (wbnh) + Compute (jhvm); 
      when qdrg => return 5; 
      when hcpf => return 4; 
      when zrbc => return 3; 
      when tpjw => return Compute (shcg) / Compute (hjmp); 
      when jnqv => return Compute (wwqp) + Compute (jcgh); 
      when fzrd => return 2; 
      when cbjl => return Compute (jdfj) * Compute (bnwj); 
      when bnjt => return 1; 
      when czpd => return 4; 
      when fjld => return Compute (tpjw) + Compute (ghvl); 
      when sbwd => return 4; 
      when rqfq => return Compute (lhmd) * Compute (nfvj); 
      when pwpn => return 3; 
      when ztzq => return Compute (nqsl) - Compute (vzts); 
      when ltqr => return Compute (dqcj) + Compute (qbgj); 
      when nlgg => return Compute (rvhs) * Compute (rqss); 
      when ltsl => return 2; 
      when zgzs => return 3; 
      when mqrm => return Compute (cqcg) * Compute (cbdr); 
      when fhtw => return Compute (fcql) + Compute (brqh); 
      when zdfg => return Compute (blmp) + Compute (qwcl); 
      when dzwg => return 4; 
      when czst => return Compute (smwb) - Compute (pvcn); 
      when ncsp => return Compute (rddj) - Compute (zhpq); 
      when bsnq => return 3; 
      when sflt => return Compute (qtvv) + Compute (ftmn); 
      when vsmt => return Compute (pwlh) + Compute (gztl); 
      when vszg => return Compute (smqg) / Compute (dfzw); 
      when tqfz => return Compute (wmdp) / Compute (mwms); 
      when fhzm => return Compute (lfjp) + Compute (rjtc); 
      when wgcm => return Compute (dsgt) / Compute (tqlp); 
      when zsvh => return Compute (grdf) + Compute (svcp); 
      when hjfj => return Compute (lmcj) * Compute (hndh); 
      when pqsr => return 2; 
      when lrbp => return Compute (wgsb) / Compute (htgm); 
      when twdf => return 2; 
      when jdbp => return Compute (pnvd) * Compute (ghfr); 
      when smdh => return 4; 
      when cjhz => return Compute (pvfm) * Compute (gbtc); 
      when psfp => return 3; 
      when cqmq => return 13; 
      when nvlv => return 17; 
      when lrqh => return Compute (gmzt) + Compute (gmpl); 
      when wqlh => return 16; 
      when qrsg => return 3; 
      when zzcv => return 14; 
      when sgzh => return Compute (mvgg) * Compute (zgdb); 
      when vrbh => return Compute (wqpv) / Compute (gwhs); 
      when drlt => return 5; 
      when shrh => return 13; 
      when qrln => return 2; 
      when hslj => return Compute (mgsz) * Compute (jddd); 
      when mtsv => return 3; 
      when hmfd => return 2; 
      when grcn => return 13; 
      when fctt => return Compute (smhv) * Compute (qzpn); 
      when lhmd => return Compute (vszd) + Compute (nfgv); 
      when rtgr => return Compute (nvhn) * Compute (szvb); 
      when zmvz => return Compute (fqbj) * Compute (swhh); 
      when jdjq => return 4; 
      when vhzf => return 2; 
      when fmvc => return Compute (fjcg) * Compute (dhts); 
      when dzlc => return Compute (rbpq) + Compute (vntv); 
      when fbjn => return Compute (tvrz) / Compute (nvjh); 
      when ppcl => return 3; 
      when jjjj => return 2; 
      when wfnm => return Compute (fvrg) * Compute (tzcl); 
      when dflc => return 2; 
      when ffnt => return Compute (mzfn) + Compute (vcpt); 
      when zbnt => return 3; 
      when dqnh => return 2; 
      when slww => return Compute (stzl) + Compute (pcwz); 
      when djjs => return Compute (ltzm) + Compute (qfjg); 
      when sqml => return 7; 
      when lghm => return 2; 
      when bsbd => return Compute (rzff) * Compute (wdwf); 
      when gwhs => return Compute (czpd) * Compute (jbsh); 
      when zqpw => return Compute (jsrb) + Compute (jwjj); 
      when drgv => return 3; 
      when rswn => return 5; 
      when gnjq => return 2; 
      when sjcd => return Compute (bcpn) * Compute (fvzw); 
      when fzml => return 11; 
      when mthn => return 2; 
      when cgnr => return 4; 
      when ldbp => return Compute (vrmt) + Compute (gptt); 
      when lqrf => return 2; 
      when gjbv => return Compute (djff) * Compute (stqc); 
      when ljnf => return Compute (rtjb) * Compute (wtnp); 
      when hjmp => return 6; 
      when mrnr => return 2; 
      when zspm => return 5; 
      when qlvz => return Compute (gjzv) + Compute (hpmm); 
      when tfdv => return 2; 
      when vtvn => return Compute (hrcp) * Compute (wtlq); 
      when qlmw => return Compute (vcfm) + Compute (pqsr); 
      when hsmp => return Compute (rpwz) * Compute (wclq); 
      when wcfj => return 2; 
      when brst => return 5; 
      when fnrh => return 2; 
      when qhzd => return Compute (fqlp) + Compute (jpzw); 
      when mmps => return Compute (jfld) * Compute (lqgr); 
      when lpfz => return Compute (lqfq) * Compute (wgbc); 
      when bjcs => return Compute (mhbr) / Compute (vgjq); 
      when tbqc => return 2; 
      when qhwm => return 3; 
      when hzvm => return Compute (npnf) + Compute (nhnp); 
      when nqhg => return Compute (tcjb) + Compute (hgzs); 
      when bzdh => return 2; 
      when gbtc => return 3; 
      when wfnl => return 5; 
      when ndlf => return Compute (cdnp) + Compute (ddmr); 
      when nmlt => return Compute (pnpc) * Compute (hnlj); 
      when nhfw => return Compute (tbqc) + Compute (dnzc); 
      when rtbm => return 3; 
      when fbpz => return Compute (sqzp) + Compute (pjrt); 
      when rjtc => return Compute (tjtl) + Compute (wtlg); 
      when vspf => return Compute (bnmb) + Compute (ndsm); 
      when dhqh => return Compute (dhpp) * Compute (zsvh); 
      when ddln => return 1; 
      when lbzn => return 2; 
      when vhrn => return Compute (bjcs) - Compute (vnft); 
      when cgcc => return 6; 
      when cgvs => return 7; 
      when jqtj => return 5; 
      when wqnp => return 12; 
      when dmtd => return Compute (fjzz) + Compute (sgpj); 
      when rlvd => return Compute (vlqd) / Compute (mgsl); 
      when cvvv => return Compute (bqtp) * Compute (pnwv); 
      when rjdp => return Compute (qfzb) + Compute (csnf); 
      when hmvq => return 17; 
      when vjtf => return Compute (lbzc) * Compute (djjd); 
      when gbgs => return Compute (mqfd) - Compute (fmbj); 
      when wgbc => return 3; 
      when vdbp => return 1; 
      when vtdp => return 10; 
      when cgvv => return Compute (bfvd) * Compute (pbhg); 
      when vfhg => return Compute (nlsw) - Compute (qgnb); 
      when pcwz => return Compute (dptm) * Compute (frjj); 
      when zngg => return 4; 
      when scrz => return 2; 
      when tvrz => return Compute (wslz) / Compute (dpvl); 
      when mqzw => return 2; 
      when zfmv => return 10; 
      when gdmv => return 5; 
      when cgnv => return 2; 
      when qlvw => return Compute (rfcv) + Compute (nlst); 
      when fjzn => return Compute (hgnr) + Compute (lqml); 
      when vzgb => return 12; 
      when dsng => return 5; 
      when pbvh => return 3; 
      when qbqm => return 10; 
      when vwdt => return Compute (lgrj) / Compute (pmlg); 
      when lhsw => return Compute (lpjq) + Compute (rdbs); 
      when hzjj => return 3; 
      when jplv => return 3; 
      when zmlh => return Compute (nfbp) * Compute (fhtw); 
      when zwbh => return Compute (sshm) * Compute (rjdp); 
      when mhnm => return Compute (bwnc) + Compute (fzwr); 
      when wmdp => return Compute (tdhw) - Compute (gjmt); 
      when pbhg => return 2; 
      when qmzt => return 9; 
      when dlws => return Compute (bssd) * Compute (rtbm); 
      when rbzl => return Compute (pjlz) * Compute (qfqz); 
      when tpjp => return Compute (jdsc) * Compute (wqnp); 
      when vlgn => return 2; 
      when phwc => return 10; 
      when bcng => return Compute (ctjq) + Compute (bdtp); 
      when cvnr => return Compute (qtmp) + Compute (bmzw); 
      when bmlj => return Compute (fvng) + Compute (czvm); 
      when llhb => return 3; 
      when nbcb => return Compute (rnns) + Compute (ldbg); 
      when ffmp => return Compute (plzd) + Compute (tzcd); 
      when nnzp => return Compute (lhsw) * Compute (tlwr); 
      when szlh => return 2; 
      when gztl => return 11; 
      when mtwg => return Compute (hcpf) * Compute (pjss); 
      when mpws => return Compute (pnfb) * Compute (pbvh); 
      when fqgl => return 3; 
      when qqpt => return 5; 
      when bqtp => return 2; 
      when nttd => return 9; 
      when svvh => return Compute (shpt) * Compute (fmvc); 
      when drnb => return Compute (lqqh) * Compute (mlbw); 
      when ptnv => return Compute (csgr) + Compute (plvw); 
      when wtnp => return 3; 
      when ndgh => return 4; 
      when lrgv => return 2; 
      when nrqr => return Compute (vvbz) + Compute (vfzw); 
      when hfvj => return 2; 
      when ctqb => return Compute (tsft) * Compute (lrgv); 
      when nqlh => return Compute (zdfg) * Compute (sdhj); 
      when nhnp => return 13; 
      when tcjb => return 12; 
      when mnhc => return Compute (szlh) * Compute (pcvv); 
      when jnnp => return 3; 
      when zbqq => return Compute (mtdh) * Compute (frst); 
      when gjzv => return Compute (brst) * Compute (twjq); 
      when shwr => return 5; 
      when gmpl => return Compute (qlmg) / Compute (cddt); 
      when lfpz => return Compute (mvmz) + Compute (hsmp); 
      when tmhl => return Compute (jbsn) * Compute (fhjz); 
      when wglc => return 14; 
      when ppll => return 5; 
      when ccbp => return 4; 
      when jflm => return Compute (cclc) / Compute (rgpt); 
      when mwms => return 2; 
      when svwg => return 4; 
      when qfss => return 10; 
      when tjtl => return 7; 
      when vwmr => return Compute (lvtv) + Compute (tvgm); 
      when cllb => return 3; 
      when nssr => return 3; 
      when qcfn => return Compute (lvcn) * Compute (wjmq); 
      when rmvq => return 2; 
      when rbtb => return Compute (mgmw) / Compute (gvww); 
      when jcdb => return Compute (mjmv) * Compute (zbnt); 
      when cdpw => return Compute (ngps) * Compute (hmqr); 
      when zdsr => return 2; 
      when plzd => return Compute (rvnh) * Compute (sszh); 
      when phjj => return Compute (hzcm) * Compute (bvdb); 
      when bnmf => return Compute (jtvw) * Compute (rdfn); 
      when twtq => return Compute (jbqf) - Compute (ztct); 
      when qwcl => return Compute (jjvz) / Compute (tptf); 
      when ccjf => return 13; 
      when mrjc => return Compute (fnrh) * Compute (cstm); 
      when thlz => return 2; 
      when thcc => return Compute (ljtp) + Compute (ztwb); 
      when fjcg => return 2; 
      when flhd => return 3; 
      when gnrj => return Compute (ppjl) * Compute (mmtj); 
      when hnlf => return 5; 
      when tprm => return 2; 
      when vlhc => return 13; 
      when tvgm => return Compute (gdmv) * Compute (dqsv); 
      when qvpw => return Compute (gqpj) * Compute (jvtz); 
      when vfwd => return 5; 
      when pmwl => return Compute (lwsn) * Compute (lddq); 
      when rzbj => return 17; 
      when rncj => return 5; 
      when znfq => return 2; 
      when rjbz => return Compute (gznh) * Compute (mcht); 
      when hhws => return Compute (vclb) + Compute (ftbq); 
      when qfln => return 3; 
      when ltmj => return Compute (wghf) / Compute (zjvz); 
      when csln => return 17; 
      when snvw => return 3; 
      when ltgf => return 3; 
      when wfbp => return 2; 
      when hjlz => return Compute (qmqt) + Compute (jvgd); 
      when zvst => return 3; 
      when qvvc => return 8; 
      when bjfh => return 3; 
      when mzfn => return Compute (cdpw) * Compute (ftzm); 
      when dqnp => return Compute (bcqm) / Compute (wjgf); 
      when stlr => return Compute (vzgd) * Compute (gvbp); 
      when cvfw => return 3; 
      when rtmh => return 15; 
      when rqss => return 3; 
      when jddd => return Compute (dcph) + Compute (mrvr); 
      when gggh => return 3; 
      when gssz => return Compute (nwwb) * Compute (vmnc); 
      when tsms => return Compute (gnzq) * Compute (rqbq); 
      when lrcj => return 19; 
      when pnfb => return 3; 
      when rtjb => return 2; 
      when zvmv => return Compute (fvhm) * Compute (gmhz); 
      when dfrb => return Compute (fbtc) + Compute (bppp); 
      when mcfr => return 1; 
      when pjrt => return 1; 
      when ltds => return Compute (wrll) * Compute (wnvf); 
      when fvsb => return Compute (qncp) * Compute (cfbh); 
      when nldv => return 4; 
      when cdnm => return 3; 
      when jwqg => return 2; 
      when jvqz => return 1; 
      when sqdd => return Compute (rzbj) + Compute (mqgl); 
      when cwvl => return Compute (jszj) + Compute (rvjf); 
      when qcnv => return Compute (nrqr) + Compute (wrmw); 
      when fhjz => return 2; 
      when svst => return Compute (qgtc) + Compute (snbm); 
      when qdrc => return 17; 
      when cfmw => return 1; 
      when jgld => return 5; 
      when bppp => return Compute (llhb) * Compute (qwqw); 
      when wwqp => return Compute (vjtf) + Compute (rwmq); 
      when cprm => return 4; 
      when fcgs => return 7; 
      when swfp => return Compute (tnnt) + Compute (lvhm); 
      when frjl => return 2; 
      when nwqz => return 1; 
      when mldc => return 3; 
      when mgmw => return Compute (pfvz) * Compute (hjqq); 
      when mnrr => return Compute (vvjg) / Compute (gggh); 
      when hvtv => return Compute (qznt) * Compute (wzfr); 
      when tpdj => return 9; 
      when znlh => return 4; 
      when qbgj => return Compute (mhzn) + Compute (nmts); 
      when cnhr => return 3; 
      when djgh => return Compute (bdqg) + Compute (nzdz); 
      when lccp => return 3; 
      when frrn => return 3; 
      when bbjs => return 7; 
      when gdpg => return 8; 
      when pnwv => return Compute (lhvt) * Compute (ggnd); 
      when vhtl => return Compute (snff) * Compute (vldc); 
      when vjwz => return 2; 
      when qwqq => return 2; 
      when rdnj => return Compute (smwv) + Compute (dcbt); 
      when nltq => return Compute (cbqn) - Compute (lrzn); 
      when fpvs => return Compute (jvsz) / Compute (fzdg); 
      when shcg => return Compute (qpqs) - Compute (snhd); 
      when stzl => return Compute (rwzt) + Compute (mnzz); 
      when rfcv => return Compute (qwnz) * Compute (mfql); 
      when snhd => return Compute (jtrg) * Compute (sqdd); 
      when gjmt => return 3; 
      when btzg => return 5; 
      when vfrh => return 9; 
      when lpjr => return Compute (bvvl) + Compute (rmvt); 
      when nzbz => return Compute (dqnh) * Compute (hplt); 
      when dhpp => return 2; 
      when hvsd => return 3; 
      when ldtj => return 2; 
      when chlt => return 4; 
      when rhtv => return Compute (fzml) * Compute (jnzr); 
      when wpgm => return 5; 
      when csfh => return 2; 
      when mhzn => return Compute (zglw) * Compute (slpm); 
      when wtbs => return Compute (ltdq) * Compute (vhzf); 
      when cpqv => return 15; 
      when dngq => return 2; 
      when vszd => return 12; 
      when pgvq => return Compute (ptvh) * Compute (hgwm); 
      when smnv => return 3; 
      when zrfw => return 7; 
      when jbvq => return 7; 
      when rmqt => return Compute (gcmv) / Compute (vlgn); 
      when pfvz => return 2; 
      when fmvw => return Compute (qnbn) * Compute (jpcd); 
      when jqhr => return 4; 
      when szwj => return 3; 
      when tqfr => return Compute (mrmm) + Compute (bcnd); 
      when rmvt => return 3; 
      when qllm => return 3; 
      when gcmz => return Compute (zrnr) * Compute (vffj); 
      when sqcb => return Compute (sfmn) + Compute (hvtv); 
      when chvn => return 3; 
      when fzvh => return Compute (ztrt) * Compute (nbpq); 
      when sbtd => return 3; 
      when hnst => return Compute (wvfb) + Compute (zphs); 
      when ljtp => return 16; 
      when lqml => return 4; 
      when dpqz => return 3; 
      when pwgj => return Compute (flrw) * Compute (cbfh); 
      when qvbn => return 2; 
      when ppsl => return Compute (dlws) + Compute (bjwg); 
      when jfmb => return Compute (jbtg) * Compute (cwhc); 
      when bzqw => return 2; 
      when nhcr => return 3; 
      when llnl => return 3; 
      when fzwr => return 5; 
      when mfst => return Compute (cdtq) / Compute (qfsz); 
      when flhf => return 3; 
      when whbb => return 8; 
      when rllb => return 3; 
      when rfcm => return Compute (rsqz) * Compute (ztdn); 
      when lqft => return Compute (tcgm) * Compute (nzbz); 
      when snmq => return Compute (jwqg) + Compute (zdnh); 
      when fwmw => return Compute (jfqs) + Compute (sgjm); 
      when ngfs => return Compute (ccbp) * Compute (dphz); 
      when mqfd => return Compute (lpzb) / Compute (ftzc); 
      when lflj => return Compute (vtdp) * Compute (cjhz); 
      when vzcr => return Compute (jmqh) + Compute (zbbf);   
    end case;
  end Compute;
  
  T0 : constant Time := Clock;
  r : array (1 .. 2) of Integer_64;

begin
  part := 1;
  if verbose > 0 then
    Put_Line
      (+"Simple computation: " & Integer_64'Image (Compute_Mini (root_m)));
  end if;
  r (1) := Compute (root);

  part := 2;
  if verbose > 0 then
    Put_Line
      (+"Difference at root: " & Integer_64'Image (Compute_Mini (root_m)));
  end if;

  if Argument_Count >= 2 then
    --  Compiler test mode.
    if r (1) /= Integer_64'Value (To_String (Argument (1))) or
       r (2) /= Integer_64'Value (To_String (Argument (2)))
    then
      Set_Exit_Status (1);  --  Compiler test failed.
    end if;
  else
    Put_Line (+"Done in: " & (Clock - T0) & " seconds");
    Put_Line (+"Part 1: bla bla:" & Integer_64'Image (r (1)));
    Put_Line (+"Part 2: bli bli:" & Integer_64'Image (r (2)));
    --  Part 1: validated by AoC: 286698846151845
    --  Part 2: validated by AoC: 
  end if;
end AoC_2022_21;
