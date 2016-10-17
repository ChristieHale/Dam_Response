      integer a, b, c, d, e, f, g, h, aa, bb, cc
      parameter (a=7, b=8, c=9, d=10, e=11, f=12, g=13, h=14, aa=15, bb=16, cc=17)
      integer WinType, Win_len, Win_len0, Win_len1, Win_len2, Win_len3
      integer Win_len4, Win_len5, Win_len6
      integer nFiles, iFile, i, j, npts, npts1, npts2, npts3, loopmax
      integer mRock, mDam1, mDam2, mDam3, mDam4, msurf1, msurf2
      integer mflag1, mflag2, mflag3, mflag4, mflag5, mflag6
      real rockTH(MAXPTS), dam1(MAXPTS), dam2(MAXPTS), dam3(MAXPTS), dam4(MAXPTS), Ky1, Ky2
      real fasRock(MAXPTS), fasDam1(MAXPTS), fasDam2(MAXPTS), fasDam3(MAXPTS), fasDam4(MAXPTS)
      real fasSurf1(MAXPTS), fasSurf2(MAXPTS)
      real TF1Sm(MAXPTS), TF2Sm(MAXPTS), TF3Sm(MAXPTS), TF4Sm(MAXPTS), TF5Sm(MAXPTS), TF6Sm(MAXPTS)
      real TF1(MAXPTS), TF2(MAXPTS), TF3(MAXPTS), TF4(MAXPTS), dt, dum1, dum2, df, pga
      real TF5(MAXPTS), TF6(MAXPTS)
      real fTFmax1, fTFmax2, fTFmax3, fTFmax4, fTFmax5, fTFmax6
      real sTFmax1, sTFmax2, sTFmax3, sTFmax4, sTFmax5, sTFmax6
      real sfmax1, sfmax2, sfmax3, sfmax4, sfmax5, sfmax6
      real ffmax1, ffmax2, ffmax3, ffmax4, ffmax5, ffmax6
      real flow1, flow2, flow3, flow4, flow5, flow6
      real fhigh1, fhigh2, fhigh3, fhigh4, fhigh5, fhigh6
      real slow1, slow2, slow3, slow4, slow5, slow6
      real shigh1, shigh2, shigh3, shigh4, shigh5, shigh6
      real sigmax
      real damping1, damping2, damping3, damping4, damping5, damping6
      real alpha1, alpha2, alpha3, alpha4, alpha5, alpha6
      real response1(MAXPTS), response2(MAXPTS), response3(MAXPTS), response4(MAXPTS)
      real response5(MAXPTS), response6(MAXPTS)
      real response12(MAXPTS), response22(MAXPTS), response32(MAXPTS), response42(MAXPTS)
      real response52(MAXPTS), response62(MAXPTS)
      real responsef1(MAXPTS), responsef2(MAXPTS), responsef3(MAXPTS), responsef4(MAXPTS)
      real responsef5(MAXPTS), responsef6(MAXPTS)
      real damping12, damping22, damping32, damping42, damping52, damping62
      real alpha12, alpha22, alpha32, alpha42, alpha52, alpha62
      real TFSDOF1(MAXPTS), TFSDOF2(MAXPTS), TFSDOF3(MAXPTS), TFSDOF4(MAXPTS) 
      real TFSDOF5(MAXPTS), TFSDOF6(MAXPTS)
      real TFSDOF12(MAXPTS), TFSDOF22(MAXPTS), TFSDOF32(MAXPTS), TFSDOF42(MAXPTS)
      real TFSDOF52(MAXPTS), TFSDOF62(MAXPTS)
      real TF2mode1(MAXPTS), TF2mode2(MAXPTS), TF2mode3(MAXPTS), TF2mode4(MAXPTS) 
      real TF2mode5(MAXPTS), TF2mode6(MAXPTS)
      real respTH1(MAXPTS), respTHf1(MAXPTS), respTH1_f1(MAXPTS)
      real respTH5(MAXPTS), velSDOF5(MAXPTS), disSDOF5(MAXPTS)
      real vel1(MAXPTS), dis1(MAXPTS), velSDOF1(MAXPTS), disSDOF1(MAXPTS)
      real vel2(MAXPTS), dis2(MAXPTS)
      real velSDOFf1(MAXPTS), disSDOFf1(MAXPTS), velSDOF1_f1(MAXPTS), disSDOF1_f1(MAXPTS) 
      real SDOF1_f1(MAXPTS)
      real surf1(MAXPTS), surf2(MAXPTS)
      complex cuRock(MAXPTS), cuDam1(MAXPTS), cuDam2(MAXPTS), cuDam3(MAXPTS), cuDam4(MAXPTS)
      complex cuSurf1(MAXPTS), cuSurf2(MAXPTS)
      complex cuSDOF1(MAXPTS), cuSDOF2(MAXPTS), cuSDOF3(MAXPTS), cuSDOF4(MAXPTS)
      complex cuSDOF5(MAXPTS), cuSDOF6(MAXPTS)
      complex cuSDOF12(MAXPTS), cuSDOF22(MAXPTS), cuSDOF32(MAXPTS), cuSDOF42(MAXPTS)
      complex cuSDOF52(MAXPTS), cuSDOF62(MAXPTS)
      complex cuSDOFf1(MAXPTS), cuSDOFf2(MAXPTS), cuSDOFf3(MAXPTS), cuSDOFf4(MAXPTS)
      complex cuSDOFf5(MAXPTS), cuSDOFf6(MAXPTS)
      complex cuSDOF1_f1(MAXPTS)
      character*80 runfile, lsrockfile, lsdamfile, rockfile, damfile, dummy
      character*80 lssurface, surfacefile 
