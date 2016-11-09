      integer a, b, c, d, e, f, g, h, aa, bb, cc, dd
      parameter (a=7, b=8, c=9, d=10, e=11, f=12, g=13, h=14, aa=15, bb=16, cc=17, dd=18)
      integer WinType, Win_len0, Win_len5, Win_len6, mRock
      integer nFiles, iFile, i, j, npts, npts1, npts2, loopmax, msurf1, msurf2
      real rockTH(MAXPTS), dam1(MAXPTS), dam2(MAXPTS), dam3(MAXPTS), dam4(MAXPTS) 
      real Ky1, Ky2, fasRock(MAXPTS), fasSurf1(MAXPTS), fasSurf2(MAXPTS)
      real TF5Sm(MAXPTS), TF6Sm(MAXPTS), dt, dum1, dum2, df, pga, pga_c
      real TF5(MAXPTS), TF6(MAXPTS), fTFmax5, fTFmax6, ffmax5, ffmax6 
      real flow5, flow15, flow6, flow16, fhigh5, fhigh6, sigmax, damping5, damping6, alpha5, alpha6
      real response5(MAXPTS), response6(MAXPTS), TFSDOF5(MAXPTS), TFSDOF6(MAXPTS)
      real respTH5(MAXPTS), velSDOF5p(MAXPTS), disSDOF5p(MAXPTS), surf1(MAXPTS), surf1n(MAXPTS)
      real respTH6(MAXPTS), velSDOF6p(MAXPTS), disSDOF6p(MAXPTS), surf2(MAXPTS), surf2n(MAXPTS)
      real respTH5n(MAXPTS), respTH6n(MAXPTS)
      real velSDOF5n(MAXPTS), disSDOF5n(MAXPTS), velSDOF6n(MAXPTS), disSDOF6n(MAXPTS)
      real vel1p(MAXPTS), dis1p(MAXPTS), vel2p(MAXPTS), dis2p(MAXPTS)      
      real vel1n(MAXPTS), dis1n(MAXPTS), vel2n(MAXPTS), dis2n(MAXPTS)
      complex cuRock(MAXPTS), cuSurf1(MAXPTS), cuSurf2(MAXPTS)
      complex cuSDOF5(MAXPTS), cuSDOF6(MAXPTS)
      character*80 runfile, lsrockfile, lsdamfile, rockfile, damfile, dummy
      character*80 lssurface, surfacefile 
