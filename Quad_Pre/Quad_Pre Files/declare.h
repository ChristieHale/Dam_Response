      integer a, b, c, d
      parameter (a=1, b=2, c=3, d=4)
      integer iNyq, nFiles, iFile, i, j, npts, npts1, mRock, printflag
      real rockTH(MAXPTS), fasRock(MAXPTS), Nyquist, dt, df, pga
      real max_fasrock, frinput, prinput
      complex cuRock(MAXPTS)
      character*80 runfile, lsrockfile, rockfile, dummy
