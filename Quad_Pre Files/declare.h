      integer a, b, c, d
      parameter (a=7, b=8, c=9, d=10)
      integer iNyq, nFiles, iFile, i, j, npts, npts1, mRock
      real rockTH(MAXPTS), fasRock(MAXPTS), Nyquist, dt, df, pga
      real max_fasrock, frinput, prinput
      complex cuRock(MAXPTS)
      character*80 runfile, lsrockfile, rockfile, dummy
