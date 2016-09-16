      integer a, b, c, d
      parameter (a=7, b=8, c=9, d=10)      
      integer i, j, n_files, ifile, npts, iper, printflag
      real pga, dt, damp, pi, period(MAXPER), max_sa(MAXPER), accel(MAXPTS), rsp(MAXPTS),
     1     maxmax_sa, omega, prinput
      character*80 run_file, gm_list, gm_file, dummy
      
      
