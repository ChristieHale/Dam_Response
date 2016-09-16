      program Quad_Pre1
      
c     This is a pre-processor program that reads NGA-West2 ground motion files
c     and calculates the period corresponding to maximum spectral acceleration
c     of the horizontal input motion. This number is an input for
c     Quad4MU, referred to as 'prinput.' Additional information is written to
c     the output file for checking (pga, npts, dt).
 
      implicit none
      include 'max_dims.h'  
      include 'declare.h'
      
      pi=4.*atan(1.)

c       read in run file 
        run_file = "run_fortran.txt"
        open (a,file=run_file,status='old')
        read (a,'( a80)') gm_list
        open (b,file=gm_list,status='old')
        read (a,*) n_files
        read (a,*) printflag 

c       write headers for output files
        open (c,file="prinput.txt",status='unknown')
        write (c,*) '      pga', '   npts', '          dt', '   maxmax_sa', '     prinput'
       
c       loop over ground motion files    
        do ifile=1,n_files

c         initialize ground motion accel and rsp
          do i=1,MAXPTS
            accel(i) = 0.0
          enddo

c         read in ground motion accel (NGA-West2)
          read (b,'( a80)') gm_file
          write (*,'( a80)') gm_file
          open (d,file=gm_file,status='old')
          do j=1,3
            read (d,'( a1)') dummy
          enddo
          read (d,'( 5x,i7,8x,f5.4)') npts, dt                     
          if ( npts .gt. 40000) npts=40000
          read (d,*) (accel(i),i=1,npts)
          close (d)

c         find PGA
          pga = 0.
          do i=1,npts
            if ( abs( accel(i)) .gt. pga ) pga = abs(accel(i))
          enddo  
          period(1) = 0.0 
          max_sa(1) = pga   

c         set damping
          damp = 0.05
          
c         set periods
          period(2) = 0.01
          do iper=3,MAXPER
            period(iper) = period(iper-1)*1.005
          enddo          

c         loop over periods, calculating response Sa
          do iper=2,MAXPER
            omega = 2*pi/period(iper)
            do i=1,MAXPTS
              rsp(i) = 0.0
            enddo
            call coeff (omega, damp, dt)
            call brs (accel, omega, damp, npts, rsp)
            max_sa(iper) = 0.
            do i=1,npts
              if ( abs( rsp(i)) .gt. max_sa(iper) ) max_sa(iper) = abs(rsp(i))
            enddo 
          enddo
          
c         find prinput
          maxmax_sa = 0.
          do iper=1,MAXPER
            if ( max_sa(iper) .gt. maxmax_sa ) then
              maxmax_sa = max_sa(iper)
              prinput = period(iper)
            endif
          enddo  
          
          if (printflag .eq. 1) then
c           print individual response spectra to file
            write (10+ifile,*) 'period(s) ', 'rsp(g) '
            do iper=1, MAXPER
              write (10+ifile,'( 2f10.4)') period(iper), max_sa(iper)
            enddo
            close(10+ifile)
          endif

c         print all pga, npts, dt, max_sa, and prinput to one file
          write (c,'(f10.5,2x,i5,2x,f10.5,2x,f10.5,2x,f10.5)') pga, npts, dt, maxmax_sa, prinput      

        enddo        
      end

c -------------------------------------------------------
 
      subroutine coeff (w, beta1, dt1)
      
      implicit none
      
      real beta1, dt1, w
      real a11, a12, a21, a22, b11, b12, b21, b22
      real beta, dt, t1, t2, t3, t4, s1, s2
      common /coef/a11,a12,a21,a22,b11,b12,b21,b22

      beta = beta1
      dt = dt1

c     Set up repeated terms
      t1 = sqrt(1.-beta**2)
      t2 = sin (w*t1*dt)
      t3 = cos (w*t1*dt)
      t4 = exp (-beta*w*dt)
      s1 = (2.*beta**2-1.) / (w**2*dt)
      s2 = 2.*beta / (w**3*dt)

c     calculate the as
      a11 = t4*(beta*t2/t1+t3)
      a12 = t4*t2 / (w*t1)
      a21 = -t4*w*t2 / t1
      a22 = t4*(t3-beta*t2/t1)
c
c     calculate the bs
      b11 = t4*((s1+beta/w)*t2 / (w*t1) + (s2+1./w**2)*t3) - s2
      b12 = -t4*(s1*t2/(w*t1)+s2*t3) - 1./w**2 + s2
      b21 = (s1+beta/w) * (t3-beta*t2/t1)
      b21 = t4*(b21 - (s2+1./w**2)*(w*t1*t2+beta*w*t3)) + 1./(w**2*dt)
      b22 = s1*(t3-beta*t2/t1)
      b22 = -t4*(b22 - s2*(w*t1*t2+beta*w*t3)) - 1./(w**2*dt)
      
      return
      end

c -------------------------------------------------------
       
      subroutine brs (x,w,beta,npts,rsp)
      
       implicit none
       
       real x(1), rsp(1), beta, w
       real d, v, a, z, ap1, dp1, vp1, t1, t2
       real a11, a12, a21, a22, b11, b12, b21, b22
       integer npts, i
       common /coef/ a11,a12,a21,a22,b11,b12,b21,b22

c      initialize
       t1 = 2.*beta*w
       t2 = w**2
       d = 0.
       v = 0.
       a = 0.
c
c      calculate the response
       do 10 i=1,npts
         ap1 = x(i)
         dp1 = a11*d + a12*v + b11*a + b12*ap1
         vp1 = a21*d + a22*v + b21*a + b22*ap1
         z = -(t1*vp1 + t2*dp1)  !spectral acceleration
 	!  z = w**2 * dp1         !pseudo-spectral acceleration
         rsp(i) = z
         a = ap1
         v = vp1
         d = dp1
  10  continue
 
      return
      end
