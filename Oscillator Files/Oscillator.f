        program Oscillator
        
c       declare variables
        implicit none
        character*40 file_pulse
        integer npts, nmax, u, i, p, k, j, f, n, g, npts1, nmin, mpad
        parameter (nmax = 9000, u = 21, n = 22)
        real*8 omega, damping, time(nmax), accel(nmax), response(nmax),
     1         dt, df, freq(nmax) 
        complex cu(nmax)
        
        write (*,*) ' '
        write (*,*) 'Oscillator'
        write (*,*) ' '
        
        omega = 30.0
        
c       read pulse input acceleration time history from a data file 
c       and store in two arrays time, accel       
        write (*,*) 'Enter the pulse acceleration time history filename.'
        write (*,*) '                          '
        read (*,*) file_pulse       
        open (u, FILE=file_pulse, STATUS='OLD')     
        read(u,*) npts, damping        
        if (npts.GT.nmax) then
           write (*,*) 'Error: npts = ', npts, 'is larger 
     1     than npts = ', nmax 
           goto 9999
        endif       
        do i= 1, npts        
          read(u,*) time(i), accel(i)          
        enddo
        close (u)

        dt = time(2) - time(1)

c       calculate coeff
        call coeff (omega, damping, dt)

c       calculate response
        call brs (accel, omega, damping, npts, response) 

c       pad the response time history to the power of 2
        npts1 = npts
	nmin = 0
        call pad (response, npts1, nmin, mpad)

c       fill complex array
        do j=1,npts1
          cu(j) = cmplx(response(j),0.0)
        enddo
        
c       calculate forward FFT
        call cool ( -1., mpad, cu )
	  df = 1. / ( npts1*dt )	  
	  freq(1) = 0.0
	  do f = 2,npts1-1
	    freq(f) = (f-1)*df
	  enddo

c       Create file for response time history
        open (p, file='Response.csv', access = 'append',
     1       status ='new')
        write(p,*) 'time, response'
        do k=1, npts
          write(p,*) time(k), response(k)
        enddo
        close(p)
        
c       Create file for FFT
        open (n, file='FFT.csv', access = 'append',
     1       status ='new')
        write(n,*) 'frequency, real, imaginary, modulus'
        do g=1, npts1-1
          write(n,*) freq(g), real(cu(g)), aimag(cu(g)), cabs(cu(g))
        enddo
        close(p)
        
        write (*,*) 'Finished.'         
        
9999        stop        
        
        end  
        
c -------------------------------------------------------
 
      subroutine coeff (w, beta1, dt1)
      
      implicit none
      real*8 beta1, dt1, w
      real*8 a11, a12, a21, a22, b11, b12, b21, b22
      real*8 beta, dt, t1, t2, t3, t4, s1, s2
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

c     calculate the a's
      a11 = t4*(beta*t2/t1+t3)
      a12 = t4*t2 / (w*t1)
      a21 = -t4*w*t2 / t1
      a22 = t4*(t3-beta*t2/t1)
c
c     calculate the b's
      b11 = t4*((s1+beta/w)*t2 / (w*t1) + (s2+1./w**2)*t3) - s2
      b12 = -t4*(s1*t2/(w*t1)+s2*t3) - 1./w**2 + s2
      b21 = (s1+beta/w) * (t3-beta*t2/t1)
      b21 = t4*(b21 - (s2+1./w**2)*(w*t1*t2+beta*w*t3)) + 1./(w**2*dt)
      b22 = s1*(t3-beta*t2/t1)
      b22 = -t4*(b22 - s2*(w*t1*t2+beta*w*t3)) - 1./(w**2*dt)
      
      return
      end subroutine coeff

c -------------------------------------------------------
       
      subroutine brs (x,w,beta,npts,rsp)
      
       implicit none
       real*8 x(1), rsp(1), beta, w
       real*8 d, v, a, z, ap1, dp1, vp1, t1, t2
       real*8 a11, a12, a21, a22, b11, b12, b21, b22
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
      end subroutine brs 
      
c ----------------------------------------------------------------------

      subroutine pad (x,npts,nmin,m)

c      This subroutine pads the x array to a power of 2 for FFT
       implicit none
       real*8 x(1)
       integer test,npts,m,nmin,i

       do 10 i=1,20
         test = 2**i
         if (test .ge. npts .and. test .ge. nmin) goto 20
  10   continue
  20   m = i
 1000  format( 2x,'Pad to ',i6,' points')

       do 30 i=npts,test
         x(i) = 0.0
  30   continue
       npts = test

      return
      end subroutine pad

c ----------------------------------------------------------------------

      subroutine cool ( signi, n, cx )
c     FFT subroutine.
c     signi = -1.  forward transform
c           =  1.  inverse transform
c     n = log base 2 (npts)

      implicit none
      real signi, pi
      integer istep, i, j, n, lx, m, l
      complex cx(1), carg, temp, cw
      
      pi = 4. * atan(1.) * signi
      lx = 2**n
      j = 1
      do 30 i=1,lx
        if (i .gt. j) goto 10
        temp = cx(j)
        cx(j) = cx(i)
        cx(i) = temp
  10    m = lx/2
  20    if (j .le. m) goto 25
        j = j-m
        m = m/2
        if (m .ge. 1) goto 20
  25    j = j+m
  30  continue
      l = 1
  40  istep = l+l
      do 50 m=1,l
        carg = cmplx( 0., pi * float(m-1) / float(l) )
        cw = cexp(carg)
        do 45 i=m,lx,istep
          temp = cw * cx(i+l) 
          cx(i+l) = cx(i) - temp
          cx(i) = cx(i) + temp
  45    continue
  50  continue
      l = istep
      if (l .lt. lx) goto 40
      
      return
      end subroutine cool
