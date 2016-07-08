
      subroutine smooth(WinType, Win_len, fasOrig, npts1, fasSmooth, faslnSmooth)

      implicit none
      include 'max_dims.H'

      integer WinType, Win_len, npts1 
      real Win(MAXPTS), fasOrig(MAXPTS), fasSmooth(MAXPTS), faslnSmooth(MAXPTS)
 
c       get window for smoothing
        if (WinType .eq. 1) then
          call Box(Win_len,Win) 
        elseif (WinType .eq. 2) then
          call Hanning(Win_len,Win)
        elseif (WinType .eq. 3) then  
          call Hamming(Win_len,Win)
        endif

c       convolve window with fasRock
        call convolve(fasOrig, npts1, Win, Win_len, fasSmooth, faslnSmooth)                   

      return
      end

c-----------------------------------------------------------------------
      
      subroutine Box(n,w)
      
      implicit none
      include 'max_dims.H'
      
      integer i, n
      real w(MAXPTS)
      
        do i=1,n
          w(i) = 1.0
        enddo
      
      return
      end
      
c-----------------------------------------------------------------------      

      subroutine Hanning(n,w)
      
      implicit none
      include 'max_dims.H'

      integer i, n
      real w(MAXPTS), pi, pn
      
      pi=4.*atan(1.)
      pn=2.*pi/float(n+1)
      
        do i=1,n
          w(i) = 0.5*(1.0-cos(pn*i))
        enddo

      return
      end
      
c-----------------------------------------------------------------------

      subroutine Hamming(n,w)
      
      implicit none
      include 'max_dims.H'

      integer i, j, n
      real w(MAXPTS), pi, pn
      
      pi=4.*atan(1.)
      pn=2.*pi/float(n-1)
      
        do i=0,n-1
          j=i+1
          w(j) = 0.54 - 0.46*cos(pn*i)
        enddo

      return
      end

c-----------------------------------------------------------------------

      subroutine convolve(FFT, FFT_len, Win, Win_len, Conv, lnConv)     
        
        implicit none
        include 'max_dims.H'

        real Conv(MAXPTS), FFT(MAXPTS), Win(MAXPTS), Wine_wt(MAXPTS),
     1       lnConv(MAXPTS), w_wt, Win_wt(MAXPTS), we_wt 
        integer Win_len, FFT_len
        integer i, k, m, c, j

c       first value is zero frequency, treat differently
        Conv(1) = -999
        lnConv(1) = -999
        
c       normalize weights to sum to 1, whole window 
        w_wt = 0.0
        do k=1,Win_len 
          w_wt = w_wt + Win(k)
        enddo        
        do c=1,Win_len
          Win_wt(c) = Win(c)/w_wt
        enddo  

c       convolusion
        m = int(0.5*Win_len)        
        do i = 2, FFT_len   
c         typical point in the middle (not edge)  
          if (i .gt. 0.5*Win_len+1) then      
            lnConv(i) = 0.0
            do k=i-m, i+m
              c = k-(i-m)+1
              lnConv(i) = lnConv(i) + alog(FFT(k))*Win_wt(c)              
            enddo
            Conv(i) = exp(lnConv(i))         
c         point at the beginning (edge)   
          elseif (i .lt. 0.5*Win_len+1) then          
c           normalize weights to sum to 1, partial window
            j=Win_len-(i+m-1)           
            we_wt = 0.0            
            do c=1+j,i+m-1+j 
              we_wt = we_wt + Win(c)              
            enddo            
            do c=1+j,i+m-1+j
              Wine_wt(c) = Win(c)/we_wt              
            enddo      
            lnConv(i) = 0.0
            do k=2,i+m
              c = k-1+j
              lnConv(i) = lnConv(i) + alog(FFT(k))*Wine_wt(c)              
            enddo
            Conv(i) = exp(lnConv(i))
          endif
        enddo
       
      return
      end
 
c ----------------------------------------------------------------------

      subroutine variance(Win_len, df, TFSm, sigmax, sigmaN, xflag)

      implicit none
      include 'max_dims.H'

      integer Win_len, m, i, k, Hz30, xflag
      real diff(MAXPTS), TFSm(MAXPTS), df, mean, sum
      real var(MAXPTS), sigma(MAXPTS)
      real sigmaN(MAXPTS)
      real sigmax
 
 
c       calculate variance over Window length  
        xflag = 0  
        m = nint(0.5*(Win_len-1))
        Hz30 = nint(30./df + 1)
        do i=m+1, Hz30    
c         calculate mean
          mean = 0.0     
          do k=i-m, i+m          
            mean = mean + alog(TFSm(k))/Win_len             
          enddo
c         calculate the difference from the mean
          do k=i-m, i+m
            diff(k) = alog(TFSm(k)) - mean             
          enddo 
c         calculate the variance
          sum = 0.0
          do k=i-m, i+m
            sum = sum + diff(k)**2.   
          enddo
          var(i) = sum/Win_len  
          sigma(i) = sqrt(var(i))
          sigmaN(i) = sigma(i)/sqrt(real(Win_len))
          if (sigmaN(i) .gt. sigmax) then
            xflag = 1
          endif  
        enddo       
 
      return
      end
