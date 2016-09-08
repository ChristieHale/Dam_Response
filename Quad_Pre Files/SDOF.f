c-----------------------------------------------------------------------

      subroutine SDOF(npts, dt, df, TFSm, ffmax, flow, fhigh, npts2, damping, alpha, 
     1                response, TFSDOF, cu)
      
       implicit none
       include 'max_dims.H'
       
       real accel(MAXPTS), omega, pi, dt, df, response(MAXPTS)
       real response1(MAXPTS), response2(MAXPTS), response_save(MAXCOEF,MAXCOEF,MAXPTS)
       real TFSDOF_trial(MAXPTS), TFSDOF(MAXPTS), min_rms
       real trial_d(MAXCOEF), trial_a(MAXCOEF), rms_sum(MAXCOEF,MAXCOEF)
       real flow, fhigh, TFSDOF_trial2(MAXCOEF,MAXCOEF,MAXPTS), TFSm(MAXPTS)
       real damping, alpha, ffmax
       complex cu(MAXPTS), cu_trial(MAXPTS), cu_trial2(MAXCOEF,MAXCOEF,MAXPTS)
       integer i, k, l, m, n, npts, npts2, iflow, ifhigh, mtrial
       parameter (pi=3.14159)

c         initialize acceleration values to zero
          do i=1,npts
            accel(i) = 0.
          enddo
          
c         accleration impulse          
          accel(100) = 1.
          omega = ffmax * 2.*pi
          
c         loop over damping from 0.08 to 0.20
          do k=1,13
            trial_d(k) = 0.07 +0.01*k
c           calculate SDOF response TH
            call coeff (omega, trial_d(k), dt)
            call brs (accel, omega, trial_d(k), npts, response1) 
c           loop over alpha from 0.7 to 1.65
            do l=1,20
              trial_a(l) = 0.65 + 0.05*l
              response2 = response1*trial_a(l)              
              do i=1,npts 
                response_save(k,l,i) = response2(i)
              enddo
c             calculate SDOF FFT / TF
              call calcFFT (response2, npts, dt, df, mtrial, TFSDOF_trial, cu_trial, npts2) 
              do i=1,npts2              
                TFSDOF_trial2(k,l,i) = TFSDOF_trial(i)
                cu_trial2(k,l,i) = cu_trial(i)
              enddo              
              iflow = nint(flow/df + 1)
              ifhigh = nint(fhigh/df + 1)          
              rms_sum(k,l) = 0.0              
              do i=iflow, ifhigh                
                rms_sum(k,l) = rms_sum(k,l) + (TFSDOF_trial2(k,l,i) - TFSm(i))**2.
              enddo                 
            enddo
          enddo

c         find best fit
          min_rms = 10.**6.
          do k=1,13
            do l=1,20            
              if (rms_sum(k,l) .lt. min_rms) then
                min_rms = rms_sum(k,l)
                damping = trial_d(k)
                alpha = trial_a(l)
                m = k
                n = l
              endif
            enddo
          enddo  
          
c        save final response
         do i=1,npts
           response(i) = 0.0
           response(i) = response_save(m,n,i)
         enddo 
         
c        save final TFSDOF and cu 
         do i=1,npts2
           TFSDOF(i) = 0.0
           TFSDOF(i) = TFSDOF_trial2(m,n,i)
           cu(i) = 0.0
           cu(i) = cu_trial2(m,n,i)
         enddo       
          
      return
      end

c-----------------------------------------------------------------------

      subroutine SDOF2(npts, dt, df, TFSm, response1, response2, flow, shigh, npts3, 
     1                 response, TFSDOF, cu)
      
       implicit none
       include 'max_dims.H'
       
       integer npts, npts3, i, k, m, iflow, ishigh, mtrial
       real dt, df, response1(MAXPTS), response2(MAXPTS), responset(MAXPTS)
       real response_save(MAXCOEF,MAXPTS), response(MAXPTS)
       real resp2_shift(MAXPTS), TFSDOF_trial(MAXPTS), TFSDOF_trial2(MAXCOEF,MAXPTS)
       real TFSDOF(MAXPTS), flow, shigh, rms_sum(MAXCOEF), TFSm(MAXPTS), min_rms
       complex cu_trial(MAXPTS), cu_trial2(MAXCOEF,MAXPTS), cu(MAXPTS)

c         loop over shift, response2
          do k=10,35 
            do i=1,npts 
              if (i-k .le. 0) then
                resp2_shift(i) = 0.0
              else
                resp2_shift(i) = response2(i-k)
              endif
            enddo
c           calculate SDOF FFT / TF            
            do i=1,npts
              responset(i) = response1(i) + resp2_shift(i)
            enddo            
            do i=1,npts 
              response_save(k,i) = responset(i)
            enddo
            call calcFFT (responset, npts, dt, df, mtrial, TFSDOF_trial, cu_trial, npts3) 
            do i=1,npts3              
              TFSDOF_trial2(k,i) = TFSDOF_trial(i)
              cu_trial2(k,i) = cu_trial(i)
            enddo
            iflow = nint(flow/df + 1)
            ishigh = nint(shigh/df + 1)                 
            rms_sum(k) = 0.0              
            do i=iflow,ishigh                
              rms_sum(k) = rms_sum(k) + (TFSDOF_trial2(k,i) - TFSm(i))**2.
            enddo
          enddo

c         find best fit
          min_rms = 10.**6.
          do k=10,35          
            if (rms_sum(k) .lt. min_rms) then
              min_rms = rms_sum(k)
              m = k
            endif
          enddo

c        save final response
         do i=1,npts
           response(i) = 0.0
           response(i) = response_save(m,i)
         enddo 
           
c        save final TFSDOF and cu 
         do i=1,npts3
           TFSDOF(i) = 0.0
           TFSDOF(i) = TFSDOF_trial2(m,i)
           cu(i) = 0.0
           cu(i) = cu_trial2(m,i)
         enddo 
      
      return
      end
