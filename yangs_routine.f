C************************SUBROUTINE YANG**********************************
C-------------------------------------------------------------------------
C  THIS SUBROUTINE: CALCULATE THE TOTAL SEDIMENT TRANSPORT OF A RIVER AT
C                  A NODE USING YANG'S (1973) UNIT STREAM POWER EQUATION
C VARIABLES
C Y(#space nodes)
C BLWT(#space nodes)
C DBDX(#space nodes),XFLUX(#space nodes)
C WIDM(#space nodes)
C SIGMA,VOLCON
C DELT,DELX
C-------------------------------------------------------------------------
	SUBROUTINE ERODEP90(MFLUX,PTS,FACTOR)
 	IMPLICIT NONE
 	REAL*8 USTAR(400),TOX(400),RHO,DIMID,OMEGA,A,B,C,D,E,F,VCR,REY
 	REAL*8 DLESSU,VISKIN,CONC,STRDATA(6,400),MFLUX(400)
 	REAL*8 WID(400),DELT,SIGMA,VOLCON,DELX,ELEV(400),PHI,DMFLUXDX(400)
 	REAL*8 XPRINT,TPRINT,G,Q,DCONT,WSEBC,MANN,PTS,FACTOR
 	REAL*8 DUMMY(400),DUMMY1,FLEXR,THICKW(400),THICKS(400),
     #         THICKC(400),RHOC,RHOM,RHOA,SSELEV(400),ALPHA
 	INTEGER*4 JN,M,NT,ICONT,XN
 	CHARACTER*25 RUN
C------------------------------------------------------------------------
C     COMMON BLOCKS
C------------------------------------------------------------------------
	COMMON /COM1/M,NT,DELX,STRDATA,WID,ELEV,DCONT,WSEBC,MANN
C 	COMMON /COM2/RUN,Q,ICONT,DELT,XPRINT,TPRINT,TOX,RHO,G
 	COMMON /COM2/Q,DELT,XPRINT,TPRINT,TOX,RHO,G,ICONT,RUN
 	COMMON /COM3/VISKIN,DIMID
 	COMMON /COM4/FLEXR,THICKW,THICKS,THICKC,SIGMA,VOLCON,RHOC,
     #               RHOM,RHOA,SSELEV,ALPHA,XN
C     #               RHOM,RHOA,SSELEV,XN,ALPHA
C------------------------------------------------------------------------
C CALCULATE THE SETTLING VELOCITY OF DIMID
C------------------------------------------------------------------------
      CALL SETTLE (OMEGA)
      REY = OMEGA*DIMID/VISKIN
C------------------------------------------------------------------------
C SET SOME CONSTANTS AND CALCULATE SOME VALUES
C------------------------------------------------------------------------
      VOLCON = 0.8
      SIGMA = 2650.0
      PHI = 0.6
      IF (DIMID .LE. 0.004) THEN
         A = 5.435
         B = 0.286
         C = 0.457
         D = 1.799
         E = 0.409
         F = 0.314
      ELSE
         A = 6.681
         B = 0.633
         C = 4.816
         D = 2.784
         E = 0.305
         F = 0.282
      ENDIF
C------------------------------------------------------------------------
C YANG'S EQUATION
C------------------------------------------------------------------------
      DO 1 JN = 1, M
         USTAR(JN) = SQRT(TOX(JN)/RHO)
C------------------------------------------------------------------------
C CALCULATE THE CRITICAL VELOCITY
C------------------------------------------------------------------------
         IF (USTAR(JN)*DIMID/VISKIN .LE. 70.0) THEN
            VCR = OMEGA*(2.5/(DLOG(USTAR(JN)*DIMID/VISKIN)-0.06D0))
         ELSE
            VCR = 2.05*OMEGA
         ENDIF
         IF (STRDATA(5,JN) - VCR .GT. 0.0D0) THEN
            DLESSU = USTAR(JN)/OMEGA
            CONC = A - B*DLOG10(REY) - C*DLOG10(DLESSU) + (D-E*DLOG10(
     #         REY)-F*DLOG10(DLESSU))*DLOG10((STRDATA(3,JN)/OMEGA)*(
     #         STRDATA(5,JN)-VCR))
            CONC = FACTOR*10**CONC
         ELSE
            CONC = 0.0D0
         ENDIF
         MFLUX(JN) = CONC*STRDATA(5,JN)*STRDATA(6,JN)*WID(JN)*RHO/
     #      1000000.0
    1 CONTINUE
      RETURN 
      END
