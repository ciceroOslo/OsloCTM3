# RATEFILE GENERATED USING *SELECT* AND ACMSU REACTION DATABASE                                     
# MASTER RATEFILE: trimol.d                                                                         
# REACTION NETWORK: OPEN                                                                            
# TRIMOLECULAR REACTIONS - MASTER RATEFILE - Paul Brown, Oliver Wild, David Rowley & Glenn Carver   
# Centre for Atmospheric Science, Cambridge, U.K.  Release date:  11th May 1994                     
# Halogen reactions added by Srinath Krishnan: September 2024
# SCCS version information: @(#)trimol.d	1.6 5/12/94                                                
    1 HO2        HO2        H2O2       O2            0.00 2.10E-33  0.00   -920.0 0.00E+00  0.00 0.00E+00  JPL19       
    2 HO2        NO2        HO2NO2     m             0.60 1.90E-31 -3.40      0.0 4.00E-12 -0.30 0.00E+00  JPL19        
    3 HO2NO2     m          HO2        NO2           0.60 1.90E-31 -3.40  10900.0 4.00E-12 -0.30 2.10E-27  JPL19
    4 N2O5       m          NO2        NO3           0.60 2.40E-30 -3.00  10840.0 1.30E-12 +0.10 5.80E-27  JPL19
    5 NO2        NO3        N2O5       m             0.60 2.40E-30 -3.00      0.0 1.30E-12 +0.10 0.00E+00  JPL19        
    6 OH         NO2        HONO2      m             0.60 1.80E-30 -3.00      0.0 2.80E-11  0.00 0.00E+00  JPL19         
    7 OH         OH         H2O2       m             0.60 6.90E-31 -1.00      0.0 2.60E-11  0.00 0.00E+00  JPL19         
    8 MeCO3      NO2        PAN        m             0.60 7.30E-29 -4.10      0.0 9.50E-12 -1.60 0.00E+00  JPL19         
    9 PAN        m          MeCO3      NO2           0.60 7.30E-29 -4.10  14000.0 9.50E-12 -1.60 9.00E-29  JPL19
   10 Alkene     OH         ROHOO      HO2           0.60 1.10E-28 -3.50      0.0 8.50E-12 -1.75 0.00E+00  JPL19(C2H4)
   11 Cl         O2         m          ClOO          0.60 2.20E-33 -3.10      0.0 1.80E-10  0.00 0.00E+00  JPL19
   12 ClO        ClO        m          Cl2O2         0.60 1.90E-32 -3.60      0.0 3.70E-12 -1.60 0.00E+00  JPL19
   13 ClO        NO2        m          ClNO3         0.60 1.80E-31 -3.40      0.0 1.50E-11 -1.90 0.00E+00  JPL19
   13 ClOO       m          Cl         O2            0.60 2.20E-33 -3.10   2370.0 1.80E-10  0.00 1.24E-24  JPL19
   14 Cl2O2      m          ClO        ClO           0.60 1.90E-32 -3.60   8537.0 3.70E-12 -1.60 2.16E-27  JPL19 #ClO + ClO?
   15 Cl         C3H6       m          AlkanO2       0.60 4.00E-28  0.00      0.0 2.80E-10  0.00 0.00E+00  Atkinson07 #unknown
   16 Br         NO2        m          BrNO2         0.60 4.30E-31 -2.40      0.0 2.70E-11  0.00 0.00E+00  JPL19
   17 Bro        NO2        m          BrNO3         0.60 5.50E-31 -3.10      0.0 6.60E-12 -2.90 0.00E+00  JPL19
   18 I          NO         m          INO           0.60 1.80E-32 -1.00      0.0 1.70E-11  0.00 0.00E+00  JPL19,Atkinson07Fc
   19 I          NO2        m          INO2          0.63 3.00E-31 -1.00      0.0 6.60E-11  0.00 0.00E+00  JPL19,Atkinson07Fc
   20 IO         NO2        m          INO3          0.40 7.70E-31 -3.50      0.0 7.70E-12 -1.50 0.00E+00  JPL19,Atkinson07Fc
 9999                                                0.00 0.00E+00  0.00      0.0 0.00E-00  0.00    
                                                                                 
                                                                                 
The following removed - non-modelled and rapidly decomposing products                                             
    1 HO2        HCHO       HOCH2OO                  0.00 0.00E+00  0.00      0.0 0.00E+00  0.00      0.0           
    5 MeOO       NO2        MeO2NO2    m             0.36 2.50E-30 -5.50      0.0 7.50E-12  0.00      0.0           
   13 OH         NO         HONO       m          1420.00 7.40E-31 -2.40      0.0 3.30E-11  0.00      0.0           
                                                                                 
                                                                                 
                                                                                 
                                                                                 
 NOTES:                                                                          
 -----                                                                           
  All reaction data taken from IUPAC supplement IV unless                        
  otherwise indicated.                                                           
                                                                                 
  JPL - data from JPL 06-2
                                                                                 
  ? - reaction products unknown                                                  
  * - user strongly advised to consult source material                           
  B - branching ratio assumed equal for all channels in the                      
       absence of more information                                               
  F - parameters given are not sufficient for a full                             
       calculation of Fc - see source material -                                 
       however the additional term is small in comparison                        
       (eg. NO2+NO3->N2O5 and reverse reaction                                   
         Full calculation: Fc=exp(-T/250)+exp(-1050/T)                           
          at 200 K this term=0.45 additional term=0.005                          
          at 300 K          =0.30                =0.03  )                        
  M - reaction does not actually involve use of third body                       
  PD - rate at 1 bar; pressure dependence unknown                                
  PB - rate at 1 bar; pressure dependence unknown; these                         
       reactions are written as bimolecular reactions in                         
       the IUPAC assessment                                                      
  U - upper limit for rate coefficient                                           
  A1 - two rates are given for [O2] & [N2}. Rate is calculated by                
       ( ko2x[0.21] + kn2x[0.78] ) / ( [0.21]+[0.78] )                           
  Alkene - C2H4
                                                                                 
  Dependent reactions:                                                           
                                                                                 
  D1 - Depends on the concentration of H2O. See JPL 1992 and the paper           
       it references: R.R.Lii et al, J.Phys.Chem 85, 1981, p2833. This           
       reaction (and the bimolecular branch) need to be multiplied by            
       the following factor:                                                     
       (1 + 1.4E-21[H2O]exp(2200/T).                                             
                                                                                 
 Changes since 11/05/94 release:
 (1) Added halogen reaction rates.
 (2) Updated reaction rates to JPL19.
 
 Changes since 22/11/93 release:                                                 
 (1) Corrected virtually all rates. Previously they had been multiplied          
     by [N2] when written as such in IUPAC, but this is incorrect as             
     the rate is assumed to apply equally in O2 unless otherwise specified.      
 (2) Added note that HO2+HO2 reaction is H2O dependent.                          
 (3) Used high pressure limit for O3(3P)+NO2 from IUPAC J. Phys. Ref. Data.       
     This is different from that reported in Atm. Env.                           
 Changes since 08/3/93 release:                                                  
 (1) O now written as O3(3P)                                                      
 (2) some comments altered                  
                                      
