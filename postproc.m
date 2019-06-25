! CODE USED TO AGGREGATE OUTPUTS FROM BIOI2T TO BE USED BY HARPER ET AL.
! INPUTS:
! - Maps of potentials
! - Maps of land areas available for biomass production
! SCENARIOS:
! - Different constraints on land for bioenergy
! OUTPUT:
! - Variable of length NC with data on biomass potentials across all scenarios
! - Variable of length NC with data on land availability across all scenarios
! - Both of the above for WOODY, SUGAR, MAIZE and NWOODY CROPS
!
! Vassilis Daiglou, 25th June 2019

extern C void sumarray (export float *, int, int, int *, float *);
extern C void sumpartdim(export float **, int, int, int, int *, float **);
extern C void calcCostCurve  (int *,int, float *,float *,int,export float **, export int **);

#INCLUDE ../BioI2T/constants.m

! Time and Solution Declarations
t.min = 1970;
t.max = 2100;
t.step = 10;
t.sample = 10;
t.method = RK2;

! Beggining of main module
MODULE Main;
BEGIN;

REAL	Area[NC]			= FILE ("../BioI2T/Data/Area.dat");
INTEGER	region[NC]			= FILE ("../BioI2T/Data/IMAGE/Region27.dat");

CONST 	CAT = 500;

! CROP PRODUCTIVITIES
REAL	WOODYProductivityGJ_NoCons[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/NoConstraint/Maps/WOODYProductivityGJ.dat");
REAL	WOODYProductivityGJ_BioRes[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/BioReserve/Maps/WOODYProductivityGJ.dat");
REAL	WOODYProductivityGJ_Degrad[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/Degraded/Maps/WOODYProductivityGJ.dat");
REAL	WOODYProductivityGJ_WaterS[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/WaterShort/Maps/WOODYProductivityGJ.dat");
REAL	WOODYProductivityGJ_WetLan[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/WetLand/Maps/WOODYProductivityGJ.dat");
REAL	WOODYProductivityGJ_Abando[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/Abandoned/Maps/WOODYProductivityGJ.dat");
REAL	WOODYProductivityGJ_All[NC](t)		= FILE ("../outputlib/BioI2T/Constraints/All/Maps/WOODYProductivityGJ.dat");
	
REAL	NWOODProductivityGJ_NoCons[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/NoConstraint/Maps/NWOODProductivityGJ.dat");
REAL	NWOODProductivityGJ_BioRes[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/BioReserve/Maps/NWOODProductivityGJ.dat");
REAL	NWOODProductivityGJ_Degrad[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/Degraded/Maps/NWOODProductivityGJ.dat");
REAL	NWOODProductivityGJ_WaterS[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/WaterShort/Maps/NWOODProductivityGJ.dat");
REAL	NWOODProductivityGJ_WetLan[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/WetLand/Maps/NWOODProductivityGJ.dat");
REAL	NWOODProductivityGJ_Abando[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/Abandoned/Maps/NWOODProductivityGJ.dat");
REAL	NWOODProductivityGJ_All[NC](t)		= FILE ("../outputlib/BioI2T/Constraints/All/Maps/NWOODProductivityGJ.dat");

! IMPLEMENTATION MAPS (PER CROP AND SCENARIO)
REAL	ImplMap_NoCons[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/NoConstraint/Maps/ImplementationMap.dat");
REAL	ImplMap_BioRes[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/BioReserve/Maps/ImplementationMap.dat");
REAL	ImplMap_Degrad[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/Degraded/Maps/ImplementationMap.dat");
REAL	ImplMap_WaterS[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/WaterShort/Maps/ImplementationMap.dat");
REAL	ImplMap_WetLan[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/WetLand/Maps/ImplementationMap.dat");
REAL	ImplMap_Abando[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/Abandoned/Maps/ImplementationMap.dat");
REAL	ImplMap_All[NC](t)		= FILE ("../outputlib/BioI2T/Constraints/All/Maps/ImplementationMap.dat");

! VARIABLES
REAL	WOODYLandHa_NoCons[NC](t),WOODYLandHa_BioRes[NC](t),WOODYLandHa_Degrad[NC](t),WOODYLandHa_WaterS[NC](t),WOODYLandHa_WetLan[NC](t),WOODYLandHa_Abando[NC](t),WOODYLandHa_All[NC](t);
REAL	NWOODLandHa_NoCons[NC](t),NWOODLandHa_BioRes[NC](t),NWOODLandHa_Degrad[NC](t),NWOODLandHa_WaterS[NC](t),NWOODLandHa_WetLan[NC](t),NWOODLandHa_Abando[NC](t),NWOODLandHa_All[NC](t);

REAL	WOODYLandHaR_Scen[NRC,CAT,7](t);	! Ha - Maps of Land available for crop production	
REAL	NWOODLandHaR_Scen[NRC,CAT,7](t);	! Ha - Maps of Land available for crop production

REAL 	WOODYYield_Scen[NRC,CAT,7](t);	! GJ/Ha - Maps of crop yield
REAL 	NWOODYield_Scen[NRC,CAT,7](t);	! GJ/Ha - Maps of crop yield


! CALCULATIONS
! Land availability (Ha)
WOODYLandHa_NoCons[C]	= SWITCH(WOODYProductivityGJ_NoCons[C] > EPS ? ImplMap_NoCons[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
WOODYLandHa_BioRes[C]	= SWITCH(WOODYProductivityGJ_Biores[C] > EPS ? ImplMap_BioRes[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
WOODYLandHa_Degrad[C]	= SWITCH(WOODYProductivityGJ_Degrad[C] > EPS ? ImplMap_Degrad[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
WOODYLandHa_WaterS[C]	= SWITCH(WOODYProductivityGJ_WaterS[C] > EPS ? ImplMap_WaterS[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
WOODYLandHa_WetLan[C]	= SWITCH(WOODYProductivityGJ_WetLan[C] > EPS ? ImplMap_WetLan[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
WOODYLandHa_Abando[C]	= SWITCH(WOODYProductivityGJ_Abando[C] > EPS ? ImplMap_Abando[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
WOODYLandHa_All[C]	= SWITCH(WOODYProductivityGJ_All[C] > EPS ? ImplMap_All[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;

NWOODLandHa_NoCons[C]	= SWITCH(NWOODProductivityGJ_NoCons[C] > EPS ? ImplMap_NoCons[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
NWOODLandHa_BioRes[C]	= SWITCH(NWOODProductivityGJ_Biores[C] > EPS ? ImplMap_BioRes[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
NWOODLandHa_Degrad[C]	= SWITCH(NWOODProductivityGJ_Degrad[C] > EPS ? ImplMap_Degrad[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
NWOODLandHa_WaterS[C]	= SWITCH(NWOODProductivityGJ_WaterS[C] > EPS ? ImplMap_WaterS[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
NWOODLandHa_WetLan[C]	= SWITCH(NWOODProductivityGJ_WetLan[C] > EPS ? ImplMap_WetLan[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
NWOODLandHa_Abando[C]	= SWITCH(NWOODProductivityGJ_Abando[C] > EPS ? ImplMap_Abando[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
NWOODLandHa_All[C]	= SWITCH(NWOODProductivityGJ_All[C] > EPS ? ImplMap_All[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;


! ***** CONVERT MAPS TO CATEGORIES ****
! Yield Curves
REAL 	WOODYYieldCurve_NoCons[NRC,CAT](t),WOODYYieldCurve_BioRes[NRC,CAT](t),WOODYYieldCurve_Degrad[NRC,CAT](t),WOODYYieldCurve_WaterS[NRC,CAT](t),
	WOODYYieldCurve_WetLan[NRC,CAT](t),WOODYYieldCurve_Abando[NRC,CAT](t),WOODYYieldCurve_All[NRC,CAT](t);
INTEGER WOODYYieldPointer_NoCons[NRC,CAT](t),WOODYYieldPointer_BioRes[NRC,CAT](t),WOODYYieldPointer_Degrad[NRC,CAT](t),WOODYYieldPointer_WaterS[NRC,CAT](t),
	WOODYYieldPointer_WetLan[NRC,CAT](t),WOODYYieldPointer_Abando[NRC,CAT](t),WOODYYieldPointer_All[NRC,CAT](t);

WOODYYieldCurve_NoCons[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
WOODYYieldCurve_BioRes[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
WOODYYieldCurve_Degrad[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
WOODYYieldCurve_WaterS[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
WOODYYieldCurve_WetLan[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
WOODYYieldCurve_Abando[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
WOODYYieldCurve_All[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
WOODYYieldPointer_NoCons[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
WOODYYieldPointer_BioRes[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
WOODYYieldPointer_Degrad[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
WOODYYieldPointer_WaterS[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
WOODYYieldPointer_WetLan[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
WOODYYieldPointer_Abando[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
WOODYYieldPointer_All[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;

calcCostCurve (region,NRC,WOODYProductivityGJ_NoCons,WOODYLandHa_NoCons,CAT,WOODYYieldCurve_NoCons,WOODYYieldPointer_NoCons);
calcCostCurve (region,NRC,WOODYProductivityGJ_BioRes,WOODYLandHa_BioRes,CAT,WOODYYieldCurve_BioRes,WOODYYieldPointer_BioRes);
calcCostCurve (region,NRC,WOODYProductivityGJ_Degrad,WOODYLandHa_Degrad,CAT,WOODYYieldCurve_Degrad,WOODYYieldPointer_Degrad);
calcCostCurve (region,NRC,WOODYProductivityGJ_WaterS,WOODYLandHa_WaterS,CAT,WOODYYieldCurve_WaterS,WOODYYieldPointer_WaterS);
calcCostCurve (region,NRC,WOODYProductivityGJ_WetLan,WOODYLandHa_WetLan,CAT,WOODYYieldCurve_WetLan,WOODYYieldPointer_WetLan);
calcCostCurve (region,NRC,WOODYProductivityGJ_Abando,WOODYLandHa_Abando,CAT,WOODYYieldCurve_Abando,WOODYYieldPointer_Abando);
calcCostCurve (region,NRC,WOODYProductivityGJ_All,WOODYLandHa_All,CAT,WOODYYieldCurve_All,WOODYYieldPointer_All);

REAL 	NWOODYieldCurve_NoCons[NRC,CAT](t),NWOODYieldCurve_BioRes[NRC,CAT](t),NWOODYieldCurve_Degrad[NRC,CAT](t),NWOODYieldCurve_WaterS[NRC,CAT](t),
	NWOODYieldCurve_WetLan[NRC,CAT](t),NWOODYieldCurve_Abando[NRC,CAT](t),NWOODYieldCurve_All[NRC,CAT](t);
INTEGER NWOODYieldPointer_NoCons[NRC,CAT](t),NWOODYieldPointer_BioRes[NRC,CAT](t),NWOODYieldPointer_Degrad[NRC,CAT](t),NWOODYieldPointer_WaterS[NRC,CAT](t),
	NWOODYieldPointer_WetLan[NRC,CAT](t),NWOODYieldPointer_Abando[NRC,CAT](t),NWOODYieldPointer_All[NRC,CAT](t);

NWOODYieldCurve_NoCons[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
NWOODYieldCurve_BioRes[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
NWOODYieldCurve_Degrad[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
NWOODYieldCurve_WaterS[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
NWOODYieldCurve_WetLan[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
NWOODYieldCurve_Abando[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
NWOODYieldCurve_All[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
NWOODYieldPointer_NoCons[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
NWOODYieldPointer_BioRes[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
NWOODYieldPointer_Degrad[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
NWOODYieldPointer_WaterS[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
NWOODYieldPointer_WetLan[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
NWOODYieldPointer_Abando[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
NWOODYieldPointer_All[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;

calcCostCurve (region,NRC,NWOODProductivityGJ_NoCons,NWOODLandHa_NoCons,CAT,NWOODYieldCurve_NoCons,NWOODYieldPointer_NoCons);
calcCostCurve (region,NRC,NWOODProductivityGJ_BioRes,NWOODLandHa_BioRes,CAT,NWOODYieldCurve_BioRes,NWOODYieldPointer_BioRes);
calcCostCurve (region,NRC,NWOODProductivityGJ_Degrad,NWOODLandHa_Degrad,CAT,NWOODYieldCurve_Degrad,NWOODYieldPointer_Degrad);
calcCostCurve (region,NRC,NWOODProductivityGJ_WaterS,NWOODLandHa_WaterS,CAT,NWOODYieldCurve_WaterS,NWOODYieldPointer_WaterS);
calcCostCurve (region,NRC,NWOODProductivityGJ_WetLan,NWOODLandHa_WetLan,CAT,NWOODYieldCurve_WetLan,NWOODYieldPointer_WetLan);
calcCostCurve (region,NRC,NWOODProductivityGJ_Abando,NWOODLandHa_Abando,CAT,NWOODYieldCurve_Abando,NWOODYieldPointer_Abando);
calcCostCurve (region,NRC,NWOODProductivityGJ_All,NWOODLandHa_All,CAT,NWOODYieldCurve_All,NWOODYieldPointer_All);

! Land Availability
REAL	WOODYLandR_NoCons[NRC](t),WOODYLandR_BioRes[NRC](t),WOODYLandR_Degrad[NRC](t),WOODYLandR_WaterS[NRC](t),WOODYLandR_WetLan[NRC](t),WOODYLandR_Abando[NRC](t),WOODYLandR_All[NRC](t);

WOODYLandR_NoCons[R] = 0,R = 1 to NRC;
WOODYLandR_BioRes[R] = 0,R = 1 to NRC;
WOODYLandR_Degrad[R] = 0,R = 1 to NRC;
WOODYLandR_WaterS[R] = 0,R = 1 to NRC;
WOODYLandR_WetLan[R] = 0,R = 1 to NRC;
WOODYLandR_Abando[R] = 0,R = 1 to NRC;
WOODYLandR_All[R] = 0,R = 1 to NRC;

sumarray (WOODYLandR_NoCons, NC, NRC, region, WOODYLandHa_NoCons);
sumarray (WOODYLandR_BioRes, NC, NRC, region, WOODYLandHa_BioRes);
sumarray (WOODYLandR_Degrad, NC, NRC, region, WOODYLandHa_Degrad);
sumarray (WOODYLandR_WaterS, NC, NRC, region, WOODYLandHa_WaterS);
sumarray (WOODYLandR_WetLan, NC, NRC, region, WOODYLandHa_WetLan);
sumarray (WOODYLandR_Abando, NC, NRC, region, WOODYLandHa_Abando);
sumarray (WOODYLandR_All, NC, NRC, region, WOODYLandHa_All);

REAL	NWOODLandR_NoCons[NRC](t),NWOODLandR_BioRes[NRC](t),NWOODLandR_Degrad[NRC](t),NWOODLandR_WaterS[NRC](t),NWOODLandR_WetLan[NRC](t),NWOODLandR_Abando[NRC](t),NWOODLandR_All[NRC](t);

NWOODLandR_NoCons[R] = 0,R = 1 to NRC;
NWOODLandR_BioRes[R] = 0,R = 1 to NRC;
NWOODLandR_Degrad[R] = 0,R = 1 to NRC;
NWOODLandR_WaterS[R] = 0,R = 1 to NRC;
NWOODLandR_WetLan[R] = 0,R = 1 to NRC;
NWOODLandR_Abando[R] = 0,R = 1 to NRC;
NWOODLandR_All[R] = 0,R = 1 to NRC;

sumarray (NWOODLandR_NoCons, NC, NRC, region, NWOODLandHa_NoCons);
sumarray (NWOODLandR_BioRes, NC, NRC, region, NWOODLandHa_BioRes);
sumarray (NWOODLandR_Degrad, NC, NRC, region, NWOODLandHa_Degrad);
sumarray (NWOODLandR_WaterS, NC, NRC, region, NWOODLandHa_WaterS);
sumarray (NWOODLandR_WetLan, NC, NRC, region, NWOODLandHa_WetLan);
sumarray (NWOODLandR_Abando, NC, NRC, region, NWOODLandHa_Abando);
sumarray (NWOODLandR_All, NC, NRC, region, NWOODLandHa_All);


! Aggregate
WOODYYield_Scen[R,i,1]	= WOODYYieldCurve_NoCons[R,i], R = 1 TO NRC, i = 1 TO CAT;
WOODYYield_Scen[R,i,2]	= WOODYYieldCurve_Biores[R,i], R = 1 TO NRC, i = 1 TO CAT;
WOODYYield_Scen[R,i,3]	= WOODYYieldCurve_Degrad[R,i], R = 1 TO NRC, i = 1 TO CAT;
WOODYYield_Scen[R,i,4]	= WOODYYieldCurve_WaterS[R,i], R = 1 TO NRC, i = 1 TO CAT;
WOODYYield_Scen[R,i,5]	= WOODYYieldCurve_WetLan[R,i], R = 1 TO NRC, i = 1 TO CAT;
WOODYYield_Scen[R,i,6]	= WOODYYieldCurve_Abando[R,i], R = 1 TO NRC, i = 1 TO CAT;
WOODYYield_Scen[R,i,7]	= WOODYYieldCurve_All[R,i], R = 1 TO NRC, i = 1 TO CAT;

NWOODYield_Scen[R,i,1]	= NWOODYieldCurve_NoCons[R,i], R = 1 TO NRC, i = 1 TO CAT;
NWOODYield_Scen[R,i,2]	= NWOODYieldCurve_Biores[R,i], R = 1 TO NRC, i = 1 TO CAT;
NWOODYield_Scen[R,i,3]	= NWOODYieldCurve_Degrad[R,i], R = 1 TO NRC, i = 1 TO CAT;
NWOODYield_Scen[R,i,4]	= NWOODYieldCurve_WaterS[R,i], R = 1 TO NRC, i = 1 TO CAT;
NWOODYield_Scen[R,i,5]	= NWOODYieldCurve_WetLan[R,i], R = 1 TO NRC, i = 1 TO CAT;
NWOODYield_Scen[R,i,6]	= NWOODYieldCurve_Abando[R,i], R = 1 TO NRC, i = 1 TO CAT;
NWOODYield_Scen[R,i,7]	= NWOODYieldCurve_All[R,i], R = 1 TO NRC, i = 1 TO CAT;

WOODYLandHaR_Scen[R,i,1]= WOODYLandR_NoCons[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
WOODYLandHaR_Scen[R,i,2]= WOODYLandR_BioRes[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
WOODYLandHaR_Scen[R,i,3]= WOODYLandR_Degrad[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
WOODYLandHaR_Scen[R,i,4]= WOODYLandR_WaterS[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
WOODYLandHaR_Scen[R,i,5]= WOODYLandR_WetLan[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
WOODYLandHaR_Scen[R,i,6]= WOODYLandR_Abando[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
WOODYLandHaR_Scen[R,i,7]= WOODYLandR_All[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;

NWOODLandHaR_Scen[R,i,1]= NWOODLandR_NoCons[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
NWOODLandHaR_Scen[R,i,2]= NWOODLandR_BioRes[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
NWOODLandHaR_Scen[R,i,3]= NWOODLandR_Degrad[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
NWOODLandHaR_Scen[R,i,4]= NWOODLandR_WaterS[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
NWOODLandHaR_Scen[R,i,5]= NWOODLandR_WetLan[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
NWOODLandHaR_Scen[R,i,6]= NWOODLandR_Abando[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
NWOODLandHaR_Scen[R,i,7]= NWOODLandR_All[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;


! OUTPUTS
FILE("Output\WOODYYield_Scen.dat")			= WOODYYield_Scen;
FILE("Output\NWOODYield_Scen.dat")			= NWOODYield_Scen;

FILE("Output\WOODYLandHaR_Scen.dat")			= WOODYLandHaR_Scen;
FILE("Output\NWOODLandHaR_Scen.dat")			= NWOODLandHaR_Scen;

END;