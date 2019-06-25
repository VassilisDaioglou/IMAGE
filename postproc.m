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

CONST 	CAT = 200;

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

REAL	MAIZEProductivityGJ_NoCons[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/NoConstraint/Maps/MAIZEProductivityGJ.dat");
REAL	MAIZEProductivityGJ_BioRes[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/BioReserve/Maps/MAIZEProductivityGJ.dat");
REAL	MAIZEProductivityGJ_Degrad[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/Degraded/Maps/MAIZEProductivityGJ.dat");
REAL	MAIZEProductivityGJ_WaterS[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/WaterShort/Maps/MAIZEProductivityGJ.dat");
REAL	MAIZEProductivityGJ_WetLan[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/WetLand/Maps/MAIZEProductivityGJ.dat");
REAL	MAIZEProductivityGJ_Abando[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/Abandoned/Maps/MAIZEProductivityGJ.dat");
REAL	MAIZEProductivityGJ_All[NC](t)		= FILE ("../outputlib/BioI2T/Constraints/All/Maps/MAIZEProductivityGJ.dat");

REAL	SUGARProductivityGJ_NoCons[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/NoConstraint/Maps/SUGARProductivityGJ.dat");
REAL	SUGARProductivityGJ_BioRes[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/BioReserve/Maps/SUGARProductivityGJ.dat");
REAL	SUGARProductivityGJ_Degrad[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/Degraded/Maps/SUGARProductivityGJ.dat");
REAL	SUGARProductivityGJ_WaterS[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/WaterShort/Maps/SUGARProductivityGJ.dat");
REAL	SUGARProductivityGJ_WetLan[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/WetLand/Maps/SUGARProductivityGJ.dat");
REAL	SUGARProductivityGJ_Abando[NC](t)	= FILE ("../outputlib/BioI2T/Constraints/Abandoned/Maps/SUGARProductivityGJ.dat");
REAL	SUGARProductivityGJ_All[NC](t)		= FILE ("../outputlib/BioI2T/Constraints/All/Maps/SUGARProductivityGJ.dat");

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
REAL	MAIZELandHa_NoCons[NC](t),MAIZELandHa_BioRes[NC](t),MAIZELandHa_Degrad[NC](t),MAIZELandHa_WaterS[NC](t),MAIZELandHa_WetLan[NC](t),MAIZELandHa_Abando[NC](t),MAIZELandHa_All[NC](t);
REAL	SUGARLandHa_NoCons[NC](t),SUGARLandHa_BioRes[NC](t),SUGARLandHa_Degrad[NC](t),SUGARLandHa_WaterS[NC](t),SUGARLandHa_WetLan[NC](t),SUGARLandHa_Abando[NC](t),SUGARLandHa_All[NC](t);

REAL	WOODYLandHaR_Scen[NRC,CAT,7](t);	! Ha - Maps of Land available for crop production	
REAL	NWOODLandHaR_Scen[NRC,CAT,7](t);	! Ha - Maps of Land available for crop production
REAL	MAIZELandHaR_Scen[NRC,CAT,7](t);	! Ha - Maps of Land available for crop production
REAL	SUGARLandHaR_Scen[NRC,CAT,7](t);	! Ha - Maps of Land available for crop production

REAL 	WOODYYield_Scen[NRC,CAT,7](t);	! GJ/Ha - Maps of crop yield
REAL 	NWOODYield_Scen[NRC,CAT,7](t);	! GJ/Ha - Maps of crop yield
REAL 	MAIZEYield_Scen[NRC,CAT,7](t);	! GJ/Ha - Maps of crop yield
REAL 	SUGARYield_Scen[NRC,CAT,7](t);	! GJ/Ha - Maps of crop yield


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

MAIZELandHa_NoCons[C]	= SWITCH(MAIZEProductivityGJ_NoCons[C] > EPS ? ImplMap_NoCons[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
MAIZELandHa_BioRes[C]	= SWITCH(MAIZEProductivityGJ_Biores[C] > EPS ? ImplMap_BioRes[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
MAIZELandHa_Degrad[C]	= SWITCH(MAIZEProductivityGJ_Degrad[C] > EPS ? ImplMap_Degrad[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
MAIZELandHa_WaterS[C]	= SWITCH(MAIZEProductivityGJ_WaterS[C] > EPS ? ImplMap_WaterS[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
MAIZELandHa_WetLan[C]	= SWITCH(MAIZEProductivityGJ_WetLan[C] > EPS ? ImplMap_WetLan[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
MAIZELandHa_Abando[C]	= SWITCH(MAIZEProductivityGJ_Abando[C] > EPS ? ImplMap_Abando[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
MAIZELandHa_All[C]	= SWITCH(MAIZEProductivityGJ_All[C] > EPS ? ImplMap_All[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;

SUGARLandHa_NoCons[C]	= SWITCH(SUGARProductivityGJ_NoCons[C] > EPS ? ImplMap_NoCons[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
SUGARLandHa_BioRes[C]	= SWITCH(SUGARProductivityGJ_Biores[C] > EPS ? ImplMap_BioRes[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
SUGARLandHa_Degrad[C]	= SWITCH(SUGARProductivityGJ_Degrad[C] > EPS ? ImplMap_Degrad[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
SUGARLandHa_WaterS[C]	= SWITCH(SUGARProductivityGJ_WaterS[C] > EPS ? ImplMap_WaterS[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
SUGARLandHa_WetLan[C]	= SWITCH(SUGARProductivityGJ_WetLan[C] > EPS ? ImplMap_WetLan[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
SUGARLandHa_Abando[C]	= SWITCH(SUGARProductivityGJ_Abando[C] > EPS ? ImplMap_Abando[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;
SUGARLandHa_All[C]	= SWITCH(SUGARProductivityGJ_All[C] > EPS ? ImplMap_All[C] * Area[C] * Haperkm2, ELSE 0), C = 1 TO NC;

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

REAL 	MAIZEYieldCurve_NoCons[NRC,CAT](t),MAIZEYieldCurve_BioRes[NRC,CAT](t),MAIZEYieldCurve_Degrad[NRC,CAT](t),MAIZEYieldCurve_WaterS[NRC,CAT](t),
	MAIZEYieldCurve_WetLan[NRC,CAT](t),MAIZEYieldCurve_Abando[NRC,CAT](t),MAIZEYieldCurve_All[NRC,CAT](t);
INTEGER MAIZEYieldPointer_NoCons[NRC,CAT](t),MAIZEYieldPointer_BioRes[NRC,CAT](t),MAIZEYieldPointer_Degrad[NRC,CAT](t),MAIZEYieldPointer_WaterS[NRC,CAT](t),
	MAIZEYieldPointer_WetLan[NRC,CAT](t),MAIZEYieldPointer_Abando[NRC,CAT](t),MAIZEYieldPointer_All[NRC,CAT](t);

MAIZEYieldCurve_NoCons[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
MAIZEYieldCurve_BioRes[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
MAIZEYieldCurve_Degrad[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
MAIZEYieldCurve_WaterS[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
MAIZEYieldCurve_WetLan[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
MAIZEYieldCurve_Abando[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
MAIZEYieldCurve_All[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
MAIZEYieldPointer_NoCons[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
MAIZEYieldPointer_BioRes[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
MAIZEYieldPointer_Degrad[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
MAIZEYieldPointer_WaterS[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
MAIZEYieldPointer_WetLan[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
MAIZEYieldPointer_Abando[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
MAIZEYieldPointer_All[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;

calcCostCurve (region,NRC,MAIZEProductivityGJ_NoCons,MAIZELandHa_NoCons,CAT,MAIZEYieldCurve_NoCons,MAIZEYieldPointer_NoCons);
calcCostCurve (region,NRC,MAIZEProductivityGJ_BioRes,MAIZELandHa_BioRes,CAT,MAIZEYieldCurve_BioRes,MAIZEYieldPointer_BioRes);
calcCostCurve (region,NRC,MAIZEProductivityGJ_Degrad,MAIZELandHa_Degrad,CAT,MAIZEYieldCurve_Degrad,MAIZEYieldPointer_Degrad);
calcCostCurve (region,NRC,MAIZEProductivityGJ_WaterS,MAIZELandHa_WaterS,CAT,MAIZEYieldCurve_WaterS,MAIZEYieldPointer_WaterS);
calcCostCurve (region,NRC,MAIZEProductivityGJ_WetLan,MAIZELandHa_WetLan,CAT,MAIZEYieldCurve_WetLan,MAIZEYieldPointer_WetLan);
calcCostCurve (region,NRC,MAIZEProductivityGJ_Abando,MAIZELandHa_Abando,CAT,MAIZEYieldCurve_Abando,MAIZEYieldPointer_Abando);
calcCostCurve (region,NRC,MAIZEProductivityGJ_All,MAIZELandHa_All,CAT,MAIZEYieldCurve_All,MAIZEYieldPointer_All);

REAL 	SUGARYieldCurve_NoCons[NRC,CAT](t),SUGARYieldCurve_BioRes[NRC,CAT](t),SUGARYieldCurve_Degrad[NRC,CAT](t),SUGARYieldCurve_WaterS[NRC,CAT](t),
	SUGARYieldCurve_WetLan[NRC,CAT](t),SUGARYieldCurve_Abando[NRC,CAT](t),SUGARYieldCurve_All[NRC,CAT](t);
INTEGER SUGARYieldPointer_NoCons[NRC,CAT](t),SUGARYieldPointer_BioRes[NRC,CAT](t),SUGARYieldPointer_Degrad[NRC,CAT](t),SUGARYieldPointer_WaterS[NRC,CAT](t),
	SUGARYieldPointer_WetLan[NRC,CAT](t),SUGARYieldPointer_Abando[NRC,CAT](t),SUGARYieldPointer_All[NRC,CAT](t);

SUGARYieldCurve_NoCons[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
SUGARYieldCurve_BioRes[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
SUGARYieldCurve_Degrad[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
SUGARYieldCurve_WaterS[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
SUGARYieldCurve_WetLan[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
SUGARYieldCurve_Abando[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
SUGARYieldCurve_All[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
SUGARYieldPointer_NoCons[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
SUGARYieldPointer_BioRes[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
SUGARYieldPointer_Degrad[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
SUGARYieldPointer_WaterS[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
SUGARYieldPointer_WetLan[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
SUGARYieldPointer_Abando[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;
SUGARYieldPointer_All[R,i] = 0, R = 1 TO NRC, i = 1 TO CAT;

calcCostCurve (region,NRC,SUGARProductivityGJ_NoCons,SUGARLandHa_NoCons,CAT,SUGARYieldCurve_NoCons,SUGARYieldPointer_NoCons);
calcCostCurve (region,NRC,SUGARProductivityGJ_BioRes,SUGARLandHa_BioRes,CAT,SUGARYieldCurve_BioRes,SUGARYieldPointer_BioRes);
calcCostCurve (region,NRC,SUGARProductivityGJ_Degrad,SUGARLandHa_Degrad,CAT,SUGARYieldCurve_Degrad,SUGARYieldPointer_Degrad);
calcCostCurve (region,NRC,SUGARProductivityGJ_WaterS,SUGARLandHa_WaterS,CAT,SUGARYieldCurve_WaterS,SUGARYieldPointer_WaterS);
calcCostCurve (region,NRC,SUGARProductivityGJ_WetLan,SUGARLandHa_WetLan,CAT,SUGARYieldCurve_WetLan,SUGARYieldPointer_WetLan);
calcCostCurve (region,NRC,SUGARProductivityGJ_Abando,SUGARLandHa_Abando,CAT,SUGARYieldCurve_Abando,SUGARYieldPointer_Abando);
calcCostCurve (region,NRC,SUGARProductivityGJ_All,SUGARLandHa_All,CAT,SUGARYieldCurve_All,SUGARYieldPointer_All);

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

REAL	MAIZELandR_NoCons[NRC](t),MAIZELandR_BioRes[NRC](t),MAIZELandR_Degrad[NRC](t),MAIZELandR_WaterS[NRC](t),MAIZELandR_WetLan[NRC](t),MAIZELandR_Abando[NRC](t),MAIZELandR_All[NRC](t);

MAIZELandR_NoCons[R] = 0,R = 1 to NRC;
MAIZELandR_BioRes[R] = 0,R = 1 to NRC;
MAIZELandR_Degrad[R] = 0,R = 1 to NRC;
MAIZELandR_WaterS[R] = 0,R = 1 to NRC;
MAIZELandR_WetLan[R] = 0,R = 1 to NRC;
MAIZELandR_Abando[R] = 0,R = 1 to NRC;
MAIZELandR_All[R] = 0,R = 1 to NRC;

sumarray (MAIZELandR_NoCons, NC, NRC, region, MAIZELandHa_NoCons);
sumarray (MAIZELandR_BioRes, NC, NRC, region, MAIZELandHa_BioRes);
sumarray (MAIZELandR_Degrad, NC, NRC, region, MAIZELandHa_Degrad);
sumarray (MAIZELandR_WaterS, NC, NRC, region, MAIZELandHa_WaterS);
sumarray (MAIZELandR_WetLan, NC, NRC, region, MAIZELandHa_WetLan);
sumarray (MAIZELandR_Abando, NC, NRC, region, MAIZELandHa_Abando);
sumarray (MAIZELandR_All, NC, NRC, region, MAIZELandHa_All);

REAL	SUGARLandR_NoCons[NRC](t),SUGARLandR_BioRes[NRC](t),SUGARLandR_Degrad[NRC](t),SUGARLandR_WaterS[NRC](t),SUGARLandR_WetLan[NRC](t),SUGARLandR_Abando[NRC](t),SUGARLandR_All[NRC](t);

SUGARLandR_NoCons[R] = 0,R = 1 to NRC;
SUGARLandR_BioRes[R] = 0,R = 1 to NRC;
SUGARLandR_Degrad[R] = 0,R = 1 to NRC;
SUGARLandR_WaterS[R] = 0,R = 1 to NRC;
SUGARLandR_WetLan[R] = 0,R = 1 to NRC;
SUGARLandR_Abando[R] = 0,R = 1 to NRC;
SUGARLandR_All[R] = 0,R = 1 to NRC;

sumarray (SUGARLandR_NoCons, NC, NRC, region, SUGARLandHa_NoCons);
sumarray (SUGARLandR_BioRes, NC, NRC, region, SUGARLandHa_BioRes);
sumarray (SUGARLandR_Degrad, NC, NRC, region, SUGARLandHa_Degrad);
sumarray (SUGARLandR_WaterS, NC, NRC, region, SUGARLandHa_WaterS);
sumarray (SUGARLandR_WetLan, NC, NRC, region, SUGARLandHa_WetLan);
sumarray (SUGARLandR_Abando, NC, NRC, region, SUGARLandHa_Abando);
sumarray (SUGARLandR_All, NC, NRC, region, SUGARLandHa_All);

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

MAIZEYield_Scen[R,i,1]	= MAIZEYieldCurve_NoCons[R,i], R = 1 TO NRC, i = 1 TO CAT;
MAIZEYield_Scen[R,i,2]	= MAIZEYieldCurve_Biores[R,i], R = 1 TO NRC, i = 1 TO CAT;
MAIZEYield_Scen[R,i,3]	= MAIZEYieldCurve_Degrad[R,i], R = 1 TO NRC, i = 1 TO CAT;
MAIZEYield_Scen[R,i,4]	= MAIZEYieldCurve_WaterS[R,i], R = 1 TO NRC, i = 1 TO CAT;
MAIZEYield_Scen[R,i,5]	= MAIZEYieldCurve_WetLan[R,i], R = 1 TO NRC, i = 1 TO CAT;
MAIZEYield_Scen[R,i,6]	= MAIZEYieldCurve_Abando[R,i], R = 1 TO NRC, i = 1 TO CAT;
MAIZEYield_Scen[R,i,7]	= MAIZEYieldCurve_All[R,i], R = 1 TO NRC, i = 1 TO CAT;

SUGARYield_Scen[R,i,1]	= SUGARYieldCurve_NoCons[R,i], R = 1 TO NRC, i = 1 TO CAT;
SUGARYield_Scen[R,i,2]	= SUGARYieldCurve_Biores[R,i], R = 1 TO NRC, i = 1 TO CAT;
SUGARYield_Scen[R,i,3]	= SUGARYieldCurve_Degrad[R,i], R = 1 TO NRC, i = 1 TO CAT;
SUGARYield_Scen[R,i,4]	= SUGARYieldCurve_WaterS[R,i], R = 1 TO NRC, i = 1 TO CAT;
SUGARYield_Scen[R,i,5]	= SUGARYieldCurve_WetLan[R,i], R = 1 TO NRC, i = 1 TO CAT;
SUGARYield_Scen[R,i,6]	= SUGARYieldCurve_Abando[R,i], R = 1 TO NRC, i = 1 TO CAT;
SUGARYield_Scen[R,i,7]	= SUGARYieldCurve_All[R,i], R = 1 TO NRC, i = 1 TO CAT;

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

MAIZELandHaR_Scen[R,i,1]= MAIZELandR_NoCons[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
MAIZELandHaR_Scen[R,i,2]= MAIZELandR_BioRes[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
MAIZELandHaR_Scen[R,i,3]= MAIZELandR_Degrad[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
MAIZELandHaR_Scen[R,i,4]= MAIZELandR_WaterS[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
MAIZELandHaR_Scen[R,i,5]= MAIZELandR_WetLan[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
MAIZELandHaR_Scen[R,i,6]= MAIZELandR_Abando[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
MAIZELandHaR_Scen[R,i,7]= MAIZELandR_All[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;

SUGARLandHaR_Scen[R,i,1]= SUGARLandR_NoCons[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
SUGARLandHaR_Scen[R,i,2]= SUGARLandR_BioRes[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
SUGARLandHaR_Scen[R,i,3]= SUGARLandR_Degrad[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
SUGARLandHaR_Scen[R,i,4]= SUGARLandR_WaterS[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
SUGARLandHaR_Scen[R,i,5]= SUGARLandR_WetLan[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
SUGARLandHaR_Scen[R,i,6]= SUGARLandR_Abando[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;
SUGARLandHaR_Scen[R,i,7]= SUGARLandR_All[R]/CAT, R = 1 TO NRC, i = 1 TO CAT;

! OUTPUTS
FILE("Output\WOODYYield_Scen.dat")			= WOODYYield_Scen;
FILE("Output\NWOODYield_Scen.dat")			= NWOODYield_Scen;
FILE("Output\MAIZEYield_Scen.dat")			= MAIZEYield_Scen;
FILE("Output\SUGARYield_Scen.dat")			= SUGARYield_Scen;

FILE("Output\WOODYLandHaR_Scen.dat")			= WOODYLandHaR_Scen;
FILE("Output\NWOODLandHaR_Scen.dat")			= NWOODLandHaR_Scen;
FILE("Output\MAIZELandHaR_Scen.dat")			= MAIZELandHaR_Scen;
FILE("Output\SUGARLandHaR_Scen.dat")			= SUGARLandHaR_Scen;

END;