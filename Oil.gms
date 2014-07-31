**************************************************************************
*
*   Author:      bob
*   Called by:   CAPMOD.GMS
*   Created:     16-06-2014 10:11:03
*   Purpose:     Scenario definition file
*
**************************************************************************
*
$setglobal SCENDES 
*
*  Baseline scenario
*
$include ..\model\scen\base_scenarios\noShock.gms
*
*  Category : Output shocks
*
$include ..\model\scen\Output_shocks\technical_Progres.gms
