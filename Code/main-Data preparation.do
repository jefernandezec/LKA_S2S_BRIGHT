**===Data preparation por the BRIGHT S2S implementation=================
**===S2S Databases preparation =========================================
*Authors:        Marta Schoch (mschoch@worldbank.org)
*				 Tiloka da Silva
*                Jaime Fernandez
*Last update:	12/18/25
*----------------------------------------------------------------------
*====================================================================


clear
set more off

*global code "C:\Users\wb562318\Github\BRIGHT_PEA\Code"
global code "C:\Users\wb553773\GitHub\BRIGHT_PEA\Code"

*global data "C:/Users/wb562318/OneDrive - WBG/Documents/POV-SAR/SL/PA/Analysis/Data"
global data "C:\Users\wb553773\WBG\Marta Schoch - Analysis\Data"
global lfs2019  $data/LFS
global hies2019 $data/HIES

*global ifpri "C:\Users\wb562318\OneDrive - WBG\Documents\POV-SAR\SL\PA\Analysis\Data\IFPRI\World Bank BRIGHT"
global ifpri "C:\Users\wb553773\WBG\Marta Schoch - Analysis\Data\IFPRI\World Bank BRIGHT"


global long "$ifpri\Data\Long"
global wide "$ifpri\Data\Wide"
global rundata "$ifpri\rundata"
global out "$ifpri\Output"

//Run Code 
do "$code/01_HIES_harmonization.do"
do "$code/02_BRIGHT_harmonization.do"
do "$code/03_comparable_consumption_HIES.do"
do "$code/04_comparable_consumption_BRIGHT.do"
do "$code/05_PreSimulation.do"
