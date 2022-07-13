/*****************************************************************************
* Project: Value of Children's Hospitals
* Date: 9/4/2018
* Description: Generate share of pediatric inpatient discharges per hospital
/*****************************************************************************/
*OPTIONS NOSOURCE NOSOURCE2 NOSYMBOLGEN NOMPRINT;
%INCLUDE "...sas_vertica.txt";
LIBNAME hcci vertica server='vertica' 
		port=5433 
		user="&userid." 
		password="&password." 
		database='enclave' 
		schema='hcci' 
		preserve_tab_names=yes preserve_col_names=yes;

proc sql;
connect to vertica (user="&userid." password="&password." server='vertica' database='enclave');

/*****************************************************************************/
/* Create Tables for Inpatient Claims */
/*****************************************************************************/
/* 1. Select relevant population and years */
execute (create local temp table ipclaims on commit preserve rows as select distinct
		s.*,
		r.AGE_BAND_CD,
		r.GDR,
		r.MBR_ZIP_5_CD,
		r.PROD,
		r.MKT_SGMNT_CD,
		r.FUNDING
	from hcci.ip_sddv2_1015 s
	inner join hcci.member_sddv2_1015 r
	on s.Z_PATID = r.Z_PATID AND s.NONCOM_FLAG = r.NONCOM_FLAG AND s.MNTH = r.MNTH AND s.YR = r.YR
	where s.YR in ('2010', '2011', '2012', '2013', '2014', '2015') 
		AND s.NONCOM_FLAG=0 AND s.INDV_FLAG=0 AND r.AGE_BAND_CD < 7 
		AND r.GDR in ('1', '2') AND r.PROD IN ('HMO', 'EPO', 'POS', 'PPO')
;)
by vertica;

/* 2. Inpatient Claims, 2010 */
create table ip_claims10 as
select * from connection to vertica
(select
		s.Z_PATID,
		s.YR,
		s.MNTH,
		s.TOB,
		SUBSTR(s.TOB, 1, 2) AS TOB2,
		s.FST_DT,
		s.LST_DT,
		s.FST_ADMTDT,
		s.LAST_DISCHDT,
		s.Z_ADMIT_ID,
		s.ADMIT_SRC,
		s.LOS,
		s.MDC,
		s.CHARGE,
		s.CALC_ALLWD,
		s.UNITS,
		s.DIAG1,
		s.DIAG2,
		s.DIAG3,
		s.ICD10_CM1,
		s.ICD10_CM2,
		s.ICD10_CM3,
		s.ICD10_CM4,
		s.ICD10_CM5,
		s.DRG,
		s.DRG_TYPE,
		s.DSTATUS,
		s.PROC_CD,
		s.PROC1,
		s.PROC2,
		s.PROC3,
		s.ICD10_PCS1,
		s.ICD10_PCS2,
		s.ICD10_PCS3,
		s.ICD10_PCS4,
		s.ICD10_PCS5,
		s.RVNU_CD,
		s.POS,
		s.HNPI,
		s.HNPI_BE,
		s.PROVCAT,
		s.PROV_ZIP_5_CD,
		s.NTWRK_IND,
		s.PRIMARY_COV_IND,
		s.AGE_BAND_CD,
		s.GDR,
		s.MBR_ZIP_5_CD,
		s.PROD,
		s.MKT_SGMNT_CD,
		s.FUNDING
from ipclaims s
where YR ='2010' 
;)
; 

/* 3. Inpatient Claims, 2011 */
create table ip_claims11 as
select * from connection to vertica
(select
		s.Z_PATID,
		s.YR,
		s.MNTH,
		s.TOB,
		SUBSTR(s.TOB, 1, 2) AS TOB2,
		s.FST_DT,
		s.LST_DT,
		s.FST_ADMTDT,
		s.LAST_DISCHDT,
		s.Z_ADMIT_ID,
		s.ADMIT_SRC,
		s.LOS,
		s.MDC,
		s.CHARGE,
		s.CALC_ALLWD,
		s.UNITS,
		s.DIAG1,
		s.DIAG2,
		s.DIAG3,
		s.ICD10_CM1,
		s.ICD10_CM2,
		s.ICD10_CM3,
		s.ICD10_CM4,
		s.ICD10_CM5,
		s.DRG,
		s.DRG_TYPE,
		s.DSTATUS,
		s.PROC_CD,
		s.PROC1,
		s.PROC2,
		s.PROC3,
		s.ICD10_PCS1,
		s.ICD10_PCS2,
		s.ICD10_PCS3,
		s.ICD10_PCS4,
		s.ICD10_PCS5,
		s.RVNU_CD,
		s.POS,
		s.HNPI,
		s.HNPI_BE,
		s.PROVCAT,
		s.PROV_ZIP_5_CD,
		s.NTWRK_IND,
		s.PRIMARY_COV_IND,
		s.AGE_BAND_CD,
		s.GDR,
		s.MBR_ZIP_5_CD,
		s.PROD,
		s.MKT_SGMNT_CD,
		s.FUNDING
from ipclaims s
where YR ='2011';)
; 


/* 4. Inpatient Claims, 2012 */
create table ip_claims12 as
select * from connection to vertica
(select
		s.Z_PATID,
		s.YR,
		s.MNTH,
		s.TOB,
		SUBSTR(s.TOB, 1, 2) AS TOB2,
		s.FST_DT,
		s.LST_DT,
		s.FST_ADMTDT,
		s.LAST_DISCHDT,
		s.Z_ADMIT_ID,
		s.ADMIT_SRC,
		s.LOS,
		s.MDC,
		s.CHARGE,
		s.CALC_ALLWD,
		s.UNITS,
		s.DIAG1,
		s.DIAG2,
		s.DIAG3,
		s.ICD10_CM1,
		s.ICD10_CM2,
		s.ICD10_CM3,
		s.ICD10_CM4,
		s.ICD10_CM5,
		s.DRG,
		s.DRG_TYPE,
		s.DSTATUS,
		s.PROC_CD,
		s.PROC1,
		s.PROC2,
		s.PROC3,
		s.ICD10_PCS1,
		s.ICD10_PCS2,
		s.ICD10_PCS3,
		s.ICD10_PCS4,
		s.ICD10_PCS5,
		s.RVNU_CD,
		s.POS,
		s.HNPI,
		s.HNPI_BE,
		s.PROVCAT,
		s.PROV_ZIP_5_CD,
		s.NTWRK_IND,
		s.PRIMARY_COV_IND,
		s.AGE_BAND_CD,
		s.GDR,
		s.MBR_ZIP_5_CD,
		s.PROD,
		s.MKT_SGMNT_CD,
		s.FUNDING
from ipclaims s
where YR ='2012';)
; 


/* 5. Inpatient Claims, 2013 */
create table ip_claims13 as
select * from connection to vertica
(select
		s.Z_PATID,
		s.YR,
		s.MNTH,
		s.TOB,
		SUBSTR(s.TOB, 1, 2) AS TOB2,
		s.FST_DT,
		s.LST_DT,
		s.FST_ADMTDT,
		s.LAST_DISCHDT,
		s.Z_ADMIT_ID,
		s.ADMIT_SRC,
		s.LOS,
		s.MDC,
		s.CHARGE,
		s.CALC_ALLWD,
		s.UNITS,
		s.DIAG1,
		s.DIAG2,
		s.DIAG3,
		s.ICD10_CM1,
		s.ICD10_CM2,
		s.ICD10_CM3,
		s.ICD10_CM4,
		s.ICD10_CM5,
		s.DRG,
		s.DRG_TYPE,
		s.DSTATUS,
		s.PROC_CD,
		s.PROC1,
		s.PROC2,
		s.PROC3,
		s.ICD10_PCS1,
		s.ICD10_PCS2,
		s.ICD10_PCS3,
		s.ICD10_PCS4,
		s.ICD10_PCS5,
		s.RVNU_CD,
		s.POS,
		s.HNPI,
		s.HNPI_BE,
		s.PROVCAT,
		s.PROV_ZIP_5_CD,
		s.NTWRK_IND,
		s.PRIMARY_COV_IND,
		s.AGE_BAND_CD,
		s.GDR,
		s.MBR_ZIP_5_CD,
		s.PROD,
		s.MKT_SGMNT_CD,
		s.FUNDING
from ipclaims s
where YR ='2013';)
; 

/* 6. Inpatient Claims, 2014 */
create table ip_claims14 as
select * from connection to vertica
(select
		s.Z_PATID,
		s.YR,
		s.MNTH,
		s.TOB,
		SUBSTR(s.TOB, 1, 2) AS TOB2,
		s.FST_DT,
		s.LST_DT,
		s.FST_ADMTDT,
		s.LAST_DISCHDT,
		s.Z_ADMIT_ID,
		s.ADMIT_SRC,
		s.LOS,
		s.MDC,
		s.CHARGE,
		s.CALC_ALLWD,
		s.UNITS,
		s.DIAG1,
		s.DIAG2,
		s.DIAG3,
		s.ICD10_CM1,
		s.ICD10_CM2,
		s.ICD10_CM3,
		s.ICD10_CM4,
		s.ICD10_CM5,
		s.DRG,
		s.DRG_TYPE,
		s.DSTATUS,
		s.PROC_CD,
		s.PROC1,
		s.PROC2,
		s.PROC3,
		s.ICD10_PCS1,
		s.ICD10_PCS2,
		s.ICD10_PCS3,
		s.ICD10_PCS4,
		s.ICD10_PCS5,
		s.RVNU_CD,
		s.POS,
		s.HNPI,
		s.HNPI_BE,
		s.PROVCAT,
		s.PROV_ZIP_5_CD,
		s.NTWRK_IND,
		s.PRIMARY_COV_IND,
		s.AGE_BAND_CD,
		s.GDR,
		s.MBR_ZIP_5_CD,
		s.PROD,
		s.MKT_SGMNT_CD,
		s.FUNDING
from ipclaims s
where YR ='2014';)
; 

/* 7. Inpatient Claims, 2015 */
create table ip_claims15 as
select * from connection to vertica
(select
		s.Z_PATID,
		s.YR,
		s.MNTH,
		s.TOB,
		SUBSTR(s.TOB, 1, 2) AS TOB2,
		s.FST_DT,
		s.LST_DT,
		s.FST_ADMTDT,
		s.LAST_DISCHDT,
		s.Z_ADMIT_ID,
		s.ADMIT_SRC,
		s.LOS,
		s.MDC,
		s.CHARGE,
		s.CALC_ALLWD,
		s.UNITS,
		s.DIAG1,
		s.DIAG2,
		s.DIAG3,
		s.ICD10_CM1,
		s.ICD10_CM2,
		s.ICD10_CM3,
		s.ICD10_CM4,
		s.ICD10_CM5,
		s.DRG,
		s.DRG_TYPE,
		s.DSTATUS,
		s.PROC_CD,
		s.PROC1,
		s.PROC2,
		s.PROC3,
		s.ICD10_PCS1,
		s.ICD10_PCS2,
		s.ICD10_PCS3,
		s.ICD10_PCS4,
		s.ICD10_PCS5,
		s.RVNU_CD,
		s.POS,
		s.HNPI,
		s.HNPI_BE,
		s.PROVCAT,
		s.PROV_ZIP_5_CD,
		s.NTWRK_IND,
		s.PRIMARY_COV_IND,
		s.AGE_BAND_CD,
		s.GDR,
		s.MBR_ZIP_5_CD,
		s.PROD,
		s.MKT_SGMNT_CD,
		s.FUNDING
from ipclaims s
where YR ='2015';)
; 


disconnect from vertica;
quit;


proc sql;
CREATE TABLE ip_admits10 AS
	SELECT DISTINCT
	z_patid, z_admit_id, dstatus, fst_admtdt, last_dischdt, hnpi, hnpi_be,
	mdc, drg, age_band_cd, gdr, mbr_zip_5_cd, prod, mkt_sgmnt_cd, funding, prov_zip_5_cd	
	FROM work.ip_claims10
	WHERE tob2 in ('11','12', '18', '41', '85')
;

CREATE TABLE ip_admits11 AS
	SELECT DISTINCT
	z_patid, z_admit_id, dstatus, fst_admtdt, last_dischdt,  hnpi, hnpi_be,
	mdc, drg, age_band_cd, gdr, mbr_zip_5_cd, prod, mkt_sgmnt_cd, funding, prov_zip_5_cd	
	FROM work.ip_claims11
	WHERE tob2 in ('11','12', '18', '41', '85')
;

CREATE TABLE ip_admits12 AS
	SELECT DISTINCT
	z_patid, z_admit_id, dstatus, fst_admtdt, last_dischdt, hnpi, hnpi_be,
	mdc, drg, age_band_cd, gdr, mbr_zip_5_cd, prod, mkt_sgmnt_cd, funding, prov_zip_5_cd	
	FROM work.ip_claims12
	WHERE tob2 in ('11','12', '18', '41', '85')
;

CREATE TABLE ip_admits13 AS
	SELECT DISTINCT
	z_patid, z_admit_id, dstatus, fst_admtdt, last_dischdt, hnpi, hnpi_be,
	mdc, drg, age_band_cd, gdr, mbr_zip_5_cd, prod, mkt_sgmnt_cd, funding, prov_zip_5_cd
	FROM work.ip_claims13
	WHERE tob2 in ('11','12', '18', '41', '85')
;

CREATE TABLE ip_admits14 AS
	SELECT DISTINCT
	z_patid, z_admit_id, dstatus, fst_admtdt, last_dischdt, hnpi, hnpi_be,
	mdc, drg, age_band_cd, gdr, mbr_zip_5_cd, prod, mkt_sgmnt_cd, funding, prov_zip_5_cd	
	FROM work.ip_claims14
	WHERE tob2 in ('11','12', '18', '41', '85')
;

CREATE TABLE ip_admits15 AS
	SELECT DISTINCT
	z_patid, z_admit_id, dstatus, fst_admtdt, last_dischdt, hnpi, hnpi_be,
	mdc, drg, age_band_cd, gdr, mbr_zip_5_cd, prod, mkt_sgmnt_cd, funding, prov_zip_5_cd	
	FROM work.ip_claims15
	WHERE tob2 in ('11','12', '18', '41', '85')
;


QUIT;

proc export data=work.ip_admits10
	outfile='H:\Research\ipadmits10.csv' replace
	dbms=csv;
run;

proc export data=work.ip_admits11
	outfile='H:\Research\ipadmits11.csv' replace
	dbms=csv;
run;

proc export data=work.ip_admits12
	outfile='H:\Research\ipadmits12.csv' replace
	dbms=csv;
run;

proc export data=work.ip_admits13
	outfile='H:\Research\ipadmits13.csv' replace
	dbms=csv;
run;

proc export data=work.ip_admits14
	outfile='H:\Research\ipadmits14.csv' replace
	dbms=csv;
run;

proc export data=work.ip_admits15
	outfile='H:\Research\ipadmits15.csv' replace
	dbms=csv;
run;


