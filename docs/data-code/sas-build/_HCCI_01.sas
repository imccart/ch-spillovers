/*****************************************************************************
* Project: Value of Children's Hospitals
* Date: 1/30/2020
* Description: Pull IP and OP claims for relevant pediatric population
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
/* Create Tables of Unique Patients */
/*****************************************************************************/
create table ippatients as
select * from connection to vertica
(select distinct
		s.Z_PATID
	from hcci.ip_sddv2_1015 s
	where s.YR in ('2010', '2011', '2012', '2013', '2014', '2015') 
		AND s.NONCOM_FLAG=0 AND s.INDV_FLAG=0
		AND (s.PROC1 in ('4466', '4467', '470', '4701', '4709', '640', '171', '172', '530', '531', '625', 
				'810', '8100', '8103', '8105', '8108', '7901', '7911', '7921', '7931', '1511', '1512', '1513', '152',
				'1521', '1522', '154', '282', '283', '286', '200', '2001', '2009', '534', '5342', '5349', '5441')
			OR s.PROC2 in ('4466', '4467', '470', '4701', '4709', '640', '171', '172', '530', '531', '625', 
				'810', '8100', '8103', '8105', '8108', '7901', '7911', '7921', '7931', '1511', '1512', '1513', '152',
				'1521', '1522', '154', '282', '283', '286', '200', '2001', '2009', '534', '5342', '5349', '5441')
			OR s.PROC3 in ('4466', '4467', '470', '4701', '4709', '640', '171', '172', '530', '531', '625', 
				'810', '8100', '8103', '8105', '8108', '7901', '7911', '7921', '7931', '1511', '1512', '1513', '152',
				'1521', '1522', '154', '282', '283', '286', '200', '2001', '2009', '534', '5342', '5349', '5441'))
;)
;

create table oppatients as
select * from connection to vertica
(select distinct
		s.Z_PATID
	from hcci.op_sddv2_1015 s
	where s.YR in ('2010', '2011', '2012', '2013', '2014', '2015') 
		AND s.NONCOM_FLAG=0 AND s.INDV_FLAG=0
		AND s.PROC_CD in ('43280', '43327', '43328', '44950', '4960', '44970', '54150', '54160', '54161', 
			'49491', '49492', '49495', '49496', '49500', '49501', '49505', '49507', '49520', '49521', '49525',
			'49529', '49595', '49650', '54560', '54640', '54650', '54692', '22800', '22802', '22804', '24500',
			'24505', '24515', '24516', '24530', '24535', '24538', '24545', '24546', '24560', '24565', '24566',
			'24575', '24576', '24577', '24579', '24582', '67311', '67312', '67314', '67316', '67318', '42820', 
			'42821', '42825', '42826', '42830', '42831', '42835', '42836', '69420', '69421', '69433', '69436',
			'49570', '49572', '49580', '49582', '49585', '49587')
;)
;


/*****************************************************************************/
/* Create Tables for Inpatient Claims */
/*****************************************************************************/
/* 1. Select relevant population and years */
create table ipclaims as
select * from connection to vertica
(select distinct
		s.*,
		r.AGE_BAND_CD,
		r.GDR,
		r.MBR_ZIP_5_CD,
		r.PROD,
		r.MKT_SGMNT_CD,
		r.FUNDING,
		r.CDHP_CD,
		r.Z_GROUP_ID
	from hcci.ip_sddv2_1015 s
	inner join hcci.member_sddv2_1015 r
	on s.Z_PATID = r.Z_PATID AND s.NONCOM_FLAG = r.NONCOM_FLAG AND s.MNTH = r.MNTH AND s.YR = r.YR
	where s.YR in ('2010', '2011', '2012', '2013', '2014', '2015') 
		AND s.NONCOM_FLAG=0 AND s.INDV_FLAG=0 AND r.AGE_BAND_CD < 7 
		AND r.GDR in ('1', '2') AND r.PROD IN ('HMO', 'EPO', 'POS', 'PPO')
;)
;

/* 2. Inpatient Claims, 2010 */
create table ip_claims10 as
	select
		s.Z_PATID,
		s.Z_GROUP_ID,
		s.YR,
		s.MNTH,
		s.TOB,
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
		s.COINS,
		s.COPAY,
		s.DEDUCT,
		s.TOT_MEM_CS,
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
	from work.ipclaims s
	where YR ='2010' 
; 


/* 3. Inpatient Claims, 2011 */
create table ip_claims11 as
	select
		s.Z_PATID,
		s.Z_GROUP_ID,
		s.YR,
		s.MNTH,
		s.TOB,
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
		s.COINS,
		s.COPAY,
		s.DEDUCT,
		s.TOT_MEM_CS,
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
	from work.ipclaims s
	where YR ='2011'
; 


/* 4. Inpatient Claims, 2012 */
create table ip_claims12 as
	select
		s.Z_PATID,
		s.Z_GROUP_ID,
		s.YR,
		s.MNTH,
		s.TOB,
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
		s.COINS,
		s.COPAY,
		s.DEDUCT,
		s.TOT_MEM_CS,
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
	from work.ipclaims s
	where YR ='2012'
; 


/* 5. Inpatient Claims, 2013 */
create table ip_claims13 as
	select
		s.Z_PATID,
		s.Z_GROUP_ID,
		s.YR,
		s.MNTH,
		s.TOB,
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
		s.COINS,
		s.COPAY,
		s.DEDUCT,
		s.TOT_MEM_CS,
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
	from work.ipclaims s
	where YR ='2013'
; 

/* 6. Inpatient Claims, 2014 */
create table ip_claims14 as
	select
		s.Z_PATID,
		s.Z_GROUP_ID,
		s.YR,
		s.MNTH,
		s.TOB,
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
		s.COINS,
		s.COPAY,
		s.DEDUCT,
		s.TOT_MEM_CS,
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
	from work.ipclaims s
	where YR ='2014'
; 

/* 7. Inpatient Claims, 2015 */
create table ip_claims15 as
	select
		s.Z_PATID,
		s.Z_GROUP_ID,
		s.YR,
		s.MNTH,
		s.TOB,
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
		s.COINS,
		s.COPAY,
		s.DEDUCT,
		s.TOT_MEM_CS,
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
	from work.ipclaims s
	where YR ='2015'
; 


/*****************************************************************************/
/* Create Tables for Outpatient Claims */
/*****************************************************************************/
/* 1. Select relevant population and years */
create table opclaims as
select * from connection to vertica
(select distinct
		s.*,
		r.AGE_BAND_CD,
		r.GDR,
		r.MBR_ZIP_5_CD,
		r.PROD,
		r.MKT_SGMNT_CD,
		r.FUNDING,
		r.CDHP_CD,
		r.Z_GROUP_ID
	from hcci.op_sddv2_1015 s
	inner join hcci.member_sddv2_1015 r
	on s.Z_PATID = r.Z_PATID AND s.NONCOM_FLAG = r.NONCOM_FLAG AND s.MNTH = r.MNTH AND s.YR = r.YR
	where s.YR in ('2010', '2011', '2012', '2013', '2014', '2015') 
		AND s.NONCOM_FLAG=0 AND s.INDV_FLAG=0 AND r.AGE_BAND_CD < 7 
		AND r.GDR in ('1', '2') AND r.PROD IN ('HMO', 'EPO', 'POS', 'PPO')
;)
;

/* 2. Outpatient Claims, 2010 */
create table op_claims10 as
	select
		s.Z_PATID,
		s.Z_GROUP_ID,
		s.YR,
		s.MNTH,
		s.TOB,
		s.FST_DT,
		s.LST_DT,
		s.Z_VISITID,
		s.CHARGE,
		s.CALC_ALLWD,
		s.COINS,
		s.COPAY,
		s.DEDUCT,
		s.TOT_MEM_CS,
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
	from work.opclaims s
	where YR ='2010'
; 

/* 3. Outpatient Claims, 2011 */
create table op_claims11 as
	select
		s.Z_PATID,
		s.Z_GROUP_ID,
		s.YR,
		s.MNTH,
		s.TOB,
		s.FST_DT,
		s.LST_DT,
		s.Z_VISITID,
		s.CHARGE,
		s.CALC_ALLWD,
		s.COINS,
		s.COPAY,
		s.DEDUCT,
		s.TOT_MEM_CS,
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
	from work.opclaims s
	where YR ='2011'
; 


/* 4. Outpatient Claims, 2012 */
create table op_claims12 as
	select
		s.Z_PATID,
		s.Z_GROUP_ID,
		s.YR,
		s.MNTH,
		s.TOB,
		s.FST_DT,
		s.LST_DT,
		s.Z_VISITID,
		s.CHARGE,
		s.CALC_ALLWD,
		s.COINS,
		s.COPAY,
		s.DEDUCT,
		s.TOT_MEM_CS,
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
	from work.opclaims s
	where YR ='2012'
; 


/* 5. Outpatient Claims, 2013 */
create table op_claims13 as
	select
		s.Z_PATID,
		s.Z_GROUP_ID,
		s.YR,
		s.MNTH,
		s.TOB,
		s.FST_DT,
		s.LST_DT,
		s.Z_VISITID,
		s.CHARGE,
		s.CALC_ALLWD,
		s.COINS,
		s.COPAY,
		s.DEDUCT,
		s.TOT_MEM_CS,
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
	from work.opclaims s
	where YR ='2013'
; 

/* 6. Outpatient Claims, 2014 */
create table op_claims14 as
	select
		s.Z_PATID,
		s.Z_GROUP_ID,
		s.YR,
		s.MNTH,
		s.TOB,
		s.FST_DT,
		s.LST_DT,
		s.Z_VISITID,
		s.CHARGE,
		s.CALC_ALLWD,
		s.COINS,
		s.COPAY,
		s.DEDUCT,
		s.TOT_MEM_CS,
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
	from work.opclaims s
	where YR ='2014'
; 

/* 6. Outpatient Claims, 2015 */
create table op_claims15 as
	select
		s.Z_PATID,
		s.Z_GROUP_ID,
		s.YR,
		s.MNTH,
		s.TOB,
		s.FST_DT,
		s.LST_DT,
		s.Z_VISITID,
		s.CHARGE,
		s.CALC_ALLWD,
		s.COINS,
		s.COPAY,
		s.DEDUCT,
		s.TOT_MEM_CS,
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
	from work.opclaims s
	where YR ='2015'
; 

disconnect from vertica;
quit;


proc sql;
CREATE TABLE unique_patients AS
	SELECT * FROM work.ippatients 
	UNION 
	SELECT * FROM work.oppatients
;

CREATE TABLE ip_claims10_proc AS
	SELECT a.* FROM work.ip_claims10 AS a
	INNER JOIN work.unique_patients AS p
	on a.Z_PATID=p.Z_PATID WHERE p.Z_PATID IS NOT MISSING
;

CREATE TABLE ip_claims11_proc AS
	SELECT a.* FROM work.ip_claims11 AS a
	INNER JOIN work.unique_patients AS p
	on a.Z_PATID=p.Z_PATID WHERE p.Z_PATID IS NOT MISSING
;

CREATE TABLE ip_claims12_proc AS
	SELECT a.* FROM work.ip_claims12 AS a
	INNER JOIN work.unique_patients AS p
	on a.Z_PATID=p.Z_PATID WHERE p.Z_PATID IS NOT MISSING
;

CREATE TABLE ip_claims13_proc AS
	SELECT a.* FROM work.ip_claims13 AS a
	INNER JOIN work.unique_patients AS p
	on a.Z_PATID=p.Z_PATID WHERE p.Z_PATID IS NOT MISSING
;

CREATE TABLE ip_claims14_proc AS
	SELECT a.* FROM work.ip_claims14 AS a
	INNER JOIN work.unique_patients AS p
	on a.Z_PATID=p.Z_PATID WHERE p.Z_PATID IS NOT MISSING
;

CREATE TABLE ip_claims15_proc AS
	SELECT a.* FROM work.ip_claims15 AS a
	INNER JOIN work.unique_patients AS p
	on a.Z_PATID=p.Z_PATID WHERE p.Z_PATID IS NOT MISSING
;




CREATE TABLE op_claims10_proc AS
	SELECT a.* FROM work.op_claims10 AS a
	INNER JOIN work.unique_patients AS p
	on a.Z_PATID=p.Z_PATID WHERE p.Z_PATID IS NOT MISSING
;

CREATE TABLE op_claims11_proc AS
	SELECT a.* FROM work.op_claims11 AS a
	INNER JOIN work.unique_patients AS p
	on a.Z_PATID=p.Z_PATID WHERE p.Z_PATID IS NOT MISSING
;

CREATE TABLE op_claims12_proc AS
	SELECT a.* FROM work.op_claims12 AS a
	INNER JOIN work.unique_patients AS p
	on a.Z_PATID=p.Z_PATID WHERE p.Z_PATID IS NOT MISSING
;

CREATE TABLE op_claims13_proc AS
	SELECT a.* FROM work.op_claims13 AS a
	INNER JOIN work.unique_patients AS p
	on a.Z_PATID=p.Z_PATID WHERE p.Z_PATID IS NOT MISSING
;

CREATE TABLE op_claims14_proc AS
	SELECT a.* FROM work.op_claims14 AS a
	INNER JOIN work.unique_patients AS p
	on a.Z_PATID=p.Z_PATID WHERE p.Z_PATID IS NOT MISSING
;

CREATE TABLE op_claims15_proc AS
	SELECT a.* FROM work.op_claims15 AS a
	INNER JOIN work.unique_patients AS p
	on a.Z_PATID=p.Z_PATID WHERE p.Z_PATID IS NOT MISSING
;

QUIT;

proc export data=work.ip_claims10_proc
	outfile='H:\Research\ipclaims10.csv' replace
	dbms=csv;
run;

proc export data=work.ip_claims11_proc
	outfile='H:\Research\ipclaims11.csv' replace
	dbms=csv;
run;

proc export data=work.ip_claims12_proc
	outfile='H:\Research\ipclaims12.csv' replace
	dbms=csv;
run;

proc export data=work.ip_claims13_proc
	outfile='H:\Research\ipclaims13.csv' replace
	dbms=csv;
run;

proc export data=work.ip_claims14_proc
	outfile='H:\Research\ipclaims14.csv' replace
	dbms=csv;
run;

proc export data=work.ip_claims15_proc
	outfile='H:\Research\ipclaims15.csv' replace
	dbms=csv;
run;



proc export data=work.op_claims10_proc
	outfile='H:\Research\opclaims10.csv' replace
	dbms=csv;
run;

proc export data=work.op_claims11_proc
	outfile='H:\Research\opclaims11.csv' replace
	dbms=csv;
run;

proc export data=work.op_claims12_proc
	outfile='H:\Research\opclaims12.csv' replace
	dbms=csv;
run;

proc export data=work.op_claims13_proc
	outfile='H:\Research\opclaims13.csv' replace
	dbms=csv;
run;

proc export data=work.op_claims14_proc
	outfile='H:\Research\opclaims14.csv' replace
	dbms=csv;
run;

proc export data=work.op_claims15_proc
	outfile='H:\Research\opclaims15.csv' replace
	dbms=csv;
run;


