CREATE DATABASE IF NOT EXISTS db_data
  DEFAULT
  CHARACTER SET = 'utf8'
  COLLATE = 'utf8_danish_ci';

use db_data;

CREATE TABLE IF NOT EXISTS `bowelfunctionformdatacontract` (
  `SkjemaGUID` varchar(255) NOT NULL,
  `ParentCNum` int NOT NULL,
  `BfxnbaDt` datetime DEFAULT NULL,
  `Gifxnun` int NOT NULL,
  `SurgicalIntervention` int NOT NULL,
  `Apndec` int NOT NULL,
  `ApndecDt` datetime DEFAULT NULL,
  `ApndecDtUnknown` varchar(255) NOT NULL,
  `Chcyec` int NOT NULL,
  `ChcyecDt` datetime DEFAULT NULL,
  `ChcyecDtUnknown` varchar(255) NOT NULL,
  `Hemec` int NOT NULL,
  `HemecDt` datetime DEFAULT NULL,
  `HemecDtUnknown` varchar(255) NOT NULL,
  `Colost` int NOT NULL,
  `ColostDt` datetime DEFAULT NULL,
  `ColostDtUnknown` varchar(255) NOT NULL,
  `Ileost` int NOT NULL,
  `IleostDt` datetime DEFAULT NULL,
  `IleostDtUnknown` varchar(255) NOT NULL,
  `Apndic` int NOT NULL,
  `ApndicDt` datetime DEFAULT NULL,
  `ApndicDtUnknown` varchar(255) NOT NULL,
  `Otgisurg` int NOT NULL,
  `OtgisurgDt` datetime DEFAULT NULL,
  `OtgisurgDtUnknown` varchar(255) NOT NULL,
  `Defawrns` int NOT NULL,
  `DefcmthUn` varchar(255) NOT NULL,
  `DefcmthNa` varchar(255) NOT NULL,
  `DefcmthM1` varchar(255) NOT NULL,
  `OthdefS1` varchar(255) NOT NULL,
  `DefcmthM2` varchar(255) NOT NULL,
  `OthdefS2` varchar(255) NOT NULL,
  `DefcmthM3` varchar(255) NOT NULL,
  `OthdefS3` varchar(255) NOT NULL,
  `DefcmthM4` varchar(255) NOT NULL,
  `OthdefS4` varchar(255) NOT NULL,
  `DefcmthM5` varchar(255) NOT NULL,
  `OthdefS5` varchar(255) NOT NULL,
  `DefcmthM6` varchar(255) NOT NULL,
  `OthdefS6` varchar(255) NOT NULL,
  `DefcmthM7` varchar(255) NOT NULL,
  `OthdefS7` varchar(255) NOT NULL,
  `DefcmthM8` varchar(255) NOT NULL,
  `OthdefS8` varchar(255) NOT NULL,
  `DefcmthM9` varchar(255) NOT NULL,
  `OthdefS9` varchar(255) NOT NULL,
  `DefcmthM10` varchar(255) NOT NULL,
  `OthdefS10` varchar(255) NOT NULL,
  `Avdeftm` int NOT NULL,
  `Avdeftm2` int NOT NULL,
  `Deffrq` int NOT NULL,
  `Deffrq2` int NOT NULL,
  `Deffrq3` int NOT NULL,
  `Defhdprs` int NOT NULL,
  `Dsevacar` int NOT NULL,
  `Fcincfrq` int NOT NULL,
  `Fcincfrq2` int NOT NULL,
  `Fcincfrq3` int NOT NULL,
  `Flincont` int NOT NULL,
  `Wrpadplg` int NOT NULL,
  `Wrpadplg2` int NOT NULL,
  `DrugUse` int NOT NULL,
  `Antichol` int NOT NULL,
  `Narcotic` int NOT NULL,
  `Othbfmed` int NOT NULL,
  `OralLaxatives` int NOT NULL,
  `Osmodrp` int NOT NULL,
  `Osmotab` int NOT NULL,
  `Irrtdrp` int NOT NULL,
  `Irrttab` int NOT NULL,
  `Prokinet` int NOT NULL,
  `Othorlax` int NOT NULL,
  `Fecinmed` int NOT NULL,
  `PerianalProblems` int NOT NULL,
  `PerianalProblems2` int NOT NULL,
  `Hemrhoid` int NOT NULL,
  `Panlsore` int NOT NULL,
  `Fissures` int NOT NULL,
  `Recprlps` int NOT NULL,
  `Panloth` int NOT NULL,
  `Abpain` int NOT NULL,
  `Abpain2` int NOT NULL,
  `NBD` int NOT NULL,
  `HovedskjemaGUID` varchar(255) NOT NULL,
  `FormTypeId` int NOT NULL,
  `UnitId` int NOT NULL,
  `RHF` varchar(255) DEFAULT NULL,
  `HF` varchar(255) DEFAULT NULL,
  `Hospital` varchar(255) DEFAULT NULL,
  `HealthUnitName` varchar(255) DEFAULT NULL,
  `HealthUnitShortName` varchar(255) DEFAULT NULL,
  `HealthUnitId` int DEFAULT NULL,
  `CreationDate` datetime DEFAULT NULL,
  `ProceedingID` varchar(255) DEFAULT NULL,
  `LastUpdate` datetime DEFAULT NULL,
  `FormStatus` varchar(255) NOT NULL,
  `PatientAge` int DEFAULT NULL,
  `PatientGender` varchar(255) NOT NULL,
  `MunicipalNumber` int DEFAULT NULL,
  `CurrentMunicipalNumber` int DEFAULT NULL,
  `Municipal` varchar(255) DEFAULT NULL,
  `PostalCode` int DEFAULT NULL,
  `DistrictCode` varchar(255) DEFAULT NULL,
  `AddressQuality` int NOT NULL,
  `FirstTimeClosed` datetime DEFAULT NULL,
  `FormDate` datetime NOT NULL,
  `FormVersionNumber` int DEFAULT NULL,
  `PatientInRegistryGuid` varchar(255) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_danish_ci;

CREATE TABLE IF NOT EXISTS `eq5dlformdatacontract` (
  `SkjemaGUID` varchar(255) NOT NULL,
  `Eq5dQ1Mobility` int NOT NULL,
  `Eq5dQ2Selfcare` int NOT NULL,
  `Eq5dQ3UsualActivities` int NOT NULL,
  `Eq5dQ4PainDiscomfort` int NOT NULL,
  `Eq5dQ5AnxietyDepression` int NOT NULL,
  `Eq5dQ6HealthToday` int NOT NULL,
  `Eq5d5lDt` datetime DEFAULT NULL,
  `ParentCNum` int NOT NULL,
  `HovedskjemaGUID` varchar(255) NOT NULL,
  `FormTypeId` int NOT NULL,
  `UnitId` int NOT NULL,
  `RHF` varchar(255) DEFAULT NULL,
  `HF` varchar(255) DEFAULT NULL,
  `Hospital` varchar(255) DEFAULT NULL,
  `HealthUnitName` varchar(255) DEFAULT NULL,
  `HealthUnitShortName` varchar(255) DEFAULT NULL,
  `HealthUnitId` int DEFAULT NULL,
  `CreationDate` datetime DEFAULT NULL,
  `ProceedingID` varchar(255) DEFAULT NULL,
  `LastUpdate` datetime DEFAULT NULL,
  `FormStatus` varchar(255) NOT NULL,
  `PatientAge` int DEFAULT NULL,
  `PatientGender` varchar(255) NOT NULL,
  `MunicipalNumber` int DEFAULT NULL,
  `CurrentMunicipalNumber` int DEFAULT NULL,
  `Municipal` varchar(255) DEFAULT NULL,
  `PostalCode` int DEFAULT NULL,
  `DistrictCode` varchar(255) DEFAULT NULL,
  `AddressQuality` int NOT NULL,
  `FirstTimeClosed` datetime DEFAULT NULL,
  `FormDate` datetime NOT NULL,
  `FormVersionNumber` int DEFAULT NULL,
  `PatientInRegistryGuid` varchar(255) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_danish_ci

 CREATE TABLE IF NOT EXISTS `lifequalityformdatacontract` (
  `SkjemaGUID` varchar(255) NOT NULL,
  `QolDt` datetime DEFAULT NULL,
  `SatGenrl` int DEFAULT NULL,
  `SatPhys` int DEFAULT NULL,
  `SatPsych` int DEFAULT NULL,
  `IsProms` varchar(255) NOT NULL,
  `ParentCNum` int DEFAULT NULL,
  `HovedskjemaGUID` varchar(255) NOT NULL,
  `FormTypeId` int NOT NULL,
  `UnitId` int NOT NULL,
  `RHF` varchar(255) DEFAULT NULL,
  `HF` varchar(255) DEFAULT NULL,
  `Hospital` varchar(255) DEFAULT NULL,
  `HealthUnitName` varchar(255) DEFAULT NULL,
  `HealthUnitShortName` varchar(255) DEFAULT NULL,
  `HealthUnitId` int DEFAULT NULL,
  `CreationDate` datetime DEFAULT NULL,
  `ProceedingID` varchar(255) DEFAULT NULL,
  `LastUpdate` datetime DEFAULT NULL,
  `FormStatus` varchar(255) NOT NULL,
  `PatientAge` int DEFAULT NULL,
  `PatientGender` varchar(255) NOT NULL,
  `MunicipalNumber` int DEFAULT NULL,
  `CurrentMunicipalNumber` int DEFAULT NULL,
  `Municipal` varchar(255) DEFAULT NULL,
  `PostalCode` int DEFAULT NULL,
  `DistrictCode` varchar(255) DEFAULT NULL,
  `AddressQuality` int NOT NULL,
  `FirstTimeClosed` datetime DEFAULT NULL,
  `FormDate` datetime NOT NULL,
  `FormVersionNumber` int DEFAULT NULL,
  `PatientInRegistryGuid` varchar(255) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_danish_ci

 CREATE TABLE IF NOT EXISTS `mainformdatacontract` (
  `SkjemaGUID` varchar(255) NOT NULL,
  `AdmittedOtherHosptl` varchar(255) NOT NULL,
  `Consent` int NOT NULL,
  `ConsentDt` datetime DEFAULT NULL,
  `InjuryDt` datetime DEFAULT NULL,
  `InjuryDateUnknown` varchar(255) NOT NULL,
  `AdmitDt` datetime DEFAULT NULL,
  `AdmitRehDt` datetime DEFAULT NULL,
  `DischgDt` datetime DEFAULT NULL,
  `Dead` int NOT NULL,
  `DeathDt` datetime DEFAULT NULL,
  `OutOfHosptlDy` int DEFAULT NULL,
  `OutOfHosptlDy2` int DEFAULT NULL,
  `OutOfRehabDy` int DEFAULT NULL,
  `BeforeRehDy` int DEFAULT NULL,
  `HosptlDy` int DEFAULT NULL,
  `RehabDy` int DEFAULT NULL,
  `Scietiol` int NOT NULL,
  `Ntsci` int NOT NULL,
  `VrtbrInj` int NOT NULL,
  `AssocInj` int NOT NULL,
  `SpnlSurg` int NOT NULL,
  `VentAssi` int NOT NULL,
  `PlaceDis` int NOT NULL,
  `PPlacedis` int NOT NULL,
  `ANeuNoMeasure` varchar(255) NOT NULL,
  `ANeuExmDt` datetime DEFAULT NULL,
  `ASensLvlAreaL` int NOT NULL,
  `ASensLvlLC` int NOT NULL,
  `ASensLvlLT` int NOT NULL,
  `ASensLvlLL` int NOT NULL,
  `ASensLvlLS` int NOT NULL,
  `ASensLvlAreaR` int NOT NULL,
  `ASensLvlRC` int NOT NULL,
  `ASensLvlRT` int NOT NULL,
  `ASensLvlRL` int NOT NULL,
  `ASensLvlRS` int NOT NULL,
  `AMtrLvlAreaL` int NOT NULL,
  `AMtrLvlLC` int NOT NULL,
  `AMtrLvlLT` int NOT NULL,
  `AMtrLvlLL` int NOT NULL,
  `AMtrLvlLS` int NOT NULL,
  `AMtrLvlAreaR` int NOT NULL,
  `AMtrLvlRC` int NOT NULL,
  `AMtrLvlRT` int NOT NULL,
  `AMtrLvlRL` int NOT NULL,
  `AMtrLvlRS` int NOT NULL,
  `AAis` int NOT NULL,
  `FNeuNoMeasure` varchar(255) NOT NULL,
  `FNeuExmDt` datetime DEFAULT NULL,
  `FSensLvlAreaL` int NOT NULL,
  `FSensLvlLC` int NOT NULL,
  `FSensLvlLT` int NOT NULL,
  `FSensLvlLL` int NOT NULL,
  `FSensLvlLS` int NOT NULL,
  `FSensLvlAreaR` int NOT NULL,
  `FSensLvlRC` int NOT NULL,
  `FSensLvlRT` int NOT NULL,
  `FSensLvlRL` int NOT NULL,
  `FSensLvlRS` int NOT NULL,
  `FMtrLvlAreaL` int NOT NULL,
  `FMtrLvlLC` int NOT NULL,
  `FMtrLvlLT` int NOT NULL,
  `FMtrLvlLL` int NOT NULL,
  `FMtrLvlLS` int NOT NULL,
  `FMtrLvlAreaR` int NOT NULL,
  `FMtrLvlRC` int NOT NULL,
  `FMtrLvlRT` int NOT NULL,
  `FMtrLvlRL` int NOT NULL,
  `FMtrLvlRS` int NOT NULL,
  `FAis` int NOT NULL,
  `RecCtrl` int NOT NULL,
  `FormTypeId` int NOT NULL,
  `UnitId` int DEFAULT NULL,
  `RHF` varchar(255) DEFAULT NULL,
  `HF` varchar(255) DEFAULT NULL,
  `Hospital` varchar(255) DEFAULT NULL,
  `HealthUnitName` varchar(255) DEFAULT NULL,
  `HealthUnitShortName` varchar(255) DEFAULT NULL,
  `HealthUnitId` int DEFAULT NULL,
  `CreationDate` datetime DEFAULT NULL,
  `ProceedingID` varchar(255) DEFAULT NULL,
  `LastUpdate` datetime DEFAULT NULL,
  `FormStatus` varchar(255) NOT NULL,
  `PatientAge` int DEFAULT NULL,
  `PatientGender` varchar(255) NOT NULL,
  `MunicipalNumber` int DEFAULT NULL,
  `CurrentMunicipalNumber` int DEFAULT NULL,
  `Municipal` varchar(255) DEFAULT NULL,
  `PostalCode` int DEFAULT NULL,
  `DistrictCode` varchar(255) DEFAULT NULL,
  `AddressQuality` int NOT NULL,
  `FirstTimeClosed` datetime DEFAULT NULL,
  `FormDate` datetime DEFAULT NULL,
  `DaysToNextControl` int DEFAULT NULL,
  `FormVersionNumber` int DEFAULT NULL,
  `DeathDate` datetime DEFAULT NULL,
  `DeathDateUpdateTime` datetime DEFAULT NULL,
  `ANeurologicalLevel` varchar(255) DEFAULT NULL,
  `FNeurologicalLevel` varchar(255) DEFAULT NULL,
  `ComplicNone` varchar(255) NOT NULL,
  `PressureUlcer` varchar(255) NOT NULL,
  `VTE` varchar(255) NOT NULL,
  `UTI` varchar(255) NOT NULL,
  `Sepsis` varchar(255) NOT NULL,
  `Pneumonia` varchar(255) NOT NULL,
  `Spasticity` varchar(255) NOT NULL,
  `Syringomyelia` varchar(255) NOT NULL,
  `HeterotopicOssification` varchar(255) NOT NULL,
  `AutonomicDysreflexia` varchar(255) NOT NULL,
  `OrthostaticHypotension` varchar(255) NOT NULL,
  `Osteoporosis` varchar(255) NOT NULL,
  `ComplicOther` varchar(255) NOT NULL,
  `ComplicOtherText` varchar(255) DEFAULT NULL,
  `PatientInRegistryGuid` varchar(255) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_danish_ci

 CREATE TABLE IF NOT EXISTS `urinarytractfunctionformdatacontract` (
  `SkjemaGUID` varchar(255) NOT NULL,
  `LutfxnDt` datetime DEFAULT NULL,
  `Utimprun` int NOT NULL,
  `Awarblad` int NOT NULL,
  `EmbladUn` varchar(255) NOT NULL,
  `EmbladM1` varchar(255) NOT NULL,
  `EmbladS1` varchar(255) NOT NULL,
  `EmbladM2` varchar(255) NOT NULL,
  `EmbladS2` varchar(255) NOT NULL,
  `EmbladM3` varchar(255) NOT NULL,
  `EmbladS3` varchar(255) NOT NULL,
  `EmbladM4` varchar(255) NOT NULL,
  `EmbladS4` varchar(255) NOT NULL,
  `EmbladM5` varchar(255) NOT NULL,
  `EmbladS5` varchar(255) NOT NULL,
  `EmbladM6` varchar(255) NOT NULL,
  `EmbladS6` varchar(255) NOT NULL,
  `EmbladM7` varchar(255) NOT NULL,
  `EmbladS7` varchar(255) NOT NULL,
  `EmbladM8` varchar(255) NOT NULL,
  `EmbladS8` varchar(255) NOT NULL,
  `EmbladM9` varchar(255) NOT NULL,
  `EmbladS9` varchar(255) NOT NULL,
  `EmbladM10` varchar(255) NOT NULL,
  `EmbladS10` varchar(255) NOT NULL,
  `EmbladM11` varchar(255) NOT NULL,
  `EmbladS11` varchar(255) NOT NULL,
  `EmbladM12` varchar(255) NOT NULL,
  `EmbladS12` varchar(255) NOT NULL,
  `Avbladem` int DEFAULT NULL,
  `Incontnc` int NOT NULL,
  `Incontnc2` int NOT NULL,
  `Collect` int NOT NULL,
  `Condcath` int NOT NULL,
  `Diaperpd` int NOT NULL,
  `Ostmybag` int NOT NULL,
  `Othcolap` int NOT NULL,
  `AnyDrugs` int NOT NULL,
  `AnyDrugs2` int NOT NULL,
  `Bladrelx` int NOT NULL,
  `Spncrelx` int NOT NULL,
  `DrugsAnti` int NOT NULL,
  `Antiuti` int NOT NULL,
  `Antiprop` int NOT NULL,
  `Othdrg` int NOT NULL,
  `Surgicalpr` int NOT NULL,
  `Spcath` varchar(255) NOT NULL,
  `SpcathDt` datetime DEFAULT NULL,
  `SpcathDtUnknown` varchar(255) NOT NULL,
  `Bstnrm` varchar(255) NOT NULL,
  `BstnrmDt` datetime DEFAULT NULL,
  `BstnrmDtUnknown` varchar(255) NOT NULL,
  `Ustnrm` varchar(255) NOT NULL,
  `UstnrmDt` datetime DEFAULT NULL,
  `UstnrmDtUnknown` varchar(255) NOT NULL,
  `Bladag` varchar(255) NOT NULL,
  `BladagDt` datetime DEFAULT NULL,
  `BladagDtUnknown` varchar(255) NOT NULL,
  `Ustent` varchar(255) NOT NULL,
  `UstentDt` datetime DEFAULT NULL,
  `UstentDtUnknown` varchar(255) NOT NULL,
  `Botox` varchar(255) NOT NULL,
  `BotoxDt` datetime DEFAULT NULL,
  `BotoxDtUnknown` varchar(255) NOT NULL,
  `Artsph` varchar(255) NOT NULL,
  `ArtsphDt` datetime DEFAULT NULL,
  `ArtsphDtUnknown` varchar(255) NOT NULL,
  `Ilvscs` varchar(255) NOT NULL,
  `IlvscsDt` datetime DEFAULT NULL,
  `IlvscsDtUnknown` varchar(255) NOT NULL,
  `Ilurts` varchar(255) NOT NULL,
  `IlurtsDt` datetime DEFAULT NULL,
  `IlurtsDtUnknown` varchar(255) NOT NULL,
  `Ccathv` varchar(255) NOT NULL,
  `CcathvDt` datetime DEFAULT NULL,
  `CcathvDtUnknown` varchar(255) NOT NULL,
  `Sarstm` varchar(255) NOT NULL,
  `SarstmDt` datetime DEFAULT NULL,
  `SarstmDtUnknown` varchar(255) NOT NULL,
  `Othsrg` varchar(255) NOT NULL,
  `OthsrgDt` datetime DEFAULT NULL,
  `OthsrgDtUnknown` varchar(255) NOT NULL,
  `Ursxchly` int NOT NULL,
  `ParentCNum` int NOT NULL,
  `ProceedingID` varchar(255) DEFAULT NULL,
  `HovedskjemaGUID` varchar(255) NOT NULL,
  `FormTypeId` int NOT NULL,
  `UnitId` int NOT NULL,
  `RHF` varchar(255) DEFAULT NULL,
  `HF` varchar(255) DEFAULT NULL,
  `Hospital` varchar(255) DEFAULT NULL,
  `HealthUnitName` varchar(255) DEFAULT NULL,
  `HealthUnitShortName` varchar(255) DEFAULT NULL,
  `HealthUnitId` int DEFAULT NULL,
  `CreationDate` datetime DEFAULT NULL,
  `LastUpdate` datetime DEFAULT NULL,
  `FormStatus` varchar(255) NOT NULL,
  `PatientAge` int DEFAULT NULL,
  `PatientGender` varchar(255) NOT NULL,
  `MunicipalNumber` int DEFAULT NULL,
  `CurrentMunicipalNumber` int DEFAULT NULL,
  `Municipal` varchar(255) DEFAULT NULL,
  `PostalCode` int DEFAULT NULL,
  `DistrictCode` varchar(255) DEFAULT NULL,
  `AddressQuality` int NOT NULL,
  `FirstTimeClosed` datetime DEFAULT NULL,
  `FormDate` datetime NOT NULL,
  `FormVersionNumber` int DEFAULT NULL,
  `PatientInRegistryGuid` varchar(255) DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_danish_ci

 CREATE ALGORITHM=UNDEFINED DEFINER=`dbo_nordicscir_rds`@`%` SQL SECURITY DEFINER VIEW `count_all_tables` AS select (select count(0) from `bowelfunctionformdatacontract`) AS `Bowl`,(select count(0) from `lifequalityformdatacontract`) AS `life`,(select count(0) from `mainformdatacontract`) AS `main`,(select count(0) from `urinarytractfunctionformdatacontract`) AS `urin`;

CREATE DATABASE IF NOT EXISTS db_log
  CHARACTER SET = 'utf8'
  COLLATE = 'utf8_danish_ci';

USE db_log;

CREATE TABLE IF NOT EXISTS `appLog` (
  `id` int(9) unsigned NOT NULL AUTO_INCREMENT,
  `time` datetime DEFAULT NULL,
  `user` varchar(127) COLLATE utf8_danish_ci DEFAULT NULL,
  `name` varchar(255) COLLATE utf8_danish_ci DEFAULT NULL,
  `group` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `role` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `resh_id` varchar(31) COLLATE utf8_danish_ci DEFAULT NULL,
  `message` varchar(2047) COLLATE utf8_danish_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;


CREATE TABLE IF NOT EXISTS `reportLog` (
  `id` int(9) unsigned NOT NULL AUTO_INCREMENT,
  `time` datetime DEFAULT NULL,
  `user` varchar(127) COLLATE utf8_danish_ci DEFAULT NULL,
  `name` varchar(255) COLLATE utf8_danish_ci DEFAULT NULL,
  `group` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `role` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `resh_id` varchar(31) COLLATE utf8_danish_ci DEFAULT NULL,
  `environment` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `call` varchar(2047) COLLATE utf8_danish_ci DEFAULT NULL,
  `message` varchar(2047) COLLATE utf8_danish_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;


CREATE DATABASE IF NOT EXISTS db_autoreport
  CHARACTER SET = 'utf8'
  COLLATE = 'utf8_danish_ci';

use db_autoreport;

CREATE TABLE IF NOT EXISTS `autoreport` (
  `id` varchar(32) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `synopsis` varchar(74) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `package` varchar(17) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `fun` varchar(28) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `params` text COLLATE utf8mb4_danish_ci,
  `owner` varchar(14) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `email` varchar(48) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `organization` varchar(22) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `terminateDate` varchar(10) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `interval` varchar(7) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `intervalName` varchar(11) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `runDayOfYear` text COLLATE utf8mb4_danish_ci,
  `type` varchar(12) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `ownerName` varchar(26) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `startDate` varchar(10) COLLATE utf8mb4_danish_ci DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3 COLLATE=utf8mb4_danish_ci;
