-- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Copyright (c) 2010-2050, Nadia Alshahwan (nadia.alshahwan@gmail.com). All rights reserved. This program and the accompanying materials are made available under the terms of the 3-Clause BSD License which accompanies this distribution, and is available at http://www.opensource.org/licenses/BSD-3-Clause. 
-- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


-- MySQL dump 10.11
--
-- Host: localhost    Database: watt
-- ------------------------------------------------------
-- Server version	5.0.51a

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `applications`
--

DROP TABLE IF EXISTS `applications`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `applications` (
  `appid` int(10) unsigned NOT NULL auto_increment,
  `appurl` varchar(200) default NULL,
  `appname` varchar(20) default NULL,
  PRIMARY KEY  (`appid`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;


--
-- Table structure for table `coverage`
--

DROP TABLE IF EXISTS `coverage`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `coverage` (
  `appname` varchar(20) default NULL,
  `traceid` int(255) default NULL,
  `requestid` int(255) default NULL,
  `filename` varchar(100) default NULL,
  `linenum` int(255) default NULL,
  KEY `traceid` (`traceid`,`requestid`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `formdata`
--

DROP TABLE IF EXISTS `formdata`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `formdata` (
  `appname` varchar(20) default NULL,
  `traceid` int(255) default NULL,
  `requestid` int(255) default NULL,
  `field` varchar(20) default NULL,
  `value` varchar(50) default NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `forminputs`
--

DROP TABLE IF EXISTS `forminputs`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `forminputs` (
  `formclass` varchar(32) default NULL,
  `inputname` varchar(100) default NULL,
  `type` varchar(32) default NULL,
  `defval` varchar(30000) default NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `forms`
--

DROP TABLE IF EXISTS `forms`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `forms` (
  `appname` varchar(20) default NULL,
  `formclass` varchar(32) default NULL,
  `formname` varchar(100) default NULL,
  `login` varchar(1) default 'n',
  `formseq` varchar(1000) default NULL,
  `formclassid` int(255) default NULL,
  `sourcepage` int(255) default NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `formselects`
--

DROP TABLE IF EXISTS `formselects`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `formselects` (
  `formclass` varchar(32) default NULL,
  `selectname` varchar(100) default NULL,
  `optionval` varchar(100) default NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `links`
--

DROP TABLE IF EXISTS `links`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `links` (
  `appname` varchar(20) default NULL,
  `url` varchar(200) default NULL,
  `sourcepage` int(255) default NULL,
  UNIQUE KEY `appname` (`appname`,`url`,`sourcepage`)
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `requests`
--

DROP TABLE IF EXISTS `requests`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `requests` (
  `appname` varchar(20) default NULL,
  `traceid` int(255) default NULL,
  `requestid` int(255) default NULL,
  `url` varchar(200) default NULL,
  `method` varchar(10) default NULL,
  `responsecode` int(255) default NULL,
  `linktype` varchar(20) default NULL,
  `filename` varchar(100) default NULL,
  `targetpage` int(255) default NULL,
  `sourcepage` int(255) default NULL,
  `seq` varchar(3000) default NULL,
  `linkclass` varchar(200) default NULL,
  KEY `linkclass` (`linkclass`),
  KEY `targetpage` (`targetpage`),
  KEY `seq` (`seq`(1000))
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `seqsim`
--

DROP TABLE IF EXISTS `seqsim`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `seqsim` (
  `seq1` varchar(3000) default NULL,
  `seq2` varchar(3000) default NULL,
  `lcss` varchar(3000) default NULL,
  `sim` double(5,4) default NULL,
  `appname` varchar(20) default NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;

--
-- Table structure for table `traces`
--

DROP TABLE IF EXISTS `traces`;
SET @saved_cs_client     = @@character_set_client;
SET character_set_client = utf8;
CREATE TABLE `traces` (
  `appname` varchar(20) default NULL,
  `traceid` int(255) default NULL,
  `requestid` int(255) default NULL
) ENGINE=MyISAM DEFAULT CHARSET=latin1;
SET character_set_client = @saved_cs_client;
