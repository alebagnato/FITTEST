/*****************************************************************************************************************************************************************************************************************/
Copyright (c) 2010-2050, Nadia Alshahwan (nadia.alshahwan@gmail.com). All rights reserved. This program and the accompanying materials are made available under the terms of the 3-Clause BSD License which accompanies this distribution, and is available at http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these results has received funding from the European Community`s Seventh Framework Programme (FP7/2007-2013) under the grant agreement  FP7-257574 FITTEST.
/*****************************************************************************************************************************************************************************************************************/

WATT is a web crawler that explores a web application collecting information about its structure. Starting from a user provided URL, the crawler follows links and submits forms in the application in a depth-first manner generating traces of the actions performed. 



While crawling WATT collects information about form structures, discovered links, executed requests, input data used and response codes to executed requests and saves this information to the database. 

The output (HTML page) of every request is also saved. 



WATT can be used to:

* discover broken links (response code is not 200)
* understand the application by looking at links and forms it contains as well as the sequence of requests and the visual output.
* Finding inconsistencies: For example, links that should not be available to a certain type of user or illegal input values that are accepted by the application indicating missing validations.
* Examining the HTML output of executing requests to find functional errors.

 
  


---------------------------------------------------
Installation:
---------------------------------------------------
Database:

- Create a database called watt:

create database watt;

- Create a user watt:

Create user 'watt'@localhost identified by 'watt';

- Run script watt.sql to create tables.

- Grant all privileges on watt to use watt:

grant all on watt.* to 'watt'@localhost;
grant FILE on *.* to 'watt'@localhost;


Crawler:

Move watt folder to server in root directory.

-install all needed perl modules:

sudo cpan LWP::Simple
sudo cpan HTML::Parser
sudo cpan HTML::FormatText (installation might fail because Module::Build version needs to be upgraded sudo cpan Module::Build)
sudo cpan Data::Random
sudo cpan Math::Round
sudo cpan Algorithm::LCSS
sudo cpan Time::HiRes
sudo cpan LWP::Protocol::https

-create a backup file of the application's database and place it in the watt folder:
sudo mysqldump yourdatabase >/watt/yourdatabase.sql



To collect coverage data (optional):

-Install xdebug

sudo apt-get install php5-dev php-pear
sudo pecl install xdebug

-place pre.php in a folder on the web server 

-add following line to php.ini and restart the server:
zend_extension=/path/to/xdebug.so

-change php.ini to include the following line:
auto_prepend_file=/path/to/pre.php

-restart the server

-create file coverage.txt in the watt folder and make sure it has write permission
 
---------------------------------------------------------------------
 Running WATT
---------------------------------------------------------------------

-standard command

sudo perl crawler.pl -app=appname -login -user=username -password=password -url=http://url -time=time

-app=appname
This is to tell the crawler the application name. Has to be the same name of the database reset file. Example
database reset file : yourdatabase.sql
-app=yourdatabase

-login -user=username -password=password

this tells the crawler the username/password to login to the application. if those 3 options are removed the crawler will use random values

-url=http://url

This is the starting page of crawling


-time=time
This tells the crawler how long to crawl for in seconds
example -time=600 for 10 minutes

other options:
-input=[random,default,empty]

by default the crawler uses random when filling forms: generate random strings for text boxes and select random option for drop-down menus etc

-folder=foldername
This tells the crawler where to save output pages. Default is /watt/downloads/

-----------------------------------------------------------------------
Examining Results
-----------------------------------------------------------------------
-All output files from requests are saved to the output folder (/watt/downloads)
The file name is the appname-traceid-requestid.html.

-all crawling data: urls called, forms found and their structure, input data used is saved to the watt database:
The output is a number of traces each containing a number of requests that where executed as part of the trace. 

Tables of interest:

requests: contains all links that were called during crawling

formdata: contains the input values that were used for every link of type form

forms, forminputs and formselects: contains the structure of forms discovered during crawling.

links: contains all links discovered during crawling even if they were not selected to be called.
coverage: contains the filenames and line numbers of code that was covered during crawling. 

some useful queries:

* check if any request was unsuccessful:
select traceid,requestid,url,responsecode from requests where responsecode<>200;

* check all distinct urls called and their type (link or form):
select distinctrow url, linktype from requests;

*check all requests in a trace
select requestid,url,linktype from requests where traceid=1;

* check input values used for a certain request:
select field,value from formdata where traceid=1 and requestid=10;

* check all forms discovered in the application:
select f.formname,i.inputname,i.type,i.defval from forms f ,forminputs i where i.formclass=f.formclass union select f.formname,s.selectname,'SELECT',s.optionval from forms f,formselects s where f.formclass=s.formclass order by 1;    

*check how many lines where covered
select count(distinctrow filename,linenum) from coverage where filename like '%yourdatabase%';






