/*****************************************************************************************************************************************************************************************************************/
Copyright (c) 2010-2050, UCL. All rights reserved. This program and the accompanying materials are made available under the terms of the 3-Clause BSD License which accompanies this distribution, and is available at http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these results has received funding from the European Community`s Seventh Framework Programme (FP7/2007-2013) under the grant agreement  FP7-257574 FITTEST.
/*****************************************************************************************************************************************************************************************************************/

package eu.fittest.ucl.watt;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import eu.fittest.ucl.watt.api.WattFormData;
import eu.fittest.ucl.watt.utils.Utils;

public class Database {
	
	private static boolean INITIALIZED = false;
	
	private static String DB_NAME = "wattdb";
	private static String DB_WATT_USER = "watt";
	
	private final String dbUser;
	private final String dbPassword;
	
	private String dbHost;
	private String dbURL;
	
	
	public Database(String dbUser, String dbPassword) {
		this.dbUser = dbUser;
		this.dbPassword = dbPassword;
		this.dbHost = "localhost";
		this.dbURL = "jdbc:mysql://" + this.dbHost;
	}
	
	private Connection getDBConnection(String db) {
		if(!INITIALIZED) {
			INITIALIZED = true;
			boolean missingJDBC = false;
			try {
				Class.forName("com.mysql.jdbc.Driver");
			} catch (ClassNotFoundException e) {
				Utils.log(e.getMessage());
				missingJDBC = true;
			}
			if(missingJDBC)
				return null;
		}
		Connection con = null;
		try {
			String url = this.dbURL;
			if(db != null)
				url += "/" + db;
			con = DriverManager.getConnection(url, this.dbUser, this.dbPassword);
		} catch (SQLException e) {
			Utils.log(e.getMessage());
			con = null;
		} 
		return con;
	}
	
	private void releaseResources(Connection con, Statement stmt) {
		if(stmt != null) {
			try {
				if(!stmt.isClosed())
					stmt.close();
			} catch(SQLException e) {
				Utils.log(e.getMessage());
			}
		}
		if(con != null)
		{
			try {
				if(!con.isClosed())
					con.close();
			} catch (SQLException e) {
				Utils.log(e.getMessage());
			}
		}
	}
	
	private boolean createDatabase() {
		Connection con = this.getDBConnection(null);
		if(con == null) return false;
		Statement s = null;
		int numUpdated = 0;
		try {
			s = con.createStatement();
			s.executeUpdate("DROP DATABASE IF EXISTS " + DB_NAME);
			numUpdated = s.executeUpdate("CREATE DATABASE " + DB_NAME);
		} catch (SQLException e) {
			Utils.log(e.getMessage());
		} finally {
			releaseResources(con, s);
		}
		
		return numUpdated == 1;
	}
			
	public boolean runSQLSetupScript() {
		if(!this.createDatabase()) return false;
		if(!this.createDatabaseUser()) return false;
		if(!this.grantPrivileges()) return false;
		return true;
	}
	
	private boolean createDatabaseUser() {
		Connection con = this.getDBConnection(DB_NAME);
		if(con == null) return false;
		Statement s = null;
		int numUpdated = 0;
		try {
			s = con.createStatement();
			ResultSet r = s.executeQuery("SELECT EXISTS(SELECT 1 FROM mysql.user WHERE user = '" + DB_WATT_USER + "')");
			boolean userExists = false;
			while(r.next()) {
				if(r.getInt(1) == 1) {
					userExists = true;
				}
			}
			r.close();
			if(!userExists) {
				numUpdated = s.executeUpdate("CREATE USER '" + DB_WATT_USER + "'@" + this.dbHost + " IDENTIFIED BY  '" + DB_WATT_USER + "'");
			} else
				numUpdated = 1;
			
		} catch (SQLException e) {
			Utils.log(e.getMessage());
		} finally {
			releaseResources(con, s);
		}
		
		return numUpdated == 1;
	}
	
	private boolean grantPrivileges() {
		Connection con = this.getDBConnection(DB_NAME);
		if(con == null) return false;
		Statement s = null;
		boolean success = true;
		try {
			s = con.createStatement();
			s.executeUpdate("grant all on " + DB_NAME + ".* to '" + DB_WATT_USER + "'@" + this.dbHost);
			s.executeUpdate("grant FILE on *.* to '" + DB_WATT_USER + "'@" + this.dbHost);
		} catch (SQLException e) {
			Utils.log(e.getMessage());
			success = false;
		} finally {
			releaseResources(con, s);
		}
		return success;
	}


	
	public String getDbUser() {
		return dbUser;
	}

	public String getDbPassword() {
		return dbPassword;
	}
	public static String getDBName() {
		return DB_NAME;
	}
	
	public List<String> getSuccessfulRequestURL() {
		Connection con = this.getDBConnection(DB_NAME);
		if(con == null) return null;
		Statement s = null;
		List<String> urls = new ArrayList<String>();
		try {
			s = con.createStatement();
			ResultSet r = s.executeQuery("select url from requests where responsecode = 200");
			while(r.next()) {
				urls.add(r.getString(1));
			}
			r.close();
		} catch (SQLException e) {
			Utils.log(e.getMessage());
		} finally {
			releaseResources(con, s);
		}
		return urls;
	}
	
	public List<String> getUnsuccessfulRequestURL() {
		Connection con = this.getDBConnection(DB_NAME);
		if(con == null) return null;
		Statement s = null;
		List<String> urls = new ArrayList<String>();
		try {
			s = con.createStatement();
			ResultSet r = s.executeQuery("select url from requests where responsecode <> 200");
			while(r.next()) {
				urls.add(r.getString(1));
			}
			r.close();
		} catch (SQLException e) {
			Utils.log(e.getMessage());
		} finally {
			releaseResources(con, s);
		}
		return urls;
	}
	
	public List<Hashtable<String, String>> getDistinctCrawledURLs() {
		Connection con = this.getDBConnection(DB_NAME);
		if(con == null) return null;
		Statement s = null;
		List<Hashtable<String, String>> urls = new ArrayList<Hashtable<String, String>>();
		try {
			s = con.createStatement();
			ResultSet r = s.executeQuery("select distinctrow url, linktype from requests");
			while(r.next()) {
				Hashtable<String, String> ht = new Hashtable<String, String>();
				ht.put("url", r.getString(1));
				ht.put("linktype", r.getString(2));
				urls.add(ht);
			}
			r.close();
		} catch (SQLException e) {
			Utils.log(e.getMessage());
		} finally {
			releaseResources(con, s);
		}
		return urls;
	}
	public List<WattFormData> getAllSubmittedFormData() {
		Connection con = this.getDBConnection(DB_NAME);
		if(con == null) return null;
		Statement s = null;
		List<WattFormData> data = new ArrayList<WattFormData>();
		try {
			s = con.createStatement();
			ResultSet r = s.executeQuery("select f.formname,i.inputname,i.type,i.defval from forms f ,forminputs i where i.formclass=f.formclass union select f.formname,s.selectname,'SELECT',s.optionval from forms f,formselects s where f.formclass=s.formclass");
			while(r.next()) {
				WattFormData wf = new WattFormData();
				wf.setFormName(r.getString(1));
				wf.setInputName(r.getString(2));
				wf.setInputType(r.getString(3));
				wf.setInputValue(r.getString(4));
				data.add(wf);
			}
			r.close();
		} catch (SQLException e) {
			Utils.log(e.getMessage());
		} finally {
			releaseResources(con, s);
		}
		return data;
	}
}
