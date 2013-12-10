package eu.fittest.common.services.compression.impl;

import java.io.File;

import eu.fittest.common.core.exception.FITTESTException;

public class CompressionServiceImplTest {

	public static void main(String[] args) throws FITTESTException {
		CompressionServiceImpl serv = new CompressionServiceImpl();	
		File zip = new File(System.getProperty("java.io.tmpdir"), "test.zip");
		serv.zip(new File(".settings"), zip.getAbsolutePath());
		long crc = serv.checksum(zip);
		System.out.println("CRC= " +crc);
		File gzip = new File(System.getProperty("java.io.tmpdir"), "test.gzip");
		serv.gzip(zip, gzip.getAbsolutePath());
		
		File unzip = new File(System.getProperty("java.io.tmpdir"), "testun.zip");
		
		File unzipped = new File(System.getProperty("java.io.tmpdir"), "testunzip");
		serv.gunzip(gzip, unzip.getAbsolutePath());
		serv.unzip(unzip,unzipped.getAbsolutePath());
	}
}
