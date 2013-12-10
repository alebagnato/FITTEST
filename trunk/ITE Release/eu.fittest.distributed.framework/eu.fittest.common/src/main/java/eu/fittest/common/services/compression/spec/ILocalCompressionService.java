package eu.fittest.common.services.compression.spec;

import java.io.File;

import eu.fittest.common.core.exception.FITTESTException;
import eu.fittest.common.core.service.ILocalService;

public interface ILocalCompressionService extends ILocalService{
	File zipAndGzip(File from) throws FITTESTException;
	File gunzipAndUnzip(File from) throws FITTESTException;
	void zip(File from, String to) throws FITTESTException;
	void unzip(File from, String to) throws FITTESTException;
	void gzip(File from, String to) throws FITTESTException;
	void gunzip(File from, String to)  throws FITTESTException;
	long checksum(File f) throws FITTESTException;
	void isChecksumValid(File file, long checksum) throws FITTESTException;
}
