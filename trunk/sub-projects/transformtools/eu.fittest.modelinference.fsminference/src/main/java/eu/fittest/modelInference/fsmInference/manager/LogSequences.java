/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.modelInference.fsmInference.manager;

import java.io.File;
import java.util.Vector;

import eu.fittest.modelInference.fsmInference.utility.*;

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class LogSequences {
	Utility utils = new Utility();

	PermutationSelection perms3 = new PermutationSelection();
	public int numberOfCalibrationFiles = 0;

	public void forceNewInstace() {
		instance = null;
		instance = new LogSequences();
	}

	private static LogSequences instance = null;

	private LogSequences() {
	}

	public static LogSequences getInstace() {
		if (instance == null) {
			instance = new LogSequences();
		}
		return instance;
	}

	public File[] setUp(String folderPath, int numberOfListsToGeneate,
			boolean subdir) {
		try {

			instance.folderPath = folderPath;
			instance.sequenceList = new Vector<File[]>();

			File[] listOfFilesT = null;
			File[] listOfFilesT1 = null;
			File[] listOfFilesT2 = null;

			if (subdir) {
				listOfFilesT1 = getFileList(folderPath);
				listOfFilesT2 = getFileList(folderPath
						+ System.getProperty("file.separator") + "xCalibration");
				numberOfCalibrationFiles = listOfFilesT2.length;
				listOfFilesT = new File[listOfFilesT1.length
						+ listOfFilesT2.length]; // -1

				for (int i = 0; i < listOfFilesT1.length; i++) {
					if (listOfFilesT1[i] != null)
						listOfFilesT[i] = listOfFilesT1[i];
				}
				for (int i = 0; i < listOfFilesT2.length; i++) {
					if (listOfFilesT2[i] != null)
						listOfFilesT[listOfFilesT1.length + i] = listOfFilesT2[i]; // listOfFilesT1.length-1+i
				}

				if (listOfFilesT != null) {
					instance.sequenceList.add(listOfFilesT);
					generateRandomLists2(-1, numberOfListsToGeneate,
							listOfFilesT.length);
				}

			} else {
				listOfFilesT1 = getFileList(folderPath);
				if (listOfFilesT1 != null) {
					instance.sequenceList.add(listOfFilesT1);
					generateRandomLists2(-1, numberOfListsToGeneate,
							listOfFilesT1.length);
				}

			}

			return listOfFilesT1;
		} catch (Exception e) {
			System.out.println("Problems in loading logs ");
			e.printStackTrace();
			return null;
		}
	}

	public Vector<File[]> getSequenceList() {
		return instance.sequenceList;
	}

	public Vector<File[]> sequenceList = null;

	String folderPath = "";

	/*
	 * It returns the list of files
	 */
	private File[] getFileList(String folderPath) {

		File folder = new File(folderPath);
		File[] listOfFiles = folder.listFiles();

		File[] listOfFilesOedered = new File[listOfFiles.length];
		String fname = "";
		int index = 0;

		for (int i = 0; i < listOfFiles.length; i++) {
			if (listOfFiles[i].isFile()) {
				fname = listOfFiles[i].getName();
				// if ((fname.startsWith("log_"))&&(fname.endsWith(".txt"))) {
				if (fname.startsWith("log_")) {
					listOfFilesOedered[index] = listOfFiles[i];
					index++;
				}
			}
		}

		Vector<File> tmpFiles = new Vector<File>();
		for (int i = 0; i < listOfFilesOedered.length; i++) {
			if (listOfFilesOedered[i] != null)
				tmpFiles.add(listOfFilesOedered[i]);
		}

		listOfFilesOedered = new File[tmpFiles.size()];
		for (int i = 0; i < listOfFilesOedered.length; i++) {
			listOfFilesOedered[i] = tmpFiles.get(i);
		}

		return listOfFilesOedered;
	}

	private void generateRandomLists2(int min, int maxNumberOfLists,
			int maxFilesPerList) {
		Vector<int[]> permutations = null;

		permutations = perms3.getPermutations(min, maxFilesPerList,
				maxNumberOfLists);

		Vector<Integer> selectedLists = new Vector<Integer>();

		int[] initialList = new int[maxFilesPerList];
		for (int i = 0; i < maxFilesPerList; i++) {
			initialList[i] = i;
		}

		Vector<int[]> sequenceListByInt = new Vector<int[]>();
		sequenceListByInt.add(initialList);

		int n = 0;
		int j = 0;
		boolean exit = false;
		int i = 0;
		while ((sequenceListByInt.size() < maxNumberOfLists)) {
			exit = false;
			while ((j < 5000000) && (exit == false)) {
				n = utils.randomInt(permutations.size(), false);
				if (selectedLists.contains(n)) {
				} else {
					selectedLists.add(n);
					sequenceListByInt.add(permutations.get(n));
					exit = true;
				}
				j++;
			}
			i++;
		}

		storeList(sequenceListByInt, maxNumberOfLists);

	}

	private void generateRandomLists(int maxNumberOfLists, int maxFilesPerList) {

		int[] listOfFilesByInt = new int[maxNumberOfLists];
		int i = 0;
		int j = 1;
		Vector<int[]> sequenceListByInt = new Vector<int[]>();

		while ((j < maxNumberOfLists) && (i < 5000000)) {

			listOfFilesByInt = generateOneRandomList(maxFilesPerList);

			if (exists(sequenceListByInt, listOfFilesByInt) == false) {
				System.out.println();
				for (int k = 0; k < listOfFilesByInt.length; k++) {
					System.out.print(listOfFilesByInt[k] + " ");
				}
				sequenceListByInt.add(listOfFilesByInt);
				j++;
			}

			i++;
		}

		storeList(sequenceListByInt, maxNumberOfLists);

	}

	private void storeList(Vector<int[]> sequenceListByInt, int maxNumberOfLists) {
		File[] referenceListOfFiles = null;
		int[] listOfFilesByInt = new int[maxNumberOfLists];
		File[] listOfFiles = null;

		if (sequenceListByInt.size() > 0) {
			referenceListOfFiles = instance.sequenceList.get(0);

			for (int k = 1; k < sequenceListByInt.size(); k++) {
				listOfFilesByInt = sequenceListByInt.get(k);
				listOfFiles = new File[referenceListOfFiles.length];
				for (int k2 = 0; k2 < listOfFilesByInt.length; k2++) {
					listOfFiles[k2] = referenceListOfFiles[listOfFilesByInt[k2]];
				}
				if (listOfFiles.length > 0)
					instance.sequenceList.add(listOfFiles);
			}

		}
	}

	public boolean exists(Vector<int[]> sequenceListByInt,
			int[] listOfFilesByInt) {
		int[] tmp;
		int j = 0;
		int i = 0;
		boolean exists2 = true;

		if (sequenceListByInt.size() == 0)
			return false;

		while (j < sequenceListByInt.size()) {
			tmp = sequenceListByInt.get(j);
			i = 0;
			exists2 = true;

			while ((i < tmp.length) && (exists2 == true)) {

				if (tmp[i] != listOfFilesByInt[i]) {

					exists2 = false;

				}
				i++;
			}
			if (exists2 == true) {
				return true;
			}
			j++;
		}

		return false;
	}

	protected int[] generateOneRandomList(int maxFilesPerList) {
		int[] numbers = new int[maxFilesPerList];
		int n = 0;
		int j = 0;
		int i = 0;

		while ((j < numbers.length) && (i < 5000000)) {
			n = utils.randomInt(maxFilesPerList, false);
			if (!exists(numbers, n)) {
				numbers[j] = n;
				j++;
			}
			i++;
		}

		return numbers;

	}

	protected boolean exists(int[] numbers, int tocheck) {

		for (int i = 0; i < numbers.length; i++) {
			if (numbers[i] == tocheck)
				return true;
		}

		return false;
	}

}
