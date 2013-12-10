/*

Copyright (c) 2013, FBK - Fondazione Bruno Kessler http://www.fbk.eu
All rights reserved. 

This program and the accompanying materials are made available under the terms of
the 3-Clause BSD License which accompanies this distribution, and is available at
http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these
results has received funding from the European Community`s Seventh Framework
Programme (FP7/2007-2013) under the grant agreement FP7-257574 FITTEST.

*/
package eu.fittest.modelInference.fsmInference.utility;

import java.util.Vector;

/**
 * 
 * @author Alessandro Marchetto
 * 
 */
public class PermutationSelection {

	Utility utils = new Utility();

	public static void main(String[] args) {
		PermutationSelection p = new PermutationSelection();
		p.exec();
	}

	void exec() {
		int[] tmp;
		Vector<int[]> v = getPermutations(-1, 2, 7);
		for (int i2 = 0; i2 < v.size(); i2++) {
			tmp = v.get(i2);
			print(tmp);
		}
	}

	public Vector<int[]> getPermutations(int min, int numElements,
			int maxNumPermutationsRequired) {

		int[] v = new int[numElements];
		int[] v2 = new int[numElements];
		for (int i = 0; i < v.length; i++) {
			v[i] = i;
		}

		Vector<int[]> vlist = new Vector<int[]>();
		vlist.add(v);
		int i = 0;
		int maxI = 50000;

		while ((i < maxI) && (vlist.size() < maxNumPermutationsRequired)) {

			v2 = newPermutation(min, v, v.length);

			if (!exists(vlist, v2)) {
				vlist.add(v2);
			}

			i++;
		}

		return vlist;
	}

	void print(int[] v2) {
		System.out.print(" ");
		for (int j = 0; j < v2.length; j++) {
			System.out.print(v2[j]);
		}
	}

	public int[] newPermutation(int min, int[] v, int vlength) {
		int[] v2 = new int[v.length];
		for (int j = 0; j < v2.length; j++) {
			v2[j] = v[j];
		}

		int n1 = 0;
		int n2 = 0;
		int i = 0;
		int maxI = 1;
		maxI = utils.randomInt(vlength, true);
		int tmp = 0;

		while (i < maxI) {
			if (min == -1) {
				n1 = utils.randomInt(vlength, false);
				n2 = utils.randomInt(vlength, false);
			} else {
				if (i > (maxI / 2)) {
					n1 = utils.randomInt(min, vlength, false);
					n2 = utils.randomInt(min, vlength, false);
				} else {
					n1 = utils.randomInt(min, false);
					n2 = utils.randomInt(min, false);
				}
			}

			i++;
			tmp = v2[n1];
			v2[n1] = v2[n2];
			v2[n2] = tmp;
		}

		return v2;
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
}
