/**************************************************************************************
*  Copyright (c) 2013, Universitat Politecnica de Valencia. All rights reserved.      *
*  This program and the accompanying materials are made available under the terms     *
*  of the 3-Clause BSD License which accompanies this distribution, and is available  *
*  at http://www.opensource.org/licenses/BSD-3-Clause. The research leading to these  *
*  results has received funding from the European Community`s Seventh Framework       *
*  Programme (FP7/2007-2013) under the grant agreement  FP7-257574 FITTEST.           *
**************************************************************************************/

/**
 *  @author Sebastian Bauersfeld
 */
package org.fruit.alayer.actions;

import java.io.Serializable;
import java.util.Set;
import org.fruit.alayer.ActionBuildException;
import org.fruit.alayer.Action;
import org.fruit.alayer.State;

public interface ActionBuilder extends Serializable{
    Set<Action> apply(State state) throws ActionBuildException;
}
