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
package org.fruit.alayer;

/**
 * A state describes an <code>SUT</code>'s state. It includes the currently
 * visible widgets which form a widget tree. Each of these widgets has
 * properties like "Title", "Enabled", "Shape", ... which can be queried.
 * However, states are not restricted to visual entities and can contain
 * various kinds of additional data.
 * 
 * A state forms a so called widget tree with the root node of that tree representing
 * the system itself. For example
 *
 */
public interface State extends Widget, Iterable<Widget>{ }
