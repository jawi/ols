/*
 * OpenBench LogicSniffer / SUMP project 
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA 02110, USA
 *
 * Copyright (C) 2010-2011 - J.W. Janssen, <http://www.lxtreme.nl>
 */
package nl.lxtreme.ols.client.signaldisplay;


import java.util.*;

import nl.lxtreme.ols.api.data.*;


/**
 * Allows implementors to listen for changes to the captured data.
 */
public interface IDataModelChangeListener extends EventListener
{
  // METHODS

  /**
   * Called when the acquisition is finished and the captured data is available.
   * 
   * @param aDataSet
   *          the changed data set, containing the captured data, never
   *          <code>null</code>.
   */
  void dataModelChanged( DataSet aDataSet );
}
