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
 * Copyright (C) 2006-2010 Michael Poppitz, www.sump.org
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.api.acquisition;


/**
 * Can be used to register a service that is interested in the progress status
 * of an acquisition.
 */
public interface AcquisitionProgressListener
{
  // METHODS

  /**
   * Called to periodically report the progress of the acquisition.
   * <p>
   * There is no guarantee made by the framework that this method will be
   * called. If called, it will always be called at least after the
   * {@link #acquisitionStarted(int, int, int)} and before
   * {@link #acquisitionEnded(AcquisitionResultStatus)}.
   * </p>
   * 
   * @param aPercentage
   *          a percentage, >= 0 && <= 100.
   */
  void acquisitionInProgress( int aPercentage );
}
