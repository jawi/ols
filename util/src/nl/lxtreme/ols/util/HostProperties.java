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
package nl.lxtreme.ols.util;


/**
 * 
 */
public interface HostProperties
{

  /**
   * @return
   */
  String getExecutionEnvironment();

  /**
   * Returns the current value of fullName.
   * 
   * @return the fullName
   */
  String getFullName();

  /**
   * @return
   */
  String getOSName();

  /**
   * @return
   */
  String getOSVersion();

  /**
   * @return
   */
  String getProcessor();

  /**
   * Returns the email address to use for reporting incidents.
   * 
   * @return a report incident email address, never <code>null</code>.
   */
  String getReportIncidentAddress();

  /**
   * Returns the current value of shortName.
   * 
   * @return the shortName
   */
  String getShortName();

  /**
   * Returns the version of the client.
   * 
   * @return a version string, never <code>null</code>.
   */
  String getVersion();

  /**
   * Returns whether or not debugging is enabled.
   * <p>
   * Useful for additional checks, logging and so on.
   * </p>
   * 
   * @return <code>true</code> if debug mode is enabled, <code>false</code>
   *         otherwise.
   */
  boolean isDebugMode();

}
