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
 * Provides information about the host the application is currently running on.
 */
public interface HostInfo
{
  // METHODS

  /**
   * @return the major java version, for example 7 for Java 7, 8 for Java 8, and
   *         so on.
   */
  int getJavaVersion();

  /**
   * @return <code>true</code> if the running host is (any form of) Linux,
   *         <code>false</code> otherwise.
   */
  boolean isLinux();

  /**
   * @return <code>true</code> if the running host is Mac OSX,
   *         <code>false</code> otherwise.
   */
  boolean isMacOS();

  /**
   * @return <code>true</code> if the running host is Solaris,
   *         <code>false</code> otherwise.
   */
  boolean isSolaris();

  /**
   * @return <code>true</code> if the running host is any form of Unix, that is,
   *         Linux, Solaris, and so on, <code>false</code> otherwise.
   */
  boolean isUnix();

  /**
   * @return <code>true</code> if the running host is Windows,
   *         <code>false</code> otherwise.
   */
  boolean isWindows();

  /**
   * @return <code>true</code> if an explicit about menu item needs to be added
   *         to a UI created on the running host, <code>false</code> if the
   *         running host already provides an about menu item by default.
   */
  boolean needsAboutMenuItem();

  /**
   * @return <code>true</code> if an explicit exit menu item needs to be added
   *         to a UI created on the running host, <code>false</code> if the
   *         running host already provides an exit menu item by default.
   */
  boolean needsExitMenuItem();

  /**
   * @return <code>true</code> if an explicit preferences menu item needs to be
   *         added to a UI created on the running host, <code>false</code> if
   *         the running host already provides a preferences menu item by
   *         default.
   */
  boolean needsPreferencesMenuItem();
}
