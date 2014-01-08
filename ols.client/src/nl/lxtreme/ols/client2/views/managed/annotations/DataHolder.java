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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.views.managed.annotations;


import javax.swing.table.*;


/**
 * Data holder for the {@link AnnotationTableModel}.
 */
class DataHolder
{
  // VARIABLES

  final Object[][] data;
  final TableColumn[] columns;

  // CONSTRUCTORS

  /**
   * Creates a new {@link DataHolder} instance.
   */
  public DataHolder( final Object[][] aData, final TableColumn[] aColumns )
  {
    this.data = aData;
    this.columns = aColumns;
  }

  /**
   * Creates a new {@link DataHolder} instance.
   */
  public DataHolder( final TableColumn[] aColumns )
  {
    this.data = new Object[0][0];
    this.columns = aColumns;
  }
}
