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
 * Copyright (C) 2010-2012 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.ui.editor;


import javax.swing.*;

import org.osgi.service.metatype.*;


/**
 * Provides a callback for unmanaged components to allow custom editors to be
 * supplied.
 */
public interface IEditorProvider
{
  // METHODS

  /**
   * Creates an editor component for the given attribute definition using the
   * given initial value.
   * 
   * @param aAttributeDefinition
   *          the attribute definition to use, cannot be <code>null</code>;
   * @param aInitialValue
   *          the initial value to use, can be <code>null</code>.
   * @return an editor component, may be <code>null</code> in which case the
   *         entire editor will be ignored.
   */
  JComponent createEditor( AttributeDefinition aAttributeDefinition, Object aInitialValue );

}
