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
package nl.lxtreme.ols.device.logicsniffer.ui;


import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.util.swing.editor.*;

import org.osgi.service.metatype.*;


/**
 * 
 */
final class OcdHelper
{
  // METHODS

  /**
   * @param aOCD
   * @param aID
   * @param aInitialValues
   * @return
   */
  @SuppressWarnings( "unchecked" )
  public static <TYPE extends JComponent> TYPE createEditor( final ObjectClassDefinition aOCD, final String aID,
      final Map<Object, Object> aInitialValues )
  {
    EditorUtils editorUtils = new EditorUtils();

    AttributeDefinition ad = getAttributeDefinition( aOCD, aID );

    Object initialValue = editorUtils.getDefaultValue( ad, aInitialValues.get( aID ) );
    TYPE result = ( TYPE )editorUtils.createEditor( ad, initialValue );

    return result;
  }

  /**
   * @param aOCD
   * @param aID
   * @return
   */
  public static AttributeDefinition getAttributeDefinition( final ObjectClassDefinition aOCD, final String aID )
  {
    AttributeDefinition[] attributeDefinitions = aOCD.getAttributeDefinitions( ObjectClassDefinition.ALL );
    for ( AttributeDefinition attributeDef : attributeDefinitions )
    {
      if ( aID.equals( attributeDef.getID() ) )
      {
        return attributeDef;
      }
    }
    throw new IllegalArgumentException( "No such attribute: " + aID );
  }
}
