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


import java.awt.*;

import nl.lxtreme.ols.common.annotation.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.JLxTable.TableCellRendererAdapter;


/**
 * Provides a cell renderer adapter for rendering the background using the color
 * provided by data annotations.
 */
class AnnotationCellRenderer implements TableCellRendererAdapter
{
  // VARIABLES

  private Font font; // cache value

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public Component render( final Component aCellComponent, final Object aCellValue, final boolean aIsSelected )
  {
    if ( this.font == null )
    {
      Font f = aCellComponent.getFont();
      this.font = new Font( Font.MONOSPACED, f.getStyle(), f.getSize() );
    }
    aCellComponent.setFont( this.font );

    if ( aIsSelected )
    {
      return aCellComponent;
    }

    if ( aCellValue instanceof DataAnnotation )
    {
      Object val = ( ( DataAnnotation )aCellValue ).getProperties().get( DataAnnotation.KEY_COLOR );
      if ( val instanceof String )
      {
        aCellComponent.setBackground( ColorUtils.parseColor( ( String )val ) );
      }
      else if ( val instanceof Color )
      {
        aCellComponent.setBackground( ( Color )val );
      }
    }

    return aCellComponent;
  }
}
