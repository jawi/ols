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
package nl.lxtreme.ols.client.signaldisplay.dnd;


import java.awt.datatransfer.*;

import nl.lxtreme.ols.client.signaldisplay.signalelement.*;


/**
 * Denotes a transferable channel, used to move channels around.
 * 
 * @author jawi
 */
public class ChannelTransferable implements Transferable
{
  // CONSTANTS

  /**
   * Represents the {@link DataFlavor} for this transferable, used to
   * distinguish between the various DnD transferable.
   */
  public static final DataFlavor CHANNEL_FLAVOR = new DataFlavor( ChannelTransferable.class, "OLS%20Signal%20Element" );

  // VARIABLES

  private final IUIElement element;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ChannelTransferable} instance.
   * 
   * @param aElement
   *          the signal element to transfer, cannot be <code>null</code>.
   */
  public ChannelTransferable( final IUIElement aElement )
  {
    this.element = aElement;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public IUIElement getTransferData( final DataFlavor aFlavor ) throws UnsupportedFlavorException
  {
    if ( CHANNEL_FLAVOR.equals( aFlavor ) )
    {
      return this.element;
    }

    throw new UnsupportedFlavorException( aFlavor );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public DataFlavor[] getTransferDataFlavors()
  {
    return new DataFlavor[] { CHANNEL_FLAVOR };
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public boolean isDataFlavorSupported( final DataFlavor aFlavor )
  {
    return aFlavor.isMimeTypeEqual( CHANNEL_FLAVOR.getMimeType() );
  }
}
