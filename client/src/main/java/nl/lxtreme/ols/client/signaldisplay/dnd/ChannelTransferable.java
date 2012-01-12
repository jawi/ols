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

import nl.lxtreme.ols.client.signaldisplay.channel.*;


/**
 * Denotes a transferable channel, used to move channels around.
 * 
 * @author jawi
 */
public class ChannelTransferable implements Transferable
{
  // CONSTANTS

  public static final DataFlavor CHANNEL_FLAVOR;

  static
  {
    try
    {
      final String clazz = ChannelTransferable.class.getCanonicalName();

      CHANNEL_FLAVOR = new DataFlavor( "application/vnd.ols.x-channel;class=" + clazz, "OLS%20Channel", Thread
          .currentThread().getContextClassLoader() );
    }
    catch ( final ClassNotFoundException exception )
    {
      throw new RuntimeException( exception );
    }
  }

  // VARIABLES

  private final Channel channel;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ChannelTransferable} instance.
   * 
   * @param aChannel
   *          the channel to transfer, cannot be <code>null</code>.
   */
  public ChannelTransferable( final Channel aChannel )
  {
    this.channel = aChannel;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public Object getTransferData( final DataFlavor aFlavor ) throws UnsupportedFlavorException
  {
    if ( CHANNEL_FLAVOR.equals( aFlavor ) )
    {
      return this.channel;
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
