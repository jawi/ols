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
 * 
 * Copyright (C) 2010-2011 - J.W. Janssen, http://www.lxtreme.nl
 */
package nl.lxtreme.ols.export.image;


import java.awt.*;
import java.awt.image.*;
import java.io.*;

import javax.imageio.*;
import javax.swing.*;
import javax.swing.border.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.export.*;


/**
 * Provides a simple export-to-image functionality.
 */
public class ImageExporter implements Exporter
{
  // CONSTANTS

  /** The maximum width of the image to export. */
  private static final int MAX_WIDTH = 8192;
  /** The maximum height of the image to export. */
  private static final int MAX_HEIGHT = 8192;

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void export( final DataSet aDataSet, final JComponent aComponent, final OutputStream aStream )
      throws IOException
  {
    final Dimension dims = getExportSize( aComponent );

    final BufferedImage image = new BufferedImage( dims.width, dims.height, BufferedImage.TYPE_INT_RGB );

    // Create a graphics contents on the buffered image
    Graphics2D g2d = image.createGraphics();
    try
    {
      paintDiagram( g2d, aComponent );
    }
    finally
    {
      g2d.dispose();
      g2d = null;
    }

    if ( !ImageIO.write( image, "png", aStream ) )
    {
      throw new IOException( "Export to PNG failed! Image not supported?" );
    }
  }

  /**
   * @see nl.lxtreme.ols.api.data.export.Exporter#getFilenameExtentions()
   */
  @Override
  public String[] getFilenameExtentions()
  {
    return new String[] { "png" };
  }

  /**
   * @see nl.lxtreme.ols.api.data.export.Exporter#getName()
   */
  @Override
  public String getName()
  {
    return "PNG Image";
  }

  /**
   * Returns the export image size for the given component.
   * 
   * @param aDiagram
   *          the component to get the export image size for, cannot be
   *          <code>null</code>.
   * @return a export image size, never <code>null</code>.
   */
  private Dimension getExportSize( final JComponent aDiagram )
  {
    Dimension dims = getImageSize( aDiagram );

    final int width = Math.min( dims.width, MAX_WIDTH );
    final int height = Math.min( dims.height, MAX_HEIGHT );

    return new Dimension( width, height );
  }

  /**
   * Returns the export image size for the given component.
   * 
   * @param aDiagram
   *          the component to get the export image size for, cannot be
   *          <code>null</code>.
   * @return a export image size, never <code>null</code>.
   */
  private Dimension getImageSize( final JComponent aDiagram )
  {
    if ( aDiagram instanceof JScrollPane )
    {
      final JScrollPane scrollpane = ( JScrollPane )aDiagram;

      final Dimension visibleViewSize = scrollpane.getViewport().getExtentSize();
      final Dimension viewSize = scrollpane.getViewport().getViewSize();
      final Dimension rowHeaderSize = scrollpane.getRowHeader().getExtentSize();
      final Dimension columnHeaderSize = scrollpane.getColumnHeader().getExtentSize();

      final int width = Math.min( viewSize.width, visibleViewSize.width ) + rowHeaderSize.width;
      final int height = Math.min( viewSize.height, visibleViewSize.height ) + columnHeaderSize.height;

      return new Dimension( width, height );
    }

    return aDiagram.getSize();
  }

  /**
   * Paints the given component on the given canvas.
   * 
   * @param aCanvas
   *          the canvas to paint on, cannot be <code>null</code>;
   * @param aDiagram
   *          the component to paint, cannot be <code>null</code>.
   * @see #paintScrollPaneContents(Graphics2D, JScrollPane)
   */
  private void paintDiagram( final Graphics2D aCanvas, final JComponent aDiagram )
  {
    final Border border = aDiagram.getBorder();
    if ( border != null )
    {
      final Insets insets = border.getBorderInsets( aDiagram );
      if ( insets != null )
      {
        aCanvas.translate( -insets.left, -insets.top );
      }
    }
    aDiagram.paint( aCanvas );
  }
}
