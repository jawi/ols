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
package nl.lxtreme.ols.client.diagram.export;


import java.awt.*;
import java.awt.image.*;
import java.io.*;

import javax.imageio.*;
import javax.swing.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.export.*;
import nl.lxtreme.ols.util.ExportUtils.WriterOutputStream;


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

  // VARIABLES

  private final JComponent component;

  // CONSTRUCTORS

  /**
   * Creates a new ImageExporter instance.
   * 
   * @param aComponent
   *          the component to write to an image, cannot be <code>null</code>.
   */
  public ImageExporter( final JComponent aComponent )
  {
    this.component = aComponent;
  }

  // METHODS

  /**
   * @see nl.lxtreme.ols.api.data.export.Exporter#export(nl.lxtreme.ols.api.data.DataContainer,
   *      java.io.Writer)
   */
  @Override
  public void export( final DataContainer aContainer, final Writer aWriter ) throws IOException
  {
    final Dimension dims = getExportSize( this.component );

    final BufferedImage image = new BufferedImage( dims.width, dims.height, BufferedImage.TYPE_INT_RGB );

    // Create a graphics contents on the buffered image
    Graphics2D g2d = image.createGraphics();
    try
    {
      paintDiagram( image, g2d, this.component );
    }
    finally
    {
      g2d.dispose();
      g2d = null;
    }

    if ( !ImageIO.write( image, "png", new WriterOutputStream( aWriter ) ) )
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
    if ( aDiagram instanceof JScrollPane )
    {
      final JScrollPane scrollpane = ( JScrollPane )aDiagram;

      final Dimension viewSize = scrollpane.getViewport().getViewSize();
      final Dimension rowHeaderSize = scrollpane.getRowHeader().getViewSize();
      final Dimension columnHeaderSize = scrollpane.getColumnHeader().getViewSize();

      final int width = Math.min( viewSize.width + rowHeaderSize.width, MAX_WIDTH );
      final int height = Math.min( viewSize.height + columnHeaderSize.height, MAX_HEIGHT );

      return new Dimension( width, height );
    }

    return aDiagram.getPreferredSize();
  }

  /**
   * Paints the given component on the given canvas.
   * 
   * @param aRenderedImage
   * @param aCanvas
   *          the canvas to paint on, cannot be <code>null</code>;
   * @param aDiagram
   *          the component to paint, cannot be <code>null</code>.
   * @see #paintScrollPaneContents(Graphics2D, JScrollPane)
   */
  private void paintDiagram( final RenderedImage aRenderedImage, final Graphics2D aCanvas, final JComponent aDiagram )
  {
    if ( aDiagram instanceof JScrollPane )
    {
      paintScrollPaneContents( aRenderedImage, aCanvas, ( JScrollPane )aDiagram );
    }
    else
    {
      aDiagram.paint( aCanvas );
    }
  }

  /**
   * Paints the contents of the given scrollpane on the given canvas, expanding
   * it to its full view (without scroll bars).
   * 
   * @param aCanvas
   *          the canvas to paint on, cannot be <code>null</code>;
   * @param aScrollPane
   *          the scroll pane to paint, cannot be <code>null</code>.
   */
  private void paintScrollPaneContents( final RenderedImage aRenderedImage, final Graphics2D aCanvas,
      final JScrollPane aScrollPane )
  {
    int offsetX = 0;
    int offsetY = 0;

    final Component rowHeaderView = aScrollPane.getRowHeader().getView();
    final Component columnHeaderView = aScrollPane.getColumnHeader().getView();
    final Component cornerView = aScrollPane.getCorner( ScrollPaneConstants.UPPER_LEADING_CORNER );
    final Component contentView = aScrollPane.getViewport().getView();

    if ( rowHeaderView != null )
    {
      offsetX = rowHeaderView.getWidth();
    }
    if ( columnHeaderView != null )
    {
      offsetY = columnHeaderView.getHeight();
    }

    if ( cornerView != null )
    {
      cornerView.paint( aCanvas );
    }

    if ( columnHeaderView != null )
    {
      aCanvas.translate( offsetX, 0 );
      columnHeaderView.paint( aCanvas );
      aCanvas.translate( -offsetX, 0 );
    }

    if ( rowHeaderView != null )
    {
      aCanvas.translate( 0, offsetY );
      rowHeaderView.paint( aCanvas );
      aCanvas.translate( 0, -offsetY );
    }

    if ( contentView != null )
    {
      aCanvas.translate( offsetX, offsetY );
      contentView.paint( aCanvas );
      aCanvas.translate( -offsetX, -offsetY );
    }
  }
}
