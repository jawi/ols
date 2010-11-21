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
package nl.lxtreme.ols.export.svg;


import java.awt.*;
import java.io.*;
import java.util.*;

import javax.swing.*;
import javax.swing.border.*;

import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.api.data.export.*;

import org.apache.batik.dom.*;
import org.apache.batik.svggen.*;
import org.w3c.dom.*;


/**
 * @author jawi
 */
public class SVGExporter implements Exporter
{
  // METHODS

  /**
   * @see nl.lxtreme.ols.api.data.export.Exporter#export(nl.lxtreme.ols.api.data.DataContainer,
   *      javax.swing.JComponent, java.io.OutputStream)
   */
  @Override
  public void export( final DataContainer aContainer, final JComponent aComponent, final OutputStream aStream )
      throws IOException
  {
    // Get a DOMImplementation.
    DOMImplementation domImpl = GenericDOMImplementation.getDOMImplementation();

    // Create an instance of org.w3c.dom.Document.
    final String svgNS = "http://www.w3.org/2000/svg";
    Document document = domImpl.createDocument( svgNS, "svg", null );

    final SVGGeneratorContext ctx = SVGGeneratorContext.createDefault( document );
    ctx.setComment( "Generated on " + new Date() );

    // Create an instance of the SVG Generator.
    final SVGGraphics2D svgGenerator = new SVGGraphics2D( ctx, false /* textAsShapes */);
    // Make sure to set the correct canvas size, in order for viewers to get a
    // notion of the size of the display...
    svgGenerator.setSVGCanvasSize( getImageSize( aComponent ) );
    // "Paint" the diagram component to SVG...
    paintDiagram( svgGenerator, aComponent );

    Writer out = new OutputStreamWriter( aStream, "UTF-8" );
    try
    {
      svgGenerator.stream( out, true /* useCSS */);
    }
    finally
    {
      out.flush();
      out.close();
      out = null;
    }
  }

  /**
   * @see nl.lxtreme.ols.api.data.export.Exporter#getFilenameExtentions()
   */
  @Override
  public String[] getFilenameExtentions()
  {
    return new String[] { "svg" };
  }

  /**
   * @see nl.lxtreme.ols.api.data.export.Exporter#getName()
   */
  @Override
  public String getName()
  {
    return "SVG Image";
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

      final Dimension viewSize = scrollpane.getViewport().getViewSize();
      final Dimension rowHeaderSize = scrollpane.getRowHeader().getViewSize();
      final Dimension columnHeaderSize = scrollpane.getColumnHeader().getViewSize();

      final int width = viewSize.width + rowHeaderSize.width;
      final int height = viewSize.height + columnHeaderSize.height;

      return new Dimension( width, height );
    }

    return aDiagram.getPreferredSize();
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
    if ( aDiagram instanceof JScrollPane )
    {
      paintScrollPaneContents( aCanvas, ( JScrollPane )aDiagram );
    }
    else
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

  /**
   * Paints the contents of the given scrollpane on the given canvas, expanding
   * it to its full view (without scroll bars).
   * 
   * @param aCanvas
   *          the canvas to paint on, cannot be <code>null</code>;
   * @param aScrollPane
   *          the scroll pane to paint, cannot be <code>null</code>.
   */
  private void paintScrollPaneContents( final Graphics2D aCanvas, final JScrollPane aScrollPane )
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
