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
package nl.lxtreme.ols.tool.base;


import junit.framework.*;
import nl.lxtreme.ols.tool.base.export.*;
import nl.lxtreme.ols.tool.base.export.ExportUtils.HtmlExporter;
import nl.lxtreme.ols.tool.base.export.ExportUtils.HtmlExporter.Element;
import nl.lxtreme.ols.tool.base.export.ExportUtils.HtmlExporter.MacroResolver;


/**
 * @author jawi
 */
public class ExportUtilsTest extends TestCase
{
  // INNER TYPES

  static final class UppercaseMacroResolver implements MacroResolver
  {
    // METHODS

    /**
     * @see nl.lxtreme.ols.util.ExportUtils.HtmlExporter.MacroResolver#resolve(java.lang.String,
     *      nl.lxtreme.ols.util.ExportUtils.HtmlExporter.Element)
     */
    @Override
    public Object resolve( final String aMacro, final Element aParent )
    {
      return aMacro.toUpperCase();
    }
  }

  // VARIABLES

  private HtmlExporter exporter;

  // METHODS

  /**
   * @
   */
  public void testAddStyle()
  {
    this.exporter.addCssStyle( "font-family: sans-serif;" );

    assertEquals(
        "<html><head><title></title><style type='text/css'>font-family: sans-serif;</style></head><body></body></html>",
        this.exporter.toString() );
  }

  /**
   * 
   */
  public void testAttribute()
  {
    this.exporter.getBody().addChild( HtmlExporter.DIV ).addAttribute( "style", "visible:none;" ).addContent( "test" );

    assertEquals( "<html><head><title></title></head><body><div style='visible:none;'>test</div></body></html>",
        this.exporter.toString() );
  }

  /**
   * @
   */
  public void testHtmlDoc()
  {
    final HtmlExporter exporter = ExportUtils.createHtmlExporter();

    exporter
        .addCssStyle( "th { text-align:left;font-style:italic;font-weight:bold;font-size:medium;background-color:#C0C0FF; }" );

    final Element body = exporter.getBody();
    body.addChild( HtmlExporter.H2 ).addContent( "I2C Analysis results" );
    body.addChild( HtmlExporter.HR );
    body.addChild( HtmlExporter.DIV ).addAttribute( "style", "text-align: right; font-size: x-small;" )
        .addContent( "today" );
    body.addChild( HtmlExporter.P );

    final Element table = body.addChild( "table" ).addAttribute( "style", "width: 100%" );
    Element tr;

    tr = table.addChild( "tr" );
    tr.addChild( "td" ).addAttribute( "colspan", "2" ).addContent( "Bus configuration" );
    tr = table.addChild( "tr" );
    tr.addChild( "td" ).addAttribute( "style", "width:30%" ).addContent( "SDA" );
    tr.addChild( "td" ).addContent( "&lt;auto detect&gt;" );
    tr = table.addChild( "tr" );
    tr.addChild( "td" ).addAttribute( "style", "width:30%" ).addContent( "SCL" );
    tr.addChild( "td" ).addContent( "&lt;auto detect&gt;" );

    body.addChild( HtmlExporter.P ).addContent( "&nbsp;" );

    assertEquals(
        "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n"
            + "<html><head><title></title><style type='text/css'>th { text-align:left;font-style:italic;font-weight:bold;font-size:medium;background-color:#C0C0FF; }</style></head><body><h2>I2C Analysis results</h2><hr><div style='text-align: right; font-size: x-small;'>today</div><p></p><table style='width: 100%'><tr><td colspan='2'>Bus configuration</td></tr><tr><td style='width:30%'>SDA</td><td>&lt;auto detect&gt;</td></tr><tr><td style='width:30%'>SCL</td><td>&lt;auto detect&gt;</td></tr></table><p>&nbsp;</p></body></html>",
        exporter.toString() );
  }

  /**
   * @
   */
  public void testMultipleContent()
  {
    Element cur = this.exporter.getBody();
    final Element h1 = cur.addChild( HtmlExporter.H1 );
    h1.addContent( "test" );
    h1.addChild( HtmlExporter.DIV ).addContent( "foo" );
    h1.addContent( "string" );

    assertEquals( "<html><head><title></title></head><body><h1>test<div>foo</div>string</h1></body></html>",
        this.exporter.toString() );
  }

  /**
   * 
   */
  public void testNestedSiblingsOk()
  {
    Element cur = this.exporter.getBody();
    cur.addChild( HtmlExporter.H1 ).addContent( "test" );
    cur.addChild( HtmlExporter.DIV ).addChild( HtmlExporter.H2 ).addContent( "bar" );
    cur.addChild( HtmlExporter.HR );

    assertEquals( "<html><head><title></title></head><body><h1>test</h1><div><h2>bar</h2></div><hr></body></html>",
        this.exporter.toString() );
  }

  /**
   * @
   */
  public void testResolveMacros()
  {
    Element cur = this.exporter.getBody();
    cur.addChild( HtmlExporter.H1 ).addContent( "{foo}" );
    cur.addChild( HtmlExporter.DIV ).addContent( "{bar}" );

    assertEquals( "<html><head><title></title></head><body><h1>FOO</h1><div>BAR</div></body></html>",
        this.exporter.toString( new UppercaseMacroResolver() ) );
  }

  /**
   * 
   */
  public void testResolveNonExistingMacros()
  {
    Element cur = this.exporter.getBody();
    cur.addChild( HtmlExporter.H1 ).addContent( "{foo}" );
    cur.addChild( HtmlExporter.DIV ).addContent( "{bar}" );

    assertEquals( "<html><head><title></title></head><body><h1></h1><div></div></body></html>",
        this.exporter.toString( new NullMacroResolver() ) );
  }

  /**
   * 
   */
  public void testSiblingsOk()
  {
    Element cur = this.exporter.getBody();
    cur.addChild( HtmlExporter.H1 ).addContent( "test" );
    cur.addChild( HtmlExporter.DIV ).addContent( "foo" );

    assertEquals( "<html><head><title></title></head><body><h1>test</h1><div>foo</div></body></html>",
        this.exporter.toString() );
  }

  /**
   * 
   */
  public void testSimpleH1()
  {
    this.exporter.getBody().addChild( HtmlExporter.H1 ).addContent( "test" );

    assertEquals( "<html><head><title></title></head><body><h1>test</h1></body></html>", this.exporter.toString() );
  }

  @Override
  protected void setUp()
  {
    this.exporter = new HtmlExporterImpl( false /* aIncludeDTD */);
  }
}
