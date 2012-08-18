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
package nl.lxtreme.ols.tool.serialdebug;


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.List;
import java.util.concurrent.*;

import javax.swing.*;
import javax.swing.text.*;

import nl.lxtreme.ols.util.*;


/**
 * Provides a custom {@link JTextPane} representing a console. Code is based on
 * the Console plugin of jEdit.
 * <p>
 * Copyright (C) 2004 Slava Pestov<br/>
 * Copyright (C) 2012 Jan Willem Janssen
 * </p>
 */
public class ConsolePane extends JTextPane
{
  // CONSTANTS

  /**
   * Provides a document filter that disallows any editing before the current
   * start of input.
   */
  final class AppendOnlyDocumentFilter extends DocumentFilter
  {
    // METHODS

    @Override
    public void insertString( final FilterBypass aFb, final int aOffset, final String aString, final AttributeSet aAttr )
        throws BadLocationException
    {
      final Document doc = aFb.getDocument();

      aFb.insertString( aOffset, aString, aAttr );

      if ( ConsolePane.this.mux.isReadMode() )
      {
        sendData( doc, aOffset, aString.length() );
      }
    }

    @Override
    public void remove( final FilterBypass aFb, final int aOffset, final int aLength ) throws BadLocationException
    {
      if ( ConsolePane.this.mux.isReadMode() )
      {
        sendData( "\b" );
      }

      aFb.remove( aOffset, aLength );
    }

    @Override
    public void replace( final FilterBypass aFb, final int aOffset, final int aLength, final String aText,
        final AttributeSet aAttrs ) throws BadLocationException
    {
      final Document doc = aFb.getDocument();

      aFb.replace( aOffset, aLength, aText, aAttrs );

      if ( ConsolePane.this.mux.isReadMode() )
      {
        sendData( doc, aOffset, aText.length() );
      }
    }

    /**
     * @param aFilter
     * @param offset
     * @param length
     */
    private void sendData( final Document aDocument, final int offset, final int length )
    {
      try
      {
        sendData( aDocument.getText( offset, length ) );
      }
      catch ( BadLocationException exception )
      {
        // TODO Auto-generated catch block
        exception.printStackTrace();
      }
    }

    /**
     * @param aFilter
     * @param offset
     * @param length
     */
    private void sendData( final String aText )
    {
      try
      {
        if ( ConsolePane.this.outputStreamWorker != null )
        {
          ConsolePane.this.outputStreamWorker.write( aText );
        }
      }
      catch ( IOException exception )
      {
        // TODO Auto-generated catch block
        exception.printStackTrace();
      }
    }
  }

  /**
   * Handles backspace keys.
   */
  class BackspaceAction extends AbstractAction
  {
    private static final long serialVersionUID = 1L;

    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      int selStart = getSelectionStart();
      int selEnd = getSelectionEnd();
      int inputStart = getInputStart();

      if ( selStart != selEnd )
      {
        if ( selStart < inputStart )
        {
          getToolkit().beep();
        }
        else
        {
          replaceSelection( "" );
        }
      }
      else
      {
        final Document document = getDocument();

        int caret = getCaretPosition();
        if ( caret < inputStart )
        {
          // Wrap around to the end of the document...
          caret = document.getLength();
        }
        else if ( caret == inputStart )
        {
          getToolkit().beep();
          return;
        }

        try
        {
          document.remove( caret - 1, 1 );
        }
        catch ( final BadLocationException e )
        {
          e.printStackTrace();
        }
      }
    }
  }

  /**
   * 
   */
  static class DocumentMultiplexer
  {
    // VARIABLES

    private final Semaphore semaphore = new Semaphore( 1 );

    // METHODS

    public boolean isReadMode()
    {
      return this.semaphore.availablePermits() == 1;
    }

    public boolean isWriteMode()
    {
      return this.semaphore.availablePermits() < 1;
    }

    public void switchToReadMode()
    {
      this.semaphore.release();
    }

    public void switchToWriteMode()
    {
      this.semaphore.acquireUninterruptibly();
    }
  }

  /**
   * A dummy action.
   */
  static class DummyAction extends AbstractAction
  {
    private static final long serialVersionUID = 1L;

    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      // Nop
    }
  }

  /**
   * 
   */
  static class EchoBuffer
  {
    // VARIABLES

    private final StringBuilder sb = new StringBuilder();

    // METHODS

    public void append( final String aText )
    {
      synchronized ( this.sb )
      {
        this.sb.append( aText );
      }
    }

    public void clear()
    {
      synchronized ( this.sb )
      {
        this.sb.setLength( 0 );
      }
    }

    public boolean shouldEcho( final String aText )
    {
      synchronized ( this.sb )
      {
        if ( this.sb.indexOf( aText ) == 0 )
        {
          String remainder = this.sb.substring( aText.length() );
          this.sb.setLength( 0 );
          this.sb.append( remainder );
          return false;
        }
        return true;
      }
    }
  }

  /**
   * Provides an action to go to the beginning of the user input.
   */
  class HomeAction extends AbstractAction
  {
    private static final long serialVersionUID = 1L;

    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      setCaretPosition( getInputStart() );
    }
  }

  /**
   * Serial reader that asynchronously reads data from an inputstream and
   * displays it on a text area.
   */
  final class InputStreamWorker extends SwingWorker<Void, Integer>
  {
    // VARIABLES

    private final InputStream inputStream;

    // CONSTRUCTORS

    /**
     * Creates a new {@link InputStreamWorker} instance.
     */
    public InputStreamWorker( final InputStream aInputStream )
    {
      this.inputStream = aInputStream;
    }

    // METHODS

    @Override
    protected Void doInBackground() throws Exception
    {
      while ( !isCancelled() && !Thread.currentThread().isInterrupted() )
      {
        int i = this.inputStream.read();
        if ( i >= 0 )
        {
          publish( Integer.valueOf( i ) );
        }
      }
      return null;
    }

    @Override
    protected void process( final List<Integer> aReadChars )
    {
      if ( !ConsolePane.this.mux.isWriteMode() )
      {
        // Blocks until we can write...
        ConsolePane.this.mux.switchToWriteMode();
      }

      try
      {
        final Document doc = getDocument();

        for ( Integer i : aReadChars )
        {
          String c = convertToText( i );
          if ( !ConsolePane.this.echoBuffer.shouldEcho( c ) )
          {
            System.out.println( "S[" + c + "]" );
            continue;
          }

          AttributeSet attrs = getPlainTextAttributes();
          if ( c.startsWith( "<" ) && c.endsWith( ">" ) )
          {
            attrs = getEscapeAttributes();
          }

          try
          {
            int offset = doc.getLength();

            doc.insertString( offset, c, attrs );
          }
          catch ( BadLocationException exception )
          {
            throw new RuntimeException( exception );
          }

          setInputStart( doc.getLength() );
          setCaretPosition( doc.getLength() );
        }
      }
      finally
      {
        ConsolePane.this.mux.switchToReadMode();
      }
    }

    /**
     * Converts the given (byte, ASCII) value into a text representation.
     * 
     * @param aValue
     *          the value to convert, cannot be <code>null</code>.
     * @return a text representation of the given (ASCII) value.
     */
    private String convertToText( final Integer aValue )
    {
      String text;

      int value = aValue.intValue();
      if ( ( value >= 0 ) && ( value < ASCII_NAMES.length ) )
      {
        text = ASCII_NAMES[value];
      }
      else
      {
        text = String.format( "<%02d>", aValue );
      }
      return text;
    }

  }

  final class OutputStreamWorker implements Closeable
  {
    // VARIABLES

    private final OutputStream outputStream;

    // CONSTRUCTORS

    /**
     * Creates a new {@link OutputStreamWorker} instance.
     */
    public OutputStreamWorker( final OutputStream aOutputStream )
    {
      this.outputStream = aOutputStream;
    }

    // METHODS

    @Override
    public void close() throws IOException
    {
      try
      {
        this.outputStream.flush();
      }
      finally
      {
        this.outputStream.close();
      }
    }

    public void write( final String aData ) throws IOException
    {
      System.out.println( ">>> " + aData );

      ConsolePane.this.echoBuffer.append( aData );

      this.outputStream.write( aData.getBytes() );
      this.outputStream.flush();
    }
  }

  /**
   * Provides an action to select the current user input.
   */
  class SelectHomeAction extends AbstractAction
  {
    private static final long serialVersionUID = 1L;

    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      select( getInputStart(), getCaretPosition() );
    }
  }

  // CONSTANTS

  public static final Color ESCAPED_TEXT_COLOR = new Color( 0x00, 0x80, 0xFF );
  public static final Color PLAIN_TEXT_COLOR = new Color( 0xE6, 0xE6, 0xE6 );
  public static final Color STATUS_TEXT_COLOR = new Color( 0xFF, 0x80, 0x00 );
  public static final Color BACKGROUND_COLOR = new Color( 0x1E, 0x21, 0x26 );

  private static final long serialVersionUID = 1L;

  private static final String PROPERTY_INPUTSTART = "InputStart";

  private static final String[] ASCII_NAMES = { "<nul>", "<soh>", "<stx>", "<etx>", "<eot>", "<enq>", "<ack>",
      "<bell>", "\b", "\t", "\n", "<vt>", "<np>", "\r", "<so>", "<si>", "<dle>", "<dc1>", "<dc2>", "<dc3>", "<dc4>",
      "<nak>", "<syn>", "<etb>", "<can>", "<em>", "<sub>", "<esc>", "<fs>", "<gs>", "<rs>", "<us>", " ", "!", "\"",
      "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8",
      "9", ":", ";", "<", "=", ">", "?", "@", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N",
      "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[", "\\", "]", "^", "_", "`", "a", "b", "c", "d",
      "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
      "{", "|", "}", "~", "<del>" };

  // VARIABLES

  private final DocumentFilter documentFilter;
  private final DocumentMultiplexer mux;
  private final EchoBuffer echoBuffer;

  private volatile InputStreamWorker inputStreamWorker;
  private volatile OutputStreamWorker outputStreamWorker;

  // CONSTRUCTORS

  /**
   * Creates a new {@link ConsolePane} instance.
   */
  public ConsolePane()
  {
    this.documentFilter = new AppendOnlyDocumentFilter();
    this.mux = new DocumentMultiplexer();
    this.echoBuffer = new EchoBuffer();

    final InputMap inputMap = getInputMap();
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_BACK_SPACE, 0 ), new BackspaceAction() );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_HOME, 0 ), new HomeAction() );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_HOME, InputEvent.SHIFT_MASK ), new SelectHomeAction() );

    setDocument( getDocument() );
    setInputStart( 0 );
  }

  // METHODS

  /**
   * @param aDocument
   * @return
   */
  static final int getInputStart( final Document aDocument )
  {
    final Integer i = ( Integer )aDocument.getProperty( PROPERTY_INPUTSTART );
    return ( i == null ) ? 0 : i.intValue();
  }

  /**
   * @param aDocument
   * @param aPosition
   */
  static final void setInputStart( final Document aDocument, final int aPosition )
  {
    aDocument.putProperty( PROPERTY_INPUTSTART, new Integer( aPosition ) );
  }

  /**
   * Appends the given status text to the current document.
   * 
   * @param aText
   *          the status text to append, cannot be <code>null</code>.
   */
  public void appendStatusText( final String aText )
  {
    if ( !this.mux.isWriteMode() )
    {
      this.mux.switchToWriteMode();
    }

    try
    {
      final Document doc = getDocument();
      doc.insertString( doc.getLength(), "\n".concat( aText ).concat( "\n" ), getStatusTextAttributes() );

      setCharacterAttributes( getPlainTextAttributes(), true /* replace */);

      setInputStart( getDocument().getLength() );
    }
    catch ( BadLocationException exception )
    {
      throw new RuntimeException( exception );
    }
    finally
    {
      this.mux.switchToReadMode();
    }
  }

  /**
   * Connects this console pane to the given input and output stream.
   * 
   * @param aInputStream
   * @param aOutputStream
   */
  public final void connect( final InputStream aInputStream, final OutputStream aOutputStream )
  {
    disconnect();

    this.inputStreamWorker = new InputStreamWorker( aInputStream );
    this.outputStreamWorker = new OutputStreamWorker( aOutputStream );

    this.inputStreamWorker.execute();
    setEditable( true );
  }

  /**
   * Disconnects this console pane.
   */
  public final void disconnect()
  {
    if ( this.inputStreamWorker != null )
    {
      this.inputStreamWorker.cancel( true /* mayInterruptIfRunning */);
      this.inputStreamWorker = null;
    }
    if ( this.outputStreamWorker != null )
    {
      HostUtils.closeResource( this.outputStreamWorker );
      this.outputStreamWorker = null;
    }

    setEditable( false );
  }

  @Override
  public void setDocument( final Document aDocument )
  {
    Document oldDocument = getDocument();
    if ( ( oldDocument != null ) && ( aDocument instanceof AbstractDocument ) )
    {
      ( ( AbstractDocument )oldDocument ).setDocumentFilter( null );
    }

    if ( aDocument instanceof AbstractDocument )
    {
      ( ( AbstractDocument )aDocument ).setDocumentFilter( this.documentFilter );
    }

    super.setDocument( aDocument );
  }

  /**
   * @return
   */
  final int getInputStart()
  {
    return ConsolePane.getInputStart( getDocument() );
  }

  /**
   * @param aPosition
   */
  final void setInputStart( final int aPosition )
  {
    ConsolePane.setInputStart( getDocument(), aPosition );
  }

  /**
   * @return a new attribute set, never <code>null</code>.
   */
  private SimpleAttributeSet createAttributeSet()
  {
    SimpleAttributeSet attrs = new SimpleAttributeSet();
    StyleConstants.setFontFamily( attrs, "Monospaced" );
    StyleConstants.setFontSize( attrs, 14 );
    return attrs;
  }

  /**
   * Returns the text attributes for escaped characters.
   * 
   * @return a attribute set, never <code>null</code>.
   */
  private AttributeSet getEscapeAttributes()
  {
    SimpleAttributeSet attrs = createAttributeSet();
    StyleConstants.setForeground( attrs, ESCAPED_TEXT_COLOR );
    return attrs;
  }

  /**
   * Returns the text attributes for plain text.
   * 
   * @return a attribute set, never <code>null</code>.
   */
  private AttributeSet getPlainTextAttributes()
  {
    SimpleAttributeSet attrs = createAttributeSet();
    StyleConstants.setForeground( attrs, PLAIN_TEXT_COLOR );
    return attrs;
  }

  /**
   * Returns the status text attributes for the given text.
   * 
   * @return a attribute set, never <code>null</code>.
   */
  private AttributeSet getStatusTextAttributes()
  {
    SimpleAttributeSet attrs = createAttributeSet();
    StyleConstants.setForeground( attrs, STATUS_TEXT_COLOR );
    StyleConstants.setBold( attrs, true );
    return attrs;
  }
}
