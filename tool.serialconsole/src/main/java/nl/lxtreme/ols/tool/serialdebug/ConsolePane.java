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

import nl.lxtreme.ols.tool.serialdebug.AnsiInterpreter.*;
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
      int inputStart = getInputStart();

      if ( ConsolePane.this.mux.isReadMode() )
      {
        sendData( aString );

        setInputStart( inputStart - aString.length() );
      }
      else
      {
        aFb.insertString( aOffset, aString, aAttr );

        setInputStart( inputStart + aString.length() );
      }
    }

    @Override
    public void remove( final FilterBypass aFb, final int aOffset, final int aLength ) throws BadLocationException
    {
      int inputStart = getInputStart();

      if ( ConsolePane.this.mux.isReadMode() )
      {
        for ( int i = 0; i < aLength; i++ )
        {
          sendData( "\b" );
        }

        setInputStart( inputStart - aLength );
      }
      else
      {
        aFb.remove( aOffset, aLength );
      }
    }

    @Override
    public void replace( final FilterBypass aFb, final int aOffset, final int aLength, final String aText,
        final AttributeSet aAttrs ) throws BadLocationException
    {
      int inputStart = getInputStart();

      if ( ConsolePane.this.mux.isReadMode() )
      {
        sendData( aText );

        setInputStart( inputStart - aText.length() );
      }
      else
      {
        aFb.replace( aOffset, aLength, aText, aAttrs );

        setInputStart( inputStart + aText.length() );
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
   * Provides a multiplexer for writing to a document from two individual
   * sources.
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
  final class InputStreamWorker extends SwingWorker<Void, Integer> implements TermCallback
  {
    // VARIABLES

    private final InputStream inputStream;
    private final AnsiInterpreter ansiInterpreter;

    private volatile AttributeSet attrs;

    // CONSTRUCTORS

    /**
     * Creates a new {@link InputStreamWorker} instance.
     */
    public InputStreamWorker( final InputStream aInputStream )
    {
      this.inputStream = aInputStream;
      this.ansiInterpreter = new AnsiInterpreter();

      this.attrs = getPlainTextAttributes();
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
    @Override
    public void onAttributeChange( final AttributeSet aAttributes )
    {
      this.attrs = aAttributes;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void onClearLine( final int aMode )
    {
      Document doc = getDocument();

      int caretPos = getCaretPosition();

      Element rootElem = doc.getDefaultRootElement();
      int lineIndex = rootElem.getElementIndex( caretPos );
      Element lineElem = rootElem.getElement( lineIndex );

      int lineStart = lineElem.getStartOffset();
      int lineEnd = lineElem.getEndOffset() - 1;

      if ( aMode == 1 )
      {
        // clear from cursor to start of line...
        if ( caretPos > lineStart )
        {
          removeText( doc, lineStart, caretPos - lineStart );
        }
      }
      else if ( aMode == 2 )
      {
        // clear entire line...
        removeText( doc, lineStart, lineEnd - lineStart );
      }
      else
      {
        // clear from cursor to end of line...
        if ( caretPos < lineEnd )
        {
          removeText( doc, caretPos, lineEnd - caretPos );
        }
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void onClearScreen( final int aMode )
    {
      setDocument( new DefaultStyledDocument() );
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void onMoveCursorAbsolute( final int aXpos, final int aYpos )
    {
      Document doc = getDocument();

      Element rootElem = doc.getDefaultRootElement();
      Element lineElem = rootElem.getElement( rootElem.getElementCount() - 1 );

      int lineStart = lineElem.getStartOffset();
      int lineEnd = lineElem.getEndOffset() - 1;

      if ( aXpos >= 0 )
      {
        setCaretPosition( Math.min( lineEnd, lineStart + aXpos ) );
      }

      if ( aYpos >= 0 )
      {
        // XXX
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void onMoveCursorRelative( final int aXpos, final int aYpos )
    {
      Document doc = getDocument();

      int caretPosition = getCaretPosition();

      Element rootElem = doc.getDefaultRootElement();
      int lineIndex = rootElem.getElementIndex( caretPosition );
      Element lineElem = rootElem.getElement( lineIndex );

      int lineStart = lineElem.getStartOffset();
      int lineEnd = lineElem.getEndOffset() - 1;

      if ( aXpos != 0 )
      {
        setCaretPosition( Math.min( doc.getLength(), caretPosition + aXpos ) );
      }

      if ( aYpos < 0 )
      {
        // XXX
      }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void onText( final char aChar )
    {
      final AbstractDocument doc = ( AbstractDocument )getDocument();
      final AttributeSet _attrs = this.attrs;

      Element rootElem = doc.getDefaultRootElement();
      Element lineElem = rootElem.getElement( rootElem.getElementCount() - 1 );

      int lineStart = lineElem.getStartOffset();
      int lineEnd = lineElem.getEndOffset() - 1;

      try
      {
        int caret = getCaretPosition();
        // When the caret is not at the end, and we did not obtain a linefeed
        // (\n), we should replace the current line...
        if ( ( caret >= lineStart ) && ( caret < lineEnd ) && ( aChar != '\n' ) )
        {
          doc.replace( caret, lineEnd - caret, Character.toString( aChar ), _attrs );
        }
        else
        {
          doc.insertString( lineEnd, Character.toString( aChar ), _attrs );
          setCaretPosition( doc.getLength() );
        }
      }
      catch ( BadLocationException exception )
      {
        throw new RuntimeException( exception );
      }
    }

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
        if ( isRawMode() )
        {
          handleRawText( aReadChars );
        }
        else
        {
          handleAnsiText( aReadChars );
        }
      }
      finally
      {
        ConsolePane.this.mux.switchToReadMode();
      }
    }

    /**
     * @param aDocument
     * @param aOffset
     * @param aText
     * @param aAttributes
     */
    private void appendText( final Document aDocument, final int aOffset, final String aText,
        final AttributeSet aAttributes )
    {
      try
      {
        aDocument.insertString( aOffset, aText, aAttributes );
      }
      catch ( BadLocationException exception )
      {
        throw new RuntimeException( exception );
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

    /**
     * @param aDoc
     * @param aChar
     */
    private void handleAnsiText( final List<Integer> aChars )
    {
      this.ansiInterpreter.append( aChars );
      this.ansiInterpreter.interpret( this );
    }

    /**
     * @param aDoc
     * @param aChar
     */
    private void handleRawText( final List<Integer> aChars )
    {
      final Document doc = getDocument();
      for ( Integer i : aChars )
      {
        String c = convertToText( i );

        AttributeSet attrs = getPlainTextAttributes();
        if ( c.startsWith( "<" ) && c.endsWith( ">" ) )
        {
          attrs = getEscapeAttributes();
        }

        int offset = doc.getLength();

        int charValue = i.intValue();
        if ( charValue == '\b' )
        {
          removeText( doc, offset - 1, 1 );
        }
        else if ( charValue == '\u0007' )
        {
          Toolkit.getDefaultToolkit().beep();
        }
        else
        {
          appendText( doc, offset, c, attrs );
        }
      }
    }

    /**
     * @param aDocument
     * @param aOffset
     */
    private void removeText( final Document aDocument, final int aOffset, final int aLength )
    {
      try
      {
        aDocument.remove( aOffset, aLength );
      }
      catch ( BadLocationException exception )
      {
        throw new RuntimeException( exception );
      }
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

    public void write( final byte... aData ) throws IOException
    {
      this.outputStream.write( aData );
      this.outputStream.flush();
    }

    public void write( final String aData ) throws IOException
    {
      write( aData.getBytes() );
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

  /**
   * Sends literal data.
   */
  final class SendLiteralDataAction extends AbstractAction
  {
    private static final long serialVersionUID = 1L;

    private final byte[] data;

    /**
     * Creates a new ConsolePane.SendLiteralDataAction instance.
     */
    public SendLiteralDataAction( final int... aData )
    {
      this.data = new byte[aData.length];
      for ( int i = 0; i < aData.length; i++ )
      {
        this.data[i] = ( byte )aData[i];
      }
    }

    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      try
      {
        ConsolePane.this.outputStreamWorker.write( this.data );
      }
      catch ( IOException exception )
      {
        // TODO Auto-generated catch block
        exception.printStackTrace();
      }
    }
  }

  // CONSTANTS

  public static final Color ESCAPED_TEXT_COLOR = new Color( 0x00, 0x80, 0xFF );
  public static final Color PLAIN_TEXT_COLOR = new Color( 0xE6, 0xE6, 0xE6 );
  public static final Color STATUS_TEXT_COLOR = new Color( 0xFF, 0x80, 0x00 );
  public static final Color BACKGROUND_COLOR = new Color( 0x1E, 0x21, 0x26 );

  private static final long serialVersionUID = 1L;

  /** Contains an integer value representing the start of input. */
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

  private volatile boolean rawMode;

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

    final InputMap inputMap = getInputMap();
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_BACK_SPACE, 0 ), new BackspaceAction() );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_HOME, 0 ), new HomeAction() );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_HOME, InputEvent.SHIFT_MASK ), new SelectHomeAction() );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_UP, 0 ), new SendLiteralDataAction( 0x1b, 0x4f, 0x41 ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_DOWN, 0 ), new SendLiteralDataAction( 0x1b, 0x4f, 0x42 ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_RIGHT, 0 ), new SendLiteralDataAction( 0x1b, 0x4f, 0x43 ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_LEFT, 0 ), new SendLiteralDataAction( 0x1b, 0x4f, 0x44 ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_F1, 0 ), new SendLiteralDataAction( 0x1b, 0x4f, 'P' ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_F2, 0 ), new SendLiteralDataAction( 0x1b, 0x4f, 'Q' ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_F3, 0 ), new SendLiteralDataAction( 0x1b, 0x4f, 'R' ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_F4, 0 ), new SendLiteralDataAction( 0x1b, 0x4f, 'S' ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_F5, 0 ), new SendLiteralDataAction( 0x1b, 0x4f, 't' ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_F6, 0 ), new SendLiteralDataAction( 0x1b, 0x4f, 'u' ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_F7, 0 ), new SendLiteralDataAction( 0x1b, 0x4f, 'v' ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_F8, 0 ), new SendLiteralDataAction( 0x1b, 0x4f, 'I' ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_F9, 0 ), new SendLiteralDataAction( 0x1b, 0x4f, 'w' ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_F10, 0 ), new SendLiteralDataAction( 0x1b, 0x4f, 'x' ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_C, InputEvent.CTRL_MASK ), new SendLiteralDataAction( 0x1B, 0x21,
        0x40, 0x03 ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_D, InputEvent.CTRL_MASK ), new SendLiteralDataAction( 0x04 ) );
    inputMap.put( KeyStroke.getKeyStroke( KeyEvent.VK_Z, InputEvent.CTRL_MASK ), new SendLiteralDataAction( 0x1A ) );

    setDocument( getDocument() );
    setInputStart( 0 );

    setBackground( BACKGROUND_COLOR );
    setCaretColor( PLAIN_TEXT_COLOR );

    getCaret().setVisible( true );
    getCaret().setBlinkRate( 500 );
  }

  // METHODS

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

  /**
   * Returns the current value of rawMode.
   * 
   * @return the rawMode
   */
  public boolean isRawMode()
  {
    return this.rawMode;
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
   * Sets whether or not the "raw" mode is enabled. In raw mode, all
   * non-displayable ASCII characters are represented by their name
   * representations.
   * 
   * @param aRawMode
   *          the rawMode to set.
   */
  public void setRawMode( final boolean aRawMode )
  {
    this.rawMode = aRawMode;
  }

  /**
   * @return
   */
  final int getInputStart()
  {
    final Integer i = ( Integer )getDocument().getProperty( ConsolePane.PROPERTY_INPUTSTART );
    return ( i == null ) ? 0 : i.intValue();
  }

  /**
   * @param aPosition
   */
  final void setInputStart( final int aPosition )
  {
    getDocument().putProperty( ConsolePane.PROPERTY_INPUTSTART, new Integer( aPosition ) );
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
  private SimpleAttributeSet getEscapeAttributes()
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
  private SimpleAttributeSet getPlainTextAttributes()
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
  private SimpleAttributeSet getStatusTextAttributes()
  {
    SimpleAttributeSet attrs = createAttributeSet();
    StyleConstants.setForeground( attrs, STATUS_TEXT_COLOR );
    StyleConstants.setBold( attrs, true );
    return attrs;
  }
}
