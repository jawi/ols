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


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.util.List;

import javax.microedition.io.*;
import javax.swing.*;
import javax.swing.text.*;

import nl.lxtreme.ols.tool.base.*;
import nl.lxtreme.ols.util.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;
import nl.lxtreme.ols.util.swing.component.*;

import org.osgi.service.io.*;

import purejavacomm.*;


/**
 * Provides a window in which you connect to a serial port, and talk to that
 * port.
 */
public abstract class SerialConsoleWindow extends JDialog implements Closeable
{
  // INNER TYPES

  /**
   * @see http
   *      ://javatechniques.com/blog/faster-jtextpane-text-insertion-part-ii/
   */
  static class BatchDocument extends DefaultStyledDocument
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    private static final char[] EOL_ARRAY = { '\n' };

    // VARIABLES

    private final List<ElementSpec> batch;

    // CONSTRUCTORS

    public BatchDocument()
    {
      this.batch = new ArrayList<ElementSpec>();
    }

    // METHODS

    /**
     * Adds a linefeed for later batch processing
     */
    public void appendBatchLineFeed( final AttributeSet aAttributeSet )
    {
      synchronized ( this.batch )
      {
        // Add a spec with the linefeed characters
        this.batch.add( new ElementSpec( aAttributeSet, ElementSpec.ContentType, EOL_ARRAY, 0, 1 ) );

        // Then add attributes for element start/end tags. Ideally
        // we'd get the attributes for the current position, but we
        // don't know what those are yet if we have unprocessed
        // batch inserts. Alternatives would be to get the last
        // paragraph element (instead of the first), or to process
        // any batch changes when a linefeed is inserted.
        Element paragraph = getParagraphElement( 0 );
        AttributeSet pattr = paragraph.getAttributes();
        this.batch.add( new ElementSpec( null, ElementSpec.EndTagType ) );
        this.batch.add( new ElementSpec( pattr, ElementSpec.StartTagType ) );
      }
    }

    /**
     * Adds a String (assumed to not contain linefeeds) for later batch
     * insertion.
     */
    public void appendBatchString( final String aText, AttributeSet aAttributeSet )
    {
      synchronized ( this.batch )
      {
        // Make a copy of the attributes, since we will hang onto
        // them indefinitely and the caller might change them
        // before they are processed.
        aAttributeSet = aAttributeSet.copyAttributes();
        char[] chars = aText.toCharArray();
        this.batch.add( new ElementSpec( aAttributeSet, ElementSpec.ContentType, chars, 0, aText.length() ) );
      }
    }

    public void processBatchUpdates() throws BadLocationException
    {
      processBatchUpdates( getLength() );
    }

    public void processBatchUpdates( final int aOffset ) throws BadLocationException
    {
      synchronized ( this.batch )
      {
        // Process all of the inserts in bulk
        super.insert( aOffset, this.batch.toArray( new ElementSpec[this.batch.size()] ) );
        this.batch.clear();
      }
    }
  }

  /**
   * Serial reader that asynchronously reads data from an inputstream and
   * displays it on a text area.
   */
  static class SerialReaderWorker extends SwingWorker<Void, Integer>
  {
    // VARIABLES

    private final SerialTextOutput output;
    private final InputStream inputStream;

    // CONSTRUCTORS

    /**
     * Creates a new {@link SerialReaderWorker} instance.
     */
    public SerialReaderWorker( final SerialTextOutput aOutput, final InputStream aInputStream )
    {
      this.output = aOutput;
      this.inputStream = aInputStream;
    }

    // METHODS

    /**
     * {@inheritDoc}
     */
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

    /**
     * {@inheritDoc}
     */
    @Override
    protected void process( final List<Integer> aReadChars )
    {
      for ( Integer value : aReadChars )
      {
        this.output.appendText( convertToText( value ) );
      }

      this.output.flush();
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

      int intValue = aValue.intValue();
      if ( intValue < ASCII_NAMES.length )
      {
        text = ASCII_NAMES[intValue];
      }
      else
      {
        text = String.format( "<%02d>", aValue );
      }
      return text;
    }
  }

  /**
   * Wrapper for JTextPane that controls how text is appended.
   */
  final class SerialTextOutput
  {
    // VARIABLES

    private final JTextPane output;
    private final BatchDocument document;

    // CONSTRUCTORS

    /**
     * Creates a new {@link SerialTextOutput} instance.
     */
    public SerialTextOutput( final JTextPane aOutput )
    {
      this.output = aOutput;
      this.document = new BatchDocument();

      this.output.setDocument( this.document );
      this.output.setEditable( false );
      this.output.addKeyListener( new KeyAdapter()
      {
        @Override
        public void keyTyped( final KeyEvent aEvent )
        {
          sendData( ( byte )aEvent.getKeyChar() );
        }
      } );

      ActionMap actionMap = this.output.getActionMap();
      // By default, the DefaultEditorKit & StyledEditorKit uses this action to
      // insert newlines, as our JTextPane is non-editable, it will not do any-
      // thing, but beep, which is annoying, hence we're overruling this action.
      actionMap.put( DefaultEditorKit.insertBreakAction, DUMMY_ACTION );
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
      internalAppendText( "\n", getStatusTextAttributes() );
      internalAppendText( aText, getStatusTextAttributes() );
      internalAppendText( "\n", getStatusTextAttributes() );
      flush();
    }

    /**
     * Appends the given text to the current document.
     * 
     * @param aText
     *          the text to append, cannot be <code>null</code>.
     */
    public void appendText( final String aText )
    {
      internalAppendText( aText, getTextAttributes( aText ) );
    }

    /**
     * Flushes all pending changes to the document.
     */
    public void flush()
    {
      try
      {
        this.document.processBatchUpdates();
        // Move caret to last position in document...
        this.output.setCaretPosition( this.document.getLength() - 1 );
      }
      catch ( IllegalArgumentException exception )
      {
        // Ignore; don't update caret position...
      }
      catch ( BadLocationException exception )
      {
        throw new RuntimeException( "BadLocationException while processing batch updates?!", exception );
      }
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

    /**
     * Returns the text attributes for the given text.
     * 
     * @param aText
     *          the text to create the text attributes for, cannot be
     *          <code>null</code>.
     * @return a attribute set, never <code>null</code>.
     */
    private AttributeSet getTextAttributes( final String aText )
    {
      SimpleAttributeSet attrs = createAttributeSet();

      // ignore any trailing newlines/spaces and such...
      String text = aText.trim();

      if ( text.startsWith( "<" ) && text.endsWith( ">" ) )
      {
        StyleConstants.setForeground( attrs, ESCAPED_TEXT_COLOR );
      }
      else
      {
        StyleConstants.setForeground( attrs, PLAIN_TEXT_COLOR );
      }
      return attrs;
    }

    /**
     * @param aText
     * @param aAttributes
     */
    private void internalAppendText( final String aText, final AttributeSet aAttributes )
    {
      this.document.appendBatchString( aText, aAttributes );

      if ( aText.endsWith( "\n" ) )
      {
        this.document.appendBatchLineFeed( aAttributes );
      }
    }
  }

  // CONSTANTS

  private static final long serialVersionUID = 1L;

  /** The serial port baudrates that can be chosen. */
  private static final String[] BAUDRATES = { "921600bps", "460800bps", "230400bps", "115200bps", "57600bps",
      "38400bps", "19200bps", "14400bps", "9600bps", "4800bps", "2400bps" };

  private static final String[] DATABITS = { "5", "6", "7", "8", "9" };
  private static final String[] PARITIES = { "None", "Odd", "Even" };
  private static final String[] STOPBITS = { "1", "1.5", "2" };
  private static final String[] FLOWCONTROLS = { "Off", "XON/XOFF (software)", "RTS/CTS (hardware)" };

  private static final String[] ASCII_NAMES = { "<nul>", "<soh>", "<stx>", "<etx>", "<eot>", "<enq>", "<ack>",
      "<bell>", "<bs>", "\t", "\n", "<vt>", "<np>", "\r", "<so>", "<si>", "<dle>", "<dc1>", "<dc2>", "<dc3>", "<dc4>",
      "<nak>", "<syn>", "<etb>", "<can>", "<em>", "<sub>", "<esc>", "<fs>", "<gs>", "<rs>", "<us>", " ", "!", "\"",
      "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8",
      "9", ":", ";", "<", "=", ">", "?", "@", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N",
      "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "[", "\\", "]", "^", "_", "`", "a", "b", "c", "d",
      "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
      "{", "|", "}", "~", "<del>" };

  private static final Action DUMMY_ACTION = new AbstractAction()
  {
    // CONSTANTS

    private static final long serialVersionUID = 1L;

    // METHODS

    @Override
    public void actionPerformed( final ActionEvent aEvent )
    {
      // Nop...
    }
  };

  private static final Color ESCAPED_TEXT_COLOR = new Color( 0x00, 0x80, 0xFF );
  private static final Color PLAIN_TEXT_COLOR = new Color( 0xE6, 0xE6, 0xE6 );
  private static final Color STATUS_TEXT_COLOR = new Color( 0xFF, 0x80, 0x00 );
  private static final Color BACKGROUND_COLOR = new Color( 0x1E, 0x21, 0x26 );

  // VARIABLES

  private final StringInterpreter interpreter;

  private boolean dialogResult;

  private JComboBox portSelect;
  private JComboBox portRateSelect;
  private JComboBox stopBitsSelect;
  private JComboBox dataBitsSelect;
  private JComboBox paritySelect;
  private JComboBox flowControlSelect;

  private JTextPane serialOutputTextPane;
  private SerialTextOutput serialTextOutput;
  private JTextField serialInputTextField;
  private JButton sendButton;
  private JButton connectButton;
  private JButton disconnectButton;
  private JCheckBox sendEnter;

  private volatile StreamConnection connection;
  private volatile InputStream serialInput;
  private volatile OutputStream serialOutput;
  private volatile SerialReaderWorker worker;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SerialConsoleWindow} instance.
   * 
   * @param aParent
   */
  public SerialConsoleWindow( final Window aParent )
  {
    super( aParent, "Serial Console", ModalityType.MODELESS );

    this.interpreter = new StringInterpreter();

    initDialog();
    buildDialog();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void close()
  {
    disconnect();
    setVisible( false );
    dispose();
  }

  /**
   * Shows the dialog on screen.
   */
  public boolean showDialog()
  {
    this.dialogResult = false;

    setVisible( true );

    return this.dialogResult;
  }

  /**
   * Connects to the current selected port.
   */
  final void connect()
  {
    try
    {
      String uri = getConnectionURI();
      this.connection = createStreamConnection( uri );

      this.serialInput = this.connection.openInputStream();
      this.serialOutput = this.connection.openOutputStream();

      this.worker = new SerialReaderWorker( this.serialTextOutput, this.serialInput );
      this.worker.execute();

      disableControls();

      this.serialTextOutput.appendStatusText( "Connected to " + this.portSelect.getSelectedItem() + " @ "
          + this.portRateSelect.getSelectedItem() );
    }
    catch ( IOException exception )
    {
      JErrorDialog.showDialog( getOwner(), "Connect failed!", exception );
    }
  }

  /**
   * Disconnects from the current selected port.
   */
  final void disconnect()
  {
    try
    {
      enableControls();

      if ( this.worker != null )
      {
        this.worker.cancel( false /* mayInterruptIfRunning */);
        this.worker = null;
      }

      if ( this.connection != null )
      {
        this.serialTextOutput.appendStatusText( "Disconnected ..." );

        HostUtils.closeResource( this.serialInput );
        HostUtils.closeResource( this.serialOutput );

        this.connection.close();
      }
    }
    catch ( IOException exception )
    {
      JErrorDialog.showDialog( getOwner(), "Disconnect failed!", exception );
    }
    finally
    {
      this.connection = null;
      this.serialInput = null;
      this.serialOutput = null;
    }
  }

  /**
   * Sends the text of the text input field to the serial port.
   */
  final void sendData()
  {
    sendData( this.serialInputTextField.getText() );
  }

  /**
   * Sends the given data to the serial port.
   */
  final void sendData( final byte... aBuffer )
  {
    boolean oldState = this.sendButton.isEnabled();

    try
    {
      this.sendButton.setEnabled( false );

      if ( this.connection != null )
      {
        this.serialOutput.write( aBuffer );
        this.serialOutput.flush();
      }
    }
    catch ( IOException exception )
    {
      JErrorDialog.showDialog( getOwner(), "Sending data failed!", exception );
    }
    finally
    {
      this.sendButton.setEnabled( oldState );
    }
  }

  /**
   * Sends the given text to the serial port.
   */
  final void sendData( final String aText )
  {
    sendData( this.interpreter.interpret( aText ) );
  }

  /**
   * @return a {@link ConnectorService} instance, never <code>null</code>.
   */
  protected abstract ConnectorService getConnectorService();

  /**
   * Builds this dialog by placing all components on it.
   */
  private void buildDialog()
  {
    setMinimumSize( new Dimension( 640, 480 ) );

    final JComponent settingsPane = createSettingsPane();
    final JComponent ioPane = createIOPane();

    final JPanel contentPane = new JPanel( new GridBagLayout() );
    contentPane.add( settingsPane, //
        new GridBagConstraints( 0, 0, 1, 1, 0.0, 0.0, GridBagConstraints.NORTH, GridBagConstraints.NONE, new Insets( 2,
            0, 2, 0 ), 0, 0 ) );

    contentPane.add( ioPane, //
        new GridBagConstraints( 1, 0, 1, 1, 1.0, 1.0, GridBagConstraints.NORTH, GridBagConstraints.BOTH, new Insets( 2,
            0, 2, 0 ), 0, 0 ) );

    final JButton closeButton = ToolUtils.createCloseButton();

    final JComponent buttonPane = SwingComponentUtils.createButtonPane( closeButton );

    SwingComponentUtils.setupDialogContentPane( this, contentPane, buttonPane );

    pack();
  }

  /**
   * Creates the I/O pane.
   * 
   * @return an I/O pane, never <code>null</code>.
   */
  private JComponent createIOPane()
  {
    final JPanel output = new JPanel( new GridBagLayout() );

    output.add( new JScrollPane( this.serialOutputTextPane ), new GridBagConstraints( 0, 0, 2, 1, 1.0, 1.0,
        GridBagConstraints.NORTH, GridBagConstraints.BOTH, new Insets( 4, 4, 4, 4 ), 0, 0 ) );

    output.add( this.serialInputTextField, new GridBagConstraints( 0, 1, 1, 1, 1.0, 0.0, GridBagConstraints.SOUTH,
        GridBagConstraints.HORIZONTAL, new Insets( 0, 0, 0, 0 ), 0, 0 ) );
    output.add( this.sendButton, new GridBagConstraints( 1, 1, 1, 1, 0.0, 0.0, GridBagConstraints.EAST,
        GridBagConstraints.NONE, new Insets( 0, 0, 0, 0 ), 0, 0 ) );

    return output;
  }

  /**
   * Creates the settings pane.
   * 
   * @return a settings pane, never <code>null</code>.
   */
  private JComponent createSettingsPane()
  {
    final JPanel panel = new JPanel( new SpringLayout() );

    SpringLayoutUtils.addSeparator( panel, "Serial settings" );

    panel.add( createRightAlignedLabel( "Serial port" ) );
    panel.add( this.portSelect );

    panel.add( createRightAlignedLabel( "Port speed" ) );
    panel.add( this.portRateSelect );

    panel.add( createRightAlignedLabel( "Data bits" ) );
    panel.add( this.dataBitsSelect );

    panel.add( createRightAlignedLabel( "Parity" ) );
    panel.add( this.paritySelect );

    panel.add( createRightAlignedLabel( "Stop bits" ) );
    panel.add( this.stopBitsSelect );

    panel.add( createRightAlignedLabel( "Flow control" ) );
    panel.add( this.flowControlSelect );

    SpringLayoutUtils.addSeparator( panel, "Terminal settings" );

    panel.add( createRightAlignedLabel( "Send newlines?" ) );
    panel.add( this.sendEnter );

    SpringLayoutUtils.addSeparator( panel, " " );

    panel.add( createRightAlignedLabel( "" ) );
    panel.add( this.connectButton );

    panel.add( createRightAlignedLabel( "" ) );
    panel.add( this.disconnectButton );

    SpringLayoutUtils.makeEditorGrid( panel, 10, 4 );

    return panel;
  }

  /**
   * Creates a new {@link StreamConnection} instance for the given URI.
   * 
   * @param aUri
   *          the URI to connect to, cannot be <code>null</code>.
   * @return a new {@link StreamConnection} instance, never <code>null</code>.
   * @throws IOException
   *           in case creating the connection failed.
   */
  private StreamConnection createStreamConnection( final String aUri ) throws IOException
  {
    return ( StreamConnection )getConnectorService().open( aUri, ConnectorService.READ_WRITE, true /* timeouts */);
  }

  /**
   * Disables the controls for use when connected to a serial port.
   */
  private void disableControls()
  {
    this.connectButton.setEnabled( false );

    this.disconnectButton.setEnabled( true );
    this.sendButton.setEnabled( true );
    this.serialInputTextField.setEnabled( true );
    this.serialOutputTextPane.setEnabled( true );

    this.portSelect.setEnabled( false );
    this.portRateSelect.setEnabled( false );
    this.dataBitsSelect.setEnabled( false );
    this.paritySelect.setEnabled( false );
    this.stopBitsSelect.setEnabled( false );
    this.flowControlSelect.setEnabled( false );
  }

  /**
   * Enables all controls after a disconnect.
   */
  private void enableControls()
  {
    this.connectButton.setEnabled( true );

    this.disconnectButton.setEnabled( false );
    this.sendButton.setEnabled( false );
    this.serialInputTextField.setEnabled( false );
    this.serialOutputTextPane.setEnabled( false );

    this.portSelect.setEnabled( true );
    this.portRateSelect.setEnabled( true );
    this.dataBitsSelect.setEnabled( true );
    this.paritySelect.setEnabled( true );
    this.stopBitsSelect.setEnabled( true );
    this.flowControlSelect.setEnabled( true );
  }

  /**
   * Returns the connection URI for connecting to the serial port.
   * 
   * @return a connection URI, never <code>null</code>.
   */
  private String getConnectionURI()
  {
    final String portName = String.valueOf( this.portSelect.getSelectedItem() );
    final String baudrate = String.valueOf( NumberUtils.smartParseInt( String.valueOf( this.portRateSelect
        .getSelectedItem() ) ) );

    final String bpc = String.valueOf( this.dataBitsSelect.getSelectedItem() );
    final String parity = String.valueOf( this.paritySelect.getSelectedItem() );
    final String stopBits = String.valueOf( this.stopBitsSelect.getSelectedItem() );

    String flowControl = "off";
    String fcText = String.valueOf( this.flowControlSelect.getSelectedItem() );
    if ( fcText.startsWith( "XON" ) )
    {
      flowControl = "xon_xoff";
    }
    else if ( fcText.startsWith( "RTS" ) )
    {
      flowControl = "rts_cts";
    }

    return String.format( "comm:%s;baudrate=%s;bitsperchar=%s;parity=%s;stopbits=%s;flowcontrol=%s", //
        portName, baudrate, bpc, parity, stopBits, flowControl );
  }

  /**
   * Initializes this dialog by creating and initializing all components.
   */
  private void initDialog()
  {
    this.portSelect = new JLazyComboBox( new JLazyComboBox.ItemProvider()
    {
      @Override
      @SuppressWarnings( "unchecked" )
      public Object[] getItems()
      {
        final Enumeration<CommPortIdentifier> portIdentifiers = CommPortIdentifier.getPortIdentifiers();
        final List<String> portList = new ArrayList<String>();

        while ( portIdentifiers.hasMoreElements() )
        {
          CommPortIdentifier portId = portIdentifiers.nextElement();
          if ( portId.getPortType() == CommPortIdentifier.PORT_SERIAL )
          {
            portList.add( portId.getName() );
          }
        }

        return portList.toArray( new String[portList.size()] );
      }
    } );
    // allow people to put their own port name into it...
    this.portSelect.setEditable( true );
    this.portSelect.addItemListener( new ItemListener()
    {
      @Override
      public void itemStateChanged( final ItemEvent aEvent )
      {
        disconnect();

        JComboBox cb = ( JComboBox )aEvent.getSource();
        String item = ( String )cb.getSelectedItem();

        // Do not enable the connect button until valid is selected as port...
        SerialConsoleWindow.this.connectButton.setEnabled( ( item != null ) && !"".equals( item.trim() ) );
      }
    } );

    this.portRateSelect = new JComboBox( BAUDRATES );
    this.portRateSelect.setSelectedIndex( 3 ); // 115k2

    this.dataBitsSelect = new JComboBox( DATABITS );
    this.dataBitsSelect.setSelectedIndex( 3 ); // 8 bits

    this.stopBitsSelect = new JComboBox( STOPBITS );
    this.stopBitsSelect.setSelectedIndex( 0 ); // 1

    this.paritySelect = new JComboBox( PARITIES );
    this.paritySelect.setSelectedIndex( 0 ); // NONE

    this.flowControlSelect = new JComboBox( FLOWCONTROLS );
    this.flowControlSelect.setSelectedIndex( 0 ); // Off

    this.sendEnter = new JCheckBox();
    this.sendEnter.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        JCheckBox cb = ( JCheckBox )aEvent.getSource();
        SerialConsoleWindow.this.interpreter.setAppendNewLine( cb.isSelected() );
      }
    } );

    this.serialOutputTextPane = new JTextPane();
    this.serialOutputTextPane.setBackground( BACKGROUND_COLOR );
    this.serialOutputTextPane.setBorder( BorderFactory.createEmptyBorder() );

    this.serialTextOutput = new SerialTextOutput( this.serialOutputTextPane );

    this.serialInputTextField = new JTextField( 80 );
    this.serialInputTextField.setToolTipText( "Enter raw commands here. Use $xx to enter ASCII characters directly." );
    this.serialInputTextField.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        sendData();
      }
    } );

    this.sendButton = new JButton( "Send >" );
    this.sendButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        sendData();
      }
    } );

    this.connectButton = new JButton( "Connect" );
    this.connectButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        connect();
      }
    } );

    this.disconnectButton = new JButton( "Disconnect" );
    this.disconnectButton.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        disconnect();
      }
    } );

    // Make sure we've got a consistent state for our controls...
    enableControls();

    // Initially, do not enable the connect button until something is
    // selected as port...
    this.connectButton.setEnabled( false );
  }
}
