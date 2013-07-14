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


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.createRightAlignedLabel;

import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseAdapter;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.TooManyListenersException;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SpringLayout;

import nl.lxtreme.jvt220.terminal.ITerminal;
import nl.lxtreme.jvt220.terminal.ITerminalFrontend;
import nl.lxtreme.jvt220.terminal.swing.SwingFrontend;
import nl.lxtreme.jvt220.terminal.vt220.AbstractTerminal;
import nl.lxtreme.jvt220.terminal.vt220.VT220Terminal;
import nl.lxtreme.ols.tool.base.ToolUtils;
import nl.lxtreme.ols.util.HostUtils;
import nl.lxtreme.ols.util.NumberUtils;
import nl.lxtreme.ols.util.swing.SpringLayoutUtils;
import nl.lxtreme.ols.util.swing.StandardActionFactory.CloseAction.Closeable;
import nl.lxtreme.ols.util.swing.SwingComponentUtils;
import nl.lxtreme.ols.util.swing.component.JErrorDialog;
import nl.lxtreme.ols.util.swing.component.JLazyComboBox;
import purejavacomm.CommPortIdentifier;
import purejavacomm.NoSuchPortException;
import purejavacomm.PortInUseException;
import purejavacomm.SerialPort;
import purejavacomm.SerialPortEvent;
import purejavacomm.SerialPortEventListener;
import purejavacomm.UnsupportedCommOperationException;


/**
 * Provides a window in which you connect to a serial port, and talk to that
 * port.
 */
public class SerialConsoleWindow extends JFrame implements Closeable
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  /** The serial port baudrates that can be chosen. */
  private static final String[] BAUDRATES = { "921600bps", "460800bps", "230400bps", "115200bps", "57600bps",
      "38400bps", "19200bps", "14400bps", "9600bps", "4800bps", "2400bps" };

  private static final String[] DATABITS = { "5", "6", "7", "8", "9" };
  private static final String[] PARITIES = { "None", "Odd", "Even" };
  private static final String[] STOPBITS = { "1", "1.5", "2" };
  private static final String[] FLOWCONTROLS = { "Off", "XON/XOFF (software)", "RTS/CTS (hardware)" };

  // VARIABLES

  private boolean dialogResult;

  private JComboBox portSelect;
  private JComboBox portRateSelect;
  private JComboBox stopBitsSelect;
  private JComboBox dataBitsSelect;
  private JComboBox paritySelect;
  private JComboBox flowControlSelect;

  private ITerminal terminal;
  private ITerminalFrontend terminalFrontend;

  private JTextField serialInputTextField;
  private JButton sendButton;
  private JButton connectButton;
  private JButton disconnectButton;
  private JCheckBox autoNewLineMode;

  private volatile SerialPort serialPort;
  private volatile InputStream serialInput;
  private volatile OutputStream serialOutput;

  // CONSTRUCTORS

  /**
   * Creates a new {@link SerialConsoleWindow} instance.
   * 
   * @param aParent
   */
  public SerialConsoleWindow( final Window aParent )
  {
    super( "Serial Console" );

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
      this.serialPort = openSerialPort();

      this.serialInput = this.serialPort.getInputStream();
      this.serialOutput = this.serialPort.getOutputStream();

      this.terminal = new VT220Terminal( 80, 24 );

      this.terminalFrontend.connect( this.serialOutput );
      this.terminalFrontend.setTerminal( this.terminal );

      disableControls();
    }
    catch ( Exception exception )
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

      this.terminalFrontend.disconnect();

      if ( this.serialPort != null )
      {
        HostUtils.closeResource( this.serialInput );
        HostUtils.closeResource( this.serialOutput );

        this.serialPort.close();
      }
    }
    catch ( IOException exception )
    {
      JErrorDialog.showDialog( getOwner(), "Disconnect failed!", exception );
    }
    finally
    {
      this.serialPort = null;
      this.serialInput = null;
      this.serialOutput = null;
    }
  }

  /**
   * Resizes the frame to fix its contents. When the frame is only partially
   * visible after resizing, it will be moved to make most of it visible.
   */
  void resizeFrameToFitContent()
  {
    // final Dimension frontendSize = this.terminalFrontend.getSize();
    // final Insets frameInsets = getInsets();
    //
    // int width = frameInsets.left + frameInsets.right + frontendSize.width;
    // int height = frameInsets.top + frameInsets.bottom + frontendSize.height;
    //
    // setSize( width, height );
    //
    // Rectangle screenBounds = getGraphicsConfiguration().getBounds();
    //
    // Rectangle frameBounds = getBounds();
    // if ( frameBounds.x + frameBounds.width > screenBounds.width )
    // {
    // frameBounds.x = screenBounds.x;
    // }
    // if ( frameBounds.y + frameBounds.height > screenBounds.height )
    // {
    // frameBounds.y = screenBounds.y;
    // }
    // setBounds( frameBounds );
  }

  /**
   * Sends the text of the text input field to the serial port.
   */
  final void sendData()
  {
    boolean oldState = this.sendButton.isEnabled();

    try
    {
      this.sendButton.setEnabled( false );

      final String text = new StringInterpreter().interpret( this.serialInputTextField.getText() );

      final Writer writer = this.terminalFrontend.getWriter();
      writer.write( text );
      writer.flush();
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
    boolean oldState = this.sendButton.isEnabled();

    try
    {
      this.sendButton.setEnabled( false );

      final Writer writer = this.terminalFrontend.getWriter();
      writer.write( aText );
      writer.flush();
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

    SwingComponentUtils.setupWindowContentPane( this, contentPane, buttonPane );

    addWindowListener( new WindowAdapter()
    {
      @Override
      public void windowClosing( WindowEvent aEvent )
      {
        disconnect();
      }
    } );
    addComponentListener( new ComponentAdapter()
    {
      @Override
      public void componentResized( ComponentEvent aEvent )
      {
        resizeFrameToFitContent();
      }
    } );

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

    output.add( ( JComponent )this.terminalFrontend, new GridBagConstraints( 0, 0, 2, 1, 1.0, 1.0,
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

    panel.add( createRightAlignedLabel( "Auto newline mode?" ) );
    panel.add( this.autoNewLineMode );

    SpringLayoutUtils.addSeparator( panel, " " );

    panel.add( createRightAlignedLabel( "" ) );
    panel.add( this.connectButton );

    panel.add( createRightAlignedLabel( "" ) );
    panel.add( this.disconnectButton );

    SpringLayoutUtils.makeEditorGrid( panel, 10, 4 );

    return panel;
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
    ( ( JComponent )this.terminalFrontend ).setEnabled( true );

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
    ( ( JComponent )this.terminalFrontend ).setEnabled( false );

    this.portSelect.setEnabled( true );
    this.portRateSelect.setEnabled( true );
    this.dataBitsSelect.setEnabled( true );
    this.paritySelect.setEnabled( true );
    this.stopBitsSelect.setEnabled( true );
    this.flowControlSelect.setEnabled( true );
  }

  /**
   * Returns the opened serial port.
   * 
   * @return a serial port, never <code>null</code>.
   * @throws NoSuchPortException
   *           in case the defined port does not (or no longer) exist;
   * @throws PortInUseException
   *           in case the defined port is not available to us.
   * @throws UnsupportedCommOperationException
   *           in case we're trying to perform a port operation that is not
   *           supported.
   */
  private SerialPort openSerialPort() throws IOException, NoSuchPortException, PortInUseException,
      UnsupportedCommOperationException, TooManyListenersException
  {
    String portName = String.valueOf( this.portSelect.getSelectedItem() );

    CommPortIdentifier portId = CommPortIdentifier.getPortIdentifier( portName );

    SerialPort result = ( SerialPort )portId.open( "Serial console tool", 1000 );

    int baudrate = NumberUtils.smartParseInt( String.valueOf( this.portRateSelect.getSelectedItem() ) );

    String db = ( String )this.dataBitsSelect.getSelectedItem();
    int databits;
    if ( "5".equals( db ) )
    {
      databits = SerialPort.DATABITS_5;
    }
    else if ( "6".equals( db ) )
    {
      databits = SerialPort.DATABITS_6;
    }
    else if ( "7".equals( db ) )
    {
      databits = SerialPort.DATABITS_7;
    }
    else
    {
      databits = SerialPort.DATABITS_8;
    }

    String sb = ( String )this.stopBitsSelect.getSelectedItem();
    int stopbits;
    if ( "2".equals( sb ) )
    {
      stopbits = SerialPort.STOPBITS_2;
    }
    else if ( "1.5".equals( sb ) )
    {
      stopbits = SerialPort.STOPBITS_1_5;
    }
    else
    {
      stopbits = SerialPort.STOPBITS_1;
    }

    final String par = String.valueOf( this.paritySelect.getSelectedItem() );
    int parity;
    if ( "Odd".equalsIgnoreCase( par ) )
    {
      parity = SerialPort.PARITY_NONE;
    }
    else if ( "Even".equalsIgnoreCase( par ) )
    {
      parity = SerialPort.PARITY_EVEN;
    }
    else
    {
      parity = SerialPort.PARITY_NONE;
    }

    String fc = String.valueOf( this.flowControlSelect.getSelectedItem() );
    int flowControl;
    if ( fc.startsWith( "XON" ) )
    {
      flowControl = SerialPort.FLOWCONTROL_XONXOFF_IN | SerialPort.FLOWCONTROL_XONXOFF_OUT;
    }
    else if ( fc.startsWith( "RTS" ) )
    {
      flowControl = SerialPort.FLOWCONTROL_RTSCTS_IN | SerialPort.FLOWCONTROL_RTSCTS_OUT;
    }
    else
    {
      flowControl = SerialPort.FLOWCONTROL_NONE;
    }

    result.setSerialPortParams( baudrate, databits, stopbits, parity );
    result.setFlowControlMode( flowControl );
    result.enableReceiveTimeout( 100 );
    result.enableReceiveThreshold( 0 );

    result.notifyOnDataAvailable( true );
    result.addEventListener( new SerialPortEventListener()
    {
      @Override
      public void serialEvent( SerialPortEvent event )
      {
        if ( ( event.getEventType() & SerialPortEvent.DATA_AVAILABLE ) != 0 )
        {
          SerialPort port = ( SerialPort )event.getSource();

          try
          {
            InputStream is = port.getInputStream();

            Integer[] buf = new Integer[is.available()];
            for ( int i = 0; i < buf.length; i++ )
            {
              buf[i] = Integer.valueOf( is.read() );
            }

            terminalFrontend.writeCharacters( buf );
          }
          catch ( IOException e )
          {
            e.printStackTrace();
          }
        }
      }
    } );

    return result;
  }

  /**
   * Initializes this dialog by creating and initializing all components.
   */
  private void initDialog()
  {
    this.terminalFrontend = new SwingFrontend();
    ( ( JComponent )this.terminalFrontend ).addMouseListener( new MouseAdapter()
    {
      public void mouseClicked( java.awt.event.MouseEvent e )
      {
        ( ( JComponent )e.getSource() ).requestFocus();
      }
    } );

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

    this.autoNewLineMode = new JCheckBox();
    this.autoNewLineMode.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        JCheckBox cb = ( JCheckBox )aEvent.getSource();
        ( ( AbstractTerminal )terminal ).setAutoNewlineMode( cb.isSelected() );
      }
    } );

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
