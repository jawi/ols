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

  private final StringInterpreter interpreter;

  private boolean dialogResult;

  private JComboBox portSelect;
  private JComboBox portRateSelect;
  private JComboBox stopBitsSelect;
  private JComboBox dataBitsSelect;
  private JComboBox paritySelect;
  private JComboBox flowControlSelect;

  private ConsolePane consolePane;
  private JTextField serialInputTextField;
  private JButton sendButton;
  private JButton connectButton;
  private JButton disconnectButton;
  private JCheckBox sendEnter;
  private JCheckBox rawMode;

  private volatile StreamConnection connection;
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

      this.consolePane.connect( this.serialInput, this.serialOutput );

      disableControls();

      this.consolePane.appendStatusText( "Connected to " + this.portSelect.getSelectedItem() + " @ "
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

      this.consolePane.disconnect();

      if ( this.connection != null )
      {
        this.consolePane.appendStatusText( "Disconnected ..." );

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

    output.add( new JScrollPane( this.consolePane ), new GridBagConstraints( 0, 0, 2, 1, 1.0, 1.0,
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

    panel.add( createRightAlignedLabel( "Raw mode" ) );
    panel.add( this.rawMode );

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
    this.consolePane.setEnabled( true );

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
    this.consolePane.setEnabled( false );

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

    this.rawMode = new JCheckBox();
    this.rawMode.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        JCheckBox cb = ( JCheckBox )aEvent.getSource();
        SerialConsoleWindow.this.consolePane.setRawMode( cb.isSelected() );
      }
    } );

    this.consolePane = new ConsolePane();

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
