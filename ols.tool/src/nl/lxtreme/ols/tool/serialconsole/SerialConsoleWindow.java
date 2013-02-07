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
package nl.lxtreme.ols.tool.serialconsole;


import static nl.lxtreme.ols.util.swing.SwingComponentUtils.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import javax.swing.*;

import aQute.bnd.annotation.metatype.*;

import nl.lxtreme.jvt220.terminal.*;
import nl.lxtreme.jvt220.terminal.swing.*;
import nl.lxtreme.jvt220.terminal.vt220.*;
import nl.lxtreme.ols.common.*;
import nl.lxtreme.ols.ioutil.*;
import nl.lxtreme.ols.util.swing.*;
import nl.lxtreme.ols.util.swing.component.*;
import purejavacomm.*;


/**
 * Provides a window in which you connect to a serial port, and talk to that
 * port.
 */
public class SerialConsoleWindow extends JFrame
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  // VARIABLES

  private final SerialConsoleConfig config;

  private boolean dialogResult;

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
   * @param aConfiguration
   *          the configuration to use, cannot be <code>null</code>.
   */
  public SerialConsoleWindow( final Configuration aConfiguration )
  {
    super( "Serial Console" );

    this.config = Configurable.createConfigurable( SerialConsoleConfig.class, aConfiguration.asMap() );

    initDialog();
    buildDialog();
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
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
        IOUtil.closeResource( this.serialInput );
        IOUtil.closeResource( this.serialOutput );

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

    final JButton closeButton = StandardActionFactory.createCloseButton();

    final JComponent buttonPane = createButtonPane( closeButton );

    setupDialogContentPane( this, contentPane, buttonPane );

    addWindowListener( new WindowAdapter()
    {
      @Override
      public void windowClosing( final WindowEvent aEvent )
      {
        disconnect();
      }
    } );
    addComponentListener( new ComponentAdapter()
    {
      @Override
      public void componentResized( final ComponentEvent aEvent )
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
    String portName = this.config.port();

    CommPortIdentifier portId = CommPortIdentifier.getPortIdentifier( portName );

    SerialPort result = ( SerialPort )portId.open( "Serial console tool", 1000 );

    int baudrate = this.config.baudrate();

    int databits = this.config.dataBits();

    String sb = this.config.stopBits();
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

    String par = this.config.parity();
    int parity;
    if ( "odd".equals( par ) )
    {
      parity = SerialPort.PARITY_NONE;
    }
    else if ( "even".equals( par ) )
    {
      parity = SerialPort.PARITY_EVEN;
    }
    else
    {
      parity = SerialPort.PARITY_NONE;
    }

    String fc = this.config.flowControl();
    int flowControl;
    if ( fc.startsWith( "xon" ) )
    {
      flowControl = SerialPort.FLOWCONTROL_XONXOFF_IN | SerialPort.FLOWCONTROL_XONXOFF_OUT;
    }
    else if ( fc.startsWith( "rts" ) )
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
      public void serialEvent( final SerialPortEvent event )
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

            SerialConsoleWindow.this.terminalFrontend.writeCharacters( buf );
          }
          catch ( IOException e )
          {
            // TODO Auto-generated catch block
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
      @Override
      public void mouseClicked( final java.awt.event.MouseEvent e )
      {
        ( ( JComponent )e.getSource() ).requestFocus();
      }
    } );

    this.autoNewLineMode = new JCheckBox();
    this.autoNewLineMode.addActionListener( new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent aEvent )
      {
        JCheckBox cb = ( JCheckBox )aEvent.getSource();
        ( ( AbstractTerminal )SerialConsoleWindow.this.terminal ).setAutoNewlineMode( cb.isSelected() );
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
  }
}
