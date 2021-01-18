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
package nl.lxtreme.ols.io;


import static org.junit.Assert.*;
import static org.mockito.Matchers.*;
import static org.mockito.Mockito.*;

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.concurrent.*;

import javax.microedition.io.*;

import org.junit.*;
import org.mockito.invocation.*;
import org.mockito.stubbing.*;
import org.osgi.framework.*;
import org.osgi.service.io.*;


/**
 * @author jawi
 */
public class ConnectorServiceImplTest
{
  // INNER TYPES

  public static enum Protocols
  {
    FTP, HTTP, SMTP;

    public String protocol()
    {
      return name().toLowerCase();
    }

    public String url()
    {
      return String.format( "%s://www.acme.com", protocol() );
    }
  }

  /**
   * @author jawi
   */
  static final class MyConnectionFactory implements ConnectionFactory
  {
    // VARIABLES

    private Connection connection;
    private ServiceReference<MyConnectionFactory> serviceRef;

    // METHODS

    /**
     * @see org.osgi.service.io.ConnectionFactory#createConnection(java.lang.String,
     *      int, boolean)
     */
    @Override
    public Connection createConnection( final String aProtocol, final int aMode, final boolean aTimeouts )
        throws IOException
    {
      return this.connection;
    }

    /**
     * @return the connection
     */
    public Connection getConnection()
    {
      return this.connection;
    }

    /**
     * @return the serviceRef
     */
    public ServiceReference<MyConnectionFactory> getServiceReference()
    {
      return this.serviceRef;
    }

    /**
     * @param aConnection
     *          the connection to set
     */
    public void setConnection( final Connection aConnection )
    {
      this.connection = aConnection;
    }

    /**
     * @param aRef
     */
    public void setServiceReference( final ServiceReference<MyConnectionFactory> aServiceRef )
    {
      this.serviceRef = aServiceRef;
    }
  }

  // VARIABLES

  private BundleContext context;
  private ConnectorServiceImpl connectorService;
  private int pid;

  // METHODS

  /**
   * Tests that the activator works correctly.
   *
   * @throws Exception
   */
  @Test
  @Ignore("Not sure what I wanted to test here...")
  public void checkCorrectnessOfActivator() throws Exception
  {
    this.connectorService = null;

    final MyConnectionFactory connectionFactory = create( new String[] { Protocols.HTTP.protocol() }, 2 );
    registerConnectionFactory( connectionFactory );

    final Activator activator = new Activator();
    activator.start( this.context );

    assertEquals( connectionFactory.getConnection(), this.connectorService.open( Protocols.HTTP.url() ) );
  }

  /**
   * @throws Exception
   */
  @Test
  public void openDataInputStream() throws Exception
  {
    final MyConnectionFactory factory1 = create( new String[] { Protocols.HTTP.protocol() }, 1 );
    // just added to make sure it is not accidentally selected
    final MyConnectionFactory factory2 = create( new String[] { Protocols.SMTP.protocol() }, 1 );
    final MyConnectionFactory factory3 = create( new String[] { Protocols.FTP.protocol() }, 1 );

    final StreamConnection conn = mock( StreamConnection.class );
    final InputStream in = new InputStream()
    {
      @Override
      public int available() throws IOException
      {
        return 1001;
      }

      @Override
      public int read() throws IOException
      {
        return 1;
      }
    };
    when( conn.openInputStream() ).thenReturn( in );

    registerConnectionFactory( factory1, factory2, factory3 );

    factory1.setConnection( conn );

    final DataInputStream din = this.connectorService.openDataInputStream( Protocols.HTTP.url() );

    assertEquals( din.read(), 1 );
    assertEquals( din.available(), 1001 );
  }

  /**
   * @throws Exception
   */
  @Test
  public void openDataOutputStream() throws Exception
  {
    final MyConnectionFactory factory = create( new String[] { Protocols.HTTP.protocol() }, 1 );

    final StreamConnection conn = mock( StreamConnection.class );

    final Semaphore semaphore = new Semaphore( 0 );
    final OutputStream out = new OutputStream()
    {
      @Override
      public void write( final int b ) throws IOException
      {
        semaphore.release();
      }
    };
    when( conn.openOutputStream() ).thenReturn( out );

    registerConnectionFactory( factory );

    factory.setConnection( conn );

    final DataOutputStream dout = this.connectorService.openDataOutputStream( Protocols.HTTP.url() );
    dout.write( 0 );

    assertTrue( "DataOutputStream.write never called?!", semaphore.tryAcquire( 1, TimeUnit.SECONDS ) );
  }

  /**
   * @throws Exception
   */
  @Test
  public void openInputStream() throws Exception
  {
    final MyConnectionFactory factory = create( new String[] { Protocols.HTTP.protocol() }, 1 );

    final StreamConnection conn = mock( StreamConnection.class );
    final InputStream in = new InputStream()
    {
      @Override
      public int available() throws IOException
      {
        return 1001;
      }

      @Override
      public int read() throws IOException
      {
        return 1;
      }
    };
    when( conn.openInputStream() ).thenReturn( in );

    registerConnectionFactory( factory );

    factory.setConnection( conn );

    final InputStream din = this.connectorService.openInputStream( Protocols.HTTP.url() );

    assertEquals( din.read(), 1 );
    assertEquals( din.available(), 1001 );
    assertEquals( in, din );
  }

  /**
   * @throws Exception
   */
  @Test
  public void openOutputStream() throws Exception
  {
    final MyConnectionFactory factory = create( new String[] { Protocols.HTTP.protocol() }, 1 );

    final StreamConnection conn = mock( StreamConnection.class );

    final Semaphore semaphore = new Semaphore( 0 );
    final OutputStream out = new OutputStream()
    {
      @Override
      public void write( final int b ) throws IOException
      {
        semaphore.release();
      }
    };
    when( conn.openOutputStream() ).thenReturn( out );

    registerConnectionFactory( factory );

    factory.setConnection( conn );

    final OutputStream dout = this.connectorService.openOutputStream( Protocols.HTTP.url() );
    dout.write( 0 );

    assertTrue( "OpenOutputStream.write never called?!", semaphore.tryAcquire( 1, TimeUnit.SECONDS ) );
  }

  /**
   * @throws Exception
   */
  @Test
  public void openString() throws Exception
  {
    final MyConnectionFactory factory1 = create( new String[] { Protocols.HTTP.protocol() }, 1 );
    registerConnectionFactory( factory1, "(io.scheme=http)" );

    final MyConnectionFactory factory2 = create( new String[] { Protocols.FTP.protocol() }, 2 );
    registerConnectionFactory( factory2, "(io.scheme=ftp)" );

    final Connection returnedConn1 = this.connectorService.open( Protocols.HTTP.url() );
    assertEquals( factory1.getConnection(), returnedConn1 );

    final Connection returnedConn2 = this.connectorService.open( Protocols.FTP.url() );
    assertEquals( factory2.getConnection(), returnedConn2 );
  }

  /**
   * @throws Exception
   */
  @Test
  public void openStringCFWithTwoSchemes() throws Exception
  {
    final MyConnectionFactory factory = create( new String[] { Protocols.FTP.protocol(), Protocols.HTTP.protocol() },
        1 );
    registerConnectionFactory( factory );

    Connection returnedConn = this.connectorService.open( Protocols.HTTP.url() );
    assertEquals( factory.getConnection(), returnedConn );

    returnedConn = this.connectorService.open( Protocols.FTP.url() );
    assertEquals( factory.getConnection(), returnedConn );
  }

  /**
   * @throws Exception
   */
  @Test( expected = ConnectionNotFoundException.class )
  public void openStringFailsNoScheme() throws Exception
  {
    // wrong protocol
    final MyConnectionFactory factory = create( new String[] { Protocols.FTP.protocol() }, 1 );
    registerConnectionFactory( factory, "(io.schema=ftp)" );

    this.connectorService.open( "www.acme.com" );
  }

  /**
   * @throws Exception
   */
  @Test( expected = ConnectionNotFoundException.class )
  public void openStringFailsWrongProtocol() throws Exception
  {
    // wrong protocol
    final MyConnectionFactory factory = create( new String[] { Protocols.FTP.protocol() }, 1 );
    registerConnectionFactory( factory, "(io.schema=ftp)" );

    this.connectorService.open( Protocols.HTTP.url() );
  }

  /**
   * @throws Exception
   */
  @Test
  public void openStringWithModeAndTimoutsFalse() throws Exception
  {
    openStringWithModeAndTimouts( false );
  }

  /**
   * @throws Exception
   */
  @Test
  public void openStringWithModeAndTimoutsTrue() throws Exception
  {
    openStringWithModeAndTimouts( true );
  }

  /**
   * @throws Exception
   */
  @Test
  public void openStringWithModeRead() throws Exception
  {
    openStringWithMode( ConnectorService.READ );
  }

  /**
   * @throws Exception
   */
  @Test
  public void openStringWithModeReadWrite() throws Exception
  {
    openStringWithMode( ConnectorService.READ_WRITE );
  }

  /**
   * @throws Exception
   */
  @Test
  public void openStringWithModeWrite() throws Exception
  {
    openStringWithMode( ConnectorService.WRITE );
  }

  @SuppressWarnings( "unchecked" )
  @Before
  public void setUp()
  {
    this.context = mock( BundleContext.class );

    final Answer<ServiceRegistration<?>> csAnswer = new Answer<ServiceRegistration<?>>()
    {
      @Override
      public ServiceRegistration<?> answer( final InvocationOnMock aInvocation ) throws Throwable
      {
        ConnectorServiceImplTest.this.connectorService = ( ConnectorServiceImpl )aInvocation.getArguments()[1];
        return mock( ServiceRegistration.class );
      }
    };

    when( this.context.registerService( matches( ConnectorService.class.getName() ), anyObject(),
        ( Dictionary<String, ?> )isNull() ) ).thenAnswer( csAnswer );

    this.connectorService = new ConnectorServiceImpl( this.context );

    this.pid = 0;
  }

  /**
   *
   */
  @Test
  public void testDetermineSchemeName()
  {
    assertEquals( "sms", ConnectorServiceImpl.determineSchemeName( "sms://+31612345678;expiry=24h;reply=yes;type=9" ) );
    assertEquals( "datagram", ConnectorServiceImpl.determineSchemeName( "datagram://:53" ) );
    assertEquals( "socket", ConnectorServiceImpl.determineSchemeName( "socket://www.acme.com:5302" ) );
    assertEquals( "comm", ConnectorServiceImpl.determineSchemeName( "comm://COM1;baudrate=9600;databits=9" ) );
    assertEquals( "file", ConnectorServiceImpl.determineSchemeName( "file:C:/autoexec.bat" ) );
  }

  /**
   * @param aSchemes
   * @param aRanking
   * @return
   * @throws MalformedURLException
   */
  private MyConnectionFactory create( final String[] aSchemes, final int aRanking ) throws MalformedURLException
  {
    final Connection conn = mock( Connection.class );

    final MyConnectionFactory factory = new MyConnectionFactory();
    factory.setConnection( conn );

    final ServiceReference<MyConnectionFactory> ref = create( aSchemes, factory, aRanking );
    factory.setServiceReference( ref );

    return factory;
  }

  @SuppressWarnings( "unchecked" )
  private ServiceReference<MyConnectionFactory> create( final String[] aSchemes, final MyConnectionFactory aFactory,
      final int aRanking )
  {
    final ServiceReference<MyConnectionFactory> ref = mock( ServiceReference.class );

    when( ref.getProperty( Constants.SERVICE_RANKING ) ).thenReturn( Integer.valueOf( aRanking ) );
    when( ref.getProperty( ConnectionFactory.IO_SCHEME ) ).thenReturn( aSchemes );
    when( ref.getProperty( Constants.SERVICE_PID ) ).thenReturn( Integer.valueOf( ++this.pid ) );

    aFactory.setServiceReference( ref );

    return ref;
  }

  /**
   * @param mode
   * @throws Exception
   */
  private void openStringWithMode( final int mode ) throws Exception
  {
    final MyConnectionFactory factory = create( new String[] { Protocols.HTTP.protocol() }, 1 );
    registerConnectionFactory( factory );

    assertEquals( factory.getConnection(), this.connectorService.open( Protocols.HTTP.url(), mode ) );

  }

  /**
   * @param timeouts
   * @throws Exception
   */
  private void openStringWithModeAndTimouts( final boolean timeouts ) throws Exception
  {
    final MyConnectionFactory factory = create( new String[] { Protocols.HTTP.protocol() }, 1 );
    registerConnectionFactory( factory );

    assertEquals( factory.getConnection(),
        this.connectorService.open( Protocols.HTTP.url(), ConnectorService.READ, timeouts ) );
  }

  @SuppressWarnings( "unchecked" )
  private void registerConnectionFactory( final MyConnectionFactory... aFactories ) throws InvalidSyntaxException
  {
    final ServiceReference<MyConnectionFactory>[] serviceRefs = new ServiceReference[aFactories.length];
    int i = 0;
    for ( MyConnectionFactory factory : aFactories )
    {
      serviceRefs[i] = factory.getServiceReference();
      when( this.context.getService( serviceRefs[i] ) ).thenReturn( factory );
      i++;
    }

    when( this.context.getAllServiceReferences( eq( ConnectionFactory.class.getName() ), anyString() ) )
    .thenReturn( serviceRefs );
  }

  /**
   * @param aFactories
   * @throws InvalidSyntaxException
   */
  private void registerConnectionFactory( final MyConnectionFactory aFactory, final String aFilter )
      throws InvalidSyntaxException
  {
    when( this.context.getService( aFactory.getServiceReference() ) ).thenReturn( aFactory );

    when( this.context.getAllServiceReferences( eq( ConnectionFactory.class.getName() ), eq( aFilter ) ) )
    .thenReturn( new ServiceReference[] { aFactory.getServiceReference() } );
  }
}
