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
 * Copyright (C) 2010-2014 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client2.views.managed;


import static nl.lxtreme.ols.client2.ClientConstants.*;

import java.util.*;

import javax.swing.*;

import nl.lxtreme.ols.client2.views.*;
import nl.lxtreme.ols.common.acquisition.*;
import nl.lxtreme.ols.util.swing.*;

import org.osgi.framework.*;
import org.osgi.service.event.*;

import com.jidesoft.docking.*;


/**
 * Provides a base implementation of {@link ManagedView}.
 */
public abstract class AbstractManagedView extends JPanel implements ManagedView, EventHandler
{
  // CONSTANTS

  private static final long serialVersionUID = 1L;

  private static final int INIT_X = 10;
  private static final int INIT_Y = 10;

  // VARIABLES

  // Injected by Felix DM...
  private volatile BundleContext context;
  // Locally managed...
  private volatile ServiceRegistration serviceReg;

  private final String id;

  // CONSTRUCTORS

  /**
   * Creates a new {@link AbstractManagedView} instance.
   */
  public AbstractManagedView( String aID )
  {
    this.id = aID;
  }

  // METHODS

  /**
   * {@inheritDoc}
   */
  @Override
  public void addNotify()
  {
    super.addNotify();

    build();

    revalidate();
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public String getId()
  {
    return this.id;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final void handleEvent( Event aEvent )
  {
    String topic = aEvent.getTopic();
    if ( !handleEvent( topic, aEvent ) && topic.startsWith( TOPIC_CLIENT_STATE_BASE ) )
    {
      ViewController viewController = ( ViewController )aEvent.getProperty( CS_CONTROLLER );

      updateState( viewController );
    }
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final void initialize( DockableFrame aFrame )
  {
    Properties props = new Properties();
    props.put( EventConstants.EVENT_TOPIC, getEventTopics() );
    String eventFilter = getEventFilter();
    if ( eventFilter != null && !"".equals( eventFilter.trim() ) )
    {
      props.put( EventConstants.EVENT_FILTER, eventFilter );
    }

    this.serviceReg = this.context.registerService( EventHandler.class.getName(), this, props );

    aFrame.setDefaultEscapeAction( DockableFrame.ESCAPE_ACTION_DO_NOTING );

    DockContext context = aFrame.getContext();
    context.setInitPosition( true );
    if ( !UIManager.getBoolean( UIMgr.SHOW_TOOL_WINDOWS_DEFAULT ) )
    {
      context.setCurrentMode( DockContext.STATE_HIDDEN );
    }

    doInitialize( aFrame, context );

    String name = getName();
    aFrame.setKey( this.id );
    aFrame.setTabTitle( name );
    aFrame.setSideTitle( name );

    aFrame.getContentPane().add( this );
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public final void updateState( final ViewController aController )
  {
    SwingComponentUtils.invokeOnEDT( new Runnable()
    {
      @Override
      public void run()
      {
        if ( aController == null )
        {
          doUpdateState( null, null );
        }
        else
        {
          ViewModel model = aController.getModel();
          if ( model != null )
          {
            doUpdateState( aController, model.getData() );
          }
        }
      }
    } );
  }

  /**
   * Builds the actual view by placing all of its components.
   */
  protected abstract void build();

  /**
   * Further initializes the given dockable frame.
   */
  protected void doInitialize( DockableFrame aFrame, DockContext aContext )
  {
    // Nop
  }

  /**
   * Updates the state of this view.
   * <p>
   * This method is called always from the EDT.
   * </p>
   * 
   * @param aController
   *          the view controller to use, can be <code>null</code>;
   * @param aData
   *          the acquired data to use, can only be <code>null</code> if
   *          aController is also <code>null</code>.
   */
  protected abstract void doUpdateState( ViewController aController, AcquisitionData aData );

  /**
   * @return
   */
  protected String getEventFilter()
  {
    return "";
  }

  /**
   * @return
   */
  protected String[] getEventTopics()
  {
    return new String[] { TOPIC_CLIENT_STATE_BASE.concat( "/*" ) };
  }

  /**
   * @param aTopic
   * @param aEvent
   * @return
   */
  protected boolean handleEvent( String aTopic, Event aEvent )
  {
    return false;
  }

  /**
   * @param aComponent
   *          the component to layout, cannot be <code>null</code>.
   */
  protected void makeEditorGrid( JComponent aComponent )
  {
    SpringLayoutUtils.makeEditorGrid( aComponent, INIT_X, INIT_Y );
  }

  /**
   * Called by Felix DM when stopping this component.
   */
  protected void stop()
  {
    if ( this.serviceReg != null )
    {
      this.serviceReg.unregister();
      this.serviceReg = null;
    }
  }
}
