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
package nl.lxtreme.ols.client.actionmanager;


import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.action.SmartJumpAction.JumpDirection;
import nl.lxtreme.ols.client.signaldisplay.*;
import nl.lxtreme.ols.common.*;


/**
 * Provides a factory for action managers, prefilled with all actions.
 */
public final class ActionManagerFactory
{
  // CONSTRUCTORS

  /**
   * Creates a new ActionManagerFactory instance, never used.
   */
  private ActionManagerFactory()
  {
    // NO-op
  }

  // METHODS

  /**
   * Fills the given action manager with all its actions.
   * 
   * @param aActionManager
   *          the action manager to fill, cannot be <code>null</code>;
   * @param aController
   *          the controller to use for the actions, cannot be <code>null</code>
   *          .
   */
  public static void fillActionManager( ActionManager aActionManager, ClientController aController,
      SignalDiagramController aDiagramController )
  {
    aActionManager.add( new NewProjectAction( aController ) );
    aActionManager.add( new OpenProjectAction( aController ) );
    aActionManager.add( new SaveProjectAction( aController ) ).setEnabled( false );
    aActionManager.add( new SaveProjectAsAction( aController ) ).setEnabled( false );
    aActionManager.add( new OpenDataFileAction( aController ) );
    aActionManager.add( new SaveDataFileAction( aController ) ).setEnabled( false );
    aActionManager.add( new ExitAction( aController ) );

    aActionManager.add( new CaptureAction( aController ) ).setEnabled( false );
    aActionManager.add( new CancelCaptureAction( aController ) ).setEnabled( false );
    aActionManager.add( new RepeatCaptureAction( aController ) ).setEnabled( false );

    aActionManager.add( new ZoomInAction( aDiagramController ) ).setEnabled( false );
    aActionManager.add( new ZoomOutAction( aDiagramController ) ).setEnabled( false );
    aActionManager.add( new ZoomOriginalAction( aDiagramController ) ).setEnabled( false );
    aActionManager.add( new ZoomAllAction( aDiagramController ) ).setEnabled( false );

    aActionManager.add( new SmartJumpAction( JumpDirection.LEFT, aDiagramController ) ).setEnabled( false );
    aActionManager.add( new SmartJumpAction( JumpDirection.RIGHT, aDiagramController ) ).setEnabled( false );

    aActionManager.add( new GotoTriggerAction( aController ) ).setEnabled( false );
    for ( int c = 0; c < OlsConstants.MAX_CURSORS; c++ )
    {
      aActionManager.add( new GotoNthCursorAction( aDiagramController, c ) ).setEnabled( false );
    }
    aActionManager.add( new GotoFirstCursorAction( aDiagramController ) ).setEnabled( false );
    aActionManager.add( new GotoLastCursorAction( aDiagramController ) ).setEnabled( false );
    aActionManager.add( new DeleteAllCursorsAction( aDiagramController ) ).setEnabled( false );
    aActionManager.add( new SetCursorSnapModeAction( aDiagramController ) );
    aActionManager.add( new SetCursorModeAction( aDiagramController ) );
    aActionManager.add( new RemoveAnnotationsAction( aController ) ).setEnabled( false );
    aActionManager.add( new SetMeasurementModeAction( aDiagramController ) ).setEnabled( false );

    aActionManager.add( new ShowManagerViewAction( aDiagramController ) );
    aActionManager.add( new ShowPreferencesDialogAction( aController ) );

    aActionManager.add( new HelpAboutAction( aController ) );
    aActionManager.add( new ShowBundlesAction( aController ) );
  }
}
