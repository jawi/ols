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


import nl.lxtreme.ols.api.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.action.SmartJumpAction.*;
import nl.lxtreme.ols.client.signaldisplay.*;


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
  public static void fillActionManager( final ActionManager aActionManager, final ClientController aController )
  {
    final SignalDiagramController signalDiagramController = aController.getSignalDiagramController();

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

    aActionManager.add( new ZoomInAction( signalDiagramController ) ).setEnabled( false );
    aActionManager.add( new ZoomOutAction( signalDiagramController ) ).setEnabled( false );
    aActionManager.add( new ZoomOriginalAction( signalDiagramController ) ).setEnabled( false );
    aActionManager.add( new ZoomAllAction( signalDiagramController ) ).setEnabled( false );

    aActionManager.add( new SmartJumpAction( JumpDirection.LEFT, aController ) ).setEnabled( false );
    aActionManager.add( new SmartJumpAction( JumpDirection.RIGHT, aController ) ).setEnabled( false );

    aActionManager.add( new GotoTriggerAction( aController ) ).setEnabled( false );
    for ( int c = 0; c < Ols.MAX_CURSORS; c++ )
    {
      aActionManager.add( new GotoNthCursorAction( signalDiagramController, c ) ).setEnabled( false );
    }
    aActionManager.add( new GotoFirstCursorAction( signalDiagramController ) ).setEnabled( false );
    aActionManager.add( new GotoLastCursorAction( signalDiagramController ) ).setEnabled( false );
    aActionManager.add( new DeleteAllCursorsAction( signalDiagramController ) ).setEnabled( false );
    aActionManager.add( new SetCursorSnapModeAction( signalDiagramController ) );
    aActionManager.add( new SetCursorModeAction( signalDiagramController ) );
    aActionManager.add( new RemoveAnnotationsAction( aController ) ).setEnabled( false );
    aActionManager.add( new SetMeasurementModeAction( signalDiagramController ) ).setEnabled( false );

    aActionManager.add( new ShowManagerViewAction( aController ) );
    aActionManager.add( new ShowPreferencesDialogAction( aController ) );

    aActionManager.add( new HelpAboutAction( aController ) );
    aActionManager.add( new ShowBundlesAction( aController ) );
  }
}
