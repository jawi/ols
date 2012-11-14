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
package nl.lxtreme.ols.client.action.manager;


import nl.lxtreme.ols.client.action.*;
import nl.lxtreme.ols.client.action.SmartJumpAction.JumpDirection;
import nl.lxtreme.ols.common.*;


/**
 * Provides a factory for action managers, prefilled with all actions.
 */
public final class ActionManagerFactory
{
  // CONSTRUCTORS

  /**
   * Creates a new {@link ActionManagerFactory} instance, not used.
   */
  private ActionManagerFactory()
  {
    // NO-op
  }

  // METHODS

  /**
   * Creates the various actions and registers them with the given action
   * manager.
   */
  public static void createActions( final ActionManager aActionManager )
  {
    aActionManager.add( new NewProjectAction() );
    aActionManager.add( new OpenProjectAction() );
    aActionManager.add( new SaveProjectAction() );
    aActionManager.add( new SaveProjectAsAction() );
    aActionManager.add( new OpenDataFileAction() );
    aActionManager.add( new SaveDataFileAction() );
    aActionManager.add( new ExitAction() );

    aActionManager.add( new CaptureAction() );
    aActionManager.add( new CancelCaptureAction() );
    aActionManager.add( new RepeatCaptureAction() );

    aActionManager.add( new ZoomInAction() ).setEnabled( false );
    aActionManager.add( new ZoomOutAction() ).setEnabled( false );
    aActionManager.add( new ZoomOriginalAction() ).setEnabled( false );
    aActionManager.add( new ZoomAllAction() ).setEnabled( false );

    aActionManager.add( new SmartJumpAction( JumpDirection.LEFT ) ).setEnabled( false );
    aActionManager.add( new SmartJumpAction( JumpDirection.RIGHT ) ).setEnabled( false );

    aActionManager.add( new GotoTriggerAction() ).setEnabled( false );
    for ( int c = 0; c < Ols.MAX_CURSORS; c++ )
    {
      aActionManager.add( new GotoNthCursorAction( c ) ).setEnabled( false );
    }
    aActionManager.add( new GotoFirstCursorAction() ).setEnabled( false );
    aActionManager.add( new GotoLastCursorAction() ).setEnabled( false );
    aActionManager.add( new DeleteAllCursorsAction() ).setEnabled( false );
    aActionManager.add( new SetCursorSnapModeAction() );
    aActionManager.add( new SetCursorModeAction() );
    aActionManager.add( new RemoveAnnotationsAction() ).setEnabled( false );
    aActionManager.add( new SetMeasurementModeAction() ).setEnabled( false );

    aActionManager.add( new ShowPreferencesDialogAction() );

    aActionManager.add( new HelpAboutAction() );
    aActionManager.add( new ShowBundlesAction() );
  }
}
