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
 * Copyright (C) 2010 J.W. Janssen, www.lxtreme.nl
 */
package nl.lxtreme.ols.client.action.manager;


import nl.lxtreme.ols.api.data.*;
import nl.lxtreme.ols.client.*;
import nl.lxtreme.ols.client.action.*;


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
   * Creates a new action manager instance, filled with all actions.
   * 
   * @param aController
   *          the controller to use for the actions, cannot be <code>null</code>
   *          .
   */
  public static IActionManager createActionManager( final IClientController aController )
  {
    final ActionManager actionManager = new ActionManager();
    fillActionManager( actionManager, aController );
    return actionManager;
  }

  /**
   * Fills the given action manager with all its actions.
   * 
   * @param aActionManager
   *          the action manager to fill, cannot be <code>null</code>;
   * @param aController
   *          the controller to use for the actions, cannot be <code>null</code>
   *          .
   */
  private static void fillActionManager( final ActionManager aActionManager, final IClientController aController )
  {
    aActionManager.add( new NewProjectAction( aController ) );
    aActionManager.add( new OpenProjectAction( aController ) );
    aActionManager.add( new SaveProjectAction( aController ) ).setEnabled( false );
    aActionManager.add( new SaveProjectAsAction( aController ) ).setEnabled( false );
    aActionManager.add( new OpenDataFileAction( aController ) );
    aActionManager.add( new SaveDataFileAction( aController ) ).setEnabled( false );
    aActionManager.add( new ExitAction( aController ) );

    aActionManager.add( new CaptureAction( aController ) );
    aActionManager.add( new CancelCaptureAction( aController ) ).setEnabled( false );
    aActionManager.add( new RepeatCaptureAction( aController ) ).setEnabled( false );

    aActionManager.add( new ZoomInAction( aController ) ).setEnabled( false );
    aActionManager.add( new ZoomOutAction( aController ) ).setEnabled( false );
    aActionManager.add( new ZoomDefaultAction( aController ) ).setEnabled( false );
    aActionManager.add( new ZoomFitAction( aController ) ).setEnabled( false );

    aActionManager.add( new GotoTriggerAction( aController ) ).setEnabled( false );
    for ( int c = 0; c < CapturedData.MAX_CURSORS; c++ )
    {
      aActionManager.add( new GotoNthCursorAction( aController, c ) ).setEnabled( false );
    }
    aActionManager.add( new GotoFirstCursorAction( aController ) ).setEnabled( false );
    aActionManager.add( new GotoLastCursorAction( aController ) ).setEnabled( false );
    aActionManager.add( new ClearCursors( aController ) ).setEnabled( false );
    aActionManager.add( new SetCursorModeAction( aController ) );
    for ( int c = 0; c < CapturedData.MAX_CURSORS; c++ )
    {
      aActionManager.add( new SetCursorAction( aController, c ) );
    }

    aActionManager.add( new ShowPreferencesDialogAction( aController ) );
    aActionManager.add( new ShowDiagramModeSettingsDialogAction( aController ) );
    aActionManager.add( new ShowChannelLabelsDialogAction( aController ) );

    aActionManager.add( new HelpAboutAction( aController ) );
    aActionManager.add( new ShowBundlesAction( aController ) );
  }
}
