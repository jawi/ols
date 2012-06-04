/*
 *
 */
package nl.lxtreme.ols.client.signaldisplay.laf;


import static nl.lxtreme.ols.util.ColorUtils.*;

import java.awt.*;

import javax.swing.*;

import nl.lxtreme.ols.client.signaldisplay.model.SignalDiagramModel.SignalAlignment;


/**
 * Provides the default L&F for the component.
 */
public final class LafDefaults
{
  // GENERIC

  /** Generic background color */
  public static final Color DEFAULT_BACKGROUND_COLOR = parseColor( "#1E2126" );

  /** The key for the default label font as used by Swing. */
  public static final String SWING_LABEL_FONT = "Label.font";

  /** The color to use for the trigger. */
  public static final Color DEFAULT_TRIGGER_COLOR = Color.WHITE;

  /** Default vertical spacing between two signal elements. */
  public static final int DEFAULT_SIGNAL_ELEMENT_SPACING = 4;

  // CHANNEL LABELS

  /** Background color for the channel labels. */
  public static final Color DEFAULT_CHANNEL_BACKGROUND_COLOR = parseColor( "#2E323B" );
  /** Foreground color for the channel labels. */
  public static final Color DEFAULT_CHANNEL_LABEL_COLOR = Color.WHITE;
  /** Channel label font. */
  public static final Font DEFAULT_CHANNEL_LABEL_FONT = deriveFont( SWING_LABEL_FONT, Font.BOLD );
  /** Minimal width of all channel labels. */
  public static final int DEFAULT_MINIMAL_CHANNEL_WIDTH = 40;
  /**
   * The default width of the gutter (= space between channel labels and
   * signals).
   */
  public static final int DEFAULT_GUTTER_WIDTH = 15;
  /** The default arc width. */
  public static final int DEFAULT_ARC_WIDTH = 12;

  // SIGNAL VIEW

  /** Measurement arrow color. */
  public static final Color DEFAULT_ARROW_COLOR = Color.WHITE;
  /** The default color of a signal. */
  public static final Color DEFAULT_SIGNAL_COLOR = parseColor( "7bf9dd" );
  /** Default height of a signal. */
  public static final SignalAlignment DEFAULT_SIGNAL_ALIGNMENT = SignalAlignment.CENTER;
  /** Default height of a channel. */
  public static final int DEFAULT_CHANNEL_HEIGHT = 36;
  /** Default height of a signal. */
  public static final int DEFAULT_SIGNAL_HEIGHT = 22;
  /** Default height of a signal group */
  public static final int DEFAULT_SIGNAL_GROUP_HEIGHT = 20;
  /** Default height of a analog scope. */
  public static final int DEFAULT_ANALOG_SCOPE_HEIGHT = 96;
  /** Default height of a group summary. */
  public static final int DEFAULT_GROUP_SUMMARY_HEIGHT = 30;

  // GLASS PANE

  /** Color used when rendering items during DnD. */
  public static final Color DEFAULT_GLASSPANE_COLOR = Color.YELLOW;
  /** Alpha percentage (0..100) used when rendering items during DnD. */
  public static final int DEFAULT_GLASSPANE_ALPHA_PERCENTAGE = 70;

  // TIMELINE

  /** Timeline height. */
  public static final int DEFAULT_TIMELINE_HEIGHT = 38;
  /** Timeline text color. */
  public static final Color DEFAULT_TEXT_COLOR = Color.LIGHT_GRAY;
  /** The height (in px) of the smallest timeline ticks. */
  public static final int DEFAULT_TICK_HEIGHT = 4;
  /** The color of the smallest ticks. */
  public static final Color DEFAULT_TICK_COLOR = Color.LIGHT_GRAY.darker();
  /** The color of the major ticks. */
  public static final Color DEFAULT_MAJOR_TICK_COLOR = Color.LIGHT_GRAY;
  /** The font of the major tick time labels. */
  public static final Font DEFAULT_MAJOR_TICK_FONT = deriveFont( SWING_LABEL_FONT, 0.9f, Font.PLAIN );
  /** The height (in px) of the major timeline ticks. */
  public static final int DEFAULT_MAJOR_TICK_HEIGHT = 3 * DEFAULT_TICK_HEIGHT;
  /** The color of the minor ticks. */
  public static final Color DEFAULT_MINOR_TICK_COLOR = Color.LIGHT_GRAY;
  /** The font of the minor tick time labels. */
  public static final Font DEFAULT_MINOR_TICK_FONT = deriveFont( SWING_LABEL_FONT, 0.8f, Font.PLAIN );
  /** The height (in px) of the minor timeline ticks. */
  public static final int DEFAULT_MINOR_TICK_HEIGHT = 2 * DEFAULT_TICK_HEIGHT;

  // CURSORS

  /** The font of the cursor flags. */
  public static final Font DEFAULT_CURSOR_FLAG_FONT = deriveFont( SWING_LABEL_FONT, 0.8f, Font.PLAIN );
  /** The default color of each cursor. */
  public static final Color DEFAULT_CURSOR_COLOR = parseColor( "7bf9dd" ).brighter();

  // CONSTRUCTORS

  /**
   * Creates a new LafDefaults instance. Not used.
   */
  private LafDefaults()
  {
  }

  // METHODS

  /**
   * @param aKey
   * @param aStyleMask
   * @return
   */
  private static Font deriveFont( final String aKey, final float aFactor, final int aStyleMask )
  {
    return deriveScaledFont( aKey, aFactor ).deriveFont( aStyleMask );
  }

  /**
   * @param aKey
   * @param aStyleMask
   * @return
   */
  private static Font deriveFont( final String aKey, final int aStyleMask )
  {
    return ( ( Font )UIManager.get( aKey ) ).deriveFont( aStyleMask );
  }

  /**
   * @param aKey
   * @param aFactor
   * @return
   */
  private static Font deriveScaledFont( final String aKey, final float aFactor )
  {
    final Font baseFont = ( Font )UIManager.get( aKey );
    return baseFont.deriveFont( baseFont.getSize() * aFactor );
  }
}
