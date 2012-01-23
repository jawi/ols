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
package nl.lxtreme.ols.tool.asm45;


import static nl.lxtreme.ols.util.NumberUtils.*;
import nl.lxtreme.ols.api.acquisition.*;
import nl.lxtreme.ols.api.data.annotation.AnnotationListener;
import nl.lxtreme.ols.api.tools.*;


/**
 * Does all the necessary decoding work for the plugin
 * 
 * @author Ansgar Kueckes
 */
@SuppressWarnings( "boxing" )
public class Asm45AnalyserTask implements ToolTask<Asm45DataSet>
{
  // CONSTANTS

  /**
   * HP9845 hybrid processor register symbols
   */
  private static final String[] registers = { "A", // arithmetic accumulator A
      "B", // arithmetic accumulator B
      "P", // program counter
      "R", // return stack pointer
      "R4", // I/O register 4
      "R5", // I/O register 5
      "R6", // I/O register 6
      "R7", // I/O register 7
      "R10", // pointer to interrupt vector table
      "Pa", // peripheral address register (lower 4 bits effective)
      "W", // "W-Register" (working register, reserved)
      "Dmapa", // DMA peripheral address register (lower 4 bits effective,
               // bit15=Db, bit16=Cb)
      "Dmama", // DMA memory address register
      "Dmac", // DMA count register
      "C", // stack pointer C
      "D", // stack pointer D
      "Ar2", // BCD arithmetic accumulator
      "Ar2_2", // "       " "
      "Ar2_3", // "       " "
      "Ar2_4", // "       " "
      "Se", // shift-extend register
      "R25", // -reserved-
      "R26", // -reserved-
      "R27", // -reserved-
      "R30", // -reserved- used by '45 assembler for extend/carry (least
             // significant bit, reserved)
      "R31", // -reserved- used by '45 assembler for overflow (least significant
             // bit, reserved)
      "R32", // Indirect memory access for upper half of address space (octal
             // 100000-177777), fixed to block 0 for 9845A LPU
      "R33", // Instruction fetch for lower half of address space (octal
             // 000000-077777), fixed to block 3 for 9845A LPU
      "R34", // Instruction fetch for upper half of address space (octal
             // 100000-177777), instruction working block for 9845A LPU
      "R35", // Indirect memory access for lower half of address space (octal
             // 000000-077777), indirect memory acces working block for 9845A
             // LPU
      "R36", // Base page addressing, fixed to block 1 for PPU and home/working
             // block for LPU on 9845A
      "R37" // Bus grant (DMA)
  };

  /**
   * HP9845 hybrid processor instructions<br/>
   * Note: RAL n = RAR 16-n, RBL n = RBR 16-n
   */
  private static final Asm45OpcodeTable[] hp9845Table = {
      // pseudo operations
      new Asm45OpcodeTable( 0xffff, 0x0000, "NOP", 0, 11 ), // = LDA A
      new Asm45OpcodeTable( 0xffff, 0xf14f, "CLA", 0, 11 ), // = SAR 16
      new Asm45OpcodeTable( 0xffff, 0xf94f, "CLB", 0, 11 ), // = SBR 16

      // BPC memory reference group
      new Asm45OpcodeTable( 0x7800, 0x0000, "LDA", 1, 13 ), //
      new Asm45OpcodeTable( 0x7800, 0x0800, "LDB", 1, 13 ), //
      new Asm45OpcodeTable( 0x7800, 0x1000, "CPA", 1, 16 ), //
      new Asm45OpcodeTable( 0x7800, 0x1800, "CPB", 1, 16 ), //
      new Asm45OpcodeTable( 0x7800, 0x2000, "ADA", 1, 13 ), //
      new Asm45OpcodeTable( 0x7800, 0x2800, "ADB", 1, 13 ), //
      new Asm45OpcodeTable( 0x7800, 0x3000, "STA", 1, 13 ), //
      new Asm45OpcodeTable( 0x7800, 0x3800, "STB", 1, 13 ), //
      new Asm45OpcodeTable( 0x7800, 0x4000, "JSM", 1, 17 ), //
      new Asm45OpcodeTable( 0x7800, 0x4800, "ISZ", 1, 19 ), //
      new Asm45OpcodeTable( 0x7800, 0x5000, "AND", 1, 13 ), //
      new Asm45OpcodeTable( 0x7800, 0x5800, "DSZ", 1, 19 ), //
      new Asm45OpcodeTable( 0x7800, 0x6000, "IOR", 1, 13 ), //
      new Asm45OpcodeTable( 0x7800, 0x6800, "JMP", 1, 8 ), //
      new Asm45OpcodeTable( 0x7fe0, 0x7000, "EXE", 2, 8 ), //

      // BPC skip group
      new Asm45OpcodeTable( 0xffc0, 0x7400, "RZA", 3, 14 ), //
      new Asm45OpcodeTable( 0xffc0, 0x7c00, "RZB", 3, 14 ), //
      new Asm45OpcodeTable( 0xffc0, 0x7440, "RIA", 3, 14 ), //
      new Asm45OpcodeTable( 0xffc0, 0x7c40, "RIB", 3, 14 ), //
      new Asm45OpcodeTable( 0xffc0, 0x7500, "SZA", 3, 14 ), //
      new Asm45OpcodeTable( 0xffc0, 0x7d00, "SZB", 3, 14 ), //
      new Asm45OpcodeTable( 0xffc0, 0x7540, "SIA", 3, 14 ), //
      new Asm45OpcodeTable( 0xffc0, 0x7d40, "SIB", 3, 14 ), //
      new Asm45OpcodeTable( 0xffc0, 0x7480, "SFS", 3, 14 ), //
      new Asm45OpcodeTable( 0xffc0, 0x7580, "SFC", 3, 14 ), //
      new Asm45OpcodeTable( 0xffc0, 0x74c0, "SDS", 3, 14 ), //
      new Asm45OpcodeTable( 0xffc0, 0x75c0, "SDC", 3, 14 ), //
      new Asm45OpcodeTable( 0xffc0, 0x7c80, "SSS", 3, 14 ), //
      new Asm45OpcodeTable( 0xffc0, 0x7d80, "SSC", 3, 14 ), //
      new Asm45OpcodeTable( 0xffc0, 0x7cc0, "SHS", 3, 14 ), //
      new Asm45OpcodeTable( 0xffc0, 0x7dc0, "SHC", 3, 14 ), //

      // BPC alter group
      new Asm45OpcodeTable( 0xff00, 0x7600, "SLA", 4, 14 ), //
      new Asm45OpcodeTable( 0xff00, 0x7e00, "SLB", 4, 14 ), //
      new Asm45OpcodeTable( 0xff00, 0x7700, "RLA", 4, 14 ), //
      new Asm45OpcodeTable( 0xff00, 0x7f00, "RLB", 4, 14 ), //
      new Asm45OpcodeTable( 0xff00, 0xf400, "SAP", 4, 14 ), //
      new Asm45OpcodeTable( 0xff00, 0xfc00, "SBP", 4, 14 ), //
      new Asm45OpcodeTable( 0xff00, 0xf500, "SAM", 4, 14 ), //
      new Asm45OpcodeTable( 0xff00, 0xfd00, "SBM", 4, 14 ), //
      new Asm45OpcodeTable( 0xff00, 0xf600, "SOC", 4, 14 ), //
      new Asm45OpcodeTable( 0xff00, 0xf700, "SOS", 4, 14 ), //
      new Asm45OpcodeTable( 0xff00, 0xfe00, "SEC", 4, 14 ), //
      new Asm45OpcodeTable( 0xff00, 0xff00, "SES", 4, 14 ), //

      // BPC complement group
      new Asm45OpcodeTable( 0xffff, 0xf020, "TCA", 0, 9 ), //
      new Asm45OpcodeTable( 0xffff, 0xf820, "TCB", 0, 9 ), //
      new Asm45OpcodeTable( 0xffff, 0xf060, "CMA", 0, 9 ), //
      new Asm45OpcodeTable( 0xffff, 0xf860, "CMB", 0, 9 ), //

      new Asm45OpcodeTable( 0xff80, 0xf080, "RET", 5, 16 ), //

      // BPC shift/rotate group
      new Asm45OpcodeTable( 0xfff0, 0xf100, "AAR", 6, 9 ), //
      new Asm45OpcodeTable( 0xfff0, 0xf900, "ABR", 6, 9 ), //
      new Asm45OpcodeTable( 0xfff0, 0xf140, "SAR", 6, 9 ), //
      new Asm45OpcodeTable( 0xfff0, 0xf940, "SBR", 6, 9 ), //
      new Asm45OpcodeTable( 0xfff0, 0xf180, "SAL", 6, 9 ), //
      new Asm45OpcodeTable( 0xfff0, 0xf980, "SBL", 6, 9 ), //
      new Asm45OpcodeTable( 0xfff0, 0xf1c0, "RAR", 6, 9 ), //
      new Asm45OpcodeTable( 0xfff0, 0xf9c0, "RBR", 6, 9 ), //

      // IOC interrupt group
      new Asm45OpcodeTable( 0xffff, 0x7110, "EIR", 0, 12 ), //
      new Asm45OpcodeTable( 0xffff, 0x7118, "DIR", 0, 12 ), //

      // IOC DMA group
      new Asm45OpcodeTable( 0xffff, 0x7100, "SDO", 0, 12 ), //
      new Asm45OpcodeTable( 0xffff, 0x7108, "SDI", 0, 12 ), //
      new Asm45OpcodeTable( 0xffff, 0x7120, "DMA", 0, 12 ), //
      new Asm45OpcodeTable( 0xffff, 0x7128, "PCM", 0, 12 ), //
      new Asm45OpcodeTable( 0xffff, 0x7138, "DDR", 0, 12 ), //

      // IOC stack group
      new Asm45OpcodeTable( 0xffff, 0x7140, "DBL", 0, 12 ), //
      new Asm45OpcodeTable( 0xffff, 0x7148, "CBL", 0, 12 ), //
      new Asm45OpcodeTable( 0xffff, 0x7150, "DBU", 0, 12 ), //
      new Asm45OpcodeTable( 0xffff, 0x7158, "CBU", 0, 12 ), //

      new Asm45OpcodeTable( 0xff78, 0x7160, "PWC", 7, 23 ), //
      new Asm45OpcodeTable( 0xff78, 0x7168, "PWD", 7, 23 ), //
      new Asm45OpcodeTable( 0xff78, 0x7960, "PBC", 7, 23 ), //
      new Asm45OpcodeTable( 0xff78, 0x7968, "PBD", 7, 23 ), //
      new Asm45OpcodeTable( 0xff78, 0x7170, "WWC", 7, 23 ), //
      new Asm45OpcodeTable( 0xff78, 0x7178, "WWD", 7, 23 ), //
      new Asm45OpcodeTable( 0xff78, 0x7970, "WBC", 7, 23 ), //
      new Asm45OpcodeTable( 0xff78, 0x7978, "WBD", 7, 23 ), //

      // EMC four word group
      new Asm45OpcodeTable( 0xfff0, 0x7380, "CLR", 6, 16 ), //
      new Asm45OpcodeTable( 0xfff0, 0x7300, "XFR", 6, 21 ), //

      // EMC mantissa shift group
      new Asm45OpcodeTable( 0xffff, 0x7b00, "MRX", 0, 62 ), //
      new Asm45OpcodeTable( 0xffff, 0x7b21, "DRS", 0, 56 ), //
      new Asm45OpcodeTable( 0xffff, 0x7b61, "MLY", 0, 32 ), //
      new Asm45OpcodeTable( 0xffff, 0x7b40, "MRY", 0, 33 ), //
      new Asm45OpcodeTable( 0xffff, 0x7340, "NRM", 0, 23 ), //

      // EMC arithmetic group
      new Asm45OpcodeTable( 0xffff, 0x7280, "FXA", 0, 40 ), //
      new Asm45OpcodeTable( 0xffff, 0x7200, "MWA", 0, 28 ), //
      new Asm45OpcodeTable( 0xffff, 0x7260, "CMX", 0, 59 ), //
      new Asm45OpcodeTable( 0xffff, 0x7220, "CMY", 0, 23 ), //
      new Asm45OpcodeTable( 0xffff, 0x7a00, "FMP", 0, 42 ), //
      new Asm45OpcodeTable( 0xffff, 0x7a21, "FDV", 0, 37 ), //
      new Asm45OpcodeTable( 0xffff, 0x7b8f, "MPY", 0, 65 ), //
      new Asm45OpcodeTable( 0xffff, 0x73c0, "CDC", 0, 11 ), //
      new Asm45OpcodeTable( 0, 0, null, 0, 0 ) };

  // VARIABLES

  private final ToolContext context;
  private final ToolProgressListener progressListener;

  private int lineSMCmask;
  private int lineSTMmask;
  private int lineEBGmask;
  private int lineBYTEmask;
  private int lineBLmask;
  private int lineWRTmask;
  private int lineSYNCmask;
  private int lineSMCidx;

  private boolean reportInst;
  private boolean reportData;
  private boolean reportBusGrants;

  // CONSTRUCTORS

  /**
   * Creates a new Asm45AnalyserWorker instance.
   * 
   * @param aContext
   * @param aProgressListener
   * @param aAnnotationListener
   */
  public Asm45AnalyserTask( final ToolContext aContext, final ToolProgressListener aProgressListener,
      final AnnotationListener aAnnotationListener )
  {
    this.context = aContext;
    this.progressListener = aProgressListener;
  }

  // METHODS

  /**
   * Asm45 bus decoder core routine.
   * 
   * @see javax.swing.SwingWorker#doInBackground()
   */
  @Override
  public Asm45DataSet call() throws Exception
  {
    final AcquisitionResult data = this.context.getData();

    final int[] values = data.getValues();

    // process the captured data and write to output

    int startOfDecode = this.context.getStartSampleIndex();
    int endOfDecode = this.context.getEndSampleIndex();

    int control; // 16 IDA bus control signals (includes BSC lines)
    int ida; // 16 IDA bus address/data lines

    final Asm45DataSet asm45DataSet = new Asm45DataSet( startOfDecode, endOfDecode, data );

    int idx = asm45DataSet.getStartOfDecode();
    int startIdx = 0;

    int status = 0; // last control lines status

    int clocks = 0; // system clocks consumed for the last instruction
    int block = 0; // 6-bit block address
    int address = 0; // 16-bit address within memory block
    boolean busGrant = false; // bus grant for DMA, CRT cycle steeling etc.

    String type = Asm45Data.TYPE_INSTRUCTION; // type of decoded event
    String event = "???"; // event description (mnemonic etc.)

    /*
     * Loop over the acquisition data
     */
    for ( ; idx < ( asm45DataSet.getEndOfDecode() - 1 ); idx++ )
    {
      final int dataValue = values[idx];

      control = dataValue & 0xffff0000;
      ida = ( ~dataValue & 0x0000ffff );
      clocks++;

      // start memory cycle
      if ( ( ( status & this.lineSTMmask ) != 0 ) && ( ( control & this.lineSTMmask ) == 0 ) )
      {
        startIdx = idx;
        address = ida;
        block = ( ( ( ~control ) >> 16 ) & 0x3f );
      }

      // memory cycle complete
      if ( ( ( status & this.lineSMCmask ) == 0 ) && ( ( control & this.lineSMCmask ) != 0 ) )
      {
        // filter bus grants (like DMA or CRT cycle stealing)
        if ( ( ( status & this.lineEBGmask ) != 0 ) && ( ( control & this.lineEBGmask ) == 0 ) )
        {
          busGrant = true;
          if ( ( status & this.lineBYTEmask ) == 0 )
          {
            type = Asm45Data.TYPE_DATA_WORD;
          }
          else if ( ( status & this.lineBLmask ) == 0 )
          {
            type = Asm45Data.TYPE_DATA_BYTE_RIGHT;
          }
          else
          {
            type = Asm45Data.TYPE_DATA_BYTE_LEFT;
          }
          if ( ( status & this.lineWRTmask ) != 0 )
          {
            if ( address < 32 )
            {
              event = Asm45AnalyserTask.registers[address] + String.format( "&rarr;$%04x", ida );
            }
            else
            {
              event = String.format( "%04x&rarr;$%04x", address, ida );
            }
          }
          else
          {
            if ( address < 32 )
            {
              event = Asm45AnalyserTask.registers[address] + String.format( "&larr;$%04x", ida );
            }
            else
            {
              event = String.format( "%04x&larr;$%04x", address, ida );
            }
          }
        }
        else
        {
          busGrant = false;
          if ( ( status & this.lineSYNCmask ) != 0 )
          {
            // instruction fetch
            type = Asm45Data.TYPE_INSTRUCTION;
            event = word2asm( address, ida );
          }
          else
          {
            // data transfer
            if ( ( status & this.lineBYTEmask ) == 0 )
            {
              type = Asm45Data.TYPE_DATA_WORD;
            }
            else if ( ( status & this.lineBLmask ) == 0 )
            {
              type = Asm45Data.TYPE_DATA_BYTE_RIGHT;
            }
            else
            {
              type = Asm45Data.TYPE_DATA_BYTE_LEFT;
            }
            if ( ( status & this.lineWRTmask ) != 0 )
            {
              if ( address < 32 )
              {
                event = Asm45AnalyserTask.registers[address] + String.format( "&rarr;$%04x", ida );
              }
              else
              {
                event = String.format( "%04x&rarr;$%04x", address, ida );
              }
            }
            else
            {
              if ( address < 32 )
              {
                event = Asm45AnalyserTask.registers[address] + String.format( "&larr;$%04x", ida );
              }
              else
              {
                event = String.format( "%04x&larr;$%04x", address, ida );
              }
            }
          }
        }

        // report the requested event
        if ( ( ( type == Asm45Data.TYPE_INSTRUCTION ) && this.reportInst )
            || ( !busGrant
                && ( ( type == Asm45Data.TYPE_DATA_WORD ) || ( type == Asm45Data.TYPE_DATA_BYTE_LEFT ) || ( type == Asm45Data.TYPE_DATA_BYTE_RIGHT ) ) && this.reportData )
            || ( busGrant && this.reportBusGrants ) )
        {
          reportEvent( asm45DataSet, startIdx, idx, clocks, block, address, ida, busGrant, type, event );
          clocks = 0;
        }
      }

      status = control;

      this.progressListener.setProgress( getPercentage( idx, startOfDecode, endOfDecode ) );
    }

    return asm45DataSet;
  }

  /**
   * @param aLineBLidx
   */
  public void setLineBLIndex( final int aLineBLidx )
  {
    this.lineBLmask = 1 << aLineBLidx;
  }

  /**
   * @param aLineBYTEidx
   */
  public void setLineBYTEIndex( final int aLineBYTEidx )
  {
    this.lineBYTEmask = 1 << aLineBYTEidx;
  }

  /**
   * @param aLineEBGidx
   */
  public void setLineEBGIndex( final int aLineEBGidx )
  {
    this.lineEBGmask = 1 << aLineEBGidx;
  }

  /**
   * @param aLineSMCidx
   */
  public void setLineSMCIndex( final int aLineSMCidx )
  {
    this.lineSMCidx = aLineSMCidx;
    this.lineSMCmask = 1 << aLineSMCidx;
  }

  /**
   * @param aLineSTMidx
   */
  public void setLineSTMIndex( final int aLineSTMidx )
  {
    this.lineSTMmask = 1 << aLineSTMidx;
  }

  /**
   * @param aLineSYNCidx
   */
  public void setLineSYNCIndex( final int aLineSYNCidx )
  {
    this.lineSYNCmask = 1 << aLineSYNCidx;
  }

  /**
   * @param aLineWRTidx
   */
  public void setLineWRTIndex( final int aLineWRTidx )
  {
    this.lineWRTmask = 1 << aLineWRTidx;
  }

  /**
   * @param aReportBusGrants
   */
  public void setReportBusGrants( final boolean aReportBusGrants )
  {
    this.reportBusGrants = aReportBusGrants;
  }

  /**
   * @param aReportData
   */
  public void setReportData( final boolean aReportData )
  {
    this.reportData = aReportData;
  }

  /**
   * @param aReportInst
   */
  public void setReportInst( final boolean aReportInst )
  {
    this.reportInst = aReportInst;
  }

  /**
   * decode event
   * 
   * @param address
   * @param opcode
   * @return event description (assembler instruction or data transfer)
   */
  protected String word2asm( final int address, final int opcode )
  {
    int operand;
    int count;
    int timing = 0;
    int i = 0;
    Asm45OpcodeTable op = hp9845Table[0];
    String ret_string = "";

    /* get mnemonic */
    while ( op.getMnemonic() != null )
    {
      if ( ( opcode & op.getMask() ) == op.getOpcode() )
      {
        break;
      }
      i++;
      op = hp9845Table[i];
    }

    /* if match, write mnemonic - else return */
    if ( op.getMnemonic() == null )
    {
      return "???";
    }

    ret_string = op.getMnemonic();
    timing = op.getTiming();

    switch ( op.getMode() )
    {
      case 0:
        /* no operands (full width opcode) */
        /* note: timing for EMC mantissa & arithmetics depends on actual data */
        break;

      case 1:
        /*
         * 10-bit memory reference w/ or w/o indirection and/or base page
         * reference
         */
        operand = opcode & 0x03ff;
        if ( ( opcode & 0x0200 ) != 0 )
        {
          operand -= 0x0400;
        }
        if ( ( opcode & 0x0400 ) != 0 )
        {
          /* current page */
          ret_string += String.format( " %04x", address + operand );
        }
        else
        {
          /* base page */
          if ( operand < 0 )
          {
            ret_string += String.format( " %04x", 0x10000 + operand );
          }
          else
          {
            if ( operand < 32 )
            {
              ret_string += String.format( " %s", registers[operand] );
            }
            else
            {
              ret_string += String.format( " %04x", operand );
            }
          }
        }

        /* indirect addressing */
        if ( ( opcode & 0x8000 ) != 0 )
        {
          timing += 6;
          ret_string += ",I";
        }

        /* base page reference */
        if ( ( opcode & 0x0400 ) == 0 )
        {
          if ( ( operand < 0 ) || ( operand > 31 ) )
          {
            ret_string += " [B]";
          }
        }
        break;

      case 2: /* 5-bit register (for EXE) */
        operand = opcode & 0x001f;
        ret_string += String.format( " %s", registers[operand] );
        if ( ( opcode & 0x8000 ) != 0 )
        {
          timing += 6;
          ret_string += ",I";
        }
        break;

      case 3: /* 6-bit signed skip field */
        operand = opcode & 0x003f;
        if ( ( opcode & 0x0020 ) != 0 )
        {
          operand -= 0x0040;
        }
        ret_string += String.format( " *+%d [%04x]", operand, address + operand );
        break;

      case 4:
        /* 6-bit signed skip field with hold/change and clear/set */
        operand = opcode & 0x003f;
        if ( ( opcode & 0x0020 ) != 0 )
        {
          operand -= 0x0040;
        }
        ret_string += String.format( " *+%d", operand );

        if ( ( opcode & 0x0080 ) != 0 )
        {
          if ( ( opcode & 0x0040 ) != 0 )
          {
            ret_string += ",S";
          }
          else
          {
            ret_string += ",C";
          }
        }

        ret_string += String.format( " [%04x]", address + operand );

        break;

      case 5:
        /*
         * 6-bit signed skip field w/ or w/o pop the IOC's PA stack (for RET)
         */
        operand = opcode & 0x3f;
        if ( ( opcode & 0x20 ) != 0 )
        {
          operand -= 0x0040;
        }
        ret_string += String.format( " %d", operand );
        if ( ( opcode & 0x40 ) != 0 )
        {
          ret_string += ",P";
        }
        break;

      case 6:
        /* 4-bit count */
        count = ( opcode & 0xf ) + 1;
        ret_string += String.format( " %d", count );
        if ( ( opcode & 0xfff0 ) == 0x7380 )
        {
          timing += count * 6; /* CLR */
        }
        else if ( ( opcode & 0xfff0 ) == 0x7380 )
        {
          timing += count * 12; /* XFR */
        }
        else
        {
          timing += count; /* all others */
        }
        break;

      case 7:
        /* 3-bit register with increment/decrement */
        operand = opcode & 0x7;
        ret_string += String.format( " %s", Asm45AnalyserTask.registers[operand] );
        if ( ( opcode & 0x0080 ) != 0 )
        {
          ret_string += ",D";
        }
        else
        {
          ret_string += ",I";
        }
        break;
    }

    // TODO
    assert timing >= 0;

    return ret_string;
  }

  /**
   * @param aTime
   * @param aByteValue
   */
  private void reportEvent( final Asm45DataSet aDataSet, final int aStartSampleIdx, final int aEndSampleIdx,
      final int aClocks, final int aBlock, final int aAddress, final int aValue, final boolean aBusGrant,
      final String aType, final String aEvent )
  {
    aDataSet.reportEvent( this.lineSMCidx, aStartSampleIdx, aEndSampleIdx, aClocks, aBlock, aAddress, aValue,
        aBusGrant, aType, aEvent );
  }
}

/* EOF */
