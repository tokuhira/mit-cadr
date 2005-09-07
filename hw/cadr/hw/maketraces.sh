#!/bin/sh

(while read line; do
  echo $line
done) <<EOF
<?xml version="1.0"?>
<!-- GTKWave saved traces, version 2.0.0pre5 -->
<!-- at Fri Oct 29 08:33:19 2004 -->

<config>
 <decors>
  <decor name="default">
   <trace-state-colors>
    <named-color name="font" color="#00ff00"/>
    <named-color name="low" color="#00ff00"/>
    <named-color name="high" color="#00ff00"/>
    <named-color name="x" color="#00ff00"/>
    <named-color name="xfill" color="#008000"/>
    <named-color name="trans" color="#00ff00"/>
    <named-color name="mid" color="#00ff00"/>
    <named-color name="vtrans" color="#00ff00"/>
    <named-color name="vbox" color="#00ff00"/>
    <named-color name="unloaded" color="#800000"/>
    <named-color name="analog" color="#00ff00"/>
    <named-color name="clip" color="#ff0000"/>
    <named-color name="req" color="#ff0000"/>
    <named-color name="ack" color="#008000"/>
    <named-color name="hbox" color="#ffffff"/>
   </trace-state-colors>
  </decor>
 </decors>

 <trace-groups>
  <trace-group name="default" decor="default">
  </trace-group>
 </trace-groups>

 <pane-colors>
  <named-color name="back" color="#181818"/>
  <named-color name="grid" color="#808080"/>
  <named-color name="mark" color="#0000ff"/>
  <named-color name="umark" color="#ffff00"/>
  <named-color name="pfont" color="#ffffff"/>
 </pane-colors>

 <markers>
  <marker name="primary" time="0 s"/>
 </markers>

 <traces>
EOF

(while read -r signalname; do
#
  if [ "${signalname:0:1}" == "\\" ]; then
      signalname=${signalname:1}
  fi
#  
  if [ "$signalname" == "---" ]; then
      echo '  <separator name="">'
      echo '  </separator>'
  else
      signalname="test.cpu.$signalname"
      echo '  <trace name="'$signalname'" mode="bin" rjustified="yes">'
      echo '    <signal name="'$signalname'"/>'
      echo '  </trace>'
  fi
done) <<EOF
RESET
SSPEED0
SSPEED1
\-TPR0
\-HANG
CYCLECOMPLETED
\RD.IN.PROGRESS
\USE.MD
\-CLK3G
HI11
MCLK1A
\SET.RD.IN.PROGRESS
\-RDFINISH
\-SRCMD
NOPA
CLK3
\-CLK0
\-TPCLK
MACHRUN
SSTEP
\-SSDONE
SRUN
\-ERRHALT
\-WAIT
\-STATHALT
HI4
\MBUSY.SYNC
DESTMEM
\-MEMGRANT
MBUSY
\USE.MD
\-WAIT
\MBUSY.SYNC
NEEDFETCH
LCINC
\NEXT.INSTRD
---
A
ALU32
ALUNEG
R0
JCOND
\-JCOND
GND
CONDS2
CONDS1
CONDS0
HI4
\PGF.OR.INT.OR.SB
\PGF.OR.INT
\-VMAOK
---
A
\-ALU32
\-PFR
\-PFW
\-LVMO23
\-LVMO22
\-VMO23
\-VMO22
WRCYC
MEMSTART
---
\-RESET
MEMSTART
\-MEMSTART
MEMPREPARE
MCLK1A
\-MEMOP
CLK2C
\-MEMRD
\-MEMWR
\-IFETCH
---
\-PROMCE0
\-PROMENABLE
PC9
\BOTTOM.1K
\-IDEBUG
\-PROMDISABLED
\-IWRITEDA
---
IR48
I48
\-DESTIMOD1
CLK3A
---
IPC0
IPC1
IPC2
IPC3
---
CLK4B
NPC0
NPC1
NPC2
NPC3
---
PC0
PC1
PC2
PC3
PC4
\-PROMPC0
\-PROMPC1
\-PROMPC2
\-PROMPC3
---
\-CLOCK_RESET_A
\-BOOT
SRUN
HI1
MCLK5A
\BOOT.TRAP
---
TRAPB
\-TRAP
PCS0
PCS1
IPC1
DPC1
IR13
SPC1A
SPC0
IR12
DPC0
IPC0
NPC1
NPC0
---
PCS0
HI4
\-DFALL
DISPENB
\-JCOND
JRETF
POPJ
JCOND
\-IR6
JRET
---
DISPENB
IRDISP
\-FUNCT2
---
NOP
IR43
IR44
\-IRALU
\-IRJUMP
\-IRDISP
\-IRBYTE
---
\-TRAP
internal17
\BOOT.TRAP
\-PARERR
\-TRAPENB
---
\-PARERR
MDPARERR
MDHASPAR
\USE.MD
\-WAIT
---
RESET
\-POWER_RESET
\-BOOT1
\-BOOT2
\-TPCLK
\-CLK0
\-TPR0
MACHRUN
\-TPDONE
TPWP
---
CYCLECOMPLETED
internal11
---
\-TPR0
\-TPREND
\-TPDONE
\-CLOCK_RESET_B
\-TPCLK
\-HANG
\-ILONG
---
\-CLOCK_RESET_B
\-TPDONE
TPCLK
\-WP1
\-TPW70
\-TPW30
\-TPR0
\-TPR40
\-TPR100
\-TPR160
\-TPCLK
\-TPREND
---
EOF

echo " </traces>"
echo "</config>"
exit 0


SSTEP
\-SSDONE
SRUN
\-ERRHALT
\-WAIT
\-STATHALT
MACHRUN
RUN
---
HI4
\MBUSY.SYNC
DESTMEM
\-MEMGRANT
MBUSY
\USE.MD
\-WAIT
\MBUSY.SYNC
NEEDFETCH
LCINC
---
MEMRQ
\-MBUSY.SYNC
\MBUSY.SYNC
MCLK1A
---
MEMRQ
MBUSY
MEMSTART
\-PFR
\-PFW
HI11
---
MEMSTART
\-RESET
\-MEMSTART
MEMPREPARE
MCLK1A
---
MEMPREPARE
\-MEMOP
CLK2C
---
\-MEMOP
\-MEMRD
\-MEMWR
\-IFETCH
---
\-IFETCH
NEEDFETCH
LCINC
---
LCINC
\-RESET
CLK3C
\NEXT.INSTR
\NEXT.INSTRD
\-SPOP
internal28
internal29
IRDISP
IR24
---
\NEXT.INSTRD
---
\-CLOCK_RESET_A
SPY0
\-LDCLK
\-BOOT
RUN
\-RUN
---
internal2
\PROG.BOOT
LDMODE
SPY7
\-BOOT
SRUN
HI1
MCLK5A
\-CLOCK_RESET_A
nc75
\BOOT.TRAP

---------------------------------------------
A31A
M31B
ALUF3A
ALUF2A
ALUF1A
ALUF0A
\-CIN32
ALUMODE
ALU32
---
L31
L27
\-APASSENB
A31A
\-AMEMENB
CLK3E
---
M31B
M31
HI12
\-SPCDRIVE
\-PDLDRIVE
\-MPASSM
\-MFDRIVE
MMEM31
---
R0
\-S4
S2A
S3A
SA4
SA8
SA12
SA16
SA20
SA24
SA28
---
SA20
SA24
SA28
SA0
SA4
SA8
SA12
S3A
S2A
R12
R8
S4
R4
R0
---
M29
M30
M31
M0
M1
M2
M3
S1
S0
SA3
SA2
SA1
SA0
---
\-MPASSM
TSE4A
\-IR31
\-MPASS
MFDRIVE
\-TSE1
MFENB
---
\-SPCDRIVE
\-PDLDRIVE
\-MPASSM
\-MFDRIVE
MFDRIVE
CLK4A
CLK4C
MMEM0
M0
MF0
i_MLATCH_4A05.O7
i_SPCLCH_4A10.O7
i_PLATCH_4A04.O7
i_MF_1A25.BOUT3
---
MF0
LCDRIVE
\-MDDRIVE
MPASSL
\-OPCDRIVE
DCDRIVE
\-PPDRIVE
PIDRIVE
QDRIVE
\-VMADRIVE
\-MAPDRIVE
---
DC0
OPC0
PDLIDX0
Q0
---
i_VMEMDR_1A13.BOUT3
i_VMA_1A10.BOUT3
i_QCTL_1E12.BOUT3
i_PDLPTR_4C01.BOUT3
i_PDLPTR_4C01.AOUT3
i_OPCD_1E03.BOUT3
i_OPCD_1E03.AOUT3
i_MLATCH_4A08.BOUT3
i_MD_1A05.BOUT3
i_LC_1A20.BOUT3
---
QDRIVE
SRCQ
TSE2
---
PIDRIVE
TSE4B
SRCPDLIDX

-------------------------------------------------------------------
internal29
IRDISP
IR24
CLK3C
\NEXT.INSTR
\-SPOP
internal28
\-SRCSPCPOPREAL
SPC14
\-JCOND
JRETF
JCOND
\-IR6
\-IGNPOPJ
internal31
DP
DR
DISPENB
JRET
\-IR8
IRJUMP
IR9
---
NOP
\-TRAP
\-NOPA
---
\-TRAP
internal17
\BOOT.TRAP
\-PARERR
\-TRAPENB
---
\-CLOCK_RESET_A
\-BOOT
SRUN
RUN
\-RUN
\BOOT.TRAP

---------------------------------------------------------------------
IR43
IR44
\-INOP
\-NOP11
N
internal32
IR7
DN
DISPENB
IR7
\-JCOND
JFALSE
HI4
IWRITED
JCOND
\-IR6
IRJUMP
---
A
ALU32
ALUNEG
R0
JCOND
\-JCOND
CONDS2
CONDS1
CONDS0
HI4
\PGF.OR.INT.OR.SB
\PGF.OR.INT
\-VMAOK
---
A
\-ALU32
\-PFR
\-PFW
\-LVMO23
\-LVMO22
\-VMO23
\-VMO22
WRCYC
MEMSTART
---
A31A
M31B
ALUF3A
ALUF2A
ALUF1A
ALUF0A
\-CIN32
ALUMODE
ALU32
---
\-ALUF3
---
ALUSUB
\-MULNOP
internal36
internal37
\-IRJUMP
---
ALUADD
\-MUL
HI12
Q0
---
NOP
IR43
IR44
\-IRALU
\-IRJUMP
\-IRDISP
\-IRBYTE
\-FUNCT3
\-FUNCT2
\-FUNCT1
\-FUNCT0
IR11
IR10
NOP

----------------------------------------------------------------
\-POWER_RESET
\-CLOCK_RESET_A
\-CLOCK_RESET_B
POWER_RESET_A
\-BUSINT.LM.RESET
\PROG.BOOT
\-BOOT
\-PROG.RESET
\-BUS.RESET
\PROG.BUS.RESET

CLK1
CLK2
CLK3
\-CLK0
TPCLK
MACHRUN
\-SSDONE
SSTEP
SRUN
\-ERRHALT
\-WAIT
\-STATHALT
\-PROMENABLE
\-PROMDISABLED
PROMDISABLE
PROMDISABLED
PROMENABLE
STEP
\-TPDONE

