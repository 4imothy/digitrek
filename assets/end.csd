; SPDX-License-Identifier: MIT

<CsoundSynthesizer>
<CsOptions>
-o end.wav -W -f
</CsOptions>
<CsInstruments>
sr     = 44100
ksmps  = 32
nchnls = 1
0dbfs  = 1

instr 1
  idur       = p3
  ifreq_high = p4
  ifreq_low  = p5

  kfreq expseg ifreq_high, idur * 0.75, ifreq_low, idur * 0.25, ifreq_low

  kvib lfo 0.015 * kfreq, 7, 0

  asig poscil 1, kfreq + kvib

  aenv linen 1, 0, idur, idur * 0.65

  out asig * aenv * 0.35
endin
</CsInstruments>
<CsScore>
i 1  0  0.76  300  55
</CsScore>
</CsoundSynthesizer>
