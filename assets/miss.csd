; SPDX-License-Identifier: MIT

<CsoundSynthesizer>
<CsOptions>
-o miss.wav -W -f
</CsOptions>
<CsInstruments>
sr     = 44100
ksmps  = 32
nchnls = 1
0dbfs  = 1

instr 1
  amod poscil 11310, 1131
  asig poscil 1, 800 + amod
  aenv expon 1, 0.025, 0.0001
  out asig * aenv * 0.15
endin
</CsInstruments>
<CsScore>
i 1  0  0.03
</CsScore>
</CsoundSynthesizer>
