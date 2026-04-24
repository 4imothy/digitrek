; SPDX-License-Identifier: MIT

<CsoundSynthesizer>
<CsOptions>
-o boom.wav -W -f
</CsOptions>
<CsInstruments>
sr     = 44100
ksmps  = 32
nchnls = 1
0dbfs  = 1

instr 1
  idur  = p3
  irel  = p4

  anoise rand 1

  kcutoff expseg 800, idur * irel, 120, idur * (1 - irel), 60

  afilt moogladder anoise, kcutoff, 0.4

  aenv linen 1, 0.005, idur, idur * (1 - irel)

  out afilt * aenv * 5.0
endin
</CsInstruments>
<CsScore>
i 1  0  0.17  0.35
</CsScore>
</CsoundSynthesizer>
