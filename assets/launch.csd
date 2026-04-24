; SPDX-License-Identifier: MIT

<CsoundSynthesizer>
<CsOptions>
-o launch.wav -W -f
</CsOptions>
<CsInstruments>
sr     = 44100
ksmps  = 32
nchnls = 1
0dbfs  = 1

instr 1
  idur     = p3
  irel     = p4
  ifreq_hi = p5
  ifreq_lo = p6

  kfreq expseg ifreq_hi, idur * irel, ifreq_lo, idur * (1 - irel), ifreq_lo

  asig poscil 1, kfreq

  adel lfo 0.005, 1.5, 0
  adel = adel + 0.009
  aflang flanger asig, adel, 0.6

  aout = (asig + aflang) * 0.5

  aenv linen 1, 0.005, idur, idur * (1 - irel)

  out aout * aenv * 0.1
endin
</CsInstruments>
<CsScore>
i 1  0  0.243  0.84  800  55
</CsScore>
</CsoundSynthesizer>
