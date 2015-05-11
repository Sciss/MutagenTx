val inBuf = Buffer.read(s, "/home/hhrutz/Documents/devel/MutagenTx/audio_work/mfcc_input.aif", 
  startFrame = 0, numFrames = 1024)

// inBuf.play()

play {
  val fftBuf  = LocalBuf(1024)
  val in      = PlayBuf.ar(1, inBuf.id)
  val fft     = FFT(buf = fftBuf, in = in, hop = 0.5, winType = -1)
  val mfcc    = MFCC(chain = fft, numCoeffs = 13)
  val rate    = SampleRate.ir / 512
  val tr      = Impulse.ar(rate)
  val count   = PulseCount.ar(tr)
  mfcc.poll(count sig_== 3, "coef")
  FreeSelf.kr(count sig_== 4)
}

val res = Vector(0.538461,
                -0.366776, -0.367721, 0.62556 , 0.44059,
                 0.21176 ,  0.132587, 0.211515, 0.30611,
                 0.148217,  0.27029 , 0.273416, 0.236873)

res.plot()
