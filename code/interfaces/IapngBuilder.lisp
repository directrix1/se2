(interface IapngBuilder
  (sig buildAPNG (numPlays numFrames framedata))
  (sig preparePNGs (framedata))
  (sig validateIHDR (prepdPNGs baseIHDR))
  (sig buildFrames (prepdPNGs frameNum))
  (sig buildACTL (numPlays numFrames))
  (sig buildFCTL (sequenceNum width height xOffset yOffset delayTime
                  disposeOp blendOp))
)
