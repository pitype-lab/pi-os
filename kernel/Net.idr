module Net

export
setupNetwork : IO ()
setupNetwork = do
  ------ STATUS register address = base + 0x70
  ------ RESET 
  ------ ACK
  ------ DRIVER
  ------ Select queue 0 (RX)
  ------ Select queue = 1
  ------ Fill descriptor 0: addr, len, flags
  ------ For receive descriptors, the device must be allowed to write into buffer:
  ------ Set flags = VIRTQ_DESC_F_WRITE (value = 2)

  ------ Setup avail ring: flags=0, idx=1, ring[0]=0 ---

  ------ Setup used ring initial zeros
  ------ Write queue physical addresses into MMIO
  ------ QUEUE_DESC_LOW  @ 0x40
  ------ QUEUE_AVAIL_LOW
  ------ QUEUE_USED_LOW
  ------ Finally set DRIVER_OK

  pure ()
