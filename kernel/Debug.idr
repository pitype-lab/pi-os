module Debug

import Uart

export
trace : (msg : String) -> (result : a) -> a
trace msg val = unsafePerformIO $ println msg >> pure val
