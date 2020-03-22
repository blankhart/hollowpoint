
# Ensure device is on USB mode

adb start-server
adb tcpip 5555
adb connect 10.0.0.4
