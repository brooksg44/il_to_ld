<?xml version="1.0" encoding="UTF-8"?>
<ladder>
<network>
  <contact name="X0" />
  <contact name="X1" />
  <branch>
    <contact name="X2" />
  </branch>
  <coil name="Y0" />
</network>
<network>
  <contact name="X3" />
  <contact name="X4" negated="true" />
  <coil name="Y1" />
</network>
<network>
  <contact name="X5" />
  <coil name="M0" set="true" />
</network>
<network>
  <contact name="X6" />
  <coil name="M0" reset="true" />
</network>
</ladder>