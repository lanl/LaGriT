# tinerator.config

CONFIGURATION FILE FOR TINERATOR

## activate_virtual_framebuffer
```python
activate_virtual_framebuffer()
```

Activates a virtual (headless) framebuffer for rendering 3D
scenes via VTK.

Most critically, this function is useful when this code is being run
in a Dockerized notebook, or over a server without X forwarding.

* Requires the following packages:
  * `sudo apt-get install libgl1-mesa-dev xvfb`

