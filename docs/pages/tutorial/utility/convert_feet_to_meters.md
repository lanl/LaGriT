# Convert Between Feet and Meters

This is a short macro that can be used to convert a mesh's node 
coordinate space from feet to meters or vice-versa.

```
# Convert feet to meters.
scale / 1 0 0 / relative / xyz / 0.3048 0.3048 0.3048

# Meters to Feet: *(1/.3048) = 3.280839895...
math / divide / -def- / xic / 1 0 0 / -def- / xic / 0.3048
math / divide / -def- / yic / 1 0 0 / -def- / yic / 0.3048
math / divide / -def- / zic / 1 0 0 / -def- / zic / 0.3048
```