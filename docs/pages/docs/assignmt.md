 **5. Assign material types to the regions**

Assign materials to regions using
the [**mregion**](commands/MREGION.md) command. This command has
similar syntax to the **[region](commands/REGION.md)** command except
that the interface should not be assigned to any material region. To
assign two materials, *mattop* and *matbot,* to the regions *top* and
*bottom:*

**mregion**/ mattop/ **le** cube **and gt** cutplane /

**mregion**/ matbot/ **le** cube **and lt** cutplane /

<img height="300" width="300" src="https://lanl.github.io/LaGriT/assets/images/Image225.gif">
