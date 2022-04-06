module lg_interface
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "initialize3ddiffusionmat" :: initialize3ddiffusionmat
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "finalscalar3ddiffusionmat" :: finalscalar3ddiffusionmat
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "extractnegativecoefs" :: extractnegativecoefs
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "freenegcoefs" :: freenegcoefs
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "getmatrixsizes" :: getmatrixsizes
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "getvoronoivolumes" :: getvoronoivolumes
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "freevoronoivolumes" :: freevoronoivolumes
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "getentriesperrow" :: getentriesperrow
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "freeoccupiedcolumns" :: freeoccupiedcolumns
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "getoccupiedcolumns" :: getoccupiedcolumns
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "freeentriesperrow" :: freeentriesperrow
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "getmatrixpointers" :: getmatrixpointers
    
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "fgmvwriteopenfile" :: fgmvwriteopenfile
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "fgmvwritenodedata" :: fgmvwritenodedata
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "fgmvwritecellheader" :: fgmvwritecellheader
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "fgmvwritecelldata" :: fgmvwritecelldata
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "fgmvwritevelocitydata" :: fgmvwritevelocitydata
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, ALIAS: "fgmvwritevariableheader" :: fgmvwritevariableheader

    use iso_c_binding
    implicit none

    interface
    end interface
end module