cat <<EOF
OUTPUT_FORMAT("${OUTPUT_FORMAT}")
${LIB_SEARCH_DIRS}
PROVIDE (__stack = 0x0001fffe); 
SECTIONS
{
  .text 0x00010000 : {
    *(.text)
    ${RELOCATING+ etext  =  .;}
    ${CONSTRUCTING+ __CTOR_LIST__ = .;}
    ${CONSTRUCTING+ LONG((__CTOR_END__ - __CTOR_LIST__) / 4 - 2)}
    ${CONSTRUCTING+ *(.ctors)}
    ${CONSTRUCTING+ LONG(0)}
    ${CONSTRUCTING+ __CTOR_END__ = .;}
    ${CONSTRUCTING+ __DTOR_LIST__ = .;}
    ${CONSTRUCTING+ LONG((__DTOR_END__ - __DTOR_LIST__) / 4 - 2)}
    ${CONSTRUCTING+ *(.dtors)}
    ${CONSTRUCTING+ LONG(0)}
    ${CONSTRUCTING+ __DTOR_END__ = .;}
  }
  .data : {
    *(.data)
    ${RELOCATING+ edata  =  .};
  }
  .rodata : {
    *(.rodata)
  }
  .bss : { 					
    ${RELOCATING+ __bss_start = .};
    *(.bss)
    *(COMMON)
     ${RELOCATING+ _end = ALIGN(0x2)};
     ${RELOCATING+ __end = ALIGN(0x2)};
  }
  .stab  0 ${RELOCATING+(NOLOAD)} : 
  {
    [ .stab ]
  }
  .stabstr  0 ${RELOCATING+(NOLOAD)} :
  {
    [ .stabstr ]
  }
}
EOF
