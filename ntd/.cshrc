## we hates csh
if ( { tty -s } ) then
  exec bash
endif
